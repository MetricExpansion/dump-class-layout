// Declares clang::SyntaxOnlyAction.
#include <iostream>

#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"

#include "clang/Basic/TargetInfo.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/RecordLayout.h"

#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"

// Declares llvm::cl::extrahelp.
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/JSON.h"

using namespace clang;
using namespace clang::tooling;
using namespace llvm;

// Apply a custom category to all command-line options so that they are the
// only ones displayed.
static llvm::cl::OptionCategory MyToolCategory("my-tool options");

// CommonOptionsParser declares HelpMessage with a description of the common
// command-line options related to the compilation database and input files.
// It's nice to have this help message in all tools.
static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);

// A help message for this specific tool can be added afterwards.
static cl::extrahelp MoreHelp("\nMore help text...\n");

static cl::opt<std::string> YourOwnOption(
        "output", cl::desc("The output JSON file."), cl::ValueRequired, cl::NotHidden);

static bool isMsLayout(const ASTContext& Context)
{
    return Context.getTargetInfo().getCXXABI().isMicrosoft();
}

static void PrintOffset(raw_ostream& OS,
        CharUnits Offset, unsigned IndentLevel)
{
    OS << llvm::format("%10" PRId64 " | ", (int64_t) Offset.getQuantity());
    OS.indent(IndentLevel*2);
}

static void PrintIndentNoOffset(raw_ostream& OS, unsigned IndentLevel)
{
    OS << "           | ";
    OS.indent(IndentLevel*2);
}

static void PrintBitFieldOffset(raw_ostream& OS, CharUnits Offset,
        unsigned Begin, unsigned Width,
        unsigned IndentLevel)
{
    llvm::SmallString<10> Buffer;
    {
        llvm::raw_svector_ostream BufferOS(Buffer);
        BufferOS << Offset.getQuantity() << ':';
        if (Width==0) {
            BufferOS << '-';
        }
        else {
            BufferOS << Begin << '-' << (Begin+Width-1);
        }
    }

    OS << llvm::right_justify(Buffer, 10) << " | ";
    OS.indent(IndentLevel*2);
}

enum class RecordKind {
    Base,
    VirtualBase,
    VTablePtr,
    VBasePtr,
    Field,
};

static void WriteField(json::Array& Array, const std::string&& Type, const std::string&& Name, RecordKind FieldKind, CharUnits Offset) {
    json::Object MemberInfo;
    MemberInfo["name"] = Name;
    MemberInfo["type"] = Type;
    MemberInfo["offset"] = Offset.getQuantity();
    switch (FieldKind) {
    case RecordKind::Base:
        MemberInfo["kind"] = "base";
        break;
    case RecordKind::VirtualBase:
        MemberInfo["kind"] = "vbase";
        break;
    case RecordKind::VTablePtr:
        MemberInfo["kind"] = "vtableptr";
        break;
    case RecordKind::VBasePtr:
        MemberInfo["kind"] = "vbaseptr";
        break;
    case RecordKind::Field:
        MemberInfo["kind"] = "field";
        break;
    }
    Array.emplace_back(std::move(MemberInfo));
}

static void WriteBitfield(json::Array& Array, const std::string&& Type, const std::string&& Name, CharUnits Offset, unsigned LocalBegin, unsigned LocalWidth) {

}

static void DumpRecordLayout(
        raw_ostream& OS,
        json::Array& Array,
        const RecordDecl* RD,
        const ASTContext& C,
        CharUnits Offset,
        unsigned IndentLevel,
        const char* Description,
        bool PrintSizeInfo,
        bool IncludeVirtualBases)
{
    const ASTRecordLayout& Layout = C.getASTRecordLayout(RD);
    auto CXXRD = dyn_cast<CXXRecordDecl>(RD);

    PrintOffset(OS, Offset, IndentLevel);
    OS << C.getTypeDeclType(const_cast<RecordDecl*>(RD));
    if (Description)
        OS << ' ' << Description;
    if (CXXRD && CXXRD->isEmpty())
        OS << " (empty)";
    OS << '\n';

    IndentLevel++;

    // Dump bases.
    if (CXXRD) {
        const CXXRecordDecl* PrimaryBase = Layout.getPrimaryBase();
        bool HasOwnVFPtr = Layout.hasOwnVFPtr();
        bool HasOwnVBPtr = Layout.hasOwnVBPtr();

        // Vtable pointer.
        if (CXXRD->isDynamicClass() && !PrimaryBase && !isMsLayout(C)) {
            PrintOffset(OS, Offset, IndentLevel);
            OS << '(' << *RD << " vtable pointer)\n";
            WriteField(Array, "", "vtableptr", RecordKind::VTablePtr, Offset);
        }
        else if (HasOwnVFPtr) {
            PrintOffset(OS, Offset, IndentLevel);
            // vfptr (for Microsoft C++ ABI)
            OS << '(' << *RD << " vftable pointer)\n";
            WriteField(Array, "", "vtableptr", RecordKind::VTablePtr, Offset);
        }

        // Collect nvbases.
        SmallVector<const CXXRecordDecl*, 4> Bases;
        for (const CXXBaseSpecifier& Base: CXXRD->bases()) {
            assert(!Base.getType()->isDependentType() &&
                    "Cannot layout class with dependent bases.");
            if (!Base.isVirtual())
                Bases.push_back(Base.getType()->getAsCXXRecordDecl());
        }

        // Sort nvbases by offset.
        llvm::stable_sort(
                Bases, [&](const CXXRecordDecl* L, const CXXRecordDecl* R) {
                  return Layout.getBaseClassOffset(L)<Layout.getBaseClassOffset(R);
                });

        // Dump (non-virtual) bases
        for (const CXXRecordDecl* Base: Bases) {
            CharUnits BaseOffset = Offset+Layout.getBaseClassOffset(Base);
            json::Array Inner;
            DumpRecordLayout(OS, Inner, Base, C, BaseOffset, IndentLevel,
                    Base==PrimaryBase ? "(primary base)" : "(base)",
                    /*PrintSizeInfo=*/false,
                    /*IncludeVirtualBases=*/false);
            WriteField(Array, Base->getQualifiedNameAsString(), "Base" + std::to_string(BaseOffset.getQuantity()), RecordKind::Base, BaseOffset);
        }

        // vbptr (for Microsoft C++ ABI)
        if (HasOwnVBPtr) {
            CharUnits BaseOffset = Offset+Layout.getVBPtrOffset();
            PrintOffset(OS, Offset+Layout.getVBPtrOffset(), IndentLevel);
            OS << '(' << *RD << " vbtable pointer)\n";
            WriteField(Array, "", "vbaseptr", RecordKind::VBasePtr, BaseOffset);
        }
    }

    // Dump fields.
    uint64_t FieldNo = 0;
    for (RecordDecl::field_iterator I = RD->field_begin(),
                 E = RD->field_end(); I!=E; ++I, ++FieldNo) {
        const FieldDecl& Field = **I;
        uint64_t LocalFieldOffsetInBits = Layout.getFieldOffset(FieldNo);
        CharUnits FieldOffset =
                Offset+C.toCharUnitsFromBits(LocalFieldOffsetInBits);

        // Recursively dump fields of record type.
        if (auto RT = Field.getType()->getAs<RecordType>()) {
            json::Array Inner;
            DumpRecordLayout(OS, Inner, RT->getDecl(), C, FieldOffset, IndentLevel,
                    Field.getName().data(),
                    /*PrintSizeInfo=*/false,
                    /*IncludeVirtualBases=*/true);
            WriteField(Array, C.getTypeDeclType(const_cast<RecordDecl*>(RT->getDecl())).getAsString(), Field.getNameAsString(), RecordKind::Field, FieldOffset);
            continue;
        }

        if (Field.isBitField()) {
            uint64_t LocalFieldByteOffsetInBits = C.toBits(FieldOffset-Offset);
            unsigned Begin = LocalFieldOffsetInBits-LocalFieldByteOffsetInBits;
            unsigned Width = Field.getBitWidthValue(C);
            PrintBitFieldOffset(OS, FieldOffset, Begin, Width, IndentLevel);
            // TODO: Figure out if this is right...
//            WriteBitfield(Array, C.getTypeDeclType(const_cast<RecordDecl*>(RD)).getAsString(), Field.getName().str(), FieldOffset, Begin, Width);
        }
        else {
            PrintOffset(OS, FieldOffset, IndentLevel);
        }
        const QualType& FieldType = C.getLangOpts().DumpRecordLayoutsCanonical
                                    ? Field.getType().getCanonicalType()
                                    : Field.getType();
        OS << FieldType << ' ' << Field << '\n';
        WriteField(Array, FieldType.getAsString(), Field.getNameAsString(), RecordKind::Field, FieldOffset);
    }

    // Dump virtual bases.
    if (CXXRD && IncludeVirtualBases) {
        const ASTRecordLayout::VBaseOffsetsMapTy& VtorDisps =
                Layout.getVBaseOffsetsMap();

        for (const CXXBaseSpecifier& Base: CXXRD->vbases()) {
            assert(Base.isVirtual() && "Found non-virtual class!");
            const CXXRecordDecl* VBase = Base.getType()->getAsCXXRecordDecl();

            CharUnits VBaseOffset = Offset+Layout.getVBaseClassOffset(VBase);

            if (VtorDisps.find(VBase)->second.hasVtorDisp()) {
                PrintOffset(OS, VBaseOffset-CharUnits::fromQuantity(4), IndentLevel);
                OS << "(vtordisp for vbase " << *VBase << ")\n";
            }

            json::Array Inner;
            DumpRecordLayout(OS, Inner, VBase, C, VBaseOffset, IndentLevel,
                    VBase==Layout.getPrimaryBase() ?
                    "(primary virtual base)" : "(virtual base)",
                    /*PrintSizeInfo=*/false,
                    /*IncludeVirtualBases=*/false);
            WriteField(Array, C.getTypeDeclType(VBase).getAsString(), "VBase" + std::to_string(VBaseOffset.getQuantity()), RecordKind::VirtualBase, VBaseOffset);
        }
    }

    if (!PrintSizeInfo) return;

    PrintIndentNoOffset(OS, IndentLevel-1);
    OS << "[sizeof=" << Layout.getSize().getQuantity();
    if (CXXRD && !isMsLayout(C))
        OS << ", dsize=" << Layout.getDataSize().getQuantity();
    OS << ", align=" << Layout.getAlignment().getQuantity();
    if (C.getTargetInfo().defaultsToAIXPowerAlignment())
        OS << ", preferredalign=" << Layout.getPreferredAlignment().getQuantity();

    if (CXXRD) {
        OS << ",\n";
        PrintIndentNoOffset(OS, IndentLevel-1);
        OS << " nvsize=" << Layout.getNonVirtualSize().getQuantity();
        OS << ", nvalign=" << Layout.getNonVirtualAlignment().getQuantity();
        if (C.getTargetInfo().defaultsToAIXPowerAlignment())
            OS << ", preferrednvalign="
               << Layout.getPreferredNVAlignment().getQuantity();
    }
    OS << "]\n";
}

class FindNamedClassVisitor : public RecursiveASTVisitor<FindNamedClassVisitor> {
public:
    explicit FindNamedClassVisitor(ASTContext* Context) : Context(Context) { }

    json::Array Output;

    bool VisitCXXRecordDecl(CXXRecordDecl* Declaration)
    {
        if (!Declaration->hasDefinition() || Declaration->isDependentType()) { return true; }
        FullSourceLoc FullLocation = Context->getFullLoc(Declaration->getBeginLoc());
        if (FullLocation.isValid())
            llvm::outs() << "Found declaration for "
                         << Declaration->getQualifiedNameAsString()
                         << " at "
                         << FullLocation.getSpellingLineNumber() << ":"
                         << FullLocation.getSpellingColumnNumber() << "\n";
        json::Object ClassObject;
        json::Array FieldsArray;
        DumpRecordLayout(llvm::outs(), FieldsArray, Declaration, Declaration->getASTContext(), CharUnits(), 4, "", true, true);
        ClassObject["name"] = Context->getTypeDeclType(Declaration).getAsString();
        ClassObject["fields"] = std::move(FieldsArray);
        Output.emplace_back(std::move(ClassObject));
        return true;
    }

    bool shouldVisitTemplateInstantiations() const { return true; }

private:
    ASTContext* Context;
};

class FindNamedClassConsumer : public clang::ASTConsumer {
public:
    explicit FindNamedClassConsumer(ASTContext* Context, std::string Output) : Visitor(Context), Output(Output) {}

    virtual void HandleTranslationUnit(clang::ASTContext& Context) override
    {
        Visitor.TraverseDecl(Context.getTranslationUnitDecl());
        json::Value Value(std::move(Visitor.Output));
        llvm::outs() << Value << '\n';
        std::error_code error;
        raw_fd_stream FdStream(Output, error);
        if (!error) FdStream << Value << '\n';
    }
private:
    FindNamedClassVisitor Visitor;
    std::string Output;
};

class FindNamedClassAction : public clang::ASTFrontendAction {
public:
    FindNamedClassAction(std::string Output) : Output(Output) {};
    virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(clang::CompilerInstance& Compiler, llvm::StringRef InFile) override
    {
        return std::make_unique<FindNamedClassConsumer>(&Compiler.getASTContext(), Output);
    }
private:
    std::string Output;
};

class SwiftifyActionFactory : public FrontendActionFactory {
public:
    SwiftifyActionFactory(std::string Output) : Output(Output) {};
    std::unique_ptr<FrontendAction> create() override {
        return std::make_unique<FindNamedClassAction>(Output);
    }
private:
    std::string Output;
};

int main(int argc, const char** argv) {
    std::string Output;
    YourOwnOption.setCallback([&](std::string OutputOption) {
        Output = std::move(OutputOption);
    });
    auto ExpectedParser = CommonOptionsParser::create(argc, argv, MyToolCategory);
    if (!ExpectedParser) {
        // Fail gracefully for unsupported options.
        llvm::errs() << ExpectedParser.takeError();
        return 1;
    }
    CommonOptionsParser& OptionsParser = ExpectedParser.get();
    ClangTool Tool(OptionsParser.getCompilations(), OptionsParser.getSourcePathList());
    return Tool.run(new SwiftifyActionFactory(Output));
}
