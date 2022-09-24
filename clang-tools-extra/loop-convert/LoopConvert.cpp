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


static bool isMsLayout(const ASTContext &Context) {
  return Context.getTargetInfo().getCXXABI().isMicrosoft();
}

static void PrintOffset(raw_ostream &OS,
                        CharUnits Offset, unsigned IndentLevel) {
  OS << llvm::format("%10" PRId64 " | ", (int64_t)Offset.getQuantity());
  OS.indent(IndentLevel * 2);
}

static void PrintIndentNoOffset(raw_ostream &OS, unsigned IndentLevel) {
  OS << "           | ";
  OS.indent(IndentLevel * 2);
}

static void PrintBitFieldOffset(raw_ostream &OS, CharUnits Offset,
                                unsigned Begin, unsigned Width,
                                unsigned IndentLevel) {
  llvm::SmallString<10> Buffer;
  {
    llvm::raw_svector_ostream BufferOS(Buffer);
    BufferOS << Offset.getQuantity() << ':';
    if (Width == 0) {
      BufferOS << '-';
    } else {
      BufferOS << Begin << '-' << (Begin + Width - 1);
    }
  }

  OS << llvm::right_justify(Buffer, 10) << " | ";
  OS.indent(IndentLevel * 2);
}

static void DumpRecordLayout(raw_ostream &OS, const RecordDecl *RD,
                             const ASTContext &C,
                             CharUnits Offset,
                             unsigned IndentLevel,
                             const char* Description,
                             bool PrintSizeInfo,
                             bool IncludeVirtualBases) {
  const ASTRecordLayout &Layout = C.getASTRecordLayout(RD);
  auto CXXRD = dyn_cast<CXXRecordDecl>(RD);

  PrintOffset(OS, Offset, IndentLevel);
  OS << C.getTypeDeclType(const_cast<RecordDecl *>(RD));
  if (Description)
    OS << ' ' << Description;
  if (CXXRD && CXXRD->isEmpty())
    OS << " (empty)";
  OS << '\n';

  IndentLevel++;

  // Dump bases.
  if (CXXRD) {
    const CXXRecordDecl *PrimaryBase = Layout.getPrimaryBase();
    bool HasOwnVFPtr = Layout.hasOwnVFPtr();
    bool HasOwnVBPtr = Layout.hasOwnVBPtr();

    // Vtable pointer.
    if (CXXRD->isDynamicClass() && !PrimaryBase && !isMsLayout(C)) {
      PrintOffset(OS, Offset, IndentLevel);
      OS << '(' << *RD << " vtable pointer)\n";
    } else if (HasOwnVFPtr) {
      PrintOffset(OS, Offset, IndentLevel);
      // vfptr (for Microsoft C++ ABI)
      OS << '(' << *RD << " vftable pointer)\n";
    }

    // Collect nvbases.
    SmallVector<const CXXRecordDecl *, 4> Bases;
    for (const CXXBaseSpecifier &Base : CXXRD->bases()) {
      assert(!Base.getType()->isDependentType() &&
             "Cannot layout class with dependent bases.");
      if (!Base.isVirtual())
        Bases.push_back(Base.getType()->getAsCXXRecordDecl());
    }

    // Sort nvbases by offset.
    llvm::stable_sort(
        Bases, [&](const CXXRecordDecl *L, const CXXRecordDecl *R) {
          return Layout.getBaseClassOffset(L) < Layout.getBaseClassOffset(R);
        });

    // Dump (non-virtual) bases
    for (const CXXRecordDecl *Base : Bases) {
      CharUnits BaseOffset = Offset + Layout.getBaseClassOffset(Base);
      DumpRecordLayout(OS, Base, C, BaseOffset, IndentLevel,
                       Base == PrimaryBase ? "(primary base)" : "(base)",
                       /*PrintSizeInfo=*/false,
                       /*IncludeVirtualBases=*/false);
    }

    // vbptr (for Microsoft C++ ABI)
    if (HasOwnVBPtr) {
      PrintOffset(OS, Offset + Layout.getVBPtrOffset(), IndentLevel);
      OS << '(' << *RD << " vbtable pointer)\n";
    }
  }

  // Dump fields.
  uint64_t FieldNo = 0;
  for (RecordDecl::field_iterator I = RD->field_begin(),
                                  E = RD->field_end(); I != E; ++I, ++FieldNo) {
    const FieldDecl &Field = **I;
    uint64_t LocalFieldOffsetInBits = Layout.getFieldOffset(FieldNo);
    CharUnits FieldOffset =
        Offset + C.toCharUnitsFromBits(LocalFieldOffsetInBits);

    // Recursively dump fields of record type.
    if (auto RT = Field.getType()->getAs<RecordType>()) {
      DumpRecordLayout(OS, RT->getDecl(), C, FieldOffset, IndentLevel,
                       Field.getName().data(),
                       /*PrintSizeInfo=*/false,
                       /*IncludeVirtualBases=*/true);
      continue;
    }

    if (Field.isBitField()) {
      uint64_t LocalFieldByteOffsetInBits = C.toBits(FieldOffset - Offset);
      unsigned Begin = LocalFieldOffsetInBits - LocalFieldByteOffsetInBits;
      unsigned Width = Field.getBitWidthValue(C);
      PrintBitFieldOffset(OS, FieldOffset, Begin, Width, IndentLevel);
    } else {
      PrintOffset(OS, FieldOffset, IndentLevel);
    }
    const QualType &FieldType = C.getLangOpts().DumpRecordLayoutsCanonical
                                    ? Field.getType().getCanonicalType()
                                    : Field.getType();
    OS << FieldType << ' ' << Field << '\n';
  }

  // Dump virtual bases.
  if (CXXRD && IncludeVirtualBases) {
    const ASTRecordLayout::VBaseOffsetsMapTy &VtorDisps =
        Layout.getVBaseOffsetsMap();

    for (const CXXBaseSpecifier &Base : CXXRD->vbases()) {
      assert(Base.isVirtual() && "Found non-virtual class!");
      const CXXRecordDecl *VBase = Base.getType()->getAsCXXRecordDecl();

      CharUnits VBaseOffset = Offset + Layout.getVBaseClassOffset(VBase);

      if (VtorDisps.find(VBase)->second.hasVtorDisp()) {
        PrintOffset(OS, VBaseOffset - CharUnits::fromQuantity(4), IndentLevel);
        OS << "(vtordisp for vbase " << *VBase << ")\n";
      }

      DumpRecordLayout(OS, VBase, C, VBaseOffset, IndentLevel,
                       VBase == Layout.getPrimaryBase() ?
                                                        "(primary virtual base)" : "(virtual base)",
                       /*PrintSizeInfo=*/false,
                       /*IncludeVirtualBases=*/false);
    }
  }

  if (!PrintSizeInfo) return;

  PrintIndentNoOffset(OS, IndentLevel - 1);
  OS << "[sizeof=" << Layout.getSize().getQuantity();
  if (CXXRD && !isMsLayout(C))
    OS << ", dsize=" << Layout.getDataSize().getQuantity();
  OS << ", align=" << Layout.getAlignment().getQuantity();
  if (C.getTargetInfo().defaultsToAIXPowerAlignment())
    OS << ", preferredalign=" << Layout.getPreferredAlignment().getQuantity();

  if (CXXRD) {
    OS << ",\n";
    PrintIndentNoOffset(OS, IndentLevel - 1);
    OS << " nvsize=" << Layout.getNonVirtualSize().getQuantity();
    OS << ", nvalign=" << Layout.getNonVirtualAlignment().getQuantity();
    if (C.getTargetInfo().defaultsToAIXPowerAlignment())
      OS << ", preferrednvalign="
         << Layout.getPreferredNVAlignment().getQuantity();
  }
  OS << "]\n";
}


class FindNamedClassVisitor
    : public RecursiveASTVisitor<FindNamedClassVisitor> {
public:
  explicit FindNamedClassVisitor(ASTContext *Context)
      : Context(Context) {}

  bool VisitCXXRecordDecl(CXXRecordDecl *Declaration) {
    if (!Declaration->hasDefinition() || Declaration->isDependentType()) { return true; }
    FullSourceLoc FullLocation = Context->getFullLoc(Declaration->getBeginLoc());
    if (FullLocation.isValid())
      std::cout << "Found declaration for "
                   << Declaration->getQualifiedNameAsString()
                   << " at "
                   << FullLocation.getSpellingLineNumber() << ":"
                   << FullLocation.getSpellingColumnNumber() << "\n";
    const clang::ASTRecordLayout &typeLayout( Declaration->getASTContext().getASTRecordLayout(Declaration));
    DumpRecordLayout(llvm::outs(), Declaration, Declaration->getASTContext(), CharUnits(), 4, "", true, true);
    for(clang::RecordDecl::field_iterator fit = Declaration->field_begin(); fit != Declaration->field_end(); fit++) {
      const clang::QualType qualType = fit->getType().getLocalUnqualifiedType().getCanonicalType();
      size_t fieldOffset = typeLayout.getFieldOffset(fit->getFieldIndex());
      std::cout << "member '" << qualType.getAsString() << "' with " << fieldOffset/8 << " bytes offset\n";
    }
    return true;
  }

  bool shouldVisitTemplateInstantiations() const { return true; }

private:
  ASTContext *Context;
};

class FindNamedClassConsumer : public clang::ASTConsumer {
public:
  explicit FindNamedClassConsumer(ASTContext *Context)
      : Visitor(Context) {}

  virtual void HandleTranslationUnit(clang::ASTContext &Context) override {
    Visitor.TraverseDecl(Context.getTranslationUnitDecl());
  }
private:
  FindNamedClassVisitor Visitor;
};

class FindNamedClassAction : public clang::ASTFrontendAction {
public:
  virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
      clang::CompilerInstance &Compiler, llvm::StringRef InFile) override {
    return std::make_unique<FindNamedClassConsumer>(&Compiler.getASTContext());
  }
};


int main(int argc, const char **argv) {
  auto ExpectedParser = CommonOptionsParser::create(argc, argv, MyToolCategory);
  if (!ExpectedParser) {
    // Fail gracefully for unsupported options.
    llvm::errs() << ExpectedParser.takeError();
    return 1;
  }
  CommonOptionsParser& OptionsParser = ExpectedParser.get();
  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());
  return Tool.run(newFrontendActionFactory<FindNamedClassAction>().get());
}
