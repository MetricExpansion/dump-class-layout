set(LLVM_LINK_COMPONENTS support)

#add_clang_executable(loop-convert
#  LoopConvert.cpp
#  )
#target_link_libraries(loop-convert
#  PRIVATE
#  clangAST
#  clangBasic
#  clangDriver
#  clangFrontend
#  clangRewriteFrontend
#  clangSerialization
#  clangStaticAnalyzerFrontend
#  clangTooling
#  clangToolingSyntax
#  )

add_llvm_library(swiftify MODULE LoopConvert.cpp PLUGIN_TOOL clang)

if(WIN32 OR CYGWIN)
    target_link_libraries(swiftify PRIVATE
            clangAST
            clangBasic
            clangDriver
            clangFrontend
            clangRewriteFrontend
            clangSerialization
            clangStaticAnalyzerFrontend
            clangTooling
            clangToolingSyntax
            )
endif()
