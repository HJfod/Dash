#include "AST.hpp"
#include <compiler/GDML.hpp>
#include <compiler/Compiler.hpp>

using namespace gdml;
using namespace gdml::ast;

#define THROW_TYPE_ERR(msg, hint, note) \
    return LineError { Error::TypeError,\
        msg, hint, note, start, end, source }

void PointerExpr::codegen(GDML& shared, std::ostream& stream) const noexcept {
    to->codegen(shared, stream);
    if (isReference) {
        stream << "&";
    } else {
        stream << "*";
    }
    if (qualifiers.isConst) {
        stream << "const";
    }
}

TypeCheckResult PointerExpr::compile(Compiler& compiler) const noexcept {
    GDML_TYPECHECK_CHILD(to);
    return Ok();
}


TypeCheckResult ScopeExpr::compile(Compiler& compiler) const noexcept {
    compiler.pushScope(name);
    GDML_TYPECHECK_CHILD(item);
    compiler.popScope(name);
    return Ok();
}


TypeCheckResult TypeNameExpr::compile(Compiler& compiler) const noexcept {
    if (!compiler.typeExists(name->fullName())) {
        THROW_TYPE_ERR(
            "Unknown type \"" + name->fullName() + "\"",
            "Not all C++ types are supported yet, sorry!",
            ""
        );
    }
    return Ok();
}

void TypeNameExpr::codegen(GDML& compiler, std::ostream& stream) const noexcept {
    stream << compiler.getCompiler()->getType(name->fullName())->getCodegenName();
    if (qualifiers.isConst) {
        stream << " const";
    }
}


TypeCheckResult FunctionDeclStmt::compile(Compiler& compiler) const noexcept {
    GDML_TYPECHECK_CHILD(type);
    GDML_TYPECHECK_CHILD(name);
    GDML_TYPECHECK_CHILD_O(body);
    // todo: figure out if function implementation exists
    return Ok();
}

void FunctionDeclStmt::codegen(GDML& com, std::ostream& stream) const noexcept {
    // FunctionTypeExpr::codegen returns a function pointer
    // type declaration, so can't use that
    
    // default static for file functions
    if (
        com.getRule(LanguageRule::DefaultStaticFunctions) &&
        !name->isScoped()
    ) {
        stream << "static ";
    }

    if (type->returnType.has_value()) {
        type->returnType.value()->codegen(com, stream);
        stream << " ";
    } else {
        stream << "auto ";
    }
    name->codegen(com, stream);
    stream << "(";
    bool firstArg = true;
    for (auto& param : type->parameters) {
        if (!firstArg) {
            stream << ",";
        }
        firstArg = false;
        param->codegen(com, stream);
    }
    stream << ")";
    if (body.has_value()) {
        stream << "{";
        body.value()->codegen(com, stream);
        stream << "}";
    }
}


void ImportStmt::codegen(GDML& compiler, std::ostream& stream) const noexcept {
    if (path.extension() != ".gdml") {
        if (!isRelative) {
            stream << "#include <" << path.string() << ">\n";
        }
        else if (path.is_absolute()) {
            stream << "#include \"" << path.string() << "\"\n";
        }
        else {
            stream << "#include \""
                << compiler.getInputFile()->path.parent_path().string()
                << "/" << path.string() << "\"\n";
        }
    }
}
