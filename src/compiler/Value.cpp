#include "Compiler.hpp"
#include "Value.hpp"
#include <parser/AST.hpp>

using namespace gdml;

Value::Value(Compiler& compiler) : m_compiler(compiler) {}
