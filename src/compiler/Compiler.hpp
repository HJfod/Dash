#pragma once

#include <utils/Types.hpp>

namespace gdml {
    class GDML;
    class Compiler;
    
    namespace ast {
        class AST;
    }
    
    class Type {
    protected:
        std::string m_name;
        std::string m_cppEquivalent;

        inline Type(
            std::string const& name,
            std::string const& cppEquivalent
        ) : m_name(name),
            m_cppEquivalent(cppEquivalent) {}

        inline Type(
            std::string const& name
        ) : Type(name, name) {}

        friend class Compiler;
    
    public:
        std::string getCodegenName() const;
    };

    class Compiler {
    protected:
        GDML& m_shared;
        ast::AST* m_ast;
        std::unordered_map<std::string, Type*> m_types;
        std::vector<std::string> m_scope;
    
        void loadBuiltinTypes();

        friend class GDML;

    public:
        Compiler(GDML& shared, ast::AST* ast);
        Error compile();

        void pushScope(std::string const& name);
        void popScope(std::string const& name);
        std::vector<std::string> const& getScope() const;

        Type* makeType(
            std::string const& name,
            std::string const& cppEquivalent
        );
        Type* makeType(
            std::string const& name
        );

        bool typeExists(std::string const& name) const;
        Type* getType(std::string const& name) const;

        void codegen(std::ostream& stream) const noexcept;
    };
}