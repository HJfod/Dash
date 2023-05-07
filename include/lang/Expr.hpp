#pragma once

#include "Main.hpp"
#include "Token.hpp"
#include "Type.hpp"

namespace gdml::lang {
    struct GDML_DLL Expr {
        Range range;
        Expr(Range const& range) : range(range) {}
        virtual ~Expr() = default;
        
        virtual Type typecheck(UnitParser& state) const = 0;
        virtual std::string debug(size_t indent = 0) const = 0;

        static ExprResult<Expr> pull(Stream& stream);
        static ExprResult<Expr> pullPrimary(Stream& stream);
        static ExprResult<Expr> pullPrimaryNonCall(Stream& stream);
    };

    struct GDML_DLL TypeExpr : public Expr {
        TypeExpr(Range const& range) : Expr(range) {}
        static ExprResult<TypeExpr> pull(Stream& stream);
    };

    template <class T, class Base = Expr>
    struct AExpr : public Base, public std::enable_shared_from_this<T> {
        AExpr(Range const& range) : Base(range) {}
    };

    struct GDML_DLL TypeIdentExpr : public AExpr<TypeIdentExpr, TypeExpr> {
        Ident ident;

        TypeIdentExpr(Ident const& ident, Range const& range)
            : AExpr(range), ident(ident) {}

        static ExprResult<TypeIdentExpr> pull(Stream& stream);
        Type typecheck(UnitParser& state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL LitExpr : public AExpr<LitExpr> {
        Lit value;

        LitExpr(Lit const& value, Range const& range)
            : AExpr(range), value(value) {}

        static ExprResult<LitExpr> pull(Stream& stream);
        Type typecheck(UnitParser& state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL IdentExpr : public AExpr<IdentExpr> {
        Ident ident;

        IdentExpr(Ident const& ident, Range const& range)
            : AExpr(range), ident(ident) {}

        static ExprResult<IdentExpr> pull(Stream& stream);
        Type typecheck(UnitParser& state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL VarDeclExpr : public AExpr<VarDeclExpr> {
        Ident ident;
        Option<Rc<TypeExpr>> type;
        Option<Rc<Expr>> value;

        VarDeclExpr(Ident const& ident, Option<Rc<TypeExpr>> type, Option<Rc<Expr>> value, Range const& range)
            : AExpr(range), ident(ident), type(type), value(value) {}

        static ExprResult<VarDeclExpr> pull(Stream& stream);
        Type typecheck(UnitParser& state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL BinOpExpr : public AExpr<BinOpExpr> {
        Rc<Expr> lhs;
        Rc<Expr> rhs;
        Op op;

        BinOpExpr(Rc<Expr> lhs, Rc<Expr> rhs, Op op, Range const& range)
            : AExpr(range), lhs(lhs), rhs(rhs), op(op) {}

        static ExprResult<Expr> pull(Stream& stream, size_t prec, Rc<Expr> lhs);
        static ExprResult<Expr> pull(Stream& stream);
        Type typecheck(UnitParser& state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL Param {
        Ident name;
        Option<Rc<TypeExpr>> type;
        Option<Rc<Expr>> value;
        Range range;
    };

    struct GDML_DLL FunDeclExpr : public AExpr<FunDeclExpr> {
        Option<Ident> name;
        Vec<Param> params;
        Rc<Expr> body;
        Option<Rc<TypeExpr>> retType;

        FunDeclExpr(
            Option<Ident> const& name,
            Vec<Param> const& params,
            Rc<Expr> body,
            Option<Rc<TypeExpr>> const& retType,
            Range const& range
        ) : AExpr(range), name(name), params(params), body(body), retType(retType) {}

        static ExprResult<FunDeclExpr> pull(Stream& stream);
        static ExprResult<FunDeclExpr> pullArrow(Stream& stream);
        static ParseResult<> pullParams(Vec<Param>& target, Stream& stream, bool requireTypes);
        Type typecheck(UnitParser& state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL MemberExpr : public AExpr<MemberExpr> {
        Rc<Expr> target;
        Ident member;

        MemberExpr(Rc<Expr> target, Ident const& member, Range const& range)
            : AExpr(range), target(target), member(member) {}

        static ExprResult<MemberExpr> pull(Rc<Expr> target, Stream& stream);
        Type typecheck(UnitParser& state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL CallExpr : public AExpr<CallExpr> {
        Rc<Expr> target;
        Vec<Rc<Expr>> args;

        CallExpr(Rc<Expr> target, Vec<Rc<Expr>> args, Range const& range)
            : AExpr(range), target(target), args(args) {}

        static ExprResult<CallExpr> pull(Rc<Expr> target, Stream& stream);
        Type typecheck(UnitParser& state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL PropExpr : public AExpr<PropExpr> {
        Ident prop;
        Rc<Expr> value;

        PropExpr(Ident const& prop, Rc<Expr> value, Range const& range)
            : AExpr(range), prop(prop), value(value) {}

        static ExprResult<PropExpr> pull(Stream& stream);
        Type typecheck(UnitParser& state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL NodeExpr : public AExpr<NodeExpr> {
        Option<Ident> ident;
        Vec<Rc<PropExpr>> props;
        Vec<Rc<NodeExpr>> children;

        NodeExpr(Option<Ident> ident, Vec<Rc<PropExpr>> props, Vec<Rc<NodeExpr>> children, Range const& range)
            : AExpr(range), ident(ident), props(props), children(children) {}

        static ExprResult<NodeExpr> pull(Stream& stream);
        Type typecheck(UnitParser& state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL MemberDeclExpr : public AExpr<MemberDeclExpr> {
        Ident name;
        Rc<TypeExpr> type;
        Option<Rc<Expr>> defaultValue;
        Vec<Ident> dependencies;
        Option<Rc<FunDeclExpr>> getter;
        Option<Rc<FunDeclExpr>> setter;

        MemberDeclExpr(
            Ident const& name,
            Rc<TypeExpr> type,
            Option<Rc<Expr>> const& defaultValue,
            Vec<Ident> const& dependencies,
            Option<Rc<FunDeclExpr>> getter,
            Option<Rc<FunDeclExpr>> setter,
            Range const& range
        ) : AExpr(range), name(name), type(type),
            defaultValue(defaultValue), dependencies(dependencies),
            getter(getter), setter(setter) {}

        static ExprResult<MemberDeclExpr> pull(Stream& stream);
        Type typecheck(UnitParser& state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL StructDeclExpr : public AExpr<StructDeclExpr> {
        Option<Ident> ident;
        Vec<Rc<MemberDeclExpr>> members;

        StructDeclExpr(Option<Ident> const& ident, Vec<Rc<MemberDeclExpr>> const& members, Range const& range)
            : AExpr(range), ident(ident), members(members) {}

        static ExprResult<StructDeclExpr> pull(Stream& stream);
        Type typecheck(UnitParser& state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL ListExpr : public AExpr<ListExpr> {
        Vec<Rc<Expr>> exprs;

        ListExpr(Vec<Rc<Expr>> const& exprs, Range const& range)
            : AExpr(range), exprs(exprs) {}

        static ExprResult<ListExpr> pull(Stream& stream);
        Type typecheck(UnitParser& state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL ExportExpr : public AExpr<ExportExpr> {
        Rc<Expr> expr;

        ExportExpr(Rc<Expr> const& expr, Range const& range)
            : AExpr(range), expr(expr) {}

        static ExprResult<ExportExpr> pull(Stream& stream);
        Type typecheck(UnitParser& state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL ImportExpr : public AExpr<ImportExpr> {
        StrLit from;
        Vec<Ident> imports; // empty is *

        ImportExpr(StrLit const& from, Vec<Ident> const& imports, Range const& range)
            : AExpr(range), from(from), imports(imports) {}

        static ExprResult<ImportExpr> pull(Stream& stream);
        Type typecheck(UnitParser& state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL BlockExpr : public AExpr<BlockExpr> {
        Rc<Expr> expr;

        BlockExpr(Rc<Expr> expr, Range const& range)
            : AExpr(range), expr(expr) {}
        
        static ExprResult<BlockExpr> pull(Stream& stream);
        Type typecheck(UnitParser& state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL AST : public AExpr<AST> {
        Vec<Rc<Expr>> exprs;

        AST(Vec<Rc<Expr>> const& exprs, Range const& range)
            : AExpr(range), exprs(exprs) {}

        static ExprResult<AST> pull(Stream& stream);
        Type typecheck(UnitParser& state) const override;
        std::string debug(size_t indent = 0) const override;
    };
}
