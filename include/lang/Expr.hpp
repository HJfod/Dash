#pragma once

#include "Main.hpp"
#include "Token.hpp"
#include "Type.hpp"

namespace gdml::lang {
    struct GDML_DLL Expr {
        Range range;
        Expr(Range const& range) : range(range) {}
        virtual ~Expr() = default;
        
        virtual TypeCheckResult typecheck(Rc<State> state) const = 0;
        virtual std::string debug(size_t indent = 0) const = 0;

        static ExprResult<Expr> pull(Stream& stream);
        static ExprResult<Expr> pullPrimary(Stream& stream);
        static ExprResult<Expr> pullPrimaryNonCall(Stream& stream);
    };

    template <class T>
    struct AExpr : public Expr, public std::enable_shared_from_this<T> {
        AExpr(Range const& range) : Expr(range) {}
    };

    struct GDML_DLL LitExpr : public AExpr<LitExpr> {
        Lit value;
        LitExpr(Lit const& value, Range const& range)
            : AExpr(range), value(value) {}
        static ExprResult<LitExpr> pull(Stream& stream);
        TypeCheckResult typecheck(Rc<State> state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL IdentExpr : public AExpr<IdentExpr> {
        Ident ident;
        IdentExpr(Ident const& ident, Range const& range)
            : AExpr(range), ident(ident) {}
        static ExprResult<IdentExpr> pull(Stream& stream);
        TypeCheckResult typecheck(Rc<State> state) const override;
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
        TypeCheckResult typecheck(Rc<State> state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL MemberExpr : public AExpr<MemberExpr> {
        Rc<Expr> target;
        Ident member;
        MemberExpr(Rc<Expr> target, Ident const& member, Range const& range)
            : AExpr(range), target(target), member(member) {}
        static ExprResult<MemberExpr> pull(Rc<Expr> target, Stream& stream);
        TypeCheckResult typecheck(Rc<State> state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL CallExpr : public AExpr<CallExpr> {
        Rc<Expr> target;
        Vec<Rc<Expr>> args;
        CallExpr(Rc<Expr> target, Vec<Rc<Expr>> args, Range const& range)
            : AExpr(range), target(target), args(args) {}
        static ExprResult<CallExpr> pull(Rc<Expr> target, Stream& stream);
        TypeCheckResult typecheck(Rc<State> state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL PropExpr : public AExpr<PropExpr> {
        Ident prop;
        Rc<Expr> value;
        Ident node;
        PropExpr(Ident const& prop, Rc<Expr> value, Ident const& node, Range const& range)
            : AExpr(range), prop(prop), value(value), node(node) {}
        static ExprResult<PropExpr> pull(Ident const& node, Stream& stream);
        TypeCheckResult typecheck(Rc<State> state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL NodeExpr : public AExpr<NodeExpr> {
        Ident ident;
        Vec<Rc<PropExpr>> props;
        Vec<Rc<NodeExpr>> children;
        NodeExpr(Ident ident, Vec<Rc<PropExpr>> props, Vec<Rc<NodeExpr>> children, Range const& range)
            : AExpr(range), ident(ident), props(props), children(children) {}
        static ExprResult<NodeExpr> pull(Stream& stream);
        TypeCheckResult typecheck(Rc<State> state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL StructDeclExpr : public AExpr<StructDeclExpr> {
        Option<Ident> ident;
        Vec<Rc<PropExpr>> members;
        StructDeclExpr(Option<Ident> const& ident, Vec<Rc<PropExpr>> const& members, Range const& range)
            : AExpr(range), ident(ident), members(members) {}
        static ExprResult<StructDeclExpr> pull(Stream& stream);
        TypeCheckResult typecheck(Rc<State> state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL ListExpr : public AExpr<ListExpr> {
        Vec<Rc<Expr>> exprs;
        ListExpr(Vec<Rc<Expr>> const& exprs, Range const& range)
            : AExpr(range), exprs(exprs) {}
        static ExprResult<ListExpr> pull(Stream& stream);
        TypeCheckResult typecheck(Rc<State> state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL AST : public AExpr<AST> {
        Vec<Rc<Expr>> exprs;
        AST(Vec<Rc<Expr>> const& exprs, Range const& range)
            : AExpr(range), exprs(exprs) {}
        static ExprResult<AST> pull(Stream& stream);
        TypeCheckResult typecheck(Rc<State> state) const override;
        std::string debug(size_t indent = 0) const override;
    };
}
