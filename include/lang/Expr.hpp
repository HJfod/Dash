#pragma once

#include "Main.hpp"
#include "Token.hpp"
#include "Type.hpp"

namespace gdml::lang {
    struct Expr;
    struct IdentExpr;

    struct GDML_DLL IExpr {
        Range range;
        IExpr(Range const& range) : range(range) {}
        virtual ~IExpr() = default;
        
        virtual Type typecheck(UnitParser& state) const = 0;
        virtual std::string debug(size_t indent = 0) const = 0;
    };

    struct GDML_DLL AttrExpr : public IExpr {
        Rc<IdentExpr> attribute;
        Option<Rc<Expr>> value;

        AttrExpr(Rc<IdentExpr> attr, Option<Rc<Expr>> const& value, Range const& range)
            : IExpr(range), attribute(attr), value(value) {}

        static ExprResult<AttrExpr> pull(Stream& stream);
        Type typecheck(UnitParser& state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL Expr : public IExpr {
        Vec<Rc<AttrExpr>> attrs;

        Expr(Range const& range) : IExpr(range) {}

        bool hasAttribute(IdentPath const& path) const;

        static ExprResult<Expr> pull(Stream& stream);
        static ExprResult<Expr> pullPrimary(Stream& stream);
        static ExprResult<Expr> pullPrimaryNonCall(Stream& stream);
    };

    struct GDML_DLL TypeExpr : public IExpr {
        TypeExpr(Range const& range) : IExpr(range) {}
        static ExprResult<TypeExpr> pull(Stream& stream);
    };

    template <class T, class Base = Expr>
    struct AExpr : public Base, public std::enable_shared_from_this<T> {
        AExpr(Range const& range) : Base(range) {}
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
        IdentPath path;

        IdentExpr(IdentPath const& path, Range const& range)
            : AExpr(range), path(path) {}

        static ExprResult<IdentExpr> pull(Stream& stream);
        Type typecheck(UnitParser& state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL TypeIdentExpr : public AExpr<TypeIdentExpr, TypeExpr> {
        Rc<IdentExpr> expr;

        TypeIdentExpr(Rc<IdentExpr> const& expr, Range const& range)
            : AExpr(range), expr(expr) {}

        static ExprResult<TypeIdentExpr> pull(Stream& stream);
        Type typecheck(UnitParser& state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL AliasExpr : public AExpr<AliasExpr> {
        Rc<IdentExpr> alias;
        Rc<TypeExpr> type;

        AliasExpr(Rc<IdentExpr> alias, Rc<TypeExpr> type, Range const& range)
            : AExpr(range), alias(alias), type(type) {}

        static ExprResult<AliasExpr> pull(Stream& stream);
        Type typecheck(UnitParser& state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL VarDeclExpr : public AExpr<VarDeclExpr> {
        Rc<IdentExpr> ident;
        Option<Rc<TypeExpr>> type;
        Option<Rc<Expr>> value;
        bool isExtern;

        VarDeclExpr(
            Rc<IdentExpr> ident,
            Option<Rc<TypeExpr>> const& type,
            Option<Rc<Expr>> const& value,
            bool isExtern,
            Range const& range
        ) : AExpr(range), ident(ident), type(type), value(value), isExtern(isExtern) {}

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

    struct GDML_DLL ReturnExpr : public AExpr<ReturnExpr> {
        Option<Rc<Expr>> expr;
        Option<Rc<IdentExpr>> from;

        ReturnExpr(Option<Rc<Expr>> const& expr, Option<Rc<IdentExpr>> const& from, Range const& range)
            : AExpr(range), expr(expr), from(from) {}
        
        static ExprResult<ReturnExpr> pull(Stream& stream);
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
        using AsName = std::monostate;
        using Name = std::variant<std::nullopt_t, Rc<IdentExpr>, Op, AsName>;

        Name name;
        Vec<Param> params;
        Option<Rc<Expr>> body;
        Option<Rc<TypeExpr>> retType;
        bool isExtern;

        FunDeclExpr(
            Name const& name,
            Vec<Param> const& params,
            Option<Rc<Expr>> const& body,
            Option<Rc<TypeExpr>> const& retType,
            bool isExtern,
            Range const& range
        ) : AExpr(range), name(name), params(params), body(body), retType(retType), isExtern(isExtern) {}

        static ExprResult<FunDeclExpr> pull(Stream& stream);
        static ExprResult<FunDeclExpr> pullArrow(Stream& stream);
        static ParseResult<> pullParams(Vec<Param>& target, Stream& stream, bool requireTypes);
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
        Option<Rc<IdentExpr>> ident;
        Vec<Rc<PropExpr>> props;
        Vec<Rc<NodeExpr>> children;

        NodeExpr(Option<Rc<IdentExpr>> ident, Vec<Rc<PropExpr>> props, Vec<Rc<NodeExpr>> children, Range const& range)
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
        bool required;

        MemberDeclExpr(
            Ident const& name,
            Rc<TypeExpr> type,
            Option<Rc<Expr>> const& defaultValue,
            Vec<Ident> const& dependencies,
            Option<Rc<FunDeclExpr>> getter,
            Option<Rc<FunDeclExpr>> setter,
            bool required,
            Range const& range
        ) : AExpr(range), name(name), type(type),
            defaultValue(defaultValue), dependencies(dependencies),
            getter(getter), setter(setter), required(required) {}

        static ExprResult<MemberDeclExpr> pull(Stream& stream);
        Type typecheck(UnitParser& state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL NodeDeclExpr : public AExpr<NodeDeclExpr> {
        Option<Rc<IdentExpr>> ident;
        Vec<Rc<MemberDeclExpr>> members;
        Option<Rc<IdentExpr>> extends;
        bool isStruct;
        bool isExtern;

        NodeDeclExpr(
            Option<Rc<IdentExpr>> const& ident,
            Vec<Rc<MemberDeclExpr>> const& members,
            Option<Rc<IdentExpr>> const& extends,
            bool isStruct, 
            bool isExtern, 
            Range const& range
        ) : AExpr(range), ident(ident), members(members), extends(extends), isStruct(isStruct), isExtern(isExtern) {}

        static ExprResult<NodeDeclExpr> pull(Stream& stream, bool implicitStruct = false);
        Type typecheck(UnitParser& state) const override;
        std::string debug(size_t indent = 0) const override;
    };

    struct GDML_DLL EnumDeclExpr : public AExpr<EnumDeclExpr> {
        Option<Rc<IdentExpr>> ident;
        Vec<Rc<NodeDeclExpr>> variants;
        bool isExtern;

        EnumDeclExpr(
            Option<Rc<IdentExpr>> const& ident,
            Vec<Rc<NodeDeclExpr>> const& variants,
            bool isExtern,
            Range const& range
        ) : AExpr(range), ident(ident), variants(variants), isExtern(isExtern) {}

        static ExprResult<EnumDeclExpr> pull(Stream& stream);
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
        Vec<Rc<IdentExpr>> imports; // empty is *

        ImportExpr(StrLit const& from, Vec<Rc<IdentExpr>> const& imports, Range const& range)
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
