#pragma once

#include "lang/Main.hpp"
#include "lang/Expr.hpp"

namespace gdml {
    class Parser final : public std::enable_shared_from_this<Parser> {
    protected:
        Rc<lang::Src> m_src;
        Rc<lang::Expr> m_ast;
        std::vector<std::string> m_errors;
        cocos2d::CCNode* m_target;
        bool m_hotReloadEnabled = false;

        void populate(cocos2d::CCNode* node) const;
        bool parse();
        void reset();

    public:
        static Rc<Parser> create();
        bool loadFile(ghc::filesystem::path const& path);
        void enableHotReload(bool enable);
        void addTo(cocos2d::CCNode* node);
        void refresh();

        Rc<lang::Src> getSrc() const;
    };

    void loadGDMLFromFile(cocos2d::CCNode* node, ghc::filesystem::path const& path, bool hotReload = true);
}
