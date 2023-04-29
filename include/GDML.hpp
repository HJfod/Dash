#pragma once

#include "lang/Main.hpp"
#include "lang/Expr.hpp"

namespace gdml {
    class Parser final : public std::enable_shared_from_this<Parser> {
    protected:
        Rc<lang::Expr> m_ast;
        bool m_hotReloadEnabled = false;

    public:
        static Rc<Parser> create();
        Result<> loadFile(ghc::filesystem::path const& path);
        void enableHotReload(bool enable);
        void addTo(cocos2d::CCNode* node);
    };

    void loadGDMLFromFile(cocos2d::CCNode* node, ghc::filesystem::path const& path, bool hotReload = true);
}
