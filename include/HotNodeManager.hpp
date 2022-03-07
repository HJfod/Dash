#pragma once

#include "macros.hpp"

namespace gdml {

    struct Edit {
        CCPoint pos;
        float scale;
        float scaleX;
        float scaleY;
        float rotation;
        float rotationX;
        float rotationY;
        float skewX;
        float skewY;
        CCSize contentSize;
        CCPoint anchorPoint;
        ccColor3B color;
        GLubyte opacity;
        std::string text;
        int zOrder;
    };

    class GDML_DLL HotNodeManager {
    protected:
        std::vector<CCNode*> m_nodes;
        std::unordered_map<CCNode*, Edit> m_edits;
        size_t m_pushedScenes = 0;

    public:
        static HotNodeManager* get();
        void touch(CCNode* node, bool recursive = false);
        void add(CCNode* node);
        void push(CCScene* scene);
        void clear();
    };

}
