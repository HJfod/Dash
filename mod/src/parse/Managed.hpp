#pragma once

#include <Geode.hpp>

USE_GEODE_NAMESPACE();

#ifdef GDML_EXPORT
    #define GDML_DLL __declspec(dllexport)
#else
    #define GDML_DLL __declspec(dllimport)
#endif

struct Edit {
    CCPoint pos;
    float scale;
    float rotation;
    CCSize contentSize;
    ccColor3B color;
    GLubyte opacity;
    std::string text;
};

class GDML_DLL Managed {
protected:
    std::vector<CCNode*> m_nodes;
    std::unordered_map<CCNode*, Edit> m_edits;

public:
    static Managed* get();
    void touch(CCNode* node, bool recursive = false);
    void add(CCNode* node);
    void clear();
};

template<class T>
class M : public T {
public:
    template<typename... Args>
    static T* create(Args... args) {
        auto res = T::create(args...);
        Managed::get()->add(res);
        return res;
    }
};

typedef void (__stdcall* hot_ui_build_t)(CCNode* target);
#define HOT_UI_BUILD(target) void __declspec(dllexport) __stdcall hot_ui_build(target)
