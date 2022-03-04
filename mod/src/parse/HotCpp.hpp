#pragma once

#include "shared.hpp"

class HotCpp {
protected:
    void* m_module = nullptr;

public:
    static HotCpp* get();

    Result<> load(ghc::filesystem::path const& path);
    void unload();
};
