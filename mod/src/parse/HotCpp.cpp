#include "HotCpp.hpp"

HotCpp* HotCpp::get() {
    static auto inst = new HotCpp();
    return inst;
}

Result<> HotCpp::load(ghc::filesystem::path const& path) {
    Managed::get()->clear();
    this->unload();

    auto npath = path;
    npath.replace_filename("to_load.dll");
    if (!ghc::filesystem::copy_file(path, npath, ghc::filesystem::copy_options::overwrite_existing)) {
        return Err<>("Can't copy DLL");
    }

    auto load = LoadLibraryW(npath.wstring().c_str());
    if (!load) {
        return Err<>("LoadLibraryW error: " + std::to_string(GetLastError()));
    }
    m_module = load;

    auto buildFunc = as<hot_ui_build_t>(GetProcAddress(load, "?hot_ui_build@@YGXPAVCCNode@cocos2d@@@Z"));
    if (!buildFunc) {
        return Err<>("Build function not found");
    }

    buildFunc(CCDirector::sharedDirector()->getRunningScene());

    return Ok<>();
}

void HotCpp::unload() {
    if (m_module) {
        FreeLibrary(as<HMODULE>(m_module));
        m_module = nullptr;
    }
}
