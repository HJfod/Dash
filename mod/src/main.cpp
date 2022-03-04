#include <Geode.hpp>
#include "utils/FileWatcher.hpp"
#include "parse/GDML.hpp"
#include "parse/HotCpp.hpp"

USE_GEODE_NAMESPACE();
using namespace std::chrono_literals;

constexpr const auto g_gdmlDir = "K:/GeodeSDK/HJ/GDML/test";
constexpr const auto g_gdmlFile= "K:/GeodeSDK/HJ/GDML/test/test.gdml";
constexpr const auto g_cppDir  = "K:/GeodeSDK/HJ/GDML/build/test/RelWithDebInfo";
constexpr const auto g_cppFile = "K:/GeodeSDK/HJ/GDML/build/test/RelWithDebInfo/HotUICpp.dll";

static bool g_useGDML = true;
static FileWatcher* g_gdmlWatcher = nullptr;
static FileWatcher* g_cppHatcher = nullptr;

void __fastcall CCDirector_willSwitchToScene(CCDirector* dir, edx_t, CCScene* scene) {
    Managed::get()->clear();
    return dir->willSwitchToScene(scene);
}

struct Refresh : public CCObject {
    void refreshCpp(float dt) {
        std::this_thread::sleep_for(100ms);
        auto r = HotCpp::get()->load(g_cppFile);
        if (!r) Log::get() << "HotCpp error: " << r.error();
    }

    void refreshGDML(float dt) {
        auto r = GDML::get()->parse(g_gdmlFile);
        if (!r) Log::get() << "GDML error: " << r.error();
    }
};

GEODE_API bool GEODE_CALL geode_load(Mod* mod) {
    g_gdmlWatcher = new FileWatcher(g_gdmlDir, [mod](ghc::filesystem::path const& path) -> void {
        CCDirector::sharedDirector()->getScheduler()->scheduleSelector(
            schedule_selector(Refresh::refreshGDML),
            CCDirector::sharedDirector()->getRunningScene(),
            0, 0, 0, false
        );
    }, [mod](std::string const& err) -> void {
        mod->log() << Severity::Error << err;
    });
    mod->log() << "Installed file watcher for " << g_gdmlDir;
    
    // g_cppHatcher = new FileWatcher(g_cppDir, [mod](ghc::filesystem::path const& path) -> void {
    //     CCDirector::sharedDirector()->getScheduler()->scheduleSelector(
    //         schedule_selector(Refresh::refreshCpp),
    //         CCDirector::sharedDirector()->getRunningScene(),
    //         0, 0, 0, false
    //     );
    // }, [mod](std::string const& err) -> void {
    //     mod->log() << Severity::Error << err;
    // });
    // mod->log() << "Installed file watcher for " << g_cppDir;

    mod->addHook(
        as<void*>(as<uintptr_t>(GetModuleHandleA("libcocos2d.dll")) + 0x100f80),
        as<void*>(&CCDirector_willSwitchToScene)
    );

    return true;
}

GEODE_API void GEODE_CALL geode_unload() {
    delete g_gdmlWatcher;
    // delete g_cppHatcher;
}
