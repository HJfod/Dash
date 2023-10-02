
using Std::Modify::{@modify, @override}

@modify
struct MenuLayer {
    @override
    fun init() -> bool {
        if !MenuLayer::init() {
            return false;
        }

        print("holy shit this hook was called from an interpreted language");

        true
    }
}
