
public macro @modify(this: &var Reflect::Struct) {
}

public macro @override(this: &var Reflect::Function, priority: int? = none) {}

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