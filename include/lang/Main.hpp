#pragma once

#include <Geode/DefaultInclude.hpp>
#include <Geode/utils/file.hpp>
#include <source_location>

#ifdef GEODE_IS_WINDOWS
    #ifdef HJFOD_GDML_EXPORTING
        #define GDML_DLL __declspec(dllexport)
    #else
        #define GDML_DLL __declspec(dllimport)
    #endif
#else
    #define GDML_DLL
#endif

namespace gdml {
    // this code has not been approved by the Rust foundation

    template <class T>
    struct Box final {
    private:
        std::unique_ptr<T> value;
    
    public:
        Box() : value(nullptr) {}
        Box(T* ptr) : value(ptr) {}
        template <class... Args>
        Box(Args&&... args) : value(std::make_unique<T>(std::forward<Args>(args)...)) {}

        Box(Box&& other) : value(std::move(other.value)) {}
        Box(Box const& other) : value(std::make_unique<T>(*other.value.get())) {}

        Box& operator=(Box const& other) {
            this->value = std::make_unique<T>(*other.value.get());
            return *this;
        }
        bool operator==(Box const& other) const {
            return this->value == other.value;
        }

        T clone() const {
            return *value.get();
        }
        operator T() {
            return this->clone();
        }

        T* get() {
            return value.get();
        }
        T const* get() const {
            return value.get();
        }
        T operator*() {
            return *value.get();
        }
        T const* operator*() const {
            return value.get();
        }
        T* operator->() {
            return value.get();
        }
        T const* operator->() const {
            return value.get();
        }
    };

    template <class T>
    using Rc = std::shared_ptr<T>;

    template <class T>
    using Option = std::optional<T>;
    constexpr auto None = std::nullopt;

    template <class T>
    using Vec = std::vector<T>;

    template <class K, class V>
    using Map = std::unordered_map<K, V>;
}

namespace gdml::lang {
    template <class T = geode::impl::DefaultValue>
    using ParseResult = geode::Result<T, size_t>;

    template <class T>
    using ExprResult = ParseResult<Rc<T>>;
}
