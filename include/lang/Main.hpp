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
    using Option = std::optional<T>;
    constexpr auto None = std::nullopt;

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
        Box(Box const& other)
            : value(other.value.get() ? 
                std::make_unique<T>(*other.value.get()) : 
                nullptr
            ) {}

        Box& operator=(Box const& other) {
            if (other.value.get()) {
                this->value = std::make_unique<T>(*other.value.get());
            }
            else {
                this->value = nullptr;
            }
            return *this;
        }
        bool operator==(Box const& other) const {
            return this->value == other.value;
        }

        Option<T> tryClone() const {
            if (value.get()) {
                return *value.get();
            }
            return None;
        }
        T clone() const {
            return this->tryClone().value_or(T());
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

    using String = std::string;

    template <class T>
    using Rc = std::shared_ptr<T>;

    template <class T>
    using Weak = std::weak_ptr<T>;

    template <class T>
    using Owned = std::unique_ptr<T>;

    using Path = ghc::filesystem::path;

    template <class T>
    using Vec = std::vector<T>;

    template <class K, class V>
    using Map = std::unordered_map<K, V>;

    template <class K>
    using Set = std::unordered_set<K>;

    template <class A, class B>
    using Pair = std::pair<A, B>;
}

namespace gdml::lang {
    template <class T = geode::impl::DefaultValue>
    using ParseResult = geode::Result<T, size_t>;

    template <class T>
    using ExprResult = ParseResult<Rc<T>>;
}
