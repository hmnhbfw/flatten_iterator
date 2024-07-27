#ifndef FLATTEN_VIEW_TESTS_COMMON_H
#define FLATTEN_VIEW_TESTS_COMMON_H

#include <array>
#include <deque>
#include <forward_list>
#include <initializer_list>
#include <iterator>
#include <list>
#include <type_traits>
#include <utility>
#include <vector>

// Macro helpers

#define CONCAT_TYPES_6(type, ...) type<CONCAT_TYPES_5(__VA_ARGS__)>
#define CONCAT_TYPES_5(type, ...) type<CONCAT_TYPES_4(__VA_ARGS__)>
#define CONCAT_TYPES_4(type, ...) type<CONCAT_TYPES_3(__VA_ARGS__)>
#define CONCAT_TYPES_3(type, ...) type<CONCAT_TYPES_2(__VA_ARGS__)>
#define CONCAT_TYPES_2(type, ...) type<CONCAT_TYPES_1(__VA_ARGS__)>
#define CONCAT_TYPES_1(type) type
#define GET_CONCAT_TYPES(_1, _2, _3, _4, _5, _6, macro, ...) macro
#define CONCAT_TYPES(...) GET_CONCAT_TYPES                                     \
        ( __VA_ARGS__                                                          \
        , CONCAT_TYPES_6                                                       \
        , CONCAT_TYPES_5                                                       \
        , CONCAT_TYPES_4                                                       \
        , CONCAT_TYPES_3                                                       \
        , CONCAT_TYPES_2                                                       \
        , CONCAT_TYPES_1                                                       \
        )(__VA_ARGS__)

// Empty structures as values

struct A {};
struct C {};
struct R {};
struct B {};
struct F {};
struct I {};

// Containers with different kinds of iterators

template <typename T>
using ContinuousC = std::vector<T>;

template <typename T>
using RandomAccessC = std::deque<T>;

template <typename T>
using BidirectC = std::list<T>;

template <typename T>
using ForwardC = std::forward_list<T>;

template <typename T>
class InputC {
    std::deque<T> q_ = nullptr;

    class Iterator {
        std::deque<T>* q_ptr_;
    public:
        using value_type = T;
        using difference_type = std::ptrdiff_t;
        using reference = T&;
        using pointer = T*;
        using iterator_category = std::input_iterator_tag;
    public:
        Iterator() noexcept = default;

        Iterator(std::deque<T>& q_ref) noexcept
                : q_ptr_(q_ref.empty() ? nullptr : &q_ref) {}

        Iterator& operator++() noexcept {
            remove_first_element();
            return *this;
        }

        void operator++(int) noexcept {
            ++*this;
        }

        value_type operator*() noexcept(std::is_nothrow_move_constructible_v<T>) {
            T front = std::move(q_ptr_->front());
            remove_first_element();
            return front;
        }

        friend bool operator==(const Iterator& lhs, const Iterator& rhs) noexcept {
            return lhs.q_ptr_ == rhs.q_ptr_;
        }

        friend bool operator!=(const Iterator& lhs, const Iterator& rhs) noexcept {
            return !(lhs == rhs);
        }

    private:
        void remove_first_element() noexcept {
            q_ptr_->pop_front();
            if (q_ptr_->empty()) {
                q_ptr_ = nullptr;
            }
        }
    };

public:
    using value_type = T;
    using iterator = Iterator;
public:
    InputC() = default;
    InputC(std::initializer_list<T> init) : q_(init) {}

    iterator begin() noexcept { return iterator(q_); }
    iterator end() noexcept { return iterator(); }
    void begin() const = delete;
    void end() const = delete;
    void cbegin() const = delete;
    void cend() const = delete;

    friend bool operator==(const InputC& lhs, const InputC& rhs) {
        return lhs.q_ == rhs.q_;
    }

    friend bool operator!=(const InputC& lhs, const InputC& rhs) {
        return !(lhs == rhs);
    }
};

// Built-in array type

template <typename T, std::size_t N>
using Array = T[N];

template <typename T>
using Array2 = T[2];

// For checking if argument-dependent lookup works properly
namespace adl {

struct ContainerWithFreeBeginEnd {
    std::array<int, 2> array;
};

inline auto begin(ContainerWithFreeBeginEnd& c) noexcept {
    return c.array.begin();
}

inline auto end(ContainerWithFreeBeginEnd& c) noexcept {
    return c.array.end();
}

class ContainerWithFriendBeginEnd {
    std::array<int, 2> array_;
public:
    friend auto begin(ContainerWithFriendBeginEnd& c) noexcept {
        return c.array_.begin();
    }

    friend auto end(ContainerWithFriendBeginEnd& c) noexcept {
        return c.array_.end();
    }
};

} // namespace adl

#endif // FLATTEN_VIEW_TESTS_COMMON_H
