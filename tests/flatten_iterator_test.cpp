#include <flatten_iterator/flatten_iterator.h>
#include <flatten_iterator/generator/generator.h>

#include <gtest/gtest.h>

#include <array>
#include <deque>
#include <forward_list>
#include <iterator>
#include <list>
#include <vector>

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
using InputC = std::istream_iterator<T>;

// Built-in array type
template <typename T, std::size_t N>
using Array = T[N];

// For checking if argument-dependent lookup works properly
namespace adl {

struct ContainerWithFreeBeginEnd {
    std::array<int, 2> array;
};

auto begin(ContainerWithFreeBeginEnd& c) noexcept {
    return c.array.begin();
}

auto end(ContainerWithFreeBeginEnd& c) noexcept {
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


TEST(Traits, IsIterable) {
    namespace fidt = flatten_iterator::details::traits;

    struct VoidProducer {
        void begin() noexcept {}
        void end() noexcept {}
    };
    static_assert(!fidt::is_iterable_v<VoidProducer>);

    struct NonIteratorProducer {
        int begin() noexcept { return 42; }
        int end() noexcept { return 42; }
    };
    static_assert(!fidt::is_iterable_v<NonIteratorProducer>);

    static_assert(!fidt::is_iterable_v<int>);

    static_assert(fidt::is_iterable_v<Array<int, 42>>,
                  "Built-in array must be iterable");
    static_assert(fidt::is_iterable_v<ContinuousC<int>>,
                  "Regular container must be iterable");
    static_assert(fidt::is_iterable_v<std::vector<bool>>,
                  "`std::vector<bool>` must  be iterable");
    static_assert(fidt::is_iterable_v<adl::ContainerWithFreeBeginEnd>,
                  "Class with free `begin/end` must be iterable");
    static_assert(fidt::is_iterable_v<adl::ContainerWithFriendBeginEnd>,
                  "Class with friend `begin/end` must be iterable");
}

// TODO: add tests
