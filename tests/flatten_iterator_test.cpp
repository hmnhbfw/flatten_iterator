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


template <typename Container, typename... Ranges>
void CompareTuplesOfRanges() noexcept {
    namespace fid = flatten_iterator::details;

    using std::begin, std::end;

    Container c = {};
    static_assert(std::is_same_v
            < decltype(fid::TupleOfRanges(begin(c), end(c)))
            , std::tuple<Ranges...>
            >);
}

TEST(TypeList, Ranges) {
    namespace fid = flatten_iterator::details;

    CompareTuplesOfRanges
            < ContinuousC<int>
            , fid::Range
                    < ContinuousC<int>::iterator
                    , ContinuousC<int>::iterator
                    >
            >();

    CompareTuplesOfRanges
            < std::vector<bool>
            , fid::Range
                    < std::vector<bool>::iterator
                    , std::vector<bool>::iterator
                    >
            >();

    CompareTuplesOfRanges
            < const ContinuousC<int>
            , fid::Range
                    < ContinuousC<int>::const_iterator
                    , ContinuousC<int>::const_iterator
                    >
            >();

    CompareTuplesOfRanges
            < ContinuousC<ContinuousC<int>>
            , fid::Range
                    < ContinuousC<ContinuousC<int>>::iterator
                    , ContinuousC<ContinuousC<int>>::iterator
                    >
            , fid::Range
                    < ContinuousC<int>::iterator
                    , ContinuousC<int>::iterator
                    >
            >();

    CompareTuplesOfRanges
            < const ContinuousC<ContinuousC<int>>
            , fid::Range
                    < ContinuousC<ContinuousC<int>>::const_iterator
                    , ContinuousC<ContinuousC<int>>::const_iterator
                    >
            , fid::Range
                    < ContinuousC<int>::const_iterator
                    , ContinuousC<int>::const_iterator
                    >
            >();

    CompareTuplesOfRanges
            < Array<int, 2>
            , fid::Range
                    < int*
                    , int*
                    >
            >();

    CompareTuplesOfRanges
            < const Array<int, 2>
            , fid::Range
                    < const int*
                    , const int*
                    >
            >();

    CompareTuplesOfRanges
            < Array<Array<int, 2>, 2>
            , fid::Range
                    < int(*)[2]
                    , int(*)[2]
                    >
            , fid::Range
                    < int*
                    , int*
                    >
            >();

    {
        using Range1 = fid::Range<const int(*)[2], const int(*)[2]>;
        using Range2 = fid::Range<const int*, const int*>;

        CompareTuplesOfRanges
                < Array<const Array<int, 2>, 2>
                , Range1
                , Range2
                >();

        CompareTuplesOfRanges
                < const Array<Array<int, 2>, 2>
                , Range1
                , Range2
                >();

        CompareTuplesOfRanges
                < const Array<const Array<int, 2>, 2>
                , Range1
                , Range2
                >();

    }
}

// TODO: add tests
