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


using Traits =
#if defined(__cpp_concepts) && __cpp_concepts >= 201907L
        flatten_iterator::DefaultConceptRangeTraits
#else
        flatten_iterator::DefaultPreConceptRangeTraits;
#endif


TEST(Traits, IsRange) {
    struct VoidProducer {
        void begin() noexcept {}
        void end() noexcept {}
    };
    EXPECT_TRUE(!Traits::range<VoidProducer>);

    struct NonIteratorProducer {
        int begin() noexcept { return 42; }
        int end() noexcept { return 42; }
    };
    EXPECT_TRUE(!Traits::range<NonIteratorProducer>);

    EXPECT_TRUE(!Traits::range<int>);

    EXPECT_TRUE((Traits::range<Array<int, 42>>))
            << "Built-in array must be a range";
    EXPECT_TRUE(Traits::range<ContinuousC<int>>)
            << "Regular container must be a range";
    EXPECT_TRUE(Traits::range<std::vector<bool>>)
            << "`std::vector<bool>` must be a range";
    EXPECT_TRUE(Traits::range<adl::ContainerWithFreeBeginEnd>)
            << "Class with free `begin/end` must be a range";
    EXPECT_TRUE(Traits::range<adl::ContainerWithFriendBeginEnd>)
            << "Class with friend `begin/end` must be a range";
}


template <typename R>
using TypeList = typename flatten_iterator::details
        ::RangesAllTheWayDownTraits
                < decltype( Traits::begin(std::declval<R&>()) )
                , decltype( Traits::end(std::declval<R&>()) )
                , Traits
                >::Ranges;

template <typename... PRs>
using ExpectedTypeList = std::tuple<PRs...>;

TEST(TypeList, Ranges) {
    namespace fid = flatten_iterator::details;

    EXPECT_TRUE((std::is_same_v
            < TypeList<ContinuousC<int>>
            , ExpectedTypeList
                    < fid::PseudoRange
                            < ContinuousC<int>::iterator
                            , ContinuousC<int>::iterator
                            >
                    >
            >));

    EXPECT_TRUE((std::is_same_v
            < TypeList<std::vector<bool>>
            , ExpectedTypeList
                    < fid::PseudoRange
                            < std::vector<bool>::iterator
                            , std::vector<bool>::iterator
                            >
                    >
            >));

    EXPECT_TRUE((std::is_same_v
            < TypeList<const ContinuousC<int>>
            , ExpectedTypeList
                    < fid::PseudoRange
                            < ContinuousC<int>::const_iterator
                            , ContinuousC<int>::const_iterator
                            >
                    >
            >));

    EXPECT_TRUE((std::is_same_v
            < TypeList
                    < const ContinuousC<ContinuousC<int>>
                    >
            , ExpectedTypeList
                    < fid::PseudoRange
                            < ContinuousC<ContinuousC<int>>::const_iterator
                            , ContinuousC<ContinuousC<int>>::const_iterator
                            >
                    , fid::PseudoRange
                            < ContinuousC<int>::const_iterator
                            , ContinuousC<int>::const_iterator
                            >
                    >
            >));

    EXPECT_TRUE((std::is_same_v
            < TypeList<Array<int, 2>>
            , ExpectedTypeList
                    < fid::PseudoRange
                            < int*
                            , int*
                            >
                    >
            >));

    EXPECT_TRUE((std::is_same_v
            < TypeList<const Array<int, 2>>
            , ExpectedTypeList
                    < fid::PseudoRange
                            < const int*
                            , const int*
                            >
                    >
            >));

    EXPECT_TRUE((std::is_same_v
            < TypeList
                    < Array<Array<int, 2>, 2>
                    >
            , ExpectedTypeList
                    < fid::PseudoRange
                            < int(*)[2]
                            , int(*)[2]
                            >
                    , fid::PseudoRange
                            < int*
                            , int*
                            >
                    >
            >));

    {
        using Range1 = fid::PseudoRange<const int(*)[2], const int(*)[2]>;
        using Range2 = fid::PseudoRange<const int*, const int*>;

        EXPECT_TRUE((std::is_same_v
                < TypeList
                        < Array<const Array<int, 2>, 2>
                        >
                , ExpectedTypeList
                        < Range1
                        , Range2
                        >
                >));

        EXPECT_TRUE((std::is_same_v
                < TypeList
                        < const Array<Array<int, 2>, 2>
                        >
                , ExpectedTypeList
                        < Range1
                        , Range2
                        >
                >));

        EXPECT_TRUE((std::is_same_v
                < TypeList
                        < const Array< const Array<int, 2>, 2>
                        >
                , ExpectedTypeList
                        < Range1
                        , Range2
                        >
                >));
    }
}


// TODO: add tests
