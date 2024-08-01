#include "common.h"

#include <flatten_view/details/basic_range_traits.h>

#include <gtest/gtest.h>

using RangeTraits =
#if defined(__cpp_concepts) && __cpp_concepts >= 201907L
        flatten::details::traits::ConceptRangeTraits
#else
        flatten::details::traits::PreConceptRangeTraits
#endif
        ;

TEST(RangeTraits, IsRange) {
    struct VoidProducer {
        void begin() noexcept {}
        void end() noexcept {}
    };
    static_assert(!RangeTraits::range<VoidProducer>);

    struct NonIteratorProducer {
        int begin() noexcept { return 42; }
        int end() noexcept { return 42; }
    };
    static_assert(!RangeTraits::range<NonIteratorProducer>);

    static_assert(!RangeTraits::range<int>);

    static_assert(RangeTraits::range<Array<int, 42>>);
    static_assert(RangeTraits::range<ContinuousC<int>>);
    static_assert(RangeTraits::range<RandomAccessC<int>>);
    static_assert(RangeTraits::range<BidirectC<int>>);
    static_assert(RangeTraits::range<ForwardC<int>>);
    static_assert(RangeTraits::range<InputC<int>>);

    static_assert(RangeTraits::range<std::vector<bool>>);

    static_assert(RangeTraits::range<adl::ContainerWithFreeBeginEnd>);
    static_assert(RangeTraits::range<adl::ContainerWithFriendBeginEnd>);
}
