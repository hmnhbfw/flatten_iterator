#include "common.h"

#include "flatten_view/flatten_view.h"
#include <flatten_view/range_traits.h>

#include <gtest/gtest.h>

// Traits

using RangeTraits = flatten::DefaultRangeTraits;

template <typename R>
using Traits = typename flatten::details
        ::RangesAllTheWayDownTraits
                < decltype( RangeTraits::begin(std::declval<R&>()) )
                , decltype( RangeTraits::end(std::declval<R&>()) )
                , RangeTraits
                >;

template <typename R>
using IteratorTraits = typename std::iterator_traits
        <decltype( RangeTraits::begin(std::declval<R&>()) )
        >;


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


#define MAKE_PSEUDO_RANGES_8(type, ...) \
        flatten::details::PseudoRange<type, type>, MAKE_PSEUDO_RANGES_7(__VA_ARGS__)
#define MAKE_PSEUDO_RANGES_7(type, ...) \
        flatten::details::PseudoRange<type, type>, MAKE_PSEUDO_RANGES_6(__VA_ARGS__)
#define MAKE_PSEUDO_RANGES_6(type, ...) \
        flatten::details::PseudoRange<type, type>, MAKE_PSEUDO_RANGES_5(__VA_ARGS__)
#define MAKE_PSEUDO_RANGES_5(type, ...) \
        flatten::details::PseudoRange<type, type>, MAKE_PSEUDO_RANGES_4(__VA_ARGS__)
#define MAKE_PSEUDO_RANGES_4(type, ...) \
        flatten::details::PseudoRange<type, type>, MAKE_PSEUDO_RANGES_3(__VA_ARGS__)
#define MAKE_PSEUDO_RANGES_3(type, ...) \
        flatten::details::PseudoRange<type, type>, MAKE_PSEUDO_RANGES_2(__VA_ARGS__)
#define MAKE_PSEUDO_RANGES_2(type, ...) \
        flatten::details::PseudoRange<type, type>, MAKE_PSEUDO_RANGES_1(__VA_ARGS__)
#define MAKE_PSEUDO_RANGES_1(type) \
        flatten::details::PseudoRange<type, type>
#define GET_MAKE_PSEUDO_RANGES(_1, _2, _3, _4, _5, _6, _7, _8, macro, ...) macro
#define MAKE_PSEUDO_RANGES(...) GET_MAKE_PSEUDO_RANGES                                             \
    ( __VA_ARGS__                                                                                  \
    , MAKE_PSEUDO_RANGES_8                                                                         \
    , MAKE_PSEUDO_RANGES_7                                                                         \
    , MAKE_PSEUDO_RANGES_6                                                                         \
    , MAKE_PSEUDO_RANGES_5                                                                         \
    , MAKE_PSEUDO_RANGES_4                                                                         \
    , MAKE_PSEUDO_RANGES_3                                                                         \
    , MAKE_PSEUDO_RANGES_2                                                                         \
    , MAKE_PSEUDO_RANGES_1                                                                         \
    )(__VA_ARGS__)

template <typename R>
using TypeList = typename Traits<R>::Ranges;

template <typename... PRs>
using ExpectedTypeList = std::tuple<PRs...>;

TEST(RangeTraits, RangesAsTypeList) {
    static_assert(std::is_same_v
            < TypeList<CONCAT_TYPES(Array2, int)>
            , ExpectedTypeList<MAKE_PSEUDO_RANGES(int*)>
            >);

    static_assert(std::is_same_v
            < TypeList<CONCAT_TYPES(const Array2, int)>
            , ExpectedTypeList<MAKE_PSEUDO_RANGES(const int*)>
            >);

    static_assert(std::is_same_v
            < TypeList<CONCAT_TYPES(ContinuousC, int)>
            , ExpectedTypeList<MAKE_PSEUDO_RANGES(ContinuousC<int>::iterator)>
            >);

    static_assert(std::is_same_v
            < TypeList<CONCAT_TYPES(const ContinuousC, int)>
            , ExpectedTypeList<MAKE_PSEUDO_RANGES(ContinuousC<int>::const_iterator)>
            >);

    static_assert(std::is_same_v
            < TypeList<CONCAT_TYPES(RandomAccessC, int)>
            , ExpectedTypeList<MAKE_PSEUDO_RANGES(RandomAccessC<int>::iterator)>
            >);

    static_assert(std::is_same_v
            < TypeList<CONCAT_TYPES(RandomAccessC, int)>
            , ExpectedTypeList<MAKE_PSEUDO_RANGES(RandomAccessC<int>::iterator)>
            >);

    static_assert(std::is_same_v
            < TypeList<CONCAT_TYPES(const RandomAccessC, int)>
            , ExpectedTypeList<MAKE_PSEUDO_RANGES(RandomAccessC<int>::const_iterator)>
            >);

    static_assert(std::is_same_v
            < TypeList<CONCAT_TYPES(BidirectC, int)>
            , ExpectedTypeList<MAKE_PSEUDO_RANGES(BidirectC<int>::iterator)>
            >);

    static_assert(std::is_same_v
            < TypeList<CONCAT_TYPES(const BidirectC, int)>
            , ExpectedTypeList<MAKE_PSEUDO_RANGES(BidirectC<int>::const_iterator)>
            >);

    static_assert(std::is_same_v
            < TypeList<CONCAT_TYPES(ForwardC, int)>
            , ExpectedTypeList<MAKE_PSEUDO_RANGES(ForwardC<int>::iterator)>
            >);

    static_assert(std::is_same_v
            < TypeList<CONCAT_TYPES(const ForwardC, int)>
            , ExpectedTypeList<MAKE_PSEUDO_RANGES(ForwardC<int>::const_iterator)>
            >);

    static_assert(std::is_same_v
            < TypeList<CONCAT_TYPES(InputC, int)>
            , ExpectedTypeList<MAKE_PSEUDO_RANGES(InputC<int>::iterator)>
            >);

    // NOTE: no checks for `const InputC<int>`, since `InputC<int>` cannot be
    // `const`, because it always changes its state while reading from it

    static_assert(std::is_same_v
            < TypeList<CONCAT_TYPES(std::vector, bool)>
            , ExpectedTypeList<MAKE_PSEUDO_RANGES(std::vector<bool>::iterator)>
            >);

    static_assert(std::is_same_v
            < TypeList<CONCAT_TYPES(const std::vector, bool)>
            , ExpectedTypeList<MAKE_PSEUDO_RANGES(std::vector<bool>::const_iterator)>
            >);

    static_assert(std::is_same_v
            < TypeList<CONCAT_TYPES(ContinuousC, ContinuousC, int)>
            , ExpectedTypeList<MAKE_PSEUDO_RANGES(
                    ContinuousC<ContinuousC<int>>::iterator,
                    ContinuousC<int>::iterator)>
            >);

    static_assert(std::is_same_v
            < TypeList<CONCAT_TYPES(const ContinuousC, ContinuousC, int)>
            , ExpectedTypeList<MAKE_PSEUDO_RANGES(
                    ContinuousC<ContinuousC<int>>::const_iterator,
                    ContinuousC<int>::const_iterator)>
            >);

    static_assert(std::is_same_v
            < TypeList<CONCAT_TYPES(Array2, Array2, int)>
            , ExpectedTypeList<MAKE_PSEUDO_RANGES(int(*)[2], int*)>
            >);

    static_assert(std::is_same_v
            < TypeList<CONCAT_TYPES(Array2, const Array2, int)>
            , ExpectedTypeList<MAKE_PSEUDO_RANGES(const int(*)[2], const int*)>
            >);

    static_assert(std::is_same_v
            < TypeList<CONCAT_TYPES(const Array2, Array2, int)>
            , ExpectedTypeList<MAKE_PSEUDO_RANGES(const int(*)[2], const int*)>
            >);

    static_assert(std::is_same_v
            < TypeList<CONCAT_TYPES(const Array2, const Array2, int)>
            , ExpectedTypeList<MAKE_PSEUDO_RANGES(const int(*)[2], const int*)>
            >);

    static_assert(std::is_same_v
            < TypeList<CONCAT_TYPES(ContinuousC, ContinuousC, ContinuousC, int)>
            , ExpectedTypeList<MAKE_PSEUDO_RANGES(
                    CONCAT_TYPES(ContinuousC, ContinuousC, ContinuousC, int)::iterator,
                    CONCAT_TYPES(ContinuousC, ContinuousC, int)::iterator,
                    CONCAT_TYPES(ContinuousC, int)::iterator)>
            >);

    static_assert(std::is_same_v
            < TypeList<CONCAT_TYPES(ContinuousC, RandomAccessC, BidirectC, ForwardC, InputC, int)>
            , ExpectedTypeList<MAKE_PSEUDO_RANGES(
                    CONCAT_TYPES(ContinuousC, RandomAccessC, BidirectC, ForwardC, InputC, int)
                            ::iterator,
                    CONCAT_TYPES(RandomAccessC, BidirectC, ForwardC, InputC, int)
                            ::iterator,
                    CONCAT_TYPES(BidirectC, ForwardC, InputC, int)
                            ::iterator,
                    CONCAT_TYPES(ForwardC, InputC, int)
                            ::iterator,
                    CONCAT_TYPES(InputC, int)
                            ::iterator)>
            >);

    static_assert(std::is_same_v
            < TypeList<CONCAT_TYPES(InputC, ForwardC, BidirectC, RandomAccessC, ContinuousC, int)>
            , ExpectedTypeList<MAKE_PSEUDO_RANGES(
                    CONCAT_TYPES(InputC, ForwardC, BidirectC, RandomAccessC, ContinuousC, int)
                            ::iterator,
                    CONCAT_TYPES(ForwardC, BidirectC, RandomAccessC, ContinuousC, int)
                            ::iterator,
                    CONCAT_TYPES(BidirectC, RandomAccessC, ContinuousC, int)
                            ::iterator,
                    CONCAT_TYPES(RandomAccessC, ContinuousC, int)
                            ::iterator,
                    CONCAT_TYPES(ContinuousC, int)
                            ::iterator)>
            >);
}


#define TEST_NESTED_TYPE(nested_type, expected_type, ...)                      \
    static_assert(std::is_same_v                                               \
            < Traits<CONCAT_TYPES(__VA_ARGS__)>::nested_type                   \
            , expected_type                                                    \
            >)

#define TEST_NESTED_TYPE_OF_DEEPEST_RANGE(type)                                                    \
    namespace test_##type {                                                                        \
                                                                                                   \
    template <typename V> using A_##type = typename IteratorTraits<Array2<V>>::type;               \
    template <typename V> using C_##type = typename IteratorTraits<ContinuousC<V>>::type;          \
    template <typename V> using R_##type = typename IteratorTraits<RandomAccessC<V>>::type;        \
    template <typename V> using B_##type = typename IteratorTraits<BidirectC<V>>::type;            \
    template <typename V> using F_##type = typename IteratorTraits<ForwardC<V>>::type;             \
    template <typename V> using I_##type = typename IteratorTraits<InputC<V>>::type;               \
                                                                                                   \
    TEST(RangeTraits, type) {                                                                      \
        TEST_NESTED_TYPE(type, A_##type<A>, Array2, A);                                            \
        TEST_NESTED_TYPE(type, C_##type<C>, ContinuousC, C);                                       \
        TEST_NESTED_TYPE(type, R_##type<R>, RandomAccessC, R);                                     \
        TEST_NESTED_TYPE(type, B_##type<B>, BidirectC, B);                                         \
        TEST_NESTED_TYPE(type, F_##type<F>, ForwardC, F);                                          \
        TEST_NESTED_TYPE(type, I_##type<I>, InputC, I);                                            \
                                                                                                   \
        using Bool_##type = IteratorTraits<std::vector<bool>>::type;                               \
        TEST_NESTED_TYPE(type, Bool_##type, std::vector, bool);                                    \
                                                                                                   \
        /* InputC, _ */                                                                            \
        TEST_NESTED_TYPE(type, I_##type<I>, InputC, InputC, I);                                    \
        TEST_NESTED_TYPE(type, F_##type<F>, InputC, ForwardC, F);                                  \
        TEST_NESTED_TYPE(type, B_##type<B>, InputC, BidirectC, B);                                 \
        TEST_NESTED_TYPE(type, R_##type<R>, InputC, RandomAccessC, R);                             \
        TEST_NESTED_TYPE(type, C_##type<C>, InputC, ContinuousC, C);                               \
                                                                                                   \
        /* ForwardC, _ */                                                                          \
        TEST_NESTED_TYPE(type, C_##type<C>, ForwardC, ContinuousC, C);                             \
        TEST_NESTED_TYPE(type, I_##type<I>, ForwardC, InputC, I);                                  \
        TEST_NESTED_TYPE(type, F_##type<F>, ForwardC, ForwardC, F);                                \
        TEST_NESTED_TYPE(type, B_##type<B>, ForwardC, BidirectC, B);                               \
        TEST_NESTED_TYPE(type, R_##type<R>, ForwardC, RandomAccessC, R);                           \
                                                                                                   \
        /* BidirectC, _ */                                                                         \
        TEST_NESTED_TYPE(type, I_##type<I>, BidirectC, InputC, I);                                 \
        TEST_NESTED_TYPE(type, F_##type<F>, BidirectC, ForwardC, F);                               \
        TEST_NESTED_TYPE(type, B_##type<B>, BidirectC, BidirectC, B);                              \
        TEST_NESTED_TYPE(type, R_##type<R>, BidirectC, RandomAccessC, R);                          \
        TEST_NESTED_TYPE(type, C_##type<C>, BidirectC, ContinuousC, C);                            \
                                                                                                   \
        /* RandomAccessC, _ */                                                                     \
        TEST_NESTED_TYPE(type, I_##type<I>, RandomAccessC, InputC, I);                             \
        TEST_NESTED_TYPE(type, F_##type<F>, RandomAccessC, ForwardC, F);                           \
        TEST_NESTED_TYPE(type, B_##type<B>, RandomAccessC, BidirectC, B);                          \
        TEST_NESTED_TYPE(type, R_##type<R>, RandomAccessC, RandomAccessC, R);                      \
        TEST_NESTED_TYPE(type, C_##type<C>, RandomAccessC, ContinuousC, C);                        \
                                                                                                   \
        /* ContinuousC, _ */                                                                       \
        TEST_NESTED_TYPE(type, I_##type<I>, ContinuousC, InputC, I);                               \
        TEST_NESTED_TYPE(type, F_##type<F>, ContinuousC, ForwardC, F);                             \
        TEST_NESTED_TYPE(type, B_##type<B>, ContinuousC, BidirectC, B);                            \
        TEST_NESTED_TYPE(type, R_##type<R>, ContinuousC, RandomAccessC, R);                        \
        TEST_NESTED_TYPE(type, C_##type<C>, ContinuousC, ContinuousC, C);                          \
                                                                                                   \
        /* ContinuousC, _, ContinuousC */                                                          \
        TEST_NESTED_TYPE(type, C_##type<I>, ContinuousC, InputC, ContinuousC, I);                  \
        TEST_NESTED_TYPE(type, C_##type<F>, ContinuousC, ForwardC, ContinuousC, F);                \
        TEST_NESTED_TYPE(type, C_##type<B>, ContinuousC, BidirectC, ContinuousC, B);               \
        TEST_NESTED_TYPE(type, C_##type<R>, ContinuousC, RandomAccessC, ContinuousC, R);           \
        TEST_NESTED_TYPE(type, C_##type<C>, ContinuousC, ContinuousC, ContinuousC, C);             \
                                                                                                   \
        /* RandomAccessC, _, RandomAccessC */                                                      \
        TEST_NESTED_TYPE(type, R_##type<I>, RandomAccessC, InputC, RandomAccessC, I);              \
        TEST_NESTED_TYPE(type, R_##type<F>, RandomAccessC, ForwardC, RandomAccessC, F);            \
        TEST_NESTED_TYPE(type, R_##type<B>, RandomAccessC, BidirectC, RandomAccessC, B);           \
        TEST_NESTED_TYPE(type, R_##type<R>, RandomAccessC, RandomAccessC, RandomAccessC, R);       \
        TEST_NESTED_TYPE(type, R_##type<C>, RandomAccessC, ContinuousC, RandomAccessC, C);         \
                                                                                                   \
        /* BidirectC, _, BidirectC */                                                              \
        TEST_NESTED_TYPE(type, B_##type<I>, BidirectC, InputC, BidirectC, I);                      \
        TEST_NESTED_TYPE(type, B_##type<F>, BidirectC, ForwardC, BidirectC, F);                    \
        TEST_NESTED_TYPE(type, B_##type<B>, BidirectC, BidirectC, BidirectC, B);                   \
        TEST_NESTED_TYPE(type, B_##type<R>, BidirectC, RandomAccessC, BidirectC, R);               \
        TEST_NESTED_TYPE(type, B_##type<C>, BidirectC, ContinuousC, BidirectC, C);                 \
                                                                                                   \
        /* ForwardC, _, ForwardC */                                                                \
        TEST_NESTED_TYPE(type, F_##type<I>, ForwardC, InputC, ForwardC, I);                        \
        TEST_NESTED_TYPE(type, F_##type<F>, ForwardC, ForwardC, ForwardC, F);                      \
        TEST_NESTED_TYPE(type, F_##type<B>, ForwardC, BidirectC, ForwardC, B);                     \
        TEST_NESTED_TYPE(type, F_##type<R>, ForwardC, RandomAccessC, ForwardC, R);                 \
        TEST_NESTED_TYPE(type, F_##type<C>, ForwardC, ContinuousC, ForwardC, C);                   \
                                                                                                   \
        /* InputC, _, InputC */                                                                    \
        TEST_NESTED_TYPE(type, I_##type<I>, InputC, InputC, InputC, I);                            \
        TEST_NESTED_TYPE(type, I_##type<F>, InputC, ForwardC, InputC, F);                          \
        TEST_NESTED_TYPE(type, I_##type<B>, InputC, BidirectC, InputC, B);                         \
        TEST_NESTED_TYPE(type, I_##type<R>, InputC, RandomAccessC, InputC, R);                     \
        TEST_NESTED_TYPE(type, I_##type<C>, InputC, ContinuousC, InputC, C);                       \
                                                                                                   \
        /* InputC <-> ForwardC <-> BidirectC <-> RandomAccessC <-> ContinuousC */                  \
        TEST_NESTED_TYPE(type, B_##type<I>, InputC, ForwardC, BidirectC, I);                       \
        TEST_NESTED_TYPE(type, R_##type<F>, ForwardC, BidirectC, RandomAccessC, F);                \
        TEST_NESTED_TYPE(type, C_##type<B>, BidirectC, RandomAccessC, ContinuousC, B);             \
        TEST_NESTED_TYPE(type, B_##type<C>, ContinuousC, RandomAccessC, BidirectC, C);             \
        TEST_NESTED_TYPE(type, F_##type<R>, RandomAccessC, BidirectC, ForwardC, R);                \
        TEST_NESTED_TYPE(type, I_##type<B>, BidirectC, ForwardC, InputC, B);                       \
        TEST_NESTED_TYPE(type, C_##type<I>,                                                        \
                         InputC, ForwardC, BidirectC, RandomAccessC, ContinuousC, I);              \
        TEST_NESTED_TYPE(type, I_##type<C>,                                                        \
                         ContinuousC, RandomAccessC, BidirectC, ForwardC, InputC, C);              \
    }                                                                                              \
                                                                                                   \
    } // namespace test_##type


TEST_NESTED_TYPE_OF_DEEPEST_RANGE(value_type)
TEST_NESTED_TYPE_OF_DEEPEST_RANGE(reference)
TEST_NESTED_TYPE_OF_DEEPEST_RANGE(pointer)


TEST(RangeTraits, difference_type) {
    using A_diff_t = IteratorTraits<Array2<int>>::difference_type;
    using C_diff_t = IteratorTraits<ContinuousC<int>>::difference_type;
    using R_diff_t = IteratorTraits<RandomAccessC<int>>::difference_type;
    using B_diff_t = IteratorTraits<BidirectC<int>>::difference_type;
    using F_diff_t = IteratorTraits<ForwardC<int>>::difference_type;
    using I_diff_t = IteratorTraits<InputC<int>>::difference_type;
    using diff_t = std::ptrdiff_t;

    TEST_NESTED_TYPE(difference_type, A_diff_t, Array2, int);
    TEST_NESTED_TYPE(difference_type, C_diff_t, ContinuousC, int);
    TEST_NESTED_TYPE(difference_type, R_diff_t, RandomAccessC, int);
    TEST_NESTED_TYPE(difference_type, B_diff_t, BidirectC, int);
    TEST_NESTED_TYPE(difference_type, F_diff_t, ForwardC, int);
    TEST_NESTED_TYPE(difference_type, I_diff_t, InputC, int);

    using VecBool_diff_t = typename std::iterator_traits
            < decltype( RangeTraits::begin(std::declval<std::vector<bool>&>()) )
            >::difference_type;

    TEST_NESTED_TYPE(difference_type, VecBool_diff_t, std::vector, bool);

    // InputC, _
    TEST_NESTED_TYPE(difference_type, diff_t, InputC, InputC, I);
    TEST_NESTED_TYPE(difference_type, diff_t, InputC, ForwardC, F);
    TEST_NESTED_TYPE(difference_type, diff_t, InputC, BidirectC, B);
    TEST_NESTED_TYPE(difference_type, diff_t, InputC, RandomAccessC, R);
    TEST_NESTED_TYPE(difference_type, diff_t, InputC, ContinuousC, C);

    // ForwardC, _
    TEST_NESTED_TYPE(difference_type, diff_t, ForwardC, InputC, I);
    TEST_NESTED_TYPE(difference_type, diff_t, ForwardC, ForwardC, F);
    TEST_NESTED_TYPE(difference_type, diff_t, ForwardC, BidirectC, B);
    TEST_NESTED_TYPE(difference_type, diff_t, ForwardC, RandomAccessC, R);
    TEST_NESTED_TYPE(difference_type, diff_t, ForwardC, ContinuousC, C);

    // BidirectC, _
    TEST_NESTED_TYPE(difference_type, diff_t, BidirectC, InputC, I);
    TEST_NESTED_TYPE(difference_type, diff_t, BidirectC, ForwardC, F);
    TEST_NESTED_TYPE(difference_type, diff_t, BidirectC, BidirectC, B);
    TEST_NESTED_TYPE(difference_type, diff_t, BidirectC, RandomAccessC, R);
    TEST_NESTED_TYPE(difference_type, diff_t, BidirectC, ContinuousC, C);

    // RandomAccessC, _
    TEST_NESTED_TYPE(difference_type, diff_t, RandomAccessC, InputC, I);
    TEST_NESTED_TYPE(difference_type, diff_t, RandomAccessC, ForwardC, F);
    TEST_NESTED_TYPE(difference_type, diff_t, RandomAccessC, BidirectC, B);
    TEST_NESTED_TYPE(difference_type, diff_t, RandomAccessC, RandomAccessC, R);
    TEST_NESTED_TYPE(difference_type, diff_t, RandomAccessC, ContinuousC, C);

    // ContinuousC, _
    TEST_NESTED_TYPE(difference_type, diff_t, ContinuousC, InputC, I);
    TEST_NESTED_TYPE(difference_type, diff_t, ContinuousC, ForwardC, F);
    TEST_NESTED_TYPE(difference_type, diff_t, ContinuousC, BidirectC, B);
    TEST_NESTED_TYPE(difference_type, diff_t, ContinuousC, RandomAccessC, R);
    TEST_NESTED_TYPE(difference_type, diff_t, ContinuousC, ContinuousC, C);

    // ContinuousC, _, ContinuousC
    TEST_NESTED_TYPE(difference_type, diff_t, ContinuousC, InputC, ContinuousC, I);
    TEST_NESTED_TYPE(difference_type, diff_t, ContinuousC, ForwardC, ContinuousC, F);
    TEST_NESTED_TYPE(difference_type, diff_t, ContinuousC, BidirectC, ContinuousC, B);
    TEST_NESTED_TYPE(difference_type, diff_t, ContinuousC, RandomAccessC, ContinuousC, R);
    TEST_NESTED_TYPE(difference_type, diff_t, ContinuousC, ContinuousC, ContinuousC, C);

    // RandomAccessC, _, RandomAccessC
    TEST_NESTED_TYPE(difference_type, diff_t, RandomAccessC, InputC, RandomAccessC, I);
    TEST_NESTED_TYPE(difference_type, diff_t, RandomAccessC, ForwardC, RandomAccessC, F);
    TEST_NESTED_TYPE(difference_type, diff_t, RandomAccessC, BidirectC, RandomAccessC, B);
    TEST_NESTED_TYPE(difference_type, diff_t, RandomAccessC, RandomAccessC, RandomAccessC, R);
    TEST_NESTED_TYPE(difference_type, diff_t, RandomAccessC, ContinuousC, RandomAccessC, C);

    // BidirectC, _, BidirectC
    TEST_NESTED_TYPE(difference_type, diff_t, BidirectC, InputC, BidirectC, I);
    TEST_NESTED_TYPE(difference_type, diff_t, BidirectC, ForwardC, BidirectC, F);
    TEST_NESTED_TYPE(difference_type, diff_t, BidirectC, BidirectC, BidirectC, B);
    TEST_NESTED_TYPE(difference_type, diff_t, BidirectC, RandomAccessC, BidirectC, R);
    TEST_NESTED_TYPE(difference_type, diff_t, BidirectC, ContinuousC, BidirectC, C);

    // ForwardC, _, ForwardC
    TEST_NESTED_TYPE(difference_type, diff_t, ForwardC, InputC, ForwardC, I);
    TEST_NESTED_TYPE(difference_type, diff_t, ForwardC, ForwardC, ForwardC, F);
    TEST_NESTED_TYPE(difference_type, diff_t, ForwardC, BidirectC, ForwardC, B);
    TEST_NESTED_TYPE(difference_type, diff_t, ForwardC, RandomAccessC, ForwardC, R);
    TEST_NESTED_TYPE(difference_type, diff_t, ForwardC, ContinuousC, ForwardC, C);

    // InputC, _, InputC
    TEST_NESTED_TYPE(difference_type, diff_t, InputC, InputC, InputC, I);
    TEST_NESTED_TYPE(difference_type, diff_t, InputC, ForwardC, InputC, F);
    TEST_NESTED_TYPE(difference_type, diff_t, InputC, BidirectC, InputC, B);
    TEST_NESTED_TYPE(difference_type, diff_t, InputC, RandomAccessC, InputC, R);
    TEST_NESTED_TYPE(difference_type, diff_t, InputC, ContinuousC, InputC, C);

    // InputC <-> ForwardC <-> BidirectC <-> RandomAccessC <-> ContinuousC
    TEST_NESTED_TYPE(difference_type, diff_t, InputC, ForwardC, BidirectC, I);
    TEST_NESTED_TYPE(difference_type, diff_t, ForwardC, BidirectC, RandomAccessC, F);
    TEST_NESTED_TYPE(difference_type, diff_t, BidirectC, RandomAccessC, ContinuousC, B);
    TEST_NESTED_TYPE(difference_type, diff_t, ContinuousC, RandomAccessC, BidirectC, C);
    TEST_NESTED_TYPE(difference_type, diff_t, RandomAccessC, BidirectC, ForwardC, R);
    TEST_NESTED_TYPE(difference_type, diff_t, BidirectC, ForwardC, InputC, C);
    TEST_NESTED_TYPE(difference_type, diff_t,
                     InputC, ForwardC, BidirectC, RandomAccessC, ContinuousC, I);
    TEST_NESTED_TYPE(difference_type, diff_t,
                     ContinuousC, RandomAccessC, BidirectC, ForwardC, InputC, C);
}


TEST(RangeTraits, iterator_category) {
    using C_tag =
#if __cplusplus >= 202002L
            std::contiguous_iterator_tag
#else
            std::random_access_iterator_tag
#endif
    ;
    using R_tag = std::random_access_iterator_tag;
    using B_tag = std::bidirectional_iterator_tag;
    using F_tag = std::forward_iterator_tag;
    using I_tag = std::input_iterator_tag;

    TEST_NESTED_TYPE(iterator_category, C_tag, ContinuousC, int);
    TEST_NESTED_TYPE(iterator_category, C_tag, Array2, int);
    TEST_NESTED_TYPE(iterator_category, R_tag, RandomAccessC, int);
    TEST_NESTED_TYPE(iterator_category, R_tag, RandomAccessC, int);
    TEST_NESTED_TYPE(iterator_category, B_tag, BidirectC, int);
    TEST_NESTED_TYPE(iterator_category, F_tag, ForwardC, int);
    TEST_NESTED_TYPE(iterator_category, I_tag, InputC, int);

    // InputC, _
    TEST_NESTED_TYPE(iterator_category, I_tag, InputC, InputC, int);
    TEST_NESTED_TYPE(iterator_category, I_tag, InputC, ForwardC, int);
    TEST_NESTED_TYPE(iterator_category, I_tag, InputC, BidirectC, int);
    TEST_NESTED_TYPE(iterator_category, I_tag, InputC, RandomAccessC, int);
    TEST_NESTED_TYPE(iterator_category, I_tag, InputC, ContinuousC, int);

    // ForwardC, _
    TEST_NESTED_TYPE(iterator_category, I_tag, ForwardC, InputC, int);
    TEST_NESTED_TYPE(iterator_category, F_tag, ForwardC, ForwardC, int);
    TEST_NESTED_TYPE(iterator_category, F_tag, ForwardC, BidirectC, int);
    TEST_NESTED_TYPE(iterator_category, F_tag, ForwardC, RandomAccessC, int);
    TEST_NESTED_TYPE(iterator_category, F_tag, ForwardC, ContinuousC, int);

    // BidirectC, _
    TEST_NESTED_TYPE(iterator_category, I_tag, BidirectC, InputC, int);
    TEST_NESTED_TYPE(iterator_category, F_tag, BidirectC, ForwardC, int);
    TEST_NESTED_TYPE(iterator_category, B_tag, BidirectC, BidirectC, int);
    TEST_NESTED_TYPE(iterator_category, B_tag, BidirectC, RandomAccessC, int);
    TEST_NESTED_TYPE(iterator_category, B_tag, BidirectC, ContinuousC, int);

    // RandomAccessC, _
    TEST_NESTED_TYPE(iterator_category, I_tag, RandomAccessC, InputC, int);
    TEST_NESTED_TYPE(iterator_category, F_tag, RandomAccessC, ForwardC, int);
    TEST_NESTED_TYPE(iterator_category, B_tag, RandomAccessC, BidirectC, int);
    TEST_NESTED_TYPE(iterator_category, B_tag, RandomAccessC, RandomAccessC, int);
    TEST_NESTED_TYPE(iterator_category, B_tag, RandomAccessC, ContinuousC, int);

    // ContinuousC, _
    TEST_NESTED_TYPE(iterator_category, I_tag, ContinuousC, InputC, int);
    TEST_NESTED_TYPE(iterator_category, F_tag, ContinuousC, ForwardC, int);
    TEST_NESTED_TYPE(iterator_category, B_tag, ContinuousC, BidirectC, int);
    TEST_NESTED_TYPE(iterator_category, B_tag, ContinuousC, RandomAccessC, int);
    TEST_NESTED_TYPE(iterator_category, B_tag, ContinuousC, ContinuousC, int);

    // ContinuousC, _, ContinuousC
    TEST_NESTED_TYPE(iterator_category, I_tag, ContinuousC, InputC, ContinuousC, int);
    TEST_NESTED_TYPE(iterator_category, F_tag, ContinuousC, ForwardC, ContinuousC, int);
    TEST_NESTED_TYPE(iterator_category, B_tag, ContinuousC, BidirectC, ContinuousC, int);
    TEST_NESTED_TYPE(iterator_category, B_tag, ContinuousC, RandomAccessC, ContinuousC, int);
    TEST_NESTED_TYPE(iterator_category, B_tag, ContinuousC, ContinuousC, ContinuousC, int);

    // RandomAccessC, _, RandomAccessC
    TEST_NESTED_TYPE(iterator_category, I_tag, RandomAccessC, InputC, RandomAccessC, int);
    TEST_NESTED_TYPE(iterator_category, F_tag, RandomAccessC, ForwardC, RandomAccessC, int);
    TEST_NESTED_TYPE(iterator_category, B_tag, RandomAccessC, BidirectC, RandomAccessC, int);
    TEST_NESTED_TYPE(iterator_category, B_tag, RandomAccessC, RandomAccessC, RandomAccessC, int);
    TEST_NESTED_TYPE(iterator_category, B_tag, RandomAccessC, ContinuousC, RandomAccessC, int);

    // BidirectC, _, BidirectC
    TEST_NESTED_TYPE(iterator_category, I_tag, BidirectC, InputC, BidirectC, int);
    TEST_NESTED_TYPE(iterator_category, F_tag, BidirectC, ForwardC, BidirectC, int);
    TEST_NESTED_TYPE(iterator_category, B_tag, BidirectC, BidirectC, BidirectC, int);
    TEST_NESTED_TYPE(iterator_category, B_tag, BidirectC, RandomAccessC, BidirectC, int);
    TEST_NESTED_TYPE(iterator_category, B_tag, BidirectC, ContinuousC, BidirectC, int);

    // ForwardC, _, ForwardC
    TEST_NESTED_TYPE(iterator_category, I_tag, ForwardC, InputC, ForwardC, int);
    TEST_NESTED_TYPE(iterator_category, F_tag, ForwardC, ForwardC, ForwardC, int);
    TEST_NESTED_TYPE(iterator_category, F_tag, ForwardC, BidirectC, ForwardC, int);
    TEST_NESTED_TYPE(iterator_category, F_tag, ForwardC, RandomAccessC, ForwardC, int);
    TEST_NESTED_TYPE(iterator_category, F_tag, ForwardC, ContinuousC, ForwardC, int);

    // InputC, _, InputC
    TEST_NESTED_TYPE(iterator_category, I_tag, InputC, InputC, InputC, int);
    TEST_NESTED_TYPE(iterator_category, I_tag, InputC, ForwardC, InputC, int);
    TEST_NESTED_TYPE(iterator_category, I_tag, InputC, BidirectC, InputC, int);
    TEST_NESTED_TYPE(iterator_category, I_tag, InputC, RandomAccessC, InputC, int);
    TEST_NESTED_TYPE(iterator_category, I_tag, InputC, ContinuousC, InputC, int);

    // InputC <-> ForwardC <-> BidirectC <-> RandomAccessC <-> ContinuousC
    TEST_NESTED_TYPE(iterator_category, I_tag, InputC, ForwardC, BidirectC, int);
    TEST_NESTED_TYPE(iterator_category, F_tag, ForwardC, BidirectC, RandomAccessC, int);
    TEST_NESTED_TYPE(iterator_category, B_tag, BidirectC, RandomAccessC, ContinuousC, int);
    TEST_NESTED_TYPE(iterator_category, B_tag, ContinuousC, RandomAccessC, BidirectC, int);
    TEST_NESTED_TYPE(iterator_category, F_tag, RandomAccessC, BidirectC, ForwardC, int);
    TEST_NESTED_TYPE(iterator_category, I_tag, BidirectC, ForwardC, InputC, int);
    TEST_NESTED_TYPE(iterator_category, I_tag,
                     InputC, ForwardC, BidirectC, RandomAccessC, ContinuousC, int);
    TEST_NESTED_TYPE(iterator_category, I_tag,
                     ContinuousC, RandomAccessC, BidirectC, ForwardC, InputC, int);
}


// TODO: add tests
