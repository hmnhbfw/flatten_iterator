#include <flatten_iterator/flatten_iterator.h>
#include <flatten_iterator/generator/generator.h>

#include <array>
#include <deque>
#include <forward_list>
#include <iterator>
#include <list>
#include <type_traits>
#include <utility>
#include <vector>

#include <gtest/gtest.h>

// Macro helpers

#define MAKE_TYPES_WRAPPED_6(t1, t2, t3, t4, t5, t6) t1<t2<t3<t4<t5<t6>>>>>
#define MAKE_TYPES_WRAPPED_5(t1, t2, t3, t4, t5) t1<t2<t3<t4<t5>>>>
#define MAKE_TYPES_WRAPPED_4(t1, t2, t3, t4) t1<t2<t3<t4>>>
#define MAKE_TYPES_WRAPPED_3(t1, t2, t3) t1<t2<t3>>
#define MAKE_TYPES_WRAPPED_2(t1, t2) t1<t2>
#define MAKE_TYPES_WRAPPED_1(t1) t1
#define GET_MAKE_TYPES_WRAPPED(_1, _2, _3, _4, _5, _6, macro, ...) macro
#define MAKE_TYPES_WRAPPED(...) GET_MAKE_TYPES_WRAPPED \
        ( __VA_ARGS__                                  \
        , MAKE_TYPES_WRAPPED_6                         \
        , MAKE_TYPES_WRAPPED_5                         \
        , MAKE_TYPES_WRAPPED_4                         \
        , MAKE_TYPES_WRAPPED_3                         \
        , MAKE_TYPES_WRAPPED_2                         \
        , MAKE_TYPES_WRAPPED_1                         \
        )(__VA_ARGS__)

#define TEST_NESTED_TYPE(nested_type, expected_type, ...)          \
    static_assert(std::is_same_v                                   \
            < Traits<MAKE_TYPES_WRAPPED(__VA_ARGS__)>::nested_type \
            , expected_type                                        \
            >)

// Traits

using RangeTraits =
#if defined(__cpp_concepts) && __cpp_concepts >= 201907L
        flatten::DefaultConceptRangeTraits
#else
        flatten::DefaultPreConceptRangeTraits;
#endif

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
    std::deque<T> q_;

    class Iterator {
        std::deque<T>* q_ptr_;
        bool is_singular_;
    public:
        using difference_type = std::ptrdiff_t;
        using value_type = T;
        using reference = T&;
        using pointer = T*;
        using iterator_category = std::input_iterator_tag;
    public:
        Iterator() noexcept
                : q_ptr_(nullptr), is_singular_(true) {}

        Iterator(std::deque<T>& q_ref) noexcept
                : q_ptr_(q_ref.empty() ? nullptr : &q_ref)
                , is_singular_(q_ref.empty() ? true : false) {}

        Iterator& operator++() noexcept {
            next();
            return *this;
        }

        void operator++(int) noexcept {
            ++*this;
        }

        value_type operator*() noexcept(std::is_nothrow_move_constructible_v<T>) {
            T front = std::move(q_ptr_->front());
            next();
            return front;
        }

        friend bool operator==(const Iterator& lhs, const Iterator& rhs) {
            return std::tie(lhs.q_ptr_, lhs.is_singular_) == std::tie(rhs.q_ptr_, rhs.is_singular_);
        }

        friend bool operator!=(const Iterator& lhs, const Iterator& rhs) {
            return !(lhs == rhs);
        }

    private:
        void next() noexcept {
            q_ptr_->pop_front();
            if (q_ptr_->empty()) {
                q_ptr_ = nullptr;
                is_singular_ = true;
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


template <typename R>
using TypeList = typename Traits<R>::Ranges;

template <typename... PRs>
using ExpectedTypeList = std::tuple<PRs...>;

TEST(RangeTraits, RangesAsTypeList) {
    namespace fd = flatten::details;

    static_assert(std::is_same_v
            < TypeList<Array2<int>>
            , ExpectedTypeList
                    < fd::PseudoRange
                            < int*
                            , int*
                            >
                    >
            >);

    static_assert(std::is_same_v
            < TypeList<const Array2<int>>
            , ExpectedTypeList
                    < fd::PseudoRange
                            < const int*
                            , const int*
                            >
                    >
            >);

    static_assert(std::is_same_v
            < TypeList<ContinuousC<int>>
            , ExpectedTypeList
                    < fd::PseudoRange
                            < ContinuousC<int>::iterator
                            , ContinuousC<int>::iterator
                            >
                    >
            >);

    static_assert(std::is_same_v
            < TypeList<const ContinuousC<int>>
            , ExpectedTypeList
                    < fd::PseudoRange
                            < ContinuousC<int>::const_iterator
                            , ContinuousC<int>::const_iterator
                            >
                    >
            >);

    static_assert(std::is_same_v
            < TypeList<RandomAccessC<int>>
            , ExpectedTypeList
                    < fd::PseudoRange
                            < RandomAccessC<int>::iterator
                            , RandomAccessC<int>::iterator
                            >
                    >
            >);

    static_assert(std::is_same_v
            < TypeList<const RandomAccessC<int>>
            , ExpectedTypeList
                    < fd::PseudoRange
                            < RandomAccessC<int>::const_iterator
                            , RandomAccessC<int>::const_iterator
                            >
                    >
            >);

    static_assert(std::is_same_v
            < TypeList<BidirectC<int>>
            , ExpectedTypeList
                    < fd::PseudoRange
                            < BidirectC<int>::iterator
                            , BidirectC<int>::iterator
                            >
                    >
            >);

    static_assert(std::is_same_v
            < TypeList<const BidirectC<int>>
            , ExpectedTypeList
                    < fd::PseudoRange
                            < BidirectC<int>::const_iterator
                            , BidirectC<int>::const_iterator
                            >
                    >
            >);

    static_assert(std::is_same_v
            < TypeList<ForwardC<int>>
            , ExpectedTypeList
                    < fd::PseudoRange
                            < ForwardC<int>::iterator
                            , ForwardC<int>::iterator
                            >
                    >
            >);

    static_assert(std::is_same_v
            < TypeList<const ForwardC<int>>
            , ExpectedTypeList
                    < fd::PseudoRange
                            < ForwardC<int>::const_iterator
                            , ForwardC<int>::const_iterator
                            >
                    >
            >);

    static_assert(std::is_same_v
            < TypeList<InputC<int>>
            , ExpectedTypeList
                    < fd::PseudoRange
                            < InputC<int>::iterator
                            , InputC<int>::iterator
                            >
                    >
            >);

    // NOTE: no checks for `const InputC<int>`, since `InputC<int>` cannot be
    // `const`, because it always changes its state while reading from it

    static_assert(std::is_same_v
            < TypeList<std::vector<bool>>
            , ExpectedTypeList
                    < fd::PseudoRange
                            < std::vector<bool>::iterator
                            , std::vector<bool>::iterator
                            >
                    >
            >);

    static_assert(std::is_same_v
            < TypeList<const std::vector<bool>>
            , ExpectedTypeList
                    < fd::PseudoRange
                            < std::vector<bool>::const_iterator
                            , std::vector<bool>::const_iterator
                            >
                    >
            >);

    static_assert(std::is_same_v
            < TypeList
                    < const ContinuousC<ContinuousC<int>>
                    >
            , ExpectedTypeList
                    < fd::PseudoRange
                            < ContinuousC<ContinuousC<int>>::const_iterator
                            , ContinuousC<ContinuousC<int>>::const_iterator
                            >
                    , fd::PseudoRange
                            < ContinuousC<int>::const_iterator
                            , ContinuousC<int>::const_iterator
                            >
                    >
            >);

    static_assert(std::is_same_v
            < TypeList
                    < Array2<Array2<int>>
                    >
            , ExpectedTypeList
                    < fd::PseudoRange
                            < int(*)[2]
                            , int(*)[2]
                            >
                    , fd::PseudoRange
                            < int*
                            , int*
                            >
                    >
            >);

    {
        using Range1 = fd::PseudoRange<const int(*)[2], const int(*)[2]>;
        using Range2 = fd::PseudoRange<const int*, const int*>;

        static_assert(std::is_same_v
                < TypeList
                        < Array2<const Array2<int>>
                        >
                , ExpectedTypeList
                        < Range1
                        , Range2
                        >
                >);

        static_assert(std::is_same_v
                < TypeList
                        < const Array2<Array2<int>>
                        >
                , ExpectedTypeList
                        < Range1
                        , Range2
                        >
                >);

        static_assert(std::is_same_v
                < TypeList
                        < const Array2<const Array2<int>>
                        >
                , ExpectedTypeList
                        < Range1
                        , Range2
                        >
                >);
    }

    static_assert(std::is_same_v
                < TypeList
                        < ContinuousC<ContinuousC<ContinuousC<int>>>
                        >
                , ExpectedTypeList
                        < fd::PseudoRange
                                < ContinuousC<ContinuousC<ContinuousC<int>>>::iterator
                                , ContinuousC<ContinuousC<ContinuousC<int>>>::iterator
                                >
                        , fd::PseudoRange
                                < ContinuousC<ContinuousC<int>>::iterator
                                , ContinuousC<ContinuousC<int>>::iterator
                                >
                        , fd::PseudoRange
                                < ContinuousC<int>::iterator
                                , ContinuousC<int>::iterator
                                >
                        >
                >);

    {
        using R = ContinuousC
                < RandomAccessC
                        < BidirectC
                                < ForwardC<InputC<int>>
                                >
                        >
                >;

    }

    static_assert(std::is_same_v
                < TypeList
                        < ContinuousC
                                < RandomAccessC
                                        < BidirectC
                                                < ForwardC<InputC<int>>
                                                >
                                        >
                                >
                        >
                , ExpectedTypeList
                        < fd::PseudoRange
                                < ContinuousC
                                        < RandomAccessC
                                                < BidirectC
                                                        < ForwardC<InputC<int>>
                                                        >
                                                >
                                        >::iterator
                                , ContinuousC
                                        < RandomAccessC
                                                < BidirectC
                                                        < ForwardC<InputC<int>>
                                                        >
                                                >
                                        >::iterator
                                >
                        , fd::PseudoRange
                                < RandomAccessC
                                        < BidirectC
                                                < ForwardC<InputC<int>>
                                                >
                                        >::iterator
                                , RandomAccessC
                                        < BidirectC
                                                < ForwardC<InputC<int>>
                                                >
                                        >::iterator
                                >
                        , fd::PseudoRange
                                < BidirectC
                                        < ForwardC<InputC<int>>
                                        >::iterator
                                , BidirectC
                                        < ForwardC<InputC<int>>
                                        >::iterator
                                >
                        , fd::PseudoRange
                                < ForwardC<InputC<int>>::iterator
                                , ForwardC<InputC<int>>::iterator
                                >
                        , fd::PseudoRange
                                < InputC<int>::iterator
                                , InputC<int>::iterator
                                >
                        >
                >);
}


TEST(RangeTraits, IteratorCategory) {
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
    TEST_NESTED_TYPE(iterator_category, I_tag, InputC, ForwardC, BidirectC, RandomAccessC, ContinuousC, int);
    TEST_NESTED_TYPE(iterator_category, I_tag, ContinuousC, RandomAccessC, BidirectC, ForwardC, InputC, int);
}


template <typename V>
using A_val = typename IteratorTraits<Array2<V>>::value_type;

template <typename V>
using C_val = typename IteratorTraits<ContinuousC<V>>::value_type;

template <typename V>
using R_val = typename IteratorTraits<RandomAccessC<V>>::value_type;

template <typename V>
using B_val = typename IteratorTraits<BidirectC<V>>::value_type;

template <typename V>
using F_val = typename IteratorTraits<ForwardC<V>>::value_type;

template <typename V>
using I_val = typename IteratorTraits<InputC<V>>::value_type;

TEST(RangeTraits, ValueType) {
    TEST_NESTED_TYPE(value_type, A_val<A>, Array2, A);
    TEST_NESTED_TYPE(value_type, C_val<C>, ContinuousC, C);
    TEST_NESTED_TYPE(value_type, R_val<R>, RandomAccessC, R);
    TEST_NESTED_TYPE(value_type, B_val<B>, BidirectC, B);
    TEST_NESTED_TYPE(value_type, F_val<F>, ForwardC, F);
    TEST_NESTED_TYPE(value_type, I_val<I>, InputC, I);

    using Bool_val = IteratorTraits<std::vector<bool>>::value_type;

    TEST_NESTED_TYPE(value_type, Bool_val, std::vector, bool);

    // InputC, _
    TEST_NESTED_TYPE(value_type, I_val<I>, InputC, InputC, I);
    TEST_NESTED_TYPE(value_type, F_val<F>, InputC, ForwardC, F);
    TEST_NESTED_TYPE(value_type, B_val<B>, InputC, BidirectC, B);
    TEST_NESTED_TYPE(value_type, R_val<R>, InputC, RandomAccessC, R);
    TEST_NESTED_TYPE(value_type, C_val<C>, InputC, ContinuousC, C);

    // ForwardC, _
    TEST_NESTED_TYPE(value_type, I_val<I>, ForwardC, InputC, I);
    TEST_NESTED_TYPE(value_type, F_val<F>, ForwardC, ForwardC, F);
    TEST_NESTED_TYPE(value_type, B_val<B>, ForwardC, BidirectC, B);
    TEST_NESTED_TYPE(value_type, R_val<R>, ForwardC, RandomAccessC, R);
    TEST_NESTED_TYPE(value_type, C_val<C>, ForwardC, ContinuousC, C);

    // BidirectC, _
    TEST_NESTED_TYPE(value_type, I_val<I>, BidirectC, InputC, I);
    TEST_NESTED_TYPE(value_type, F_val<F>, BidirectC, ForwardC, F);
    TEST_NESTED_TYPE(value_type, B_val<B>, BidirectC, BidirectC, B);
    TEST_NESTED_TYPE(value_type, R_val<R>, BidirectC, RandomAccessC, R);
    TEST_NESTED_TYPE(value_type, C_val<C>, BidirectC, ContinuousC, C);

    // RandomAccessC, _
    TEST_NESTED_TYPE(value_type, I_val<I>, RandomAccessC, InputC, I);
    TEST_NESTED_TYPE(value_type, F_val<F>, RandomAccessC, ForwardC, F);
    TEST_NESTED_TYPE(value_type, B_val<B>, RandomAccessC, BidirectC, B);
    TEST_NESTED_TYPE(value_type, R_val<R>, RandomAccessC, RandomAccessC, R);
    TEST_NESTED_TYPE(value_type, C_val<C>, RandomAccessC, ContinuousC, C);

    // ContinuousC, _
    TEST_NESTED_TYPE(value_type, I_val<I>, ContinuousC, InputC, I);
    TEST_NESTED_TYPE(value_type, F_val<F>, ContinuousC, ForwardC, F);
    TEST_NESTED_TYPE(value_type, B_val<B>, ContinuousC, BidirectC, B);
    TEST_NESTED_TYPE(value_type, R_val<R>, ContinuousC, RandomAccessC, R);
    TEST_NESTED_TYPE(value_type, C_val<C>, ContinuousC, ContinuousC, C);

    // ContinuousC, _, ContinuousC
    TEST_NESTED_TYPE(value_type, C_val<I>, ContinuousC, InputC, ContinuousC, I);
    TEST_NESTED_TYPE(value_type, C_val<F>, ContinuousC, ForwardC, ContinuousC, F);
    TEST_NESTED_TYPE(value_type, C_val<B>, ContinuousC, BidirectC, ContinuousC, B);
    TEST_NESTED_TYPE(value_type, C_val<R>, ContinuousC, RandomAccessC, ContinuousC, R);
    TEST_NESTED_TYPE(value_type, C_val<C>, ContinuousC, ContinuousC, ContinuousC, C);

    // RandomAccessC, _, RandomAccessC
    TEST_NESTED_TYPE(value_type, R_val<I>, RandomAccessC, InputC, RandomAccessC, I);
    TEST_NESTED_TYPE(value_type, R_val<F>, RandomAccessC, ForwardC, RandomAccessC, F);
    TEST_NESTED_TYPE(value_type, R_val<B>, RandomAccessC, BidirectC, RandomAccessC, B);
    TEST_NESTED_TYPE(value_type, R_val<R>, RandomAccessC, RandomAccessC, RandomAccessC, R);
    TEST_NESTED_TYPE(value_type, R_val<C>, RandomAccessC, ContinuousC, RandomAccessC, C);

    // BidirectC, _, BidirectC
    TEST_NESTED_TYPE(value_type, B_val<I>, BidirectC, InputC, BidirectC, I);
    TEST_NESTED_TYPE(value_type, B_val<F>, BidirectC, ForwardC, BidirectC, F);
    TEST_NESTED_TYPE(value_type, B_val<B>, BidirectC, BidirectC, BidirectC, B);
    TEST_NESTED_TYPE(value_type, B_val<R>, BidirectC, RandomAccessC, BidirectC, R);
    TEST_NESTED_TYPE(value_type, B_val<C>, BidirectC, ContinuousC, BidirectC, C);

    // ForwardC, _, ForwardC
    TEST_NESTED_TYPE(value_type, F_val<I>, ForwardC, InputC, ForwardC, I);
    TEST_NESTED_TYPE(value_type, F_val<F>, ForwardC, ForwardC, ForwardC, F);
    TEST_NESTED_TYPE(value_type, F_val<B>, ForwardC, BidirectC, ForwardC, B);
    TEST_NESTED_TYPE(value_type, F_val<R>, ForwardC, RandomAccessC, ForwardC, R);
    TEST_NESTED_TYPE(value_type, F_val<C>, ForwardC, ContinuousC, ForwardC, C);

    // InputC, _, InputC
    TEST_NESTED_TYPE(value_type, I_val<I>, InputC, InputC, InputC, I);
    TEST_NESTED_TYPE(value_type, I_val<F>, InputC, ForwardC, InputC, F);
    TEST_NESTED_TYPE(value_type, I_val<B>, InputC, BidirectC, InputC, B);
    TEST_NESTED_TYPE(value_type, I_val<R>, InputC, RandomAccessC, InputC, R);
    TEST_NESTED_TYPE(value_type, I_val<C>, InputC, ContinuousC, InputC, C);

    // InputC <-> ForwardC <-> BidirectC <-> RandomAccessC <-> ContinuousC
    TEST_NESTED_TYPE(value_type, B_val<I>, InputC, ForwardC, BidirectC, I);
    TEST_NESTED_TYPE(value_type, R_val<F>, ForwardC, BidirectC, RandomAccessC, F);
    TEST_NESTED_TYPE(value_type, C_val<B>, BidirectC, RandomAccessC, ContinuousC, B);
    TEST_NESTED_TYPE(value_type, B_val<C>, ContinuousC, RandomAccessC, BidirectC, C);
    TEST_NESTED_TYPE(value_type, F_val<R>, RandomAccessC, BidirectC, ForwardC, R);
    TEST_NESTED_TYPE(value_type, I_val<B>, BidirectC, ForwardC, InputC, B);
    TEST_NESTED_TYPE(value_type, C_val<I>, InputC, ForwardC, BidirectC, RandomAccessC, ContinuousC, I);
    TEST_NESTED_TYPE(value_type, I_val<C>, ContinuousC, RandomAccessC, BidirectC, ForwardC, InputC, C);
}


template <typename V>
using A_ref = typename IteratorTraits<Array2<V>>::reference;

template <typename V>
using C_ref = typename IteratorTraits<ContinuousC<V>>::reference;

template <typename V>
using R_ref = typename IteratorTraits<RandomAccessC<V>>::reference;

template <typename V>
using B_ref = typename IteratorTraits<BidirectC<V>>::reference;

template <typename V>
using F_ref = typename IteratorTraits<ForwardC<V>>::reference;

template <typename V>
using I_ref = typename IteratorTraits<InputC<V>>::reference;

TEST(RangeTraits, Reference) {
    TEST_NESTED_TYPE(reference, A_ref<A>, Array2, A);
    TEST_NESTED_TYPE(reference, C_ref<C>, ContinuousC, C);
    TEST_NESTED_TYPE(reference, R_ref<R>, RandomAccessC, R);
    TEST_NESTED_TYPE(reference, B_ref<B>, BidirectC, B);
    TEST_NESTED_TYPE(reference, F_ref<F>, ForwardC, F);
    TEST_NESTED_TYPE(reference, I_ref<I>, InputC, I);

    using VecBool_ref = typename std::iterator_traits
            < decltype( RangeTraits::begin(std::declval<std::vector<bool>&>()) )
            >::reference;

    TEST_NESTED_TYPE(reference, VecBool_ref, std::vector, bool);

    // InputC, _
    TEST_NESTED_TYPE(reference, I_ref<I>, InputC, InputC, I);
    TEST_NESTED_TYPE(reference, F_ref<F>, InputC, ForwardC, F);
    TEST_NESTED_TYPE(reference, B_ref<B>, InputC, BidirectC, B);
    TEST_NESTED_TYPE(reference, R_ref<R>, InputC, RandomAccessC, R);
    TEST_NESTED_TYPE(reference, C_ref<C>, InputC, ContinuousC, C);

    // ForwardC, _
    TEST_NESTED_TYPE(reference, I_ref<I>, ForwardC, InputC, I);
    TEST_NESTED_TYPE(reference, F_ref<F>, ForwardC, ForwardC, F);
    TEST_NESTED_TYPE(reference, B_ref<B>, ForwardC, BidirectC, B);
    TEST_NESTED_TYPE(reference, R_ref<R>, ForwardC, RandomAccessC, R);
    TEST_NESTED_TYPE(reference, C_ref<C>, ForwardC, ContinuousC, C);

    // BidirectC, _
    TEST_NESTED_TYPE(reference, I_ref<I>, BidirectC, InputC, I);
    TEST_NESTED_TYPE(reference, F_ref<F>, BidirectC, ForwardC, F);
    TEST_NESTED_TYPE(reference, B_ref<B>, BidirectC, BidirectC, B);
    TEST_NESTED_TYPE(reference, R_ref<R>, BidirectC, RandomAccessC, R);
    TEST_NESTED_TYPE(reference, C_ref<C>, BidirectC, ContinuousC, C);

    // RandomAccessC, _
    TEST_NESTED_TYPE(reference, I_ref<I>, RandomAccessC, InputC, I);
    TEST_NESTED_TYPE(reference, F_ref<F>, RandomAccessC, ForwardC, F);
    TEST_NESTED_TYPE(reference, B_ref<B>, RandomAccessC, BidirectC, B);
    TEST_NESTED_TYPE(reference, R_ref<R>, RandomAccessC, RandomAccessC, R);
    TEST_NESTED_TYPE(reference, C_ref<C>, RandomAccessC, ContinuousC, C);

    // ContinuousC, _
    TEST_NESTED_TYPE(reference, I_ref<I>, ContinuousC, InputC, I);
    TEST_NESTED_TYPE(reference, F_ref<F>, ContinuousC, ForwardC, F);
    TEST_NESTED_TYPE(reference, B_ref<B>, ContinuousC, BidirectC, B);
    TEST_NESTED_TYPE(reference, R_ref<R>, ContinuousC, RandomAccessC, R);
    TEST_NESTED_TYPE(reference, C_ref<C>, ContinuousC, ContinuousC, C);

    // ContinuousC, _, ContinuousC
    TEST_NESTED_TYPE(reference, C_ref<I>, ContinuousC, InputC, ContinuousC, I);
    TEST_NESTED_TYPE(reference, C_ref<F>, ContinuousC, ForwardC, ContinuousC, F);
    TEST_NESTED_TYPE(reference, C_ref<B>, ContinuousC, BidirectC, ContinuousC, B);
    TEST_NESTED_TYPE(reference, C_ref<R>, ContinuousC, RandomAccessC, ContinuousC, R);
    TEST_NESTED_TYPE(reference, C_ref<C>, ContinuousC, ContinuousC, ContinuousC, C);

    // RandomAccessC, _, RandomAccessC
    TEST_NESTED_TYPE(reference, R_ref<I>, RandomAccessC, InputC, RandomAccessC, I);
    TEST_NESTED_TYPE(reference, R_ref<F>, RandomAccessC, ForwardC, RandomAccessC, F);
    TEST_NESTED_TYPE(reference, R_ref<B>, RandomAccessC, BidirectC, RandomAccessC, B);
    TEST_NESTED_TYPE(reference, R_ref<R>, RandomAccessC, RandomAccessC, RandomAccessC, R);
    TEST_NESTED_TYPE(reference, R_ref<C>, RandomAccessC, ContinuousC, RandomAccessC, C);

    // BidirectC, _, BidirectC
    TEST_NESTED_TYPE(reference, B_ref<I>, BidirectC, InputC, BidirectC, I);
    TEST_NESTED_TYPE(reference, B_ref<F>, BidirectC, ForwardC, BidirectC, F);
    TEST_NESTED_TYPE(reference, B_ref<B>, BidirectC, BidirectC, BidirectC, B);
    TEST_NESTED_TYPE(reference, B_ref<R>, BidirectC, RandomAccessC, BidirectC, R);
    TEST_NESTED_TYPE(reference, B_ref<C>, BidirectC, ContinuousC, BidirectC, C);

    // ForwardC, _, ForwardC
    TEST_NESTED_TYPE(reference, F_ref<I>, ForwardC, InputC, ForwardC, I);
    TEST_NESTED_TYPE(reference, F_ref<F>, ForwardC, ForwardC, ForwardC, F);
    TEST_NESTED_TYPE(reference, F_ref<B>, ForwardC, BidirectC, ForwardC, B);
    TEST_NESTED_TYPE(reference, F_ref<R>, ForwardC, RandomAccessC, ForwardC, R);
    TEST_NESTED_TYPE(reference, F_ref<C>, ForwardC, ContinuousC, ForwardC, C);

    // InputC, _, InputC
    TEST_NESTED_TYPE(reference, I_ref<I>, InputC, InputC, InputC, I);
    TEST_NESTED_TYPE(reference, I_ref<F>, InputC, ForwardC, InputC, F);
    TEST_NESTED_TYPE(reference, I_ref<B>, InputC, BidirectC, InputC, B);
    TEST_NESTED_TYPE(reference, I_ref<R>, InputC, RandomAccessC, InputC, R);
    TEST_NESTED_TYPE(reference, I_ref<C>, InputC, ContinuousC, InputC, C);

    // InputC <-> ForwardC <-> BidirectC <-> RandomAccessC <-> ContinuousC
    TEST_NESTED_TYPE(reference, B_ref<I>, InputC, ForwardC, BidirectC, I);
    TEST_NESTED_TYPE(reference, R_ref<F>, ForwardC, BidirectC, RandomAccessC, F);
    TEST_NESTED_TYPE(reference, C_ref<B>, BidirectC, RandomAccessC, ContinuousC, B);
    TEST_NESTED_TYPE(reference, B_ref<C>, ContinuousC, RandomAccessC, BidirectC, C);
    TEST_NESTED_TYPE(reference, F_ref<R>, RandomAccessC, BidirectC, ForwardC, R);
    TEST_NESTED_TYPE(reference, I_ref<B>, BidirectC, ForwardC, InputC, B);
    TEST_NESTED_TYPE(reference, C_ref<I>, InputC, ForwardC, BidirectC, RandomAccessC, ContinuousC, I);
    TEST_NESTED_TYPE(reference, I_ref<C>, ContinuousC, RandomAccessC, BidirectC, ForwardC, InputC, C);
}


TEST(RangeTraits, Pointer) {
    using A_ptr = IteratorTraits<Array2<int>>::pointer;
    using C_ptr = IteratorTraits<ContinuousC<int>>::pointer;
    using R_ptr = IteratorTraits<RandomAccessC<int>>::pointer;
    using B_ptr = IteratorTraits<BidirectC<int>>::pointer;
    using F_ptr = IteratorTraits<ForwardC<int>>::pointer;
    using I_ptr = IteratorTraits<InputC<int>>::pointer;

    TEST_NESTED_TYPE(pointer, A_ptr, Array2, int);
    TEST_NESTED_TYPE(pointer, C_ptr, ContinuousC, int);
    TEST_NESTED_TYPE(pointer, R_ptr, RandomAccessC, int);
    TEST_NESTED_TYPE(pointer, B_ptr, BidirectC, int);
    TEST_NESTED_TYPE(pointer, F_ptr, ForwardC, int);
    TEST_NESTED_TYPE(pointer, I_ptr, InputC, int);

    using VecBool_ptr = typename std::iterator_traits
            < decltype( RangeTraits::begin(std::declval<std::vector<bool>&>()) )
            >::pointer;

    TEST_NESTED_TYPE(pointer, VecBool_ptr, std::vector, bool);

    // InputC, _
    TEST_NESTED_TYPE(pointer, I_ptr, InputC, InputC, int);
    TEST_NESTED_TYPE(pointer, F_ptr, InputC, ForwardC, int);
    TEST_NESTED_TYPE(pointer, B_ptr, InputC, BidirectC, int);
    TEST_NESTED_TYPE(pointer, R_ptr, InputC, RandomAccessC, int);
    TEST_NESTED_TYPE(pointer, C_ptr, InputC, ContinuousC, int);

    // ForwardC, _
    TEST_NESTED_TYPE(pointer, I_ptr, ForwardC, InputC, int);
    TEST_NESTED_TYPE(pointer, F_ptr, ForwardC, ForwardC, int);
    TEST_NESTED_TYPE(pointer, B_ptr, ForwardC, BidirectC, int);
    TEST_NESTED_TYPE(pointer, R_ptr, ForwardC, RandomAccessC, int);
    TEST_NESTED_TYPE(pointer, C_ptr, ForwardC, ContinuousC, int);

    // BidirectC, _
    TEST_NESTED_TYPE(pointer, I_ptr, BidirectC, InputC, int);
    TEST_NESTED_TYPE(pointer, F_ptr, BidirectC, ForwardC, int);
    TEST_NESTED_TYPE(pointer, B_ptr, BidirectC, BidirectC, int);
    TEST_NESTED_TYPE(pointer, R_ptr, BidirectC, RandomAccessC, int);
    TEST_NESTED_TYPE(pointer, C_ptr, BidirectC, ContinuousC, int);

    // RandomAccessC, _
    TEST_NESTED_TYPE(pointer, I_ptr, RandomAccessC, InputC, int);
    TEST_NESTED_TYPE(pointer, F_ptr, RandomAccessC, ForwardC, int);
    TEST_NESTED_TYPE(pointer, B_ptr, RandomAccessC, BidirectC, int);
    TEST_NESTED_TYPE(pointer, R_ptr, RandomAccessC, RandomAccessC, int);
    TEST_NESTED_TYPE(pointer, C_ptr, RandomAccessC, ContinuousC, int);

    // ContinuousC, _
    TEST_NESTED_TYPE(pointer, I_ptr, ContinuousC, InputC, int);
    TEST_NESTED_TYPE(pointer, F_ptr, ContinuousC, ForwardC, int);
    TEST_NESTED_TYPE(pointer, B_ptr, ContinuousC, BidirectC, int);
    TEST_NESTED_TYPE(pointer, R_ptr, ContinuousC, RandomAccessC, int);
    TEST_NESTED_TYPE(pointer, C_ptr, ContinuousC, ContinuousC, int);

    // ContinuousC, _, ContinuousC
    TEST_NESTED_TYPE(pointer, C_ptr, ContinuousC, InputC, ContinuousC, int);
    TEST_NESTED_TYPE(pointer, C_ptr, ContinuousC, ForwardC, ContinuousC, int);
    TEST_NESTED_TYPE(pointer, C_ptr, ContinuousC, BidirectC, ContinuousC, int);
    TEST_NESTED_TYPE(pointer, C_ptr, ContinuousC, RandomAccessC, ContinuousC, int);
    TEST_NESTED_TYPE(pointer, C_ptr, ContinuousC, ContinuousC, ContinuousC, int);

    // RandomAccessC, _, RandomAccessC
    TEST_NESTED_TYPE(pointer, R_ptr, RandomAccessC, InputC, RandomAccessC, int);
    TEST_NESTED_TYPE(pointer, R_ptr, RandomAccessC, ForwardC, RandomAccessC, int);
    TEST_NESTED_TYPE(pointer, R_ptr, RandomAccessC, BidirectC, RandomAccessC, int);
    TEST_NESTED_TYPE(pointer, R_ptr, RandomAccessC, RandomAccessC, RandomAccessC, int);
    TEST_NESTED_TYPE(pointer, R_ptr, RandomAccessC, ContinuousC, RandomAccessC, int);

    // BidirectC, _, BidirectC
    TEST_NESTED_TYPE(pointer, B_ptr, BidirectC, InputC, BidirectC, int);
    TEST_NESTED_TYPE(pointer, B_ptr, BidirectC, ForwardC, BidirectC, int);
    TEST_NESTED_TYPE(pointer, B_ptr, BidirectC, BidirectC, BidirectC, int);
    TEST_NESTED_TYPE(pointer, B_ptr, BidirectC, RandomAccessC, BidirectC, int);
    TEST_NESTED_TYPE(pointer, B_ptr, BidirectC, ContinuousC, BidirectC, int);

    // ForwardC, _, ForwardC
    TEST_NESTED_TYPE(pointer, F_ptr, ForwardC, InputC, ForwardC, int);
    TEST_NESTED_TYPE(pointer, F_ptr, ForwardC, ForwardC, ForwardC, int);
    TEST_NESTED_TYPE(pointer, F_ptr, ForwardC, BidirectC, ForwardC, int);
    TEST_NESTED_TYPE(pointer, F_ptr, ForwardC, RandomAccessC, ForwardC, int);
    TEST_NESTED_TYPE(pointer, F_ptr, ForwardC, ContinuousC, ForwardC, int);

    // InputC, _, InputC
    TEST_NESTED_TYPE(pointer, I_ptr, InputC, InputC, InputC, int);
    TEST_NESTED_TYPE(pointer, I_ptr, InputC, ForwardC, InputC, int);
    TEST_NESTED_TYPE(pointer, I_ptr, InputC, BidirectC, InputC, int);
    TEST_NESTED_TYPE(pointer, I_ptr, InputC, RandomAccessC, InputC, int);
    TEST_NESTED_TYPE(pointer, I_ptr, InputC, ContinuousC, InputC, int);

    // InputC <-> ForwardC <-> BidirectC <-> RandomAccessC <-> ContinuousC
    TEST_NESTED_TYPE(pointer, B_ptr, InputC, ForwardC, BidirectC, int);
    TEST_NESTED_TYPE(pointer, R_ptr, ForwardC, BidirectC, RandomAccessC, int);
    TEST_NESTED_TYPE(pointer, C_ptr, BidirectC, RandomAccessC, ContinuousC, int);
    TEST_NESTED_TYPE(pointer, B_ptr, ContinuousC, RandomAccessC, BidirectC, int);
    TEST_NESTED_TYPE(pointer, F_ptr, RandomAccessC, BidirectC, ForwardC, int);
    TEST_NESTED_TYPE(pointer, I_ptr, BidirectC, ForwardC, InputC, int);
    TEST_NESTED_TYPE(pointer, C_ptr, InputC, ForwardC, BidirectC, RandomAccessC, ContinuousC, int);
    TEST_NESTED_TYPE(pointer, I_ptr, ContinuousC, RandomAccessC, BidirectC, ForwardC, InputC, int);
}


TEST(RangeTraits, DifferenceType) {
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
    TEST_NESTED_TYPE(difference_type, diff_t, InputC, ForwardC, BidirectC, RandomAccessC, ContinuousC, I);
    TEST_NESTED_TYPE(difference_type, diff_t, ContinuousC, RandomAccessC, BidirectC, ForwardC, InputC, C);
}


// TODO: add tests
