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
    using C =
#if __cplusplus >= 202002L
            std::contiguous_iterator_tag
#else
            std::random_access_iterator_tag
#endif
    ;
    using R = std::random_access_iterator_tag;
    using B = std::bidirectional_iterator_tag;
    using F = std::forward_iterator_tag;
    using I = std::input_iterator_tag;

    TEST_NESTED_TYPE(iterator_category, C, ContinuousC, int);
    TEST_NESTED_TYPE(iterator_category, C, Array2, int);
    TEST_NESTED_TYPE(iterator_category, R, RandomAccessC, int);
    TEST_NESTED_TYPE(iterator_category, R, RandomAccessC, int);
    TEST_NESTED_TYPE(iterator_category, B, BidirectC, int);
    TEST_NESTED_TYPE(iterator_category, F, ForwardC, int);
    TEST_NESTED_TYPE(iterator_category, I, InputC, int);

    // InputC, _
    TEST_NESTED_TYPE(iterator_category, I, InputC, InputC, int);
    TEST_NESTED_TYPE(iterator_category, I, InputC, ForwardC, int);
    TEST_NESTED_TYPE(iterator_category, I, InputC, BidirectC, int);
    TEST_NESTED_TYPE(iterator_category, I, InputC, RandomAccessC, int);
    TEST_NESTED_TYPE(iterator_category, I, InputC, ContinuousC, int);

    // ForwardC, _
    TEST_NESTED_TYPE(iterator_category, I, ForwardC, InputC, int);
    TEST_NESTED_TYPE(iterator_category, F, ForwardC, ForwardC, int);
    TEST_NESTED_TYPE(iterator_category, F, ForwardC, BidirectC, int);
    TEST_NESTED_TYPE(iterator_category, F, ForwardC, RandomAccessC, int);
    TEST_NESTED_TYPE(iterator_category, F, ForwardC, ContinuousC, int);

    // BidirectC, _
    TEST_NESTED_TYPE(iterator_category, I, BidirectC, InputC, int);
    TEST_NESTED_TYPE(iterator_category, F, BidirectC, ForwardC, int);
    TEST_NESTED_TYPE(iterator_category, B, BidirectC, BidirectC, int);
    TEST_NESTED_TYPE(iterator_category, B, BidirectC, RandomAccessC, int);
    TEST_NESTED_TYPE(iterator_category, B, BidirectC, ContinuousC, int);

    // RandomAccessC, _
    TEST_NESTED_TYPE(iterator_category, I, RandomAccessC, InputC, int);
    TEST_NESTED_TYPE(iterator_category, F, RandomAccessC, ForwardC, int);
    TEST_NESTED_TYPE(iterator_category, B, RandomAccessC, BidirectC, int);
    TEST_NESTED_TYPE(iterator_category, B, RandomAccessC, RandomAccessC, int);
    TEST_NESTED_TYPE(iterator_category, B, RandomAccessC, ContinuousC, int);

    // ContinuousC, _
    TEST_NESTED_TYPE(iterator_category, I, ContinuousC, InputC, int);
    TEST_NESTED_TYPE(iterator_category, F, ContinuousC, ForwardC, int);
    TEST_NESTED_TYPE(iterator_category, B, ContinuousC, BidirectC, int);
    TEST_NESTED_TYPE(iterator_category, B, ContinuousC, RandomAccessC, int);
    TEST_NESTED_TYPE(iterator_category, B, ContinuousC, ContinuousC, int);

    // ContinuousC, _, ContinuousC
    TEST_NESTED_TYPE(iterator_category, I, ContinuousC, InputC, ContinuousC, int);
    TEST_NESTED_TYPE(iterator_category, F, ContinuousC, ForwardC, ContinuousC, int);
    TEST_NESTED_TYPE(iterator_category, B, ContinuousC, BidirectC, ContinuousC, int);
    TEST_NESTED_TYPE(iterator_category, B, ContinuousC, RandomAccessC, ContinuousC, int);
    TEST_NESTED_TYPE(iterator_category, B, ContinuousC, ContinuousC, ContinuousC, int);

    // RandomAccessC, _, RandomAccessC
    TEST_NESTED_TYPE(iterator_category, I, RandomAccessC, InputC, RandomAccessC, int);
    TEST_NESTED_TYPE(iterator_category, F, RandomAccessC, ForwardC, RandomAccessC, int);
    TEST_NESTED_TYPE(iterator_category, B, RandomAccessC, BidirectC, RandomAccessC, int);
    TEST_NESTED_TYPE(iterator_category, B, RandomAccessC, RandomAccessC, RandomAccessC, int);
    TEST_NESTED_TYPE(iterator_category, B, RandomAccessC, ContinuousC, RandomAccessC, int);

    // BidirectC, _, BidirectC
    TEST_NESTED_TYPE(iterator_category, I, BidirectC, InputC, BidirectC, int);
    TEST_NESTED_TYPE(iterator_category, F, BidirectC, ForwardC, BidirectC, int);
    TEST_NESTED_TYPE(iterator_category, B, BidirectC, BidirectC, BidirectC, int);
    TEST_NESTED_TYPE(iterator_category, B, BidirectC, RandomAccessC, BidirectC, int);
    TEST_NESTED_TYPE(iterator_category, B, BidirectC, ContinuousC, BidirectC, int);

    // ForwardC, _, ForwardC
    TEST_NESTED_TYPE(iterator_category, I, ForwardC, InputC, ForwardC, int);
    TEST_NESTED_TYPE(iterator_category, F, ForwardC, ForwardC, ForwardC, int);
    TEST_NESTED_TYPE(iterator_category, F, ForwardC, BidirectC, ForwardC, int);
    TEST_NESTED_TYPE(iterator_category, F, ForwardC, RandomAccessC, ForwardC, int);
    TEST_NESTED_TYPE(iterator_category, F, ForwardC, ContinuousC, ForwardC, int);

    // InputC, _, InputC
    TEST_NESTED_TYPE(iterator_category, I, InputC, InputC, InputC, int);
    TEST_NESTED_TYPE(iterator_category, I, InputC, ForwardC, InputC, int);
    TEST_NESTED_TYPE(iterator_category, I, InputC, BidirectC, InputC, int);
    TEST_NESTED_TYPE(iterator_category, I, InputC, RandomAccessC, InputC, int);
    TEST_NESTED_TYPE(iterator_category, I, InputC, ContinuousC, InputC, int);

    // InputC <-> ForwardC <-> BidirectC <-> RandomAccessC <-> ContinuousC
    TEST_NESTED_TYPE(iterator_category, I, InputC, ForwardC, BidirectC, int);
    TEST_NESTED_TYPE(iterator_category, F, ForwardC, BidirectC, RandomAccessC, int);
    TEST_NESTED_TYPE(iterator_category, B, BidirectC, RandomAccessC, ContinuousC, int);
    TEST_NESTED_TYPE(iterator_category, B, ContinuousC, RandomAccessC, BidirectC, int);
    TEST_NESTED_TYPE(iterator_category, F, RandomAccessC, BidirectC, ForwardC, int);
    TEST_NESTED_TYPE(iterator_category, I, BidirectC, ForwardC, InputC, int);
    TEST_NESTED_TYPE(iterator_category, I, InputC, ForwardC, BidirectC, RandomAccessC, ContinuousC, int);
    TEST_NESTED_TYPE(iterator_category, I, ContinuousC, RandomAccessC, BidirectC, ForwardC, InputC, int);
}


TEST(RangeTraits, ValueType) {
    TEST_NESTED_TYPE(value_type, int, Array2, int);
    TEST_NESTED_TYPE(value_type, int, ContinuousC, int);
    TEST_NESTED_TYPE(value_type, int, RandomAccessC, int);
    TEST_NESTED_TYPE(value_type, int, BidirectC, int);
    TEST_NESTED_TYPE(value_type, int, ForwardC, int);
    TEST_NESTED_TYPE(value_type, int, InputC, int);

    TEST_NESTED_TYPE(value_type, bool, std::vector, bool);

    // InputC, _
    TEST_NESTED_TYPE(value_type, int, InputC, InputC, int);
    TEST_NESTED_TYPE(value_type, int, InputC, ForwardC, int);
    TEST_NESTED_TYPE(value_type, int, InputC, BidirectC, int);
    TEST_NESTED_TYPE(value_type, int, InputC, RandomAccessC, int);
    TEST_NESTED_TYPE(value_type, int, InputC, ContinuousC, int);

    // ForwardC, _
    TEST_NESTED_TYPE(value_type, int, ForwardC, InputC, int);
    TEST_NESTED_TYPE(value_type, int, ForwardC, ForwardC, int);
    TEST_NESTED_TYPE(value_type, int, ForwardC, BidirectC, int);
    TEST_NESTED_TYPE(value_type, int, ForwardC, RandomAccessC, int);
    TEST_NESTED_TYPE(value_type, int, ForwardC, ContinuousC, int);

    // BidirectC, _
    TEST_NESTED_TYPE(value_type, int, BidirectC, InputC, int);
    TEST_NESTED_TYPE(value_type, int, BidirectC, ForwardC, int);
    TEST_NESTED_TYPE(value_type, int, BidirectC, BidirectC, int);
    TEST_NESTED_TYPE(value_type, int, BidirectC, RandomAccessC, int);
    TEST_NESTED_TYPE(value_type, int, BidirectC, ContinuousC, int);

    // RandomAccessC, _
    TEST_NESTED_TYPE(value_type, int, RandomAccessC, InputC, int);
    TEST_NESTED_TYPE(value_type, int, RandomAccessC, ForwardC, int);
    TEST_NESTED_TYPE(value_type, int, RandomAccessC, BidirectC, int);
    TEST_NESTED_TYPE(value_type, int, RandomAccessC, RandomAccessC, int);
    TEST_NESTED_TYPE(value_type, int, RandomAccessC, ContinuousC, int);

    // ContinuousC, _
    TEST_NESTED_TYPE(value_type, int, ContinuousC, InputC, int);
    TEST_NESTED_TYPE(value_type, int, ContinuousC, ForwardC, int);
    TEST_NESTED_TYPE(value_type, int, ContinuousC, BidirectC, int);
    TEST_NESTED_TYPE(value_type, int, ContinuousC, RandomAccessC, int);
    TEST_NESTED_TYPE(value_type, int, ContinuousC, ContinuousC, int);

    // ContinuousC, _, ContinuousC
    TEST_NESTED_TYPE(value_type, int, ContinuousC, InputC, ContinuousC, int);
    TEST_NESTED_TYPE(value_type, int, ContinuousC, ForwardC, ContinuousC, int);
    TEST_NESTED_TYPE(value_type, int, ContinuousC, BidirectC, ContinuousC, int);
    TEST_NESTED_TYPE(value_type, int, ContinuousC, RandomAccessC, ContinuousC, int);
    TEST_NESTED_TYPE(value_type, int, ContinuousC, ContinuousC, ContinuousC, int);

    // RandomAccessC, _, RandomAccessC
    TEST_NESTED_TYPE(value_type, int, RandomAccessC, InputC, RandomAccessC, int);
    TEST_NESTED_TYPE(value_type, int, RandomAccessC, ForwardC, RandomAccessC, int);
    TEST_NESTED_TYPE(value_type, int, RandomAccessC, BidirectC, RandomAccessC, int);
    TEST_NESTED_TYPE(value_type, int, RandomAccessC, RandomAccessC, RandomAccessC, int);
    TEST_NESTED_TYPE(value_type, int, RandomAccessC, ContinuousC, RandomAccessC, int);

    // BidirectC, _, BidirectC
    TEST_NESTED_TYPE(value_type, int, BidirectC, InputC, BidirectC, int);
    TEST_NESTED_TYPE(value_type, int, BidirectC, ForwardC, BidirectC, int);
    TEST_NESTED_TYPE(value_type, int, BidirectC, BidirectC, BidirectC, int);
    TEST_NESTED_TYPE(value_type, int, BidirectC, RandomAccessC, BidirectC, int);
    TEST_NESTED_TYPE(value_type, int, BidirectC, ContinuousC, BidirectC, int);

    // ForwardC, _, ForwardC
    TEST_NESTED_TYPE(value_type, int, ForwardC, InputC, ForwardC, int);
    TEST_NESTED_TYPE(value_type, int, ForwardC, ForwardC, ForwardC, int);
    TEST_NESTED_TYPE(value_type, int, ForwardC, BidirectC, ForwardC, int);
    TEST_NESTED_TYPE(value_type, int, ForwardC, RandomAccessC, ForwardC, int);
    TEST_NESTED_TYPE(value_type, int, ForwardC, ContinuousC, ForwardC, int);

    // InputC, _, InputC
    TEST_NESTED_TYPE(value_type, int, InputC, InputC, InputC, int);
    TEST_NESTED_TYPE(value_type, int, InputC, ForwardC, InputC, int);
    TEST_NESTED_TYPE(value_type, int, InputC, BidirectC, InputC, int);
    TEST_NESTED_TYPE(value_type, int, InputC, RandomAccessC, InputC, int);
    TEST_NESTED_TYPE(value_type, int, InputC, ContinuousC, InputC, int);

    // InputC <-> ForwardC <-> BidirectC <-> RandomAccessC <-> ContinuousC
    TEST_NESTED_TYPE(value_type, int, InputC, ForwardC, BidirectC, int);
    TEST_NESTED_TYPE(value_type, int, ForwardC, BidirectC, RandomAccessC, int);
    TEST_NESTED_TYPE(value_type, int, BidirectC, RandomAccessC, ContinuousC, int);
    TEST_NESTED_TYPE(value_type, int, ContinuousC, RandomAccessC, BidirectC, int);
    TEST_NESTED_TYPE(value_type, int, RandomAccessC, BidirectC, ForwardC, int);
    TEST_NESTED_TYPE(value_type, int, BidirectC, ForwardC, InputC, int);
    TEST_NESTED_TYPE(value_type, int, InputC, ForwardC, BidirectC, RandomAccessC, ContinuousC, int);
    TEST_NESTED_TYPE(value_type, int, ContinuousC, RandomAccessC, BidirectC, ForwardC, InputC, int);
}


TEST(RangeTraits, Reference) {
    using A_ref = typename std::iterator_traits
            < decltype( RangeTraits::begin(std::declval<Array2<int>&>()) )
            >::reference;
    using C_ref = typename std::iterator_traits
            < decltype( RangeTraits::begin(std::declval<ContinuousC<int>&>()) )
            >::reference;
    using R_ref = typename std::iterator_traits
            < decltype( RangeTraits::begin(std::declval<RandomAccessC<int>&>()) )
            >::reference;
    using B_ref = typename std::iterator_traits
            < decltype( RangeTraits::begin(std::declval<BidirectC<int>&>()) )
            >::reference;
    using F_ref = typename std::iterator_traits
            < decltype( RangeTraits::begin(std::declval<ForwardC<int>&>()) )
            >::reference;
    using I_ref = typename std::iterator_traits
            < decltype( RangeTraits::begin(std::declval<InputC<int>&>()) )
            >::reference;

    TEST_NESTED_TYPE(reference, A_ref, Array2, int);
    TEST_NESTED_TYPE(reference, C_ref, ContinuousC, int);
    TEST_NESTED_TYPE(reference, R_ref, RandomAccessC, int);
    TEST_NESTED_TYPE(reference, B_ref, BidirectC, int);
    TEST_NESTED_TYPE(reference, F_ref, ForwardC, int);
    TEST_NESTED_TYPE(reference, I_ref, InputC, int);

    using VecBool_ref = typename std::iterator_traits
            < decltype( RangeTraits::begin(std::declval<std::vector<bool>&>()) )
            >::reference;

    TEST_NESTED_TYPE(reference, VecBool_ref, std::vector, bool);

    // InputC, _
    TEST_NESTED_TYPE(reference, I_ref, InputC, InputC, int);
    TEST_NESTED_TYPE(reference, F_ref, InputC, ForwardC, int);
    TEST_NESTED_TYPE(reference, B_ref, InputC, BidirectC, int);
    TEST_NESTED_TYPE(reference, R_ref, InputC, RandomAccessC, int);
    TEST_NESTED_TYPE(reference, C_ref, InputC, ContinuousC, int);

    // ForwardC, _
    TEST_NESTED_TYPE(reference, I_ref, ForwardC, InputC, int);
    TEST_NESTED_TYPE(reference, F_ref, ForwardC, ForwardC, int);
    TEST_NESTED_TYPE(reference, B_ref, ForwardC, BidirectC, int);
    TEST_NESTED_TYPE(reference, R_ref, ForwardC, RandomAccessC, int);
    TEST_NESTED_TYPE(reference, C_ref, ForwardC, ContinuousC, int);

    // BidirectC, _
    TEST_NESTED_TYPE(reference, I_ref, BidirectC, InputC, int);
    TEST_NESTED_TYPE(reference, F_ref, BidirectC, ForwardC, int);
    TEST_NESTED_TYPE(reference, B_ref, BidirectC, BidirectC, int);
    TEST_NESTED_TYPE(reference, R_ref, BidirectC, RandomAccessC, int);
    TEST_NESTED_TYPE(reference, C_ref, BidirectC, ContinuousC, int);

    // RandomAccessC, _
    TEST_NESTED_TYPE(reference, I_ref, RandomAccessC, InputC, int);
    TEST_NESTED_TYPE(reference, F_ref, RandomAccessC, ForwardC, int);
    TEST_NESTED_TYPE(reference, B_ref, RandomAccessC, BidirectC, int);
    TEST_NESTED_TYPE(reference, R_ref, RandomAccessC, RandomAccessC, int);
    TEST_NESTED_TYPE(reference, C_ref, RandomAccessC, ContinuousC, int);

    // ContinuousC, _
    TEST_NESTED_TYPE(reference, I_ref, ContinuousC, InputC, int);
    TEST_NESTED_TYPE(reference, F_ref, ContinuousC, ForwardC, int);
    TEST_NESTED_TYPE(reference, B_ref, ContinuousC, BidirectC, int);
    TEST_NESTED_TYPE(reference, R_ref, ContinuousC, RandomAccessC, int);
    TEST_NESTED_TYPE(reference, C_ref, ContinuousC, ContinuousC, int);

    // ContinuousC, _, ContinuousC
    TEST_NESTED_TYPE(reference, C_ref, ContinuousC, InputC, ContinuousC, int);
    TEST_NESTED_TYPE(reference, C_ref, ContinuousC, ForwardC, ContinuousC, int);
    TEST_NESTED_TYPE(reference, C_ref, ContinuousC, BidirectC, ContinuousC, int);
    TEST_NESTED_TYPE(reference, C_ref, ContinuousC, RandomAccessC, ContinuousC, int);
    TEST_NESTED_TYPE(reference, C_ref, ContinuousC, ContinuousC, ContinuousC, int);

    // RandomAccessC, _, RandomAccessC
    TEST_NESTED_TYPE(reference, R_ref, RandomAccessC, InputC, RandomAccessC, int);
    TEST_NESTED_TYPE(reference, R_ref, RandomAccessC, ForwardC, RandomAccessC, int);
    TEST_NESTED_TYPE(reference, R_ref, RandomAccessC, BidirectC, RandomAccessC, int);
    TEST_NESTED_TYPE(reference, R_ref, RandomAccessC, RandomAccessC, RandomAccessC, int);
    TEST_NESTED_TYPE(reference, R_ref, RandomAccessC, ContinuousC, RandomAccessC, int);

    // BidirectC, _, BidirectC
    TEST_NESTED_TYPE(reference, B_ref, BidirectC, InputC, BidirectC, int);
    TEST_NESTED_TYPE(reference, B_ref, BidirectC, ForwardC, BidirectC, int);
    TEST_NESTED_TYPE(reference, B_ref, BidirectC, BidirectC, BidirectC, int);
    TEST_NESTED_TYPE(reference, B_ref, BidirectC, RandomAccessC, BidirectC, int);
    TEST_NESTED_TYPE(reference, B_ref, BidirectC, ContinuousC, BidirectC, int);

    // ForwardC, _, ForwardC
    TEST_NESTED_TYPE(reference, F_ref, ForwardC, InputC, ForwardC, int);
    TEST_NESTED_TYPE(reference, F_ref, ForwardC, ForwardC, ForwardC, int);
    TEST_NESTED_TYPE(reference, F_ref, ForwardC, BidirectC, ForwardC, int);
    TEST_NESTED_TYPE(reference, F_ref, ForwardC, RandomAccessC, ForwardC, int);
    TEST_NESTED_TYPE(reference, F_ref, ForwardC, ContinuousC, ForwardC, int);

    // InputC, _, InputC
    TEST_NESTED_TYPE(reference, I_ref, InputC, InputC, InputC, int);
    TEST_NESTED_TYPE(reference, I_ref, InputC, ForwardC, InputC, int);
    TEST_NESTED_TYPE(reference, I_ref, InputC, BidirectC, InputC, int);
    TEST_NESTED_TYPE(reference, I_ref, InputC, RandomAccessC, InputC, int);
    TEST_NESTED_TYPE(reference, I_ref, InputC, ContinuousC, InputC, int);

    // InputC <-> ForwardC <-> BidirectC <-> RandomAccessC <-> ContinuousC
    TEST_NESTED_TYPE(reference, B_ref, InputC, ForwardC, BidirectC, int);
    TEST_NESTED_TYPE(reference, R_ref, ForwardC, BidirectC, RandomAccessC, int);
    TEST_NESTED_TYPE(reference, C_ref, BidirectC, RandomAccessC, ContinuousC, int);
    TEST_NESTED_TYPE(reference, B_ref, ContinuousC, RandomAccessC, BidirectC, int);
    TEST_NESTED_TYPE(reference, F_ref, RandomAccessC, BidirectC, ForwardC, int);
    TEST_NESTED_TYPE(reference, I_ref, BidirectC, ForwardC, InputC, int);
    TEST_NESTED_TYPE(reference, C_ref, InputC, ForwardC, BidirectC, RandomAccessC, ContinuousC, int);
    TEST_NESTED_TYPE(reference, I_ref, ContinuousC, RandomAccessC, BidirectC, ForwardC, InputC, int);
}


TEST(RangeTraits, Pointer) {
    using A_ptr = typename std::iterator_traits
            < decltype( RangeTraits::begin(std::declval<Array2<int>&>()) )
            >::pointer;
    using C_ptr = typename std::iterator_traits
            < decltype( RangeTraits::begin(std::declval<ContinuousC<int>&>()) )
            >::pointer;
    using R_ptr = typename std::iterator_traits
            < decltype( RangeTraits::begin(std::declval<RandomAccessC<int>&>()) )
            >::pointer;
    using B_ptr = typename std::iterator_traits
            < decltype( RangeTraits::begin(std::declval<BidirectC<int>&>()) )
            >::pointer;
    using F_ptr = typename std::iterator_traits
            < decltype( RangeTraits::begin(std::declval<ForwardC<int>&>()) )
            >::pointer;
    using I_ptr = typename std::iterator_traits
            < decltype( RangeTraits::begin(std::declval<InputC<int>&>()) )
            >::pointer;

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

// TODO: add tests
