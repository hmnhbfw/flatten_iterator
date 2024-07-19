/// \file flatten_iterator.h
/// TODO: add file description
/// \author Pavel Tsayukov
/// \copyright MIT License

#ifndef FLATTEN_ITERATOR_FLATTEN_ITERATOR_H
#define FLATTEN_ITERATOR_FLATTEN_ITERATOR_H

#include <iterator>
#include <tuple>
#include <type_traits>

#if defined(__cpp_concepts) && __cpp_concepts >= 201907L
#include <ranges>
#endif

namespace flatten_iterator {

namespace details {

// Type wrapper
template <typename T>
struct Type {
    using type = T;
};

namespace traits {

#if !defined(__cpp_concepts) || __cpp_concepts < 201907L

namespace require {

template <bool... HasTraits>
using Traits = std::enable_if_t<(HasTraits && ...)>;

template <typename... Ts>
inline constexpr bool Requires = std::is_void_v<std::void_t<Ts...>>;

} // namespace require


namespace lang {

// [concept.same]
template <typename T, typename U>
inline constexpr bool IsSameAs = std::is_same_v<T, U> && std::is_same_v<U, T>;

// [concept.convertible]
template <typename From, typename To, typename = void>
inline constexpr bool IsConvertibleTo = false;

namespace impl {

template <typename From>
using Producer = std::add_rvalue_reference_t<From> (&)();

template <typename From, typename To>
constexpr auto ExplicitCastImpl(Producer<From> f) -> decltype( static_cast<To>(f()) );

template <typename From, typename To>
using ExplicitCast = decltype( ExplicitCastImpl<From, To>(std::declval<Producer<From>>()) );

} // namespace impl

// [concept.convertible]
template <typename From, typename To>
inline constexpr bool IsConvertibleTo
        < From, To
        , require::Traits
                < std::is_convertible_v<From, To>
                , require::Requires<impl::ExplicitCast<From, To>>
                >
        > = true;

// [concept.commonref]
template <typename T, typename U, typename = void>
inline constexpr bool IsCommonReferenceWith = false;

namespace impl {

// See the rules in [meta.trans.other]
template <typename T1, typename T2, std::size_t Rule = 1, typename = void>
struct CommonReference : CommonReference<T1, T2, Rule + 1> {};

template <typename T1, typename T2>
using CommonReferenceType = typename CommonReference<T1, T2>::type;

// `COND-RES` implementation
template <typename X, typename Y>
using CondRes = decltype( false ? std::declval<X (&)()>()() : std::declval<Y (&)()>()() );

// `COPYCV` implementation
template <typename From, typename To>
constexpr auto CopyCvImpl() noexcept {
    constexpr bool is_const = std::is_const_v<From>;
    constexpr bool is_volatile = std::is_volatile_v<From>;

    if constexpr (is_const && is_volatile) {
        return Type<std::add_cv_t<To>>{};
    } else if constexpr (is_volatile) {
        return Type<std::add_volatile_t<To>>{};
    } else if constexpr (is_const) {
        return Type<std::add_const_t<To>>{};
    } else {
        return Type<To>{};
    }
}

template <typename From, typename To>
using CopyCv = typename decltype( CopyCvImpl<From, To>() )::type;

// `COMMON-REF` implementation
template <typename T1, typename T2, typename = void>
struct CommonRefImpl {};

template <typename T1, typename T2>
using CommonRef = typename CommonRefImpl<T1, T2>::type;

template <typename X, typename Y>
using CommonLvalueRef = CondRes<CopyCv<X, Y>&, CopyCv<Y, X>&>;

template <typename T1, typename T2>
struct CommonRefImpl
        < T1&, T2&
        , require::Traits
                < std::is_reference_v<CommonLvalueRef<T1, T2>>
                >
        > {
    using type = CommonLvalueRef<T1, T2>;
};

template <typename X, typename Y>
using CommonRefC = std::remove_reference_t<CommonRef<X&, Y&>>&&;

template <typename T1, typename T2>
struct CommonRefImpl
        < T1&&, T2&&
        , require::Traits
                < std::is_convertible_v<T1&&, CommonRefC<T1, T2>>
                , std::is_convertible_v<T2&&, CommonRefC<T1, T2>>
                >
        > {
    using type = CommonRefC<T1, T2>;
};

template <typename X, typename Y>
using CommonRefD = CommonRef<const X&, Y&>;

template <typename T1, typename T2>
struct CommonRefImpl
        < T1&&, T2&
        , require::Traits
                < std::is_convertible_v<T1&&, CommonRefD<T1, T2>>
                >
        > {
    using type = CommonRefD<T1, T2>;
};

template <typename T1, typename T2>
struct CommonRefImpl<T1&, T2&&> : CommonRefImpl<T2&&, T1&> {};

// Rule 1: if `T1` and `T2` are reference types and `COMMON-REF(T1, T2)` is
// well-formed
template <typename T1, typename T2>
struct CommonReference
        < T1, T2, 1
        , require::Traits
                < std::is_reference_v<T1>
                , std::is_reference_v<T2>
                , require::Requires<CommonRef<T1, T2>>
                >
        > {
    using type = CommonRef<T1, T2>;
};

// Rule 2: otherwise, if `basic_common_reference<...>` is well-formed
// This is a customization point, it's useless for the standard C++17 so far
template <typename T1, typename T2>
struct CommonReference<T1, T2, 2> {
    // Don't use this rule
};

// Rule 3: otherwise, if `COND-RES(T1, T2)` is well-formed
template <typename T1, typename T2>
struct CommonReference
        < T1, T2, 3
        , require::Traits
                < require::Requires<CondRes<T1, T2>>
                >
        > {
    using type = CondRes<T1, T2>;
};

// Rule 4: otherwise, if `common_type_t<T1, T2>` is well-formed
template <typename T1, typename T2>
struct CommonReference
        < T1, T2, 4
        , require::Traits
                < require::Requires<std::common_type_t<T1, T2>>
                >
        > {
    using type = std::common_type_t<T1, T2>;
};

// Rule 5: otherwise, there shall be no member `type`
template <typename T1, typename T2>
struct CommonReference<T1, T2, 5> {};

} // namespace impl

// [concept.commonref]
template <typename T, typename U>
inline constexpr bool IsCommonReferenceWith
        < T, U
        , require::Traits
                < IsSameAs
                        < impl::CommonReferenceType<T, U>
                        , impl::CommonReferenceType<U, T>
                        >
                , IsConvertibleTo<T, impl::CommonReferenceType<T, U>>
                , IsConvertibleTo<U, impl::CommonReferenceType<T, U>>
                >
        > = true;

// [concept.assignable]
template <typename Lhs, typename Rhs, typename = void>
inline constexpr bool IsAssignableFrom = false;

namespace impl {

template <typename Lhs, typename Rhs>
constexpr auto ForwardThenAssign(Lhs lhs, Rhs&& rhs) -> decltype( lhs = std::forward<Rhs>(rhs) );

} // namespace impl

// [concept.assignable]
template <typename Lhs, typename Rhs>
inline constexpr bool IsAssignableFrom
        < Lhs, Rhs
        , require::Traits
                < std::is_lvalue_reference_v<Lhs>
                , IsCommonReferenceWith
                        < const std::remove_reference_t<Lhs>&
                        , const std::remove_reference_t<Rhs>&
                        >
                , IsSameAs
                        < decltype( impl::ForwardThenAssign(std::declval<Lhs>(),
                                                            std::declval<Rhs&&>()) )
                        , Lhs
                        >
                >
        > = true;

// [concept.swappable]
// TODO: so far, it's not clear what the difference between `std::swappable`
// and `std::is_swappable_v` is, but for now, it should works
template <typename T>
inline constexpr bool IsSwappable = std::is_swappable_v<T>;

// [concept.destructible]
template <typename T>
inline constexpr bool IsDestructible = std::is_nothrow_destructible_v<T>;

// [concept.constructible]
template <typename T, typename... Args>
inline constexpr bool IsConstructibleFrom =
        IsDestructible<T> && std::is_constructible_v<T, Args...>;

// [concept.default.init]
template <typename T, typename = void>
inline constexpr bool IsDefaultInitializable = false;

template <typename T>
inline constexpr bool IsDefaultInitializable
        < T
        , require::Traits
                < IsConstructibleFrom<T>
                , require::Requires
                        < decltype( T{} )
                        , decltype( static_cast<void>(::new T) )
                        >
                >
        > = true;

// [concept.moveconstructible]
template <typename T>
inline constexpr bool IsMoveConstructible =
        IsConstructibleFrom<T, T> && IsConvertibleTo<T, T>;

// [concept.copyconstructible]
template <typename T>
inline constexpr bool IsCopyConstructible =
        IsMoveConstructible<T>
        && IsConstructibleFrom<T, T&> && IsConvertibleTo<T&, T>
        && IsConstructibleFrom<T, const T&> && IsConvertibleTo<const T&, T>
        && IsConstructibleFrom<T, const T> && IsConvertibleTo<const T, T>;

} // namespace lang


namespace comparison {

// [concept.booleantestable]
template <typename T, typename = void>
inline constexpr bool IsBooleanTestable = false;

namespace impl {

template <typename T>
inline constexpr bool IsBooleanTestableImpl = lang::IsConvertibleTo<T, bool>;

template <typename T>
constexpr auto ForwardThenNot(T&& t) -> decltype( !std::forward<T>(t) );

} // namespace impl

// [concept.booleantestable]
template <typename T>
inline constexpr bool IsBooleanTestable
        < T
        , require::Traits
                < impl::IsBooleanTestableImpl<T>
                , impl::IsBooleanTestableImpl
                        < decltype( impl::ForwardThenNot(std::declval<T&&>()) )
                        >
                >
        > = true;

// [concept.equalitycomparable]
template <typename T, typename U, typename = void>
inline constexpr bool IsWeaklyEqualityComparableWith = false;

namespace impl {

template <typename T, typename U>
constexpr auto Equal(const std::remove_reference_t<T>& t, const std::remove_reference_t<U>& u)
        -> decltype( t == u, t != u, u == t, u != t, void() );

} // namespace impl

// [concept.equalitycomparable]
template <typename T, typename U>
inline constexpr bool IsWeaklyEqualityComparableWith
        < T, U
        , require::Traits
                < require::Requires<decltype( impl::Equal<T, U> )>
                >
        > = true;

} // namespace comparison


namespace object {

// [concepts.object]
template <typename T>
inline constexpr bool IsMovable =
        std::is_object_v<T> && lang::IsMoveConstructible<T>
        && lang::IsAssignableFrom<T&, T> && lang::IsSwappable<T>;

// [concepts.object]
template <typename T>
inline constexpr bool IsCopyable =
        lang::IsCopyConstructible<T> && IsMovable<T> && lang::IsAssignableFrom<T&, T&>
        && lang::IsAssignableFrom<T&, const T&> && lang::IsAssignableFrom<T&, const T>;

// [concepts.object]
template <typename T>
inline constexpr bool IsSemiregular = IsCopyable<T> && lang::IsDefaultInitializable<T>;

} // namespace object


namespace iterator {

// [iterator.concept.winc]
template <typename I, typename = void>
inline constexpr bool IsWeaklyIncrementable = false;

namespace impl {

template <typename I>
constexpr auto PreIncrement(I i) -> decltype( ++i );

template <typename I>
constexpr auto PostIncrement(I i) -> decltype( i++, void() );

// NOTE: this check is not as accurate as it should be
// TODO: no check for the `__int128` extension (gcc and clang both support it)
// TODO: no check for interger-class types
template <typename T>
inline constexpr bool IsSignedIntegerLike =
        std::is_integral_v<T> && std::is_signed_v<T>;

} // namespace impl

// [iterator.concept.winc]
template <typename I>
inline constexpr bool IsWeaklyIncrementable
        < I
        , require::Traits
                < object::IsMovable<I>
                , impl::IsSignedIntegerLike
                        // NOTE: check only iterators, so there is no need to
                        // implement `std::incrementable_traits`
                        < typename std::iterator_traits<I>::difference_type
                        >
                , lang::IsSameAs
                        < decltype( impl::PreIncrement(std::declval<I>()) )
                        , I&
                        >
                , require::Requires<decltype( impl::PostIncrement(std::declval<I>()) )>
                >
        > = true;

// [iterator.concept.iterator]
template <typename I, typename = void>
inline constexpr bool IsInputOrOutputIterator = false;

namespace impl {

template <typename T, typename = void>
inline constexpr bool CanReference = false;

template <typename T>
inline constexpr bool CanReference
        < T
        , require::Traits<require::Requires<T&>>
        > = true;

template <typename I>
constexpr auto Dereference(I i) -> decltype( *i );

} // namespace impl

// [iterator.concept.iterator]
template <typename I>
inline constexpr bool IsInputOrOutputIterator
        < I
        , require::Traits
                < impl::CanReference
                        < decltype( impl::Dereference(std::declval<I>()) )
                        >
                , IsWeaklyIncrementable<I>
                >
        > = true;

// [iterator.concept.sentinel]
template <typename I, typename S, typename = void>
inline constexpr bool IsSentinelFor = false;

template <typename I,  typename S>
inline constexpr bool IsSentinelFor
        < I, S
        , require::Traits
                < object::IsSemiregular<S>
                , IsInputOrOutputIterator<I>
                , comparison::IsWeaklyEqualityComparableWith<S, I>
                >
        > = true;

} // namespace iterator


namespace ranges {

namespace impl {

namespace adl_begin {

using std::begin;

template <typename R>
using Iterator = decltype( begin(std::declval<R&>()) );

} // namespace adl_begin

namespace adl_end {

using std::end;

template <typename R>
using Sentinel = decltype( end(std::declval<R&>()) );

} // namespace adl_end

} // namespace impl

// [range.range]
template <typename R, typename = void>
inline constexpr bool IsRange = false;

template <typename R>
inline constexpr bool IsRange
        < R
        , require::Traits
                < iterator::IsInputOrOutputIterator<impl::adl_begin::Iterator<R>>
                , iterator::IsSentinelFor
                        < impl::adl_begin::Iterator<R>
                        , impl::adl_end::Sentinel<R>
                        >
                >
        > = true;

#else // defined(__cpp_concepts) && __cpp_concepts >= 201907L

template <typename R>
inline constexpr bool IsRange = std::ranges::range<R>;

#endif

} // namespace ranges

} // namespace traits

template
#if defined(__cpp_concepts) && __cpp_concepts >= 201907L
        < std::input_or_output_iterator I
        , std::sentinel_for<I> S
        >
#else
        <typename I, typename S>
#endif
struct Range {
    I begin;
    S end;
};

template <typename I, typename S>
constexpr auto begin(Range<I, S>& range) noexcept {
    return range.begin;
}

template <typename I, typename S>
constexpr auto end(Range<I, S>& range) noexcept {
    return range.end;
}

template
#if defined(__cpp_concepts) && __cpp_concepts >= 201907L
        < std::input_or_output_iterator I
        , std::sentinel_for<I> S
        >
#else
        <typename I, typename S>
#endif
class RangesAllTheWayDownTraits {
private: // Type deduction methods

    template
            < typename Value
            , typename... RangeWrappers
            , std::enable_if_t
                    < !traits::ranges::IsRange<Value>
                    , int
                    > = 0
            >
    static constexpr auto WrapRanges(std::tuple<RangeWrappers...> acc) noexcept {
        return acc;
    }

    template
            < typename Value
            , typename... RangeWrappers
            , std::enable_if_t
                    < traits::ranges::IsRange<Value>
                    , int
                    > = 0
            >
    static constexpr auto WrapRanges(std::tuple<RangeWrappers...> acc) noexcept {
        using Iterator = traits::ranges::impl::adl_begin::Iterator<Value>;
        using Sentinel = traits::ranges::impl::adl_end::Sentinel<Value>;
        using RangeWrapper = Type<Range<Iterator, Sentinel>>;
        using NestedValue = std::remove_reference_t<decltype( *std::declval<Iterator&>() )>;

        return WrapRanges<NestedValue>(std::tuple_cat(acc, std::tuple<RangeWrapper>{}));
    }

    template <typename... RangeWrappers>
    static constexpr auto UnwrapRanges(std::tuple<RangeWrappers...>) noexcept {
        return Type<std::tuple<typename RangeWrappers::type...>>{};
    }

    static constexpr auto WrapRanges() noexcept {
        using RangeWrapper = Type<Range<I, S>>;
        using Value = std::remove_reference_t<decltype( *std::declval<I&>() )>;

        return WrapRanges<Value>(std::tuple<RangeWrapper>{});
    }

public: // TODO: section name

    // TODO: maybe extract two methods to one later
    using Ranges = typename decltype( UnwrapRanges(WrapRanges()) )::type;

    static constexpr std::size_t MaxDepth = std::tuple_size_v<Ranges>;

public: // Ranges getter/setter

    template <std::size_t Depth>
    static constexpr auto& Get(Ranges& ranges) noexcept {
        return std::get<Depth>(ranges);
    }

    template <std::size_t Depth>
    static constexpr auto& Current(Ranges& ranges) { // TODO: noexcept
        // TODO: impl
    }

    template <std::size_t Depth>
    static constexpr auto& End(Ranges& ranges) { // TODO: noexcept
        // TODO: impl
    }

    // TODO: impl: NextUp, NextDown, and similar methods

public: // Nested types
        // TODO: replace all the `void` types to the proper types

    using ValueType = void;
    using DifferenceType = void;
    using Reference = void;
    using Pointer = void;
    using IteratorCategory = void;

#if defined(__cpp_concepts) && __cpp_concepts >= 201907L
    using IteratorConcept = void;
#endif
};

} // namespace details

/// TODO: add description
template
#if defined(__cpp_concepts) && __cpp_concepts >= 201907L
        < std::input_or_output_iterator I
        , std::sentinel_for<I> S
        >
#else
        <typename I, typename S>
#endif
class FlattenIterator {
    using Traits = details::RangesAllTheWayDownTraits<I, S>;

    typename Traits::Ranges ranges_;

public: // Nested iterator types

    using value_type = typename Traits::ValueType;
    using difference_type = typename Traits::DifferenceType;
    using reference = typename Traits::Reference;
    using pointer = typename Traits::Pointer;
    using iterator_category = typename Traits::IteratorCategory;

#if defined(__cpp_concepts) && __cpp_concepts >= 201907L
    using iterator_concept = typename Traits::IteratorConcept;
#endif

public: // Constructors

    template <typename Iterator, typename Sentinel>
    FlattenIterator(Iterator begin, Sentinel end) { // TODO: add noexcept
        // TODO: impl
    }

public: // TODO: add name for section

    // TODO: impl
};

} // namespace flatten_iterator

#endif // FLATTEN_ITERATOR_FLATTEN_ITERATOR_H
