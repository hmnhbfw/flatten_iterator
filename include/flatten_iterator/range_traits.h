/// \file range_traits.h
/// TODO: add file description
/// \author Pavel Tsayukov
/// \copyright MIT License

#ifndef FLATTEN_ITERATOR_TRAITS_H
#define FLATTEN_ITERATOR_TRAITS_H

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

namespace require {

template <bool... HasTraits>
using Traits = std::enable_if_t<(HasTraits && ...)>;

template <bool... HasTraits>
using AnyOfTraits = std::enable_if_t<(HasTraits || ...)>;

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
// TODO: no check for integer-class types
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

template
        < typename T
        , typename = std::enable_if_t
                < lang::IsConvertibleTo<T, std::decay_t<T>>
                >
        >
constexpr auto DecayCopy(T&& value) -> std::decay_t<decltype( std::forward<T>(value) )>;

template <typename T>
inline constexpr bool IsClassOrEnum =
        std::is_class_v<T> || std::is_union_v<T> || std::is_enum_v<T>;

// If unqualified lookup finds a non-function then ADL doesn't take place,
// so these "poison pills" prevent that case
template <typename R> void begin(R&) = delete;
template <typename R> void begin(const R&) = delete;

template <typename R, typename = void>
inline constexpr bool HasBeginMember = false;

template <typename R>
constexpr auto BeginMemberHelper(R& r) -> decltype( DecayCopy(r.begin()) );

template <typename R>
inline constexpr bool HasBeginMember
        < R
        , require::Traits
                < iterator::IsInputOrOutputIterator
                        < decltype( BeginMemberHelper(std::declval<R&>()) )
                        >
                >
        > = true;

template <typename R, typename = void>
inline constexpr bool HasAdlBegin = false;

template <typename R>
constexpr auto AdlBeginHelper(R& r) -> decltype( DecayCopy(begin(r)) );

template <typename R>
inline constexpr bool HasAdlBegin
        < R
        , require::Traits
                < IsClassOrEnum<std::remove_reference_t<R>>
                , iterator::IsInputOrOutputIterator
                        < decltype( AdlBeginHelper(std::declval<R&>()) )
                        >
                >
        > = true;

class Begin {
    template <typename R>
    static constexpr bool IsNoexcept() noexcept {
        if constexpr (std::is_array_v<std::remove_reference_t<R>>) {
            return true;
        } else if constexpr (HasBeginMember<R>) {
            return noexcept(DecayCopy(std::declval<R&>().begin()));
        } else {
            return noexcept(DecayCopy(begin(std::declval<R&>())));
        }
    }

public:
    template
            < typename R
            , typename = require::AnyOfTraits
                    < std::is_array_v<std::remove_reference_t<R>>
                    , HasBeginMember<R>
                    , HasAdlBegin<R>
                    >
            >
    constexpr auto operator()(R&& r) const noexcept(IsNoexcept<R>()) {
        if constexpr (std::is_array_v<std::remove_reference_t<R>>) {
            static_assert(std::is_lvalue_reference_v<R>,
                          "Since an array can't be a borrowed range it must be "
                          "an lvalue reference.");
            return r + 0;
        } else if constexpr (HasBeginMember<R>) {
            return r.begin();
        } else {
            return begin(r);
        }
    }
};

} // namespace impl

inline constexpr impl::Begin BeginFn;

namespace impl {

// If unqualified lookup finds a non-function then ADL doesn't take place,
// so these "poison pills" prevent that case
template <typename R> void end(R&) = delete;
template <typename R> void end(const R&) = delete;

template <typename R, typename = void>
inline constexpr bool HasEndMember = false;

template <typename R>
constexpr auto EndMemberHelper(R& r) -> decltype( DecayCopy(r.end()) );

template <typename R>
inline constexpr bool HasEndMember
        < R
        , require::Traits
                < iterator::IsSentinelFor
                        < decltype( BeginFn(std::declval<R&>()) )
                        , decltype( EndMemberHelper(std::declval<R&>()) )
                        >
                >
        > = true;

template <typename R, typename = void>
inline constexpr bool HasAdlEnd = false;

template <typename R>
constexpr auto AdlEndHelper(R& r) -> decltype( DecayCopy(end(r)) );

template <typename R>
inline constexpr bool HasAdlEnd
        < R
        , require::Traits
                < IsClassOrEnum<std::remove_reference_t<R>>
                , iterator::IsSentinelFor
                        < decltype( BeginFn(std::declval<R&>()) )
                        , decltype( AdlEndHelper(std::declval<R&>()) )
                        >
                >
        > = true;

template <typename T>
inline constexpr bool IsBoundedArray = false;

template <typename T, std::size_t N>
inline constexpr bool IsBoundedArray<T[N]> = true;

struct End {
    template <typename R>
    static constexpr bool IsNoexcept() noexcept {
        if constexpr (IsBoundedArray<std::remove_reference_t<R>>) {
            return true;
        } else if constexpr (HasEndMember<R>) {
            return noexcept(DecayCopy(std::declval<R&>().end()));
        } else {
            return noexcept(DecayCopy(end(std::declval<R&>())));
        }
    }

public:
    template
            < typename R
            , typename = require::AnyOfTraits
                    < IsBoundedArray<std::remove_reference_t<R>>
                    , HasEndMember<R>
                    , HasAdlEnd<R>
                    >
            >
    constexpr auto operator()(R&& r) const noexcept(IsNoexcept<R>()) {
        if constexpr (IsBoundedArray<std::remove_reference_t<R>>) {
            static_assert(std::is_lvalue_reference_v<R>,
                          "Since an array can't be a borrowed range it must be "
                          "an lvalue reference.");
            return r + std::extent_v<std::remove_reference_t<R>>;
        } else if constexpr (HasEndMember<R>) {
            return r.end();
        } else {
            return end(r);
        }
    }
};

} // namespace impl

inline constexpr impl::End EndFn;

// [range.range]
template <typename R, typename = void>
inline constexpr bool IsRange = false;

namespace impl {

template <typename R>
constexpr auto RangeBegin(R& r) -> decltype( BeginFn(r) );

template <typename R>
constexpr auto RangeEnd(R& r) -> decltype( EndFn(r) );

} // namespace impl

template <typename R>
inline constexpr bool IsRange
        < R
        , require::Traits
                < iterator::IsInputOrOutputIterator
                        < decltype( impl::RangeBegin(std::declval<R&>()) )
                        >
                , iterator::IsSentinelFor
                        < decltype( impl::RangeBegin(std::declval<R&>()) )
                        , decltype( impl::RangeEnd(std::declval<R&>()) )
                        >
                >
        > = true;

} // namespace ranges

} // namespace traits


namespace require = traits::require;

template <typename I, typename S>
struct PseudoRange {
    I begin;
    S end;
};

template
        < typename I
        , typename S
        , typename RangeTraits
        , typename = require::Traits
                < RangeTraits::template input_or_output_iterator<I>
                , RangeTraits::template sentinel_for<I, S>
                >
        >
class RangesAllTheWayDownTraits {
private: // Type deduction methods

    template <typename RorV, typename... RWs>
    static constexpr auto WrapRanges(std::tuple<RWs...> acc) noexcept {
        if constexpr (!RangeTraits::template range<RorV>) {
            return acc;
        } else {
            using R = RorV;
            using Iterator = decltype( RangeTraits::begin(std::declval<R&>()) );
            using Sentinel = decltype( RangeTraits::end(std::declval<R&>()) );
            using Value = std::remove_reference_t<decltype( *std::declval<Iterator&>() )>;
            using RangeWrapper = Type<PseudoRange<Iterator, Sentinel>>;

            return WrapRanges<Value>(std::tuple_cat(acc, std::tuple<RangeWrapper>{}));
        }
    }

    template <typename... RWs>
    static constexpr auto UnwrapRanges(std::tuple<RWs...>) noexcept {
        return Type<std::tuple<typename RWs::type...>>{};
    }

    static constexpr auto WrapRanges() noexcept {
        using Value = std::remove_reference_t<decltype( *std::declval<I&>() )>;
        using RangeWrapper = Type<PseudoRange<I, S>>;

        return WrapRanges<Value>(std::tuple<RangeWrapper>{});
    }

public: // TODO: section name

    // TODO: maybe extract two methods to one later
    using Ranges = typename decltype( UnwrapRanges(WrapRanges()) )::type;

    static constexpr std::size_t MaxDepth = std::tuple_size_v<Ranges> - 1;

public: // Ranges getter/setter

    template <std::size_t Depth>
    static constexpr auto& Get(Ranges& ranges) noexcept {
        return std::get<Depth>(ranges);
    }

    template <std::size_t Depth>
    static constexpr auto& Current(Ranges& ranges) noexcept {
        return Get<Depth>(ranges).begin;
    }

    template <std::size_t Depth>
    static constexpr auto& End(Ranges& ranges) noexcept {
        return Get<Depth>(ranges).end;
    }

    static constexpr auto& Value(Ranges& ranges) noexcept {
        return *Current<MaxDepth>(ranges);
    }

    // TODO: impl: NextUp, NextDown, and similar methods

private: // TODO: section name

    using DeepestIterator = std::remove_reference_t
            < decltype( Current<MaxDepth>(std::declval<Ranges&>()) )
            >;

public: // TODO: section name
        // TODO: replace all the `void` types to the proper types

    using ValueType = typename std::iterator_traits<DeepestIterator>::value_type;
    using DifferenceType = void;
    using Reference = typename std::iterator_traits<DeepestIterator>::reference;
    using Pointer = typename std::iterator_traits<DeepestIterator>::pointer;
    using IteratorCategory = void;

#if defined(__cpp_concepts) && __cpp_concepts >= 201907L
    using IteratorConcept = void;
#endif
};

} // namespace details


/// TODO: add description
struct DefaultPreConceptRangeTraits {
    template <typename R>
    static constexpr bool range = details::traits::ranges::IsRange<R>;

    template <typename I>
    static constexpr bool input_or_output_iterator =
            details::traits::iterator::IsInputOrOutputIterator<I>;

    template <typename I, typename S>
    static constexpr bool sentinel_for =
            details::traits::iterator::IsSentinelFor<I, S>;

    static constexpr auto begin = details::traits::ranges::BeginFn;
    static constexpr auto end = details::traits::ranges::EndFn;
};

#if defined(__cpp_concepts) && __cpp_concepts >= 201907L

/// TODO: add description
struct DefaultConceptRangeTraits {
    template <typename R>
    static constexpr bool range = std::ranges::range<R>;

    template <typename I>
    static constexpr bool input_or_output_iterator = std::input_or_output_iterator<I>;

    template <typename I, typename S>
    static constexpr bool sentinel_for = std::sentinel_for<I, S>;

    static constexpr auto begin = std::ranges::begin;
    static constexpr auto end = std::ranges::end;
};

#endif // defined(__cpp_concepts) && __cpp_concepts >= 201907L

} // namespace flatten_iterator

#endif // FLATTEN_ITERATOR_TRAITS_H
