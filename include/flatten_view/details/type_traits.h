/// \file type_traits.h
/// Implementation of \c std::ranges::range concept in terms of C++17
/// \author Pavel Tsayukov
/// \copyright MIT License

#ifndef FLATTEN_VIEW_DETAILS_TYPE_TRAITS_H
#define FLATTEN_VIEW_DETAILS_TYPE_TRAITS_H

#include <iterator>
#include <type_traits>
#include <utility>

namespace flatten::details {

/// Type wrapper to return types from functions
template <typename T>
struct Type {
    using type = T;
};

namespace traits {

namespace require {

/// Analogue of a "requires-clause" that adds constraints, all of which must
/// be true
template <bool... HasTraits>
using AllOf = std::enable_if_t<(HasTraits && ...)>;

/// Analogue of a "requires-clause" that adds constraints, at least one of which
/// must be true
template <bool... HasTraits>
using AnyOf = std::enable_if_t<(HasTraits || ...)>;

/// Helper variable template to use \c std::void_t in \c require::AllOf and
/// \c require::AnyOf. Think of it as a "requires-expression".
template <typename... Ts>
inline constexpr bool Requires = std::is_void_v<std::void_t<Ts...>>;

} // namespace require


namespace lang {

/// Implementation of \c std::same_as, see [concept.same]
template <typename T, typename U>
inline constexpr bool IsSameAs = std::is_same_v<T, U> && std::is_same_v<U, T>;

/// Implementation of \c std::convertible_to, see [concept.convertible]
template <typename From, typename To, typename = void>
inline constexpr bool IsConvertibleTo = false;

namespace impl {

template <typename From>
using Producer = std::add_rvalue_reference_t<From> (&)();

template <typename From, typename To>
constexpr auto explicit_cast_impl(Producer<From> f) -> decltype( static_cast<To>(f()) );

template <typename From, typename To>
using ExplicitCast = decltype( explicit_cast_impl<From, To>(std::declval<Producer<From>>()) );

} // namespace impl

template <typename From, typename To>
inline constexpr bool IsConvertibleTo
        < From, To
        , require::AllOf
                < std::is_convertible_v<From, To>
                , require::Requires<impl::ExplicitCast<From, To>>
                >
        > = true;

/// Implementation of \c std::common_reference_with, see [concept.commonref]
template <typename T, typename U, typename = void>
inline constexpr bool IsCommonReferenceWith = false;

namespace impl {

/// See the rules in [meta.trans.other]
template <typename T1, typename T2, std::size_t Rule = 1, typename = void>
struct CommonReference : CommonReference<T1, T2, Rule + 1> {};

/// See \c std::common_reference_t, but only for two template parameters
template <typename T1, typename T2>
using CommonReferenceType = typename CommonReference<T1, T2>::type;

/// "COND-RES" type
template <typename X, typename Y>
using CondRes = decltype( false ? std::declval<X (&)()>()() : std::declval<Y (&)()>()() );

/// \c CopyCv helper function
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

/// "COPYCV" type
template <typename From, typename To>
using CopyCv = typename decltype( CopyCvImpl<From, To>() )::type;

/// \c CommonRef primary template
template <typename T1, typename T2, typename = void>
struct CommonRefImpl {};

/// "COMMON-REF" type
template <typename T1, typename T2>
using CommonRef = typename CommonRefImpl<T1, T2>::type;

template <typename X, typename Y>
using CommonLvalueRef = CondRes<CopyCv<X, Y>&, CopyCv<Y, X>&>;

/// \c CommonRef<T1&,T2&> specialization
template <typename T1, typename T2>
struct CommonRefImpl
        < T1&, T2&
        , require::AllOf
                < std::is_reference_v<CommonLvalueRef<T1, T2>>
                >
        > {
    using type = CommonLvalueRef<T1, T2>;
};

/// Implementation of the rvalue references case
template <typename X, typename Y>
using CommonRefC = std::remove_reference_t<CommonRef<X&, Y&>>&&;

/// \c CommonRef<T1&&,T2&&> specialization
template <typename T1, typename T2>
struct CommonRefImpl
        < T1&&, T2&&
        , require::AllOf
                < std::is_convertible_v<T1&&, CommonRefC<T1, T2>>
                , std::is_convertible_v<T2&&, CommonRefC<T1, T2>>
                >
        > {
    using type = CommonRefC<T1, T2>;
};

/// Implementation of the rvalue-lvalue references case
template <typename X, typename Y>
using CommonRefD = CommonRef<const X&, Y&>;

/// \c CommonRef<T1&&,T2&> specialization
template <typename T1, typename T2>
struct CommonRefImpl
        < T1&&, T2&
        , require::AllOf
                < std::is_convertible_v<T1&&, CommonRefD<T1, T2>>
                >
        > {
    using type = CommonRefD<T1, T2>;
};

/// \c CommonRef<T1&,T2&&> specialization
template <typename T1, typename T2>
struct CommonRefImpl<T1&, T2&&> : CommonRefImpl<T2&&, T1&> {};

/// Rule 1: if \c T1 and \c T2 are reference types and \c CommonRef<T1,T2> is
/// well-formed
template <typename T1, typename T2>
struct CommonReference
        < T1, T2, 1
        , require::AllOf
                < std::is_reference_v<T1>
                , std::is_reference_v<T2>
                , require::Requires<CommonRef<T1, T2>>
                >
        > {
    using type = CommonRef<T1, T2>;
};

/// Rule 2: otherwise, if \c basic_common_reference<...> is well-formed.
/// This is a customization point, it's useless for the standard C++17 so far.
template <typename T1, typename T2>
struct CommonReference<T1, T2, 2> {
    // Don't use this rule
};

/// Rule 3: otherwise, if \c CondRes<T1,T2> is well-formed
template <typename T1, typename T2>
struct CommonReference
        < T1, T2, 3
        , require::AllOf
                < require::Requires<CondRes<T1, T2>>
                >
        > {
    using type = CondRes<T1, T2>;
};

/// Rule 4: otherwise, if \c std::common_type_t<T1,T2> is well-formed
template <typename T1, typename T2>
struct CommonReference
        < T1, T2, 4
        , require::AllOf
                < require::Requires<std::common_type_t<T1, T2>>
                >
        > {
    using type = std::common_type_t<T1, T2>;
};

/// Rule 5: otherwise, there shall be no member \c type
template <typename T1, typename T2>
struct CommonReference<T1, T2, 5> {};

} // namespace impl

template <typename T, typename U>
inline constexpr bool IsCommonReferenceWith
        < T, U
        , require::AllOf
                < IsSameAs
                        < impl::CommonReferenceType<T, U>
                        , impl::CommonReferenceType<U, T>
                        >
                , IsConvertibleTo<T, impl::CommonReferenceType<T, U>>
                , IsConvertibleTo<U, impl::CommonReferenceType<T, U>>
                >
        > = true;

/// Implementation of \c std::assignable_from, see [concept.assignable]
template <typename Lhs, typename Rhs, typename = void>
inline constexpr bool IsAssignableFrom = false;

namespace impl {

template <typename Lhs, typename Rhs>
constexpr auto ForwardThenAssign(Lhs lhs, Rhs&& rhs) -> decltype( lhs = std::forward<Rhs>(rhs) );

} // namespace impl

template <typename Lhs, typename Rhs>
inline constexpr bool IsAssignableFrom
        < Lhs, Rhs
        , require::AllOf
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

/// Implementation of \c std::swappable, see [concept.swappable]
/// \note
/// So far, \c std::is_swappable_v can be used here with no harms, because
/// \c std::swappable just adds a CPO.
template <typename T>
inline constexpr bool IsSwappable = std::is_swappable_v<T>;

/// Implementation of \c std::destructible, see [concept.destructible]
template <typename T>
inline constexpr bool IsDestructible = std::is_nothrow_destructible_v<T>;

/// Implementation of \c std::constructible_from, see [concept.constructible]
template <typename T, typename... Args>
inline constexpr bool IsConstructibleFrom =
        IsDestructible<T> && std::is_constructible_v<T, Args...>;

/// Implementation of \c std::default_initializable, see [concept.default.init]
template <typename T, typename = void>
inline constexpr bool IsDefaultInitializable = false;

template <typename T>
inline constexpr bool IsDefaultInitializable
        < T
        , require::AllOf
                < IsConstructibleFrom<T>
                , require::Requires
                        < decltype( T{} )
                        , decltype( (void) ::new T )
                        >
                >
        > = true;

/// Implementation of \c std::move_constructible, see [concept.moveconstructible]
template <typename T>
inline constexpr bool IsMoveConstructible =
        IsConstructibleFrom<T, T> && IsConvertibleTo<T, T>;

/// Implementation of \c std::copy_constructible, see [concept.copyconstructible]
template <typename T>
inline constexpr bool IsCopyConstructible =
        IsMoveConstructible<T>
        && IsConstructibleFrom<T, T&> && IsConvertibleTo<T&, T>
        && IsConstructibleFrom<T, const T&> && IsConvertibleTo<const T&, T>
        && IsConstructibleFrom<T, const T> && IsConvertibleTo<const T, T>;

} // namespace lang


namespace comparison {

/// Implementation of the exposition-only concept "boolean-testable", see
/// [concept.booleantestable]
template <typename T, typename = void>
inline constexpr bool IsBooleanTestable = false;

namespace impl {

template <typename T>
inline constexpr bool IsBooleanTestableImpl = lang::IsConvertibleTo<T, bool>;

template <typename T>
constexpr auto forward_then_not(T&& t) -> decltype( !std::forward<T>(t) );

} // namespace impl

template <typename T>
inline constexpr bool IsBooleanTestable
        < T
        , require::AllOf
                < impl::IsBooleanTestableImpl<T>
                , impl::IsBooleanTestableImpl
                        < decltype( impl::forward_then_not(std::declval<T&&>()) )
                        >
                >
        > = true;

/// Implementation of the exposition-only concept "__WeaklyEqualityComparableWith",
/// see [concept.equalitycomparable]
template <typename T, typename U, typename = void>
inline constexpr bool IsWeaklyEqualityComparableWith = false;

namespace impl {

template <typename T, typename U>
constexpr auto equal_and_not_equal(const std::remove_reference_t<T>& t,
                                   const std::remove_reference_t<U>& u)
        -> decltype( t == u, t != u, u == t, u != t, void() );

} // namespace impl

template <typename T, typename U>
inline constexpr bool IsWeaklyEqualityComparableWith
        < T, U
        , require::AllOf
                < require::Requires<decltype( impl::equal_and_not_equal<T, U> )>
                >
        > = true;

} // namespace comparison


namespace object {


/// Implementation of \c std::movable, see [concepts.object]
template <typename T>
inline constexpr bool IsMovable =
        std::is_object_v<T> && lang::IsMoveConstructible<T>
        && lang::IsAssignableFrom<T&, T> && lang::IsSwappable<T>;

/// Implementation of \c std::copyable, see [concepts.object]
template <typename T>
inline constexpr bool IsCopyable =
        lang::IsCopyConstructible<T> && IsMovable<T> && lang::IsAssignableFrom<T&, T&>
        && lang::IsAssignableFrom<T&, const T&> && lang::IsAssignableFrom<T&, const T>;

/// Implementation of \c std::semiregular, see [concepts.object]
template <typename T>
inline constexpr bool IsSemiregular = IsCopyable<T> && lang::IsDefaultInitializable<T>;

} // namespace object


namespace iterator {


/// Implementation of \c std::weakly_incrementable, see [iterator.concept.winc]
template <typename I, typename = void>
inline constexpr bool IsWeaklyIncrementable = false;

namespace impl {

template <typename I>
constexpr auto pre_increment(I i) -> decltype( ++i );

template <typename I>
constexpr auto post_increment(I i) -> decltype( i++, void() );

// NOTE: this check is not as accurate as it should be
// TODO: no check for the `__int128` extension (gcc and clang both support it)
// TODO: no check for integer-class types
template <typename T>
inline constexpr bool IsSignedIntegerLike =
        std::is_integral_v<T> && std::is_signed_v<T>;

} // namespace impl

template <typename I>
inline constexpr bool IsWeaklyIncrementable
        < I
        , require::AllOf
                < object::IsMovable<I>
                , impl::IsSignedIntegerLike
                        // NOTE: check only iterators, so there is no need to
                        // implement `std::incrementable_traits`
                        < typename std::iterator_traits<I>::difference_type
                        >
                , lang::IsSameAs
                        < decltype( impl::pre_increment(std::declval<I>()) )
                        , I&
                        >
                , require::Requires<decltype( impl::post_increment(std::declval<I>()) )>
                >
        > = true;

/// Implementation of \c std::input_or_output_iterator, see [iterator.concept.iterator]
template <typename I, typename = void>
inline constexpr bool IsInputOrOutputIterator = false;

namespace impl {

template <typename T, typename = void>
inline constexpr bool CanReference = false;

template <typename T>
inline constexpr bool CanReference
        < T
        , require::AllOf<require::Requires<T&>>
        > = true;

template <typename I>
constexpr auto dereference(I i) -> decltype( *i );

} // namespace impl

template <typename I>
inline constexpr bool IsInputOrOutputIterator
        < I
        , require::AllOf
                < impl::CanReference
                        < decltype( impl::dereference(std::declval<I>()) )
                        >
                , IsWeaklyIncrementable<I>
                >
        > = true;

/// Implementation of \c std::sentinel_for, see [iterator.concept.sentinel]
template <typename I, typename S, typename = void>
inline constexpr bool IsSentinelFor = false;

template <typename I,  typename S>
inline constexpr bool IsSentinelFor
        < I, S
        , require::AllOf
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
        , typename = require::AllOf
                < lang::IsConvertibleTo<T, std::decay_t<T>>
                >
        >
constexpr auto decay_copy(T&& value) -> std::decay_t<decltype( std::forward<T>(value) )>;

template <typename T>
inline constexpr bool IsClassOrEnum =
        std::is_class_v<T> || std::is_union_v<T> || std::is_enum_v<T>;

// NOTE: these "poison pills" prevent following cases:
// - if unqualified lookup finds a non-function then ADL doesn't take place;
// - if ADL doesn't find anything, but there's a using-directive introducing
//   names from the namespace std into the global namespace, then adl_begin_impl
//   gives a false positive result.
template <typename R> void begin(R&) = delete;
template <typename R> void begin(const R&) = delete;

template <typename R, typename = void>
inline constexpr bool HasBeginMember = false;

template <typename R>
constexpr auto begin_member_impl(R& r) -> decltype( decay_copy(r.begin()) );

template <typename R>
inline constexpr bool HasBeginMember
        < R
        , require::AllOf
                < iterator::IsInputOrOutputIterator
                        < decltype( begin_member_impl(std::declval<R&>()) )
                        >
                >
        > = true;

template <typename R, typename = void>
inline constexpr bool HasAdlBegin = false;

template <typename R>
constexpr auto adl_begin_impl(R& r) -> decltype( decay_copy(begin(r)) );

template <typename R>
inline constexpr bool HasAdlBegin
        < R
        , require::AllOf
                < IsClassOrEnum<std::remove_reference_t<R>>
                , iterator::IsInputOrOutputIterator
                        < decltype( adl_begin_impl(std::declval<R&>()) )
                        >
                >
        > = true;

class Begin {
    template <typename R>
    static constexpr bool is_noexcept() noexcept {
        if constexpr (std::is_array_v<std::remove_reference_t<R>>) {
            return true;
        } else if constexpr (HasBeginMember<R>) {
            return noexcept( decay_copy(std::declval<R&>().begin()) );
        } else {
            return noexcept( decay_copy(begin(std::declval<R&>())) );
        }
    }

public:
    template
            < typename R
            , typename = require::AnyOf
                    < std::is_array_v<std::remove_reference_t<R>>
                    , HasBeginMember<R>
                    , HasAdlBegin<R>
                    >
            >
    constexpr auto operator()(R&& r) const noexcept(is_noexcept<R>()) {
        if constexpr (std::is_array_v<std::remove_reference_t<R>>) {
            static_assert(std::is_lvalue_reference_v<R>,
                          "Since an array can't be a borrowed range it must be "
                          "an lvalue reference.");
            return r + 0;
        } else if constexpr (HasBeginMember<R>) {
            return std::forward<R>(r).begin();
        } else {
            return begin(std::forward<R>(r));
        }
    }
};

} // namespace impl

inline constexpr impl::Begin BeginFn;

namespace impl {

// See the NOTE for similar "poison pills" for the begin functions
template <typename R> void end(R&) = delete;
template <typename R> void end(const R&) = delete;

template <typename R, typename = void>
inline constexpr bool HasEndMember = false;

template <typename R>
constexpr auto end_member_helper(R& r) -> decltype( decay_copy(r.end()) );

template <typename R>
inline constexpr bool HasEndMember
        < R
        , require::AllOf
                < iterator::IsSentinelFor
                        < decltype( BeginFn(std::declval<R&>()) )
                        , decltype( end_member_helper(std::declval<R&>()) )
                        >
                >
        > = true;

template <typename R, typename = void>
inline constexpr bool HasAdlEnd = false;

template <typename R>
constexpr auto adl_end_helper(R& r) -> decltype( decay_copy(end(r)) );

template <typename R>
inline constexpr bool HasAdlEnd
        < R
        , require::AllOf
                < IsClassOrEnum<std::remove_reference_t<R>>
                , iterator::IsSentinelFor
                        < decltype( BeginFn(std::declval<R&>()) )
                        , decltype( adl_end_helper(std::declval<R&>()) )
                        >
                >
        > = true;

template <typename T>
inline constexpr bool IsBoundedArray = false;

template <typename T, std::size_t N>
inline constexpr bool IsBoundedArray<T[N]> = true;

struct End {
    template <typename R>
    static constexpr bool is_noexcept() noexcept {
        if constexpr (IsBoundedArray<std::remove_reference_t<R>>) {
            return true;
        } else if constexpr (HasEndMember<R>) {
            return noexcept( decay_copy(std::declval<R&>().end()) );
        } else {
            return noexcept( decay_copy(end(std::declval<R&>())) );
        }
    }

public:
    template
            < typename R
            , typename = require::AnyOf
                    < IsBoundedArray<std::remove_reference_t<R>>
                    , HasEndMember<R>
                    , HasAdlEnd<R>
                    >
            >
    constexpr auto operator()(R&& r) const noexcept(is_noexcept<R>()) {
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

/// Implementation of \c std::ranges::range, see [range.range]
template <typename R, typename = void>
inline constexpr bool IsRange = false;

namespace impl {

template <typename R>
constexpr auto range_begin(R& r) -> decltype( BeginFn(r) );

template <typename R>
constexpr auto range_end(R& r) -> decltype( EndFn(r) );

} // namespace impl

template <typename R>
inline constexpr bool IsRange
        < R
        , require::AllOf
                < iterator::IsInputOrOutputIterator
                        < decltype( impl::range_begin(std::declval<R&>()) )
                        >
                , iterator::IsSentinelFor
                        < decltype( impl::range_begin(std::declval<R&>()) )
                        , decltype( impl::range_end(std::declval<R&>()) )
                        >
                >
        > = true;

} // namespace ranges

} // namespace traits

} // namespace flatten::details

#endif // FLATTEN_VIEW_DETAILS_TYPE_TRAITS_H
