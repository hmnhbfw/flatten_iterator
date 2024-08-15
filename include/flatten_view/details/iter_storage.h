/// \file iter_storage.h
/// Storage for a tuple of pairs iterator-sentinel
/// \author Pavel Tsayukov
/// \copyright MIT License

#ifndef FLATTEN_VIEW_DETAILS_ITER_STORAGE_H
#define FLATTEN_VIEW_DETAILS_ITER_STORAGE_H

#include "type_traits.h"

#include <cassert>
#include <tuple>
#include <type_traits>

#if defined(NDEBUG)
    #include <stdexcept>
#endif

namespace flatten::details {

/// Optional-like class that delegates the management of its resources to
/// an owner class, assuming that the owner class knows necessary invariants.
///
/// \tparam T  a type of an object optionally storing in \c NonOwningMaybe
///
/// \note The debug version of this class stores additional information, that
/// causes raising \c BadValueError if:
/// \li the destructor \c ~NonOwningMaybe is called, but \c T object is not
///     manually destroyed;
/// \li the \c operator== is called, but either left or right operands don't
///     have \c T object manually initialized;
/// \li the \c NonOwningMaybe::value() is called, but \c T object is not
///     manually initialized;
/// \li the \c NonOwningMaybe::init_value() is called, but \c T object has been
///     already manually initialized;
/// \li the \c NonOwningMaybe::mutate_existed_value() is called, but \c T object
///     is not manually initialized;
/// \li the \c NonOwningMaybe::destroy_existed_value() is called, but \c T
///     object is not manually initialized.
template <typename T>
class NonOwningMaybe {
    static constexpr struct {} Nothing = {};

    union {
        decltype( Nothing ) nothing_;
        T value_;
    };

private: // Debug information

#if defined(NDEBUG)
    enum class Tag { NOTHING, VALUE } tag_;

    class BadValueError : public std::runtime_error {
        using std::runtime_error::runtime_error;
    };
#endif

#if defined(NDEBUG)
    constexpr bool has_value() noexcept {
        return tag_ == Tag::VALUE;
    }
#endif

    template <bool B = true>
    static constexpr bool is_noexcept =
    #if defined(NDEBUG)
            false
    #else
            B
    #endif
            ;

public: // Constructor/Destructor

    constexpr NonOwningMaybe() noexcept(is_noexcept<>)
            : nothing_(Nothing)
    #if defined(NDEBUG)
            , tag_(Tag::NOTHING)
    #endif
            {}

#if __cplusplus >= 202002L
    constexpr
#endif
    ~NonOwningMaybe() noexcept(is_noexcept<>) {
    #if defined(NDEBUG)
        if (has_value()) {
            throw BadValueError(
                    "Destroying `NonOwningMaybe` is not allowed until its defined "
                    "value is manually destroyed. "
                    "Hint: use `destroy_existed_value` first.");
        }
    #endif
    }

public: // An object of this class is not allowed to manage its resources by itself

    NonOwningMaybe(const NonOwningMaybe&) = delete;
    NonOwningMaybe& operator=(const NonOwningMaybe&) = delete;
    NonOwningMaybe(NonOwningMaybe&&) = delete;
    NonOwningMaybe& operator=(NonOwningMaybe&&) = delete;

public: // Value equality

    friend constexpr bool operator==(const NonOwningMaybe& lhs, const NonOwningMaybe& rhs) noexcept(
            is_noexcept<noexcept( lhs.value_ == rhs.value_ )>) {
    #if defined(NDEBUG)
        if (!lhs.has_value() && !rhs.has_value()) {
            throw BadValueError(
                    "Both operands are not defined. "
                    "Hint: initialize their values first via `init_value`.");
        } else if (!lhs.has_value()) {
            throw BadValueError(
                    "Left operand is not defined. "
                    "Hint: initialize its value first via `init_value`.");
        } else if (!rhs.has_value()) {
            throw BadValueError(
                    "Right operand is not defined. "
                    "Hint: initialize its value first via `init_value`.");
        }
    #endif
        return lhs.value_ == rhs.value_;
    }

    friend constexpr bool operator!=(const NonOwningMaybe& lhs, const NonOwningMaybe& rhs) noexcept(
            is_noexcept<noexcept( !(lhs == rhs) )>) {
        return !(lhs == rhs);
    }

public: // Value access

    constexpr T& value() noexcept(is_noexcept<>) {
        return const_cast<T&>(
                static_cast<const NonOwningMaybe&>(*this).value());
    }

    constexpr const T& value() const noexcept(is_noexcept<>) {
    #if defined(NDEBUG)
        if (!has_value()) {
            throw BadValueError(
                    "Access to an undefined value. "
                    "Hint: initialize the value first via `init_value`.");
        }
    #endif
        return value_;
    }

public: // Value management

    template <typename... Args>
    constexpr void init_value(Args&&... args) noexcept(
            is_noexcept<noexcept( T(std::forward<Args>(args)...) )>) {
    #if defined(NDEBUG)
        if (has_value()) {
            throw BadValueError(
                    "Initialization of a defined value. "
                    "Hint: use `mutate_existed_value` instead.");
        }
    #endif
        (void) new(&value_) T(std::forward<Args>(args)...);
    #if defined(NDEBUG)
        tag_ = Tag::VALUE;
    #endif
    }

    template <typename U>
    constexpr void mutate_existed_value(U&& other) noexcept(
            is_noexcept<noexcept( value_ = std::forward<U>(other) )>) {
    #if defined(NDEBUG)
        if (!has_value()) {
            throw BadValueError(
                    "Access to an undefined value. "
                    "Hint: use `init_value` instead.");
        }
    #endif
        value_ = std::forward<U>(other);
    }

    constexpr void destroy_existed_value() noexcept(
            is_noexcept<std::is_nothrow_destructible_v<T>>) {
    #if defined(NDEBUG)
        if (!has_value()) {
            throw BadValueError(
                    "Destroying of a defined value. "
                    "Hint: initialize the value first via `init_value`.");
        }
    #endif
        value_.~T();
    #if defined(NDEBUG)
        tag_ = Tag::NOTHING;
    #endif
    }
};

template <typename Tuple, typename RangeTraits>
class IterStorage final {
    static_assert(false, "Instantiation of the primary template is not allowed here, "
                         "see the specialization and its requirements.");
};

/// Storage of pairs of iterators and their sentinels.
///
/// Let's say, we have a pair of an iterator \c I and its sentinel \c S, and say
/// that all their states between the closed interval \c [I,S] are root nodes of
/// an imaginary "iterator forest" in terms of the graph theory, i.e. a bunch of
/// trees. Let's say that the root nodes are at \c HeadDepth depth. Then each
/// node in the half-open interval \c [I,S) at the same depth except \c MaxDepth
/// has child nodes, from \c begin(*I) to \c end(*I). Those nodes, that haven't,
/// called leaves. Each \c I, whose \c *I is not iterable, is a leaf. The root
/// sentinel is always a leaf.
///
/// The "iterator forest" can contain only leaves as root nodes, e.g.
/// \c std::vector<int>::iterator models this case. In the other hand, we can
/// imagine a two-depth forest, e.g. \c std::vector<std::vector<int>>::iterator
/// or \c std::vector<std::string>::iterator, and so forth.
/// However, if we forcibly say that for \c std::vector<std::string>::iterator
/// \c MaxDepth is one, then \c std::vector<std::string>::iterator models a leaf
/// and doesn't go deeper.
///
/// Thus, the state of \c IterStorage object is a path between one of the root
/// nodes to one of the leaves. The path can have a length that's equal to
/// \c IterStorage::size(), although, the path from the root sentinel always
/// have zero length.
///
/// Let's say, the current path is \c [I0,I1,...Im,Im+1,...,In-2,In-1], where
/// \c I0 is not the root sentinel and \c n is \c IterStorage::size(). Then the
/// next state is the nearest path, that is, \c [I0,I1,...,Im,I'm+1,...,I'n-1],
/// where \c [I'm+1,...,I'n-1] is the updated part of the path, such that the
/// part's length is minimal, \c ++(Im+1)==I'm+1, and \c [I'm+2,...,I'n-1]
/// doesn't have the previous state. If \c I0 is the root sentinel, then there's
/// no next state. Similarly, the previous state is the nearest path, that is,
/// \c [I0,I1,...,Im,I'm+1,...,I'n-1], where \c [I'm+1,...,I'n-1] is the updated
/// part of the path, such that the part's length is minimal, \c --(Im+1)==I'm+1,
/// and \c [I'm+2,...,I'n-1] doesn't have the next state. If \c I0 is such that
/// \c --(*I0) is not defined or leads to undefined behavior, then that state
/// doesn't have the previous state.
///
/// E.g. if we have \c std::vector<std::vector<int>> object that's represented
/// by the following sequence \c {{1,2},{3},{},{4,5}}, then if the current state
/// has a leaf pointed to \c 2, then the next state has a leaf pointed to \c 3,
/// and the previous state has a leaf pointed to \c 1. The state which leaf
/// points to \c 3 has the next state which leaf points to \c 4 bypassing the
/// empty vector, and vice versa.
template <typename Head, typename... Tail, typename RangeTraits>
class IterStorage<std::tuple<Head, Tail...>, RangeTraits> final {
    /// Invariant: let's say \c Head is a subtuple of \c I (an iterator) and
    /// \c S (its sentinel), then if \c I==S, then the rest of subtuples is not
    /// defined, and \c IterStorage::empty() method yields \c true.
    std::tuple<Head, NonOwningMaybe<Tail>...> storage_;

private: // Shortcuts for RangeTraits variable templates and constants

    template <typename I>
    static constexpr bool IsInOrOutIterator = RangeTraits::template input_or_output_iterator<I>;

    template <typename I, typename S>
    static constexpr bool IsSentinelFor = RangeTraits::template sentinel_for<I, S>;

    static constexpr auto BeginFn = RangeTraits::begin;
    static constexpr auto EndFn = RangeTraits::end;

private: // Iterator and sentinel types

    template <typename TupleLike>
    using IteratorFrom = std::tuple_element_t<0, TupleLike>;

    template <typename TupleLike>
    using SentinelFrom = std::tuple_element_t<1, TupleLike>;

    using Iterator = IteratorFrom<Head>;
    using Sentinel = SentinelFrom<Head>;

private: // Location in the storage

    static constexpr std::size_t IteratorIndex = 0;
    static constexpr std::size_t SentinelIndex = 1;

    static constexpr std::size_t HeadDepth = 0;
    static constexpr std::size_t MaxDepth = std::tuple_size_v<decltype( storage_ )> - 1;

    static constexpr std::size_t up(std::size_t depth) noexcept {
        return depth - 1;
    }

    static constexpr std::size_t down(std::size_t depth) noexcept {
        return depth + 1;
    }

    static constexpr bool is_head(std::size_t depth) noexcept {
        return depth == HeadDepth;
    }

    static constexpr bool is_max(std::size_t depth) noexcept {
        return depth == MaxDepth;
    }

    static constexpr bool is_in_range(std::size_t depth) noexcept {
        return depth <= MaxDepth;
    }

private: // `Tuple` must be pairs of an iterator and its sentinel

    static_assert(((std::tuple_size_v<Tail> == 2) && ...),
                  "Each tuple must have two elements, that is, an iterator and its sentinel.");
    static_assert((IsInOrOutIterator<Iterator> && ... && IsInOrOutIterator<IteratorFrom<Tail>>),
                  "The first element of each tuple must be at least "
                  "an input or output iterator.");
    static_assert((IsSentinelFor<Iterator, Sentinel>
                  && ... && IsSentinelFor<IteratorFrom<Tail>, SentinelFrom<Tail>>),
                  "Each tuple must be a pair of an iterator and its sentinel.");

private: // Helper functions to use in unevaluating contexts

    static constexpr auto this_ref() -> IterStorage&;
    static constexpr auto const_this_ref() -> const IterStorage&;
    static constexpr auto this_rvalue_ref() -> IterStorage&&;

private: // Noexcept checks

    template <typename... Ts>
    static constexpr bool IsNothrowDestructible = (std::is_nothrow_destructible_v<Ts> && ...);

    template <typename... Ts>
    static constexpr bool IsNothrowCopyConstructible =
            (std::is_nothrow_copy_constructible_v<Ts> && ...);

    template <typename... Ts>
    static constexpr bool IsNothrowMoveConstructible =
            (std::is_nothrow_move_constructible_v<Ts> && ...);

    template <typename... Ts>
    static constexpr bool IsNothrowSwappable = (std::is_nothrow_swappable_v<Ts> && ...);

    static constexpr bool is_equal_noexcept(const IterStorage& lhs, const IterStorage& rhs)
            noexcept {
        if constexpr (size() == 1) {
            return noexcept( lhs.subtuple<HeadDepth>() == rhs.subtuple<HeadDepth>() );
        } else {
            return noexcept( lhs.storage_ == rhs.storage_ );
        }
    }

    template <std::size_t Depth>
    constexpr bool is_nothrow_constructible_from_begin() noexcept {
        static_assert(!is_head(Depth) && is_in_range(Depth));
        using I = decltype( BeginFn(*current<up(Depth)>()) );
        return std::is_nothrow_move_constructible_v<I>
                && noexcept( BeginFn(*current<up(Depth)>()) );
    }

    template <std::size_t Depth>
    constexpr bool is_nothrow_constructible_from_end() noexcept {
        static_assert(!is_head(Depth) && is_in_range(Depth));
        using S = decltype( EndFn(*current<up(Depth)>()) );
        return std::is_nothrow_move_constructible_v<S>
                && noexcept( EndFn(*current<up(Depth)>()) );
    }

    template <std::size_t Depth>
    constexpr bool is_nothrow_assignable_from_begin() noexcept {
        static_assert(!is_head(Depth) && is_in_range(Depth));
        using I = decltype( BeginFn(*current<up(Depth)>()) );
        return std::is_nothrow_move_assignable_v<I>
                && noexcept( BeginFn(*current<up(Depth)>()) );
    }

    template <std::size_t Depth>
    constexpr bool is_nothrow_assignable_from_end() noexcept {
        static_assert(!is_head(Depth) && is_in_range(Depth));
        using S = decltype( EndFn(*current<up(Depth)>()) );
        return std::is_nothrow_move_assignable_v<S>
                && noexcept( EndFn(*current<up(Depth)>()) );
    }

    template <std::size_t Depth>
    constexpr bool is_nothrow_dereference() noexcept {
        static_assert(is_in_range(Depth));
        return noexcept( *current<Depth>() );
    }

    template <std::size_t Depth>
    constexpr bool is_nothrow_increment() noexcept {
        static_assert(is_in_range(Depth));
        return noexcept( ++current<Depth>() );
    }

    template <std::size_t Depth>
    constexpr bool is_nothrow_decrement() noexcept {
        static_assert(is_in_range(Depth));
        return noexcept( --current<Depth>() );
    }

    template <std::size_t Depth>
    constexpr bool is_nothrow_equal() noexcept {
        static_assert(is_in_range(Depth));
        return noexcept( current<Depth>() == sentinel<Depth>() );
    }

    template <std::size_t Depth>
    constexpr bool is_nothrow_not_equal() noexcept {
        static_assert(is_in_range(Depth));
        return noexcept( current<Depth>() != sentinel<Depth>() );
    }

public: // Constructor/Destructor/Assignment operators

    /// \par Exception Safety:
    /// Strong exception guarantee.
    constexpr IterStorage(Iterator first, Sentinel last) noexcept(
            noexcept( std::make_tuple(std::move(first), std::move(last)) )
            && noexcept( construct_down_to_leftmost_leaf() ))
            : storage_(std::make_tuple(std::move(first), std::move(last)),
                       NonOwningMaybe<Tail>()...) {
        construct_down_to_leftmost_leaf();
    }

    ~IterStorage() noexcept(IsNothrowDestructible<Head, Tail...>) {
        if constexpr (size() > 1) {
            if (!empty()) {
                destroy_tail_after();
            }
        }
    }

    /// \par Exception Safety:
    /// Strong exception guarantee.
    constexpr IterStorage(const IterStorage& other) noexcept(
            IsNothrowCopyConstructible<Head, Tail...>)
            : storage_(other.subtuple<HeadDepth>(), NonOwningMaybe<Tail>()...) {
        if constexpr (size() > 1) {
            if (!empty()) {
                construct_tail_after(other);
            }
        }
    }

    /// \par Exception Safety:
    /// Strong exception guarantee if \c swap of iterators and sentinels doesn't
    /// throw any exceptions, otherwise, the states of \c *this and \c rhs is
    /// undefined.
    constexpr IterStorage& operator=(const IterStorage& rhs) noexcept(
            std::is_nothrow_copy_constructible_v<IterStorage>
            && std::is_nothrow_swappable_v<IterStorage>) {
        swap(*this, IterStorage(rhs));
    }

    /// \par Exception Safety:
    /// Strong exception guarantee if moving iterators and sentinels doesn't
    /// throw any exceptions, otherwise, the state of \c other is undefined.
    constexpr IterStorage(IterStorage&& other) noexcept(
            IsNothrowMoveConstructible<Head, Tail...>)
            : storage_(std::move(other.subtuple<HeadDepth>()), NonOwningMaybe<Tail>()...) {
        if constexpr (size() > 1) {
            if (!empty()) {
                construct_tail_after(std::move(other));
            }
        }
    }

    /// \par Exception Safety:
    /// Strong exception guarantee if \c swap of iterators and sentinels doesn't
    /// throw any exceptions, otherwise, the states of \c *this and \c rhs is
    /// undefined.
    constexpr IterStorage& operator=(IterStorage&& rhs) noexcept(
            std::is_nothrow_swappable_v<IterStorage>) {
        swap(*this, rhs);
    }

public: // Swap

    /// \par Exception Safety:
    /// Strong exception guarantee if \c swap of iterators and sentinels doesn't
    /// throw any exceptions, otherwise, the states of \c lhs and \c rhs is
    /// undefined.
    friend constexpr void swap(IterStorage& lhs, IterStorage& rhs) noexcept(
            IsNothrowSwappable<Head, Tail...>
            && IsNothrowMoveConstructible<Tail...>) {
        if constexpr (size() > 1) {
            const bool is_lhs_tail_init = !lhs.empty();
            const bool is_rhs_tail_init = !rhs.empty();
            if (is_lhs_tail_init && is_rhs_tail_init) {
                swap_tail_after(lhs, rhs);
            } else if (is_lhs_tail_init) {
                rhs.construct_tail_after(std::move(lhs));
            } else if (is_rhs_tail_init) {
                lhs.construct_tail_after(std::move(rhs));
            }
        }
        using std::swap;
        swap(lhs.subtuple<HeadDepth>(), rhs.subtuple<HeadDepth>());
    }

public: // Capacity

    /// Length of a path between any root node, except the root sentinel, and
    /// any leaf.
    static constexpr std::size_t size() noexcept {
        return MaxDepth + 1;
    }

private: // Subtuple access

    template <typename T>
    using MutRef = std::remove_const_t<std::remove_reference_t<T>>&;

    template <std::size_t Depth>
    constexpr auto& get() noexcept {
        using Ref = MutRef<decltype( const_this_ref().template get<Depth>() )>;
        return const_cast<Ref>(
                static_cast<const IterStorage&>(*this).get<Depth>());
    }

    template <std::size_t Depth>
    constexpr const auto& get() const noexcept {
        static_assert(is_in_range(Depth));
        return std::get<Depth>(storage_);
    }

    template <std::size_t Depth>
    constexpr auto& subtuple() & noexcept {
        using Ref = MutRef<decltype( const_this_ref().template subtuple<Depth>() )>;
        return const_cast<Ref>(
                static_cast<const IterStorage&>(*this).subtuple<Depth>());
    }

    template <std::size_t Depth>
    constexpr auto&& subtuple() && noexcept {
        using Ref = MutRef<decltype( const_this_ref().template subtuple<Depth>() )>;
        return std::move(const_cast<Ref>(
                static_cast<const IterStorage&>(*this).subtuple<Depth>()));
    }

    template <std::size_t Depth>
    constexpr const auto& subtuple() const & noexcept {
        if constexpr (is_head(Depth)) {
            return get<HeadDepth>();
        } else {
            return get<Depth>().value();
        }
    }

private: // Iterator/Sentinel access

    template <std::size_t Depth>
    constexpr auto& current() noexcept {
        using Ref = MutRef<decltype( const_this_ref().template current<Depth>() )>;
        return const_cast<Ref>(
                static_cast<const IterStorage&>(*this).current<Depth>());
    }

    template <std::size_t Depth>
    constexpr const auto& current() const noexcept {
        return std::get<IteratorIndex>(subtuple<Depth>());
    }

    template <std::size_t Depth>
    constexpr auto& sentinel() noexcept {
        using Ref = MutRef<decltype( const_this_ref().template sentinel<Depth>() )>;
        return const_cast<Ref>(
                static_cast<const IterStorage&>(*this).sentinel<Depth>());
    }

    template <std::size_t Depth>
    constexpr const auto& sentinel() const noexcept {
        return std::get<SentinelIndex>(subtuple<Depth>());
    }

private: // Tail subtuple initializing

    template <std::size_t Depth = HeadDepth>
    constexpr bool empty() const noexcept {
        static_assert(is_in_range(Depth));
        assert(is_head(Depth) || !empty());
        return current<Depth>() == sentinel<Depth>();
    }

private: // Deleters

    template <std::size_t Depth>
    using Subtuple = std::remove_reference_t<decltype( this_ref().template subtuple<Depth>() )>;

    template <std::size_t Depth>
    constexpr void destroy() noexcept(IsNothrowDestructible<Subtuple<Depth>>) {
        static_assert(is_in_range(Depth) && !is_head(Depth));
        assert(!empty());

        get<Depth>().destroy_existed_value();
    }

    template <std::size_t Depth = HeadDepth, std::size_t LastDepth = MaxDepth>
    constexpr void destroy_tail_after() noexcept(IsNothrowDestructible<Tail...>) {
        if constexpr (Depth < LastDepth) {
            destroy<down(Depth)>();
            return destroy_tail_after<down(Depth)>();
        }
    }

private: // Tail subtuple copy/move

    template <typename U>
    static constexpr bool is_nothrow_construct_tail() noexcept {
        if constexpr (std::is_lvalue_reference_v<U&&>) {
            return IsNothrowCopyConstructible<Tail...>;
        } else {
            return IsNothrowMoveConstructible<Tail...>;
        }
    }

    template <typename U>
    constexpr void construct_tail_after(U&& other) noexcept(
            is_nothrow_construct_tail<U&&>) {
        static_assert(std::is_same_v<std::decay_t<U>, IterStorage>);
        if constexpr (is_nothrow_construct_tail<U&&>) {
            return nothrow_construct_tail_impl(std::forward<U>(other));
        } else {
            return construct_tail_impl(std::forward<U>(other));
        }
    }

    template <std::size_t Depth = HeadDepth, typename U>
    constexpr void nothrow_construct_tail_impl(U&& other) noexcept {
        static_assert(is_in_range(Depth));

        if constexpr (!is_max(Depth)) {
            get<down(Depth)>().init_value(
                    std::forward<U>(other).template subtuple<down(Depth)>());
            return nothrow_construct_tail_impl<down(Depth)>(std::forward<U>(other));
        }
    }

    template <std::size_t Depth = HeadDepth, typename U>
    void construct_tail_impl(U&& other) {
        static_assert(is_in_range(Depth));

        if constexpr (!is_max(Depth)) {
            try {
                get<down(Depth)>().init_value(
                        std::forward<U>(other).template subtuple<down(Depth)>());
            } catch (...) {
                destroy_tail_after<HeadDepth, Depth>();
                throw;
            }
            return construct_tail_impl<down(Depth)>(std::forward<U>(other));
        }
    }

private: // Swap tail subtuples

    template <std::size_t Depth>
    static constexpr bool is_nothrow_swap_tail() noexcept {
        static_assert(is_in_range(Depth));

        if constexpr (is_max(Depth)) {
            return true;
        } else {
            return IsNothrowSwappable<Tail...>;
        }
    }

    template <std::size_t Depth = HeadDepth>
    static constexpr void swap_tail_after(IterStorage& lhs, IterStorage& rhs) noexcept(
            is_nothrow_swap_tail<Depth>()) {
        static_assert(is_in_range(Depth));

        if constexpr (!is_max(Depth)) {
            {
                using std::swap;
                swap(lhs.subtuple<down(Depth)>(), rhs.subtuple<down(Depth)>());
            }
            return swap_tail_after<down(Depth)>(lhs, rhs);
        }
    }

private: // Back and forward traversing

    template <std::size_t Depth = HeadDepth>
    constexpr bool is_nothrow_construct_down_to_leftmost_leaf(bool acc = true) noexcept {
        static_assert(is_in_range(Depth));

        if constexpr (size() == 1 || is_max(Depth)) {
            return acc;
        } else {
            return is_nothrow_construct_down_to_leftmost_leaf<down(Depth)>(
                    acc
                    && is_nothrow_equal<Depth>() && is_nothrow_not_equal<Depth>()
                    && is_nothrow_increment<Depth>()
                    && is_nothrow_dereference<Depth>()
                    && is_nothrow_constructible_from_begin<down(Depth)>()
                    && is_nothrow_constructible_from_end<down(Depth)>()
                    && is_nothrow_not_equal<down(Depth)>()
                    && (is_head(Depth) || noexcept( destroy<Depth>() )));
        }
    }

    /// Construct a path from the current node at \c Depth depth to the nearest
    /// leaf, such that the path doesn't have the previous state.
    template <std::size_t Depth = HeadDepth>
    constexpr void construct_down_to_leftmost_leaf() noexcept(
            is_nothrow_construct_down_to_leftmost_leaf()) {
        static_assert(is_in_range(Depth));

        if constexpr (size() > 1 && !is_max(Depth)) {
            for (; !empty<Depth>(); ++current<Depth>()) {
                auto&& range = *current<Depth>();
                auto first = BeginFn(range);
                auto last = EndFn(range);
                if (first == last) {
                    continue;
                }
                get<down(Depth)>().init_value(
                        std::make_tuple(std::move(first), std::move(last)));
                return construct_down_to_leftmost_leaf<down(Depth)>();
            }
            if constexpr (!is_head(Depth)) {
                destroy<Depth>();
                ++current<up(Depth)>();
                return construct_down_to_leftmost_leaf<up(Depth)>();
            }
        }
    }

    /// Construct a path, such that it is the next state for the current one.
    /// \note The behavior is undefined if the current path doesn't have the
    /// next state, e.g. it is the root sentinel.
    constexpr void next() { // TODO: noexcept
        assert(!empty());

        if constexpr (size() == 1) {
            return ++current<HeadDepth>();
        } else {
            if (++current<MaxDepth>() == sentinel<MaxDepth>()) {
                return next_up_to<up(MaxDepth)>();
            }
        }
    }

    template <std::size_t Depth>
    constexpr void next_up_to() { // TODO: noexcept
        static_assert(!is_max(Depth) && is_in_range(Depth));
        assert(!empty() && size() > 1);

        if (++current<Depth>() == sentinel<Depth>()) {
            if constexpr (is_head(Depth)) {
                return destroy_tail_after();
            } else {
                return next_up_to<up(Depth)>();
            }
        }
        return next_down_from<Depth>();
    }

    template <std::size_t Depth>
    constexpr void next_down_from() { // TODO: noexcept
        static_assert(!is_head(Depth) && is_in_range(Depth));
        assert(!empty() && size() > 1);

        if constexpr (!is_max(Depth)) {
            for (; !empty<Depth>(); ++current<Depth>()) {
                auto&& range = *current<Depth>();
                auto first = BeginFn(range);
                auto last = EndFn(range);
                if (first == last) {
                    continue;
                }
                get<down(Depth)>().mutate_existed_value(
                        std::make_tuple(std::move(first), std::move(last)));
                return next_down_from<down(Depth)>();
            }
            return next_up_to<up(Depth)>();
        }
    }

    template <std::size_t Depth>
    constexpr bool can_decrement() noexcept(
            noexcept( current<Depth>() != BeginFn(*current<up(Depth)>()) )) {
        static_assert(Depth > HeadDepth && is_in_range(Depth));
        assert(!empty());
        return current<Depth>() != BeginFn(*current<up(Depth)>());
    }

    /// Construct a path, such that it is the previous state for the current one.
    /// \note The behavior is undefined if the current path doesn't have the
    /// previous state, e.g. it is the first root node.
    constexpr void prev() { // TODO: noexcept
        if constexpr (size() == 1) {
            return --current<HeadDepth>();
        } else {
            if (empty()) {
                return construct_down_to_rightmost_leaf();
            } else if (can_decrement<MaxDepth>()) {
                return --current<MaxDepth>();
            }
            return prev_up_to<up(MaxDepth)>();
        }
    }

    template <std::size_t Depth = HeadDepth>
    constexpr void construct_down_to_rightmost_leaf() { // TODO: noexcept
        static_assert(size() > 1 && is_in_range(Depth));

        if constexpr (is_max(Depth)) {
            return --current<MaxDepth>();
        } else if constexpr (is_head(Depth)) {
            auto predicate = []() noexcept { return true; };
            return init_backforward_to_last_non_empty_range_until<Depth>(std::move(predicate));
        } else {
            auto predicate = [this, begin = BeginFn(*current<up(Depth)>())]() noexcept(
                    is_nothrow_constructible_from_begin<Depth>()
                    && noexcept( current<Depth>() != begin )) {
                return current<Depth>() != begin;
            };
            init_backforward_to_last_non_empty_range_until<Depth>(std::move(predicate));
            destroy<Depth>();
            return construct_down_to_rightmost_leaf<up(Depth)>();
        }
    }

    template <std::size_t Depth, typename Predicate>
    constexpr void init_backforward_to_last_non_empty_range_until(Predicate&& predicate) { // TODO: noexcept
        static_assert(size() > 1 && is_in_range(Depth));
        do {
            auto&& range = *--current<Depth>();
            auto first = BeginFn(range);
            auto last = EndFn(range);
            if (first == last) {
                continue;
            }
            first = EndFn(range);
            subtuple<down(Depth)>().init_value(
                    std::make_tuple(std::move(first), std::move(last)));
            return construct_down_to_rightmost_leaf<down(Depth)>();
        } while (predicate());
    }

    template <std::size_t Depth>
    constexpr void prev_up_to() { // TODO: noexcept
        static_assert(size() > 1 && !is_max(Depth) && is_in_range(Depth));
        assert(!empty());

        if constexpr (is_head(Depth)) {
            return prev_down_from<HeadDepth>();
        } else {
            if (can_decrement<Depth>()) {
                return prev_down_from<Depth>();
            }
            return prev_up_to<up(Depth)>();
        }
    }

    // TODO: revise
    template <std::size_t Depth>
    constexpr void prev_down_from() { // TODO: noexcept
        static_assert(size() > 1 && is_in_range(Depth));
        assert(!empty());

        if constexpr (is_max(Depth)) {
            return --current<MaxDepth>();
        } else if constexpr (is_head(Depth)) {
            auto predicate = []() noexcept { return true; };
            return mutate_backforward_to_last_non_empty_range_until<Depth>(std::move(predicate));
        } else {
            auto predicate = [this, begin = BeginFn(*current<up(Depth)>())]() noexcept(
                    is_nothrow_constructible_from_begin<Depth>()
                    && noexcept( current<Depth>() != begin )) {
                return current<Depth>() != begin;
            };
            mutate_backforward_to_last_non_empty_range_until<Depth>(std::move(predicate));
            return prev_up_to<up(Depth)>();
        }
    }

    template <std::size_t Depth, typename Predicate>
    constexpr void mutate_backforward_to_last_non_empty_range_until(Predicate&& predicate) { // TODO: noexcept
        static_assert(size() > 1 && is_in_range(Depth));
        do {
            auto&& range = *--current<Depth>();
            auto first = BeginFn(range);
            auto last = EndFn(range);
            if (first == last) {
                continue;
            }
            first = EndFn(range);
            subtuple<down(Depth)>().mutate_existed_value(
                    std::make_tuple(std::move(first), std::move(last)));
            return prev_down_from<down(Depth)>();
        } while (predicate());
    }

public: // Basic iterator operators

    constexpr auto operator*() noexcept(
            noexcept( *current<MaxDepth>() ))
            -> decltype( *current<MaxDepth>() ) {
        assert(!empty());
        return *current<MaxDepth>();
    }

    constexpr auto operator++() noexcept(
            noexcept( next() ))
            -> decltype( *(*this) ) {
        assert(!empty());
        next();
        return *(*this);
    }

    constexpr auto operator--() noexcept(
            noexcept( next() ))
            -> decltype( *(*this) ) {
        assert(!empty());
        prev();
        return *(*this);
    }

    friend constexpr bool operator==(const IterStorage& lhs, const IterStorage& rhs) noexcept(
            is_equal_noexcept()) {
        if constexpr (size() == 1) {
            return lhs.subtuple<HeadDepth>() == rhs.subtuple<HeadDepth>();
        } else {
            const bool is_lhs_tail_init = !lhs.empty();
            const bool is_rhs_tail_init = !rhs.empty();
            if (is_lhs_tail_init != is_rhs_tail_init) {
                return false;
            }
            if (is_lhs_tail_init) {
                return lhs.storage_ == rhs.storage_;
            } else {
                return lhs.subtuple<HeadDepth>() == rhs.subtuple<HeadDepth>();
            }
        }
    }

    friend constexpr bool operator!=(const IterStorage& lhs, const IterStorage& rhs) noexcept(
            noexcept( !(lhs == rhs) )) {
        return !(lhs == rhs);
    }
};

} // namespace flatten::details

#endif // FLATTEN_VIEW_DETAILS_ITER_STORAGE_H
