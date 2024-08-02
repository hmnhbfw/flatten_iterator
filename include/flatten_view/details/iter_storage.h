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
        if (tag_ == Tag::VALUE) {
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

public: // Value access

    constexpr T& value() noexcept(is_noexcept<>) {
        return const_cast<T&>(
                static_cast<const NonOwningMaybe&>(*this).value());
    }

    constexpr const T& value() const noexcept(is_noexcept<>) {
    #if defined(NDEBUG)
        if (tag_ == Tag::NOTHING) {
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
        if (tag_ == Tag::VALUE) {
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
        if (tag_ == Tag::NOTHING) {
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
        if (tag_ == Tag::NOTHING) {
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

/// Storage of pairs of an iterator and its sentinel.
/// \note
/// The storage's invariant: if the first (aka top, head) iterator is equal to
/// its sentinel then other pairs are undefined.
template <typename Head, typename... Tail, typename RangeTraits>
class IterStorage<std::tuple<Head, Tail...>, RangeTraits> final {
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

    static constexpr bool is_in_range(std::size_t depth) noexcept {
        return depth <= MaxDepth;
    }

    static constexpr bool is_single_subtuple() noexcept {
        return HeadDepth == MaxDepth;
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

public: // Constructor/Destructor/Assignment operators

    /// \par Exception Safety:
    /// Strong exception guarantee.
    constexpr IterStorage(Iterator first, Sentinel last) noexcept(
            noexcept( std::make_tuple(std::move(first), std::move(last)) ))
            : storage_(std::make_tuple(std::move(first), std::move(last)),
                       NonOwningMaybe<Tail>()...) {
        forward_to_nearest_leaf();
    }

    ~IterStorage() noexcept(IsNothrowDestructible<Head, Tail...>) {
        if constexpr (!is_single_subtuple()) {
            if (non_empty()) {
                destroy_tail_after();
            }
        }
    }

    /// \par Exception Safety:
    /// Strong exception guarantee.
    constexpr IterStorage(const IterStorage& other) noexcept(
            IsNothrowCopyConstructible<Head, Tail...>)
            : storage_(other.subtuple<HeadDepth>(), NonOwningMaybe<Tail>()...) {
        if constexpr (!is_single_subtuple()) {
            if (non_empty()) {
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
        if constexpr (!is_single_subtuple()) {
            if (non_empty()) {
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
        if constexpr (!is_single_subtuple()) {
            const bool is_lhs_tail_init = lhs.non_empty();
            const bool is_rhs_tail_init = rhs.non_empty();
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

    constexpr std::size_t size() const noexcept {
        return MaxDepth + 1;
    }

private: // Subtuple access

    template <typename T>
    using MutRef = std::remove_const_t<std::remove_reference_t<T>>&;

    template <typename T>
    using MutRvalueRef = std::remove_const_t<std::remove_reference_t<T>>&&;

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
        using RvalueRef = MutRvalueRef<Ref>;
        return std::move(const_cast<Ref>(
                static_cast<const IterStorage&>(*this).subtuple<Depth>()));
    }

    template <std::size_t Depth>
    constexpr const auto& subtuple() const & noexcept {
        if constexpr (Depth == HeadDepth) {
            return get<HeadDepth>();
        } else {
            return get<Depth>().value();
        }
    }

public: // Iterator/Sentinel access

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

    constexpr bool non_empty() const noexcept {
        return current<HeadDepth>() != sentinel<HeadDepth>();
    }

private: // Deleters

    template <std::size_t Depth>
    using Subtuple = std::remove_reference_t<decltype( this_ref().template subtuple<Depth>() )>;

    template <std::size_t Depth>
    constexpr void destroy() noexcept(IsNothrowDestructible<Subtuple<Depth>>) {
        static_assert(is_in_range(Depth) && Depth != HeadDepth);
        assert(non_empty());

        get<Depth>().destroy_existed_value();
    }

    template <std::size_t Depth = HeadDepth, std::size_t LastDepth = MaxDepth>
    constexpr void destroy_tail_after() noexcept(IsNothrowDestructible<Tail...>) {
        if constexpr (Depth >= LastDepth) {
            return;
        } else {
            constexpr auto NextDepth = Depth + 1;
            destroy<NextDepth>();
            return destroy_tail_after<NextDepth>();
        }
    }

private: // Tail subtuple copy/move

    template <typename U>
    static constexpr bool is_construct_tail_noexcept() noexcept {
        if constexpr (std::is_lvalue_reference_v<U&&>) {
            return IsNothrowCopyConstructible<Tail...>;
        } else {
            return IsNothrowMoveConstructible<Tail...>;
        }
    }

    template <typename U>
    constexpr void construct_tail_after(U&& other) noexcept(
            is_construct_tail_noexcept<U&&>) {
        static_assert(std::is_same_v<std::decay_t<U>, IterStorage>);
        if constexpr (is_construct_tail_noexcept<U&&>) {
            return nothrow_construct_tail_impl(std::forward<U>(other));
        } else {
            return construct_tail_impl(std::forward<U>(other));
        }
    }

    template <std::size_t Depth = HeadDepth, typename U>
    constexpr void nothrow_construct_tail_impl(U&& other) noexcept {
        if constexpr (Depth >= MaxDepth) {
            return;
        } else {
            constexpr auto NextDepth = Depth + 1;
            get<NextDepth>().init_value(std::forward<U>(other).template subtuple<NextDepth>());
            return nothrow_construct_tail_impl<NextDepth>(std::forward<U>(other));
        }
    }

    template <std::size_t Depth = HeadDepth, typename U>
    void construct_tail_impl(U&& other) {
        if constexpr (Depth >= MaxDepth) {
            return;
        } else {
            constexpr auto NextDepth = Depth + 1;
            try {
                get<NextDepth>().init_value(std::forward<U>(other).template subtuple<NextDepth>());
            } catch (...) {
                destroy_tail_after<HeadDepth, NextDepth - 1>();
                throw;
            }
            return construct_tail_impl<NextDepth>(std::forward<U>(other));
        }
    }

private: // Swap tail subtuples

    template <std::size_t Depth>
    static constexpr bool is_swap_tail_noexcept() noexcept {
        if constexpr (Depth >= MaxDepth) {
            return true;
        } else {
            return IsNothrowSwappable<Tail...>;
        }
    }

    template <std::size_t Depth = HeadDepth>
    static constexpr void swap_tail_after(IterStorage& lhs, IterStorage& rhs) noexcept(
            is_swap_tail_noexcept<Depth>()) {
        if constexpr (Depth >= MaxDepth) {
            return;
        } else {
            constexpr auto NextDepth = Depth + 1;
            {
                using std::swap;
                swap(lhs.subtuple<NextDepth>(), rhs.subtuple<NextDepth>());
            }
            return swap_tail_after<NextDepth>(lhs, rhs);
        }
    }

private: // TODO: name the section

    /// Look at \c MaxDepth subtuple for the nearest pair such that
    /// the iterator is not equal to its sentinel, otherwise, in \c HeadDepth
    /// subtuple the iterator is equal to its sentinel, e.i. the current
    /// storage models a sentinel.
    template <std::size_t Depth = HeadDepth>
    constexpr void forward_to_nearest_leaf() { // TODO: noexcept
        static_assert(is_in_range(Depth));

        if constexpr (is_single_subtuple()) {
            return;
        } else {
            for (; current<Depth>() != sentinel<Depth>(); ++current<Depth>()) {
                auto&& range = *current<Depth>();
                auto first = BeginFn(range);
                auto last = EndFn(range);
                if (first != last) {
                    get<Depth + 1>().init_value(std::make_tuple(std::move(first), std::move(last)));
                    if constexpr (Depth + 1 == MaxDepth) {
                        return;
                    } else {
                        return forward_to_nearest_leaf<Depth + 1>();
                    }
                }
            }
            if constexpr (Depth == HeadDepth) {
                return;
            } else {
                destroy<Depth>();
                ++current<Depth - 1>();
                return forward_to_nearest_leaf<Depth - 1>();
            }
        }
    }

    template <std::size_t Depth = MaxDepth>
    constexpr void next() { // TODO: noexcept
        static_assert(is_in_range(Depth));
        assert(non_empty());

        if constexpr (is_single_subtuple()) {
            ++current<HeadDepth>();
            return;
        } else {
            if (++current<Depth>() == sentinel<Depth>()) {
                if constexpr (Depth == HeadDepth) {
                    destroy_tail_after();
                    return;
                } else {
                    return next<Depth - 1>();
                }
            } else {
                if constexpr (Depth == MaxDepth) {
                    return;
                } else {
                    return next_down<Depth>();
                }
            }
        }
    }

    template <std::size_t Depth>
    constexpr void next_down() { // TODO: noexcept
        static_assert(Depth != HeadDepth && is_in_range(Depth));
        assert(non_empty());

        for (; current<Depth>() != sentinel<Depth>(); ++current<Depth>()) {
            auto&& range = *current<Depth>();
            auto first = BeginFn(range);
            auto last = EndFn(range);
            if (first != last) {
                get<Depth + 1>().mutate_existed_value(
                        std::make_tuple(std::move(first), std::move(last)));
                if constexpr (Depth + 1 == MaxDepth) {
                    return;
                } else {
                    return next_down<Depth + 1>();
                }
            }
        }
        if constexpr (Depth == HeadDepth) {
            return;
        } else {
            ++current<Depth - 1>();
            return next_down<Depth - 1>();
        }
    }

    template <std::size_t Depth = MaxDepth>
    constexpr auto prev() { // TODO: noexcept
        // TODO: impl
    }

public: // Basic iterator operators

    constexpr auto operator*() noexcept(
            noexcept( *current<MaxDepth>() ))
            -> decltype( *current<MaxDepth>() ) {
        assert(non_empty());
        return *current<MaxDepth>();
    }

    constexpr auto operator++() noexcept(
            noexcept( next() ))
            -> decltype( *(*this) ) {
        assert(non_empty());
        next();
        return *(*this);
    }

    constexpr auto operator--() noexcept(
            noexcept( next() ))
            -> decltype( *(*this) ) {
        assert(non_empty());
        prev();
        return *(*this);
    }

    friend constexpr bool operator==(const IterStorage& lhs, const IterStorage& rhs) noexcept(
            noexcept( lhs.subtuple<HeadDepth>() == rhs.subtuple<HeadDepth>() )) {
        bool result = lhs.subtuple<HeadDepth>() == rhs.subtuple<HeadDepth>();
    #if defined(NDEBUG)
        if constexpr (!is_single_subtuple()) {
            assert(lhs.non_empty() == rhs.non_empty());
            if (lhs.non_empty()) {
                for (std::size_t depth = HeadDepth + 1; depth <= MaxDepth; ++depth) {
                    result &= (lhs.subtuple<depth>() == rhs.subtuple<depth>());
                }
            }
        }
    #endif
        return result;
    }

    friend constexpr bool operator!=(const IterStorage& lhs, const IterStorage& rhs) noexcept(
            noexcept( !(lhs == rhs) )) {
        return !(lhs == rhs);
    }
};

} // namespace flatten::details

#endif // FLATTEN_VIEW_DETAILS_ITER_STORAGE_H
