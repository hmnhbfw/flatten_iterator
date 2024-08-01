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

namespace flatten::details {

inline constexpr struct {} Nothing = {};

/// Optional-like class that delegates the management of its resources to
/// an owner class, assuming that the owner class knows necessary invariants.
template <typename T>
union NonOwningMaybe {
    decltype(Nothing) nothing;
    T value;

public: // Constructor/Destructor

    constexpr NonOwningMaybe() noexcept : nothing(Nothing) {}
    ~NonOwningMaybe() {}

public: // An object of this class is not allowed to manage its resources by itself

    NonOwningMaybe(const NonOwningMaybe&) = delete;
    NonOwningMaybe& operator=(const NonOwningMaybe&) = delete;
    NonOwningMaybe(NonOwningMaybe&&) = delete;
    NonOwningMaybe& operator=(NonOwningMaybe&&) = delete;
};

template <typename Tuple, typename RangeTraits>
class IterStorage {
    static_assert(false, "Instantiation of the primary template is not allowed here, "
                         "see the specialization and its requirements.");
};

/// Storage of pairs of an iterator and its sentinel.
/// \note
/// The storage's invariant: if and only if the first (top, head) iterator is
/// equal to its sentinel then other pairs are undefined.
template <typename RangeTraits, typename Head, typename... Tail>
class IterStorage<std::tuple<Head, Tail...>, RangeTraits> {
private: // Helper type aliases and constants

    template <typename I>
    static constexpr bool IsInOrOutIterator = RangeTraits::template input_or_output_iterator<I>;

    template <typename I, typename S>
    static constexpr bool IsSentinelFor = RangeTraits::template sentinel_for<I, S>;

    template <typename TupleLike>
    using IteratorFrom = std::tuple_element_t<0, TupleLike>;

    template <typename TupleLike>
    using SentinelFrom = std::tuple_element_t<1, TupleLike>;

    using Iterator = IteratorFrom<Head>;
    using Sentinel = SentinelFrom<Head>;

    static constexpr auto this_ref() -> IterStorage&;
    static constexpr auto const_this_ref() -> const IterStorage&;
    static constexpr auto this_rvalue_ref() -> IterStorage&&;

    template <typename T>
    using LvalueRef = std::remove_cv_t<std::remove_reference_t<T>>&;

private: // `Tuple` must be pairs of an iterator and its sentinel

    static_assert(((std::tuple_size_v<Tail> == 2) && ...),
                  "Each tuple must have two elements, that is, an iterator and its sentinel.");
    static_assert((IsInOrOutIterator<Iterator> && ... && IsInOrOutIterator<IteratorFrom<Tail>>),
                  "The first element of each tuple must be at least "
                  "an input or output iterator.");
    static_assert((IsSentinelFor<Iterator, Sentinel>
                  && ... && IsSentinelFor<IteratorFrom<Tail>, SentinelFrom<Tail>>),
                  "Each tuple must be a pair of an iterator and its sentinel.");

private: // Storage

    std::tuple<Head, NonOwningMaybe<Tail>...> storage_;

private: // Location in the storage

    static constexpr std::size_t IteratorIndex = 0;
    static constexpr std::size_t SentinelIndex = 1;

    static constexpr std::size_t HeadDepth = 0;
    static constexpr std::size_t MaxDepth = std::tuple_size_v<decltype( storage_ )> - 1;

    static constexpr bool is_in_range(std::size_t depth) noexcept {
        return depth <= MaxDepth;
    }

    template <std::size_t Depth>
    using Subtuple = std::remove_reference_t<decltype( this_ref().template get<Depth>() )>;

public: // Constructor/Destructor/Assignment operators
        // TODO: revise exception guarantees: if move/swap throws any exception,
        // the state of the object will become invalid. Is it possible to lift
        // the exception guarantees to the basic?

    /// Strong exception guarantee
    constexpr IterStorage(Iterator first, Sentinel last) noexcept(
            noexcept( std::tuple(std::move(first), std::move(last)) ))
            : storage_(std::tuple(std::move(first), std::move(last)),
                       NonOwningMaybe<Tail>()...) {}

    ~IterStorage() {
        destroy<HeadDepth>();
        if (is_tail_initialized()) {
            destroy_tail();
        }
    }

    /// Strong exception guarantee
    constexpr IterStorage(const IterStorage& other) noexcept(
            (std::is_nothrow_copy_constructible_v<Head>
                    && ... && std::is_nothrow_copy_constructible_v<Tail>))
            : storage_(other.get<HeadDepth>(), NonOwningMaybe<Tail>()...) {
        if (is_tail_initialized()) {
            copy_tail(other);
        }
    }

    /// Strong exception guarantee if \c swap doesn't throw any exceptions,
    /// otherwise, no exception guarantee
    constexpr IterStorage& operator=(const IterStorage& rhs) noexcept(
            std::is_nothrow_copy_constructible_v<IterStorage>
            && std::is_nothrow_swappable_v<IterStorage>) {
        auto rhs_copy = rhs;
        swap(*this, rhs_copy);
    }

    /// Strong exception guarantee if moving iterators and sentinels doesn't
    /// throw any exceptions, otherwise, no exception guarantee
    constexpr IterStorage(IterStorage&& other) noexcept(
            (std::is_nothrow_move_constructible_v<Head>
                    && ... && std::is_nothrow_move_constructible_v<Tail>))
            : storage_(std::move(other.get<HeadDepth>()), NonOwningMaybe<Tail>()...) {
        if (is_tail_initialized()) {
            move_tail(std::move(other));
        }
    }

    /// Strong exception guarantee if \c swap doesn't throw any exceptions,
    /// otherwise, no exception guarantee
    constexpr IterStorage& operator=(IterStorage&& rhs) noexcept(
            std::is_nothrow_swappable_v<IterStorage>) {
        swap(*this, rhs);
    }

public: // Swap

    friend constexpr void swap(IterStorage& lhs, IterStorage& rhs) noexcept(
            (std::is_nothrow_swappable_v<Head> && ... && std::is_nothrow_swappable_v<Tail>)
            && (std::is_nothrow_move_constructible_v<Tail> && ...)) {
        using std::swap;
        swap(lhs.get<HeadDepth>(), rhs.get<HeadDepth>());

        const bool is_lhs_tail_init = lhs.is_tail_initialized();
        const bool is_rhs_tail_init = rhs.is_tail_initialized();
        if (is_lhs_tail_init && is_rhs_tail_init) {
            swap_tail(lhs, rhs);
        } else if (is_lhs_tail_init) {
            rhs.move_tail(std::move(lhs));
        } else if (is_rhs_tail_init) {
            lhs.move_tail(std::move(rhs));
        }
    }

public: // Capacity

    constexpr std::size_t size() const noexcept {
        return MaxDepth;
    }

private: // Subtuple access

    template <std::size_t Depth>
    constexpr auto& get() noexcept {
        using Ref = LvalueRef<decltype( const_this_ref().template get<Depth>() )>;
        return const_cast<Ref>(static_cast<const IterStorage&>(*this).get<Depth>());
    }

    template <std::size_t Depth>
    constexpr const auto& get() const noexcept {
        static_assert(is_in_range(Depth));
        if constexpr (Depth == HeadDepth) {
            return std::get<HeadDepth>(storage_);
        } else {
            return std::get<Depth>(storage_).value;
        }
    }

    template <std::size_t Depth>
    constexpr auto& current() noexcept {
        using Ref = LvalueRef<decltype( const_this_ref().template current<Depth>() )>;
        return const_cast<Ref>(static_cast<const IterStorage&>(*this).current<Depth>());
    }

    template <std::size_t Depth>
    constexpr const auto& current() const noexcept {
        static_assert(is_in_range(Depth));
        return std::get<IteratorIndex>(get<Depth>());
    }

    template <std::size_t Depth>
    constexpr auto& sentinel() noexcept {
        using Ref = LvalueRef<decltype( const_this_ref().template sentinel<Depth>() )>;
        return const_cast<Ref>(static_cast<const IterStorage&>(*this).sentinel<Depth>());
    }

    template <std::size_t Depth>
    constexpr const auto& sentinel() const noexcept {
        static_assert(is_in_range(Depth));
        return std::get<SentinelIndex>(get<Depth>());
    }

private: // Tail subtuple initializing

    constexpr bool is_tail_initialized() const noexcept {
        return current<HeadDepth>() != sentinel<HeadDepth>();
    }

    template <std::size_t Depth>
    constexpr void init(const Subtuple<Depth>& other) noexcept(
            noexcept( (void) new (&get<Depth>()) Subtuple<Depth>(other) )) {
        (void) new (&get<Depth>()) Subtuple<Depth>(other);
    }

    template <std::size_t Depth>
    constexpr Subtuple<Depth>& init(Subtuple<Depth>&& other) noexcept(
            noexcept( (void) new (&get<Depth>()) Subtuple<Depth>(std::move(other)) )) {
        (void) new (&get<Depth>()) Subtuple<Depth>(std::move(other));
    }

private: // Deleters

    template <std::size_t Depth>
    constexpr void destroy() noexcept {
        static_assert(is_in_range(Depth));
        assert(Depth == HeadDepth || is_tail_initialized());

        get<Depth>().~Subtuple<Depth>();

        if constexpr (Depth != HeadDepth) {
            get<Depth>().nothing = Nothing;
        }
    }

    template <std::size_t Depth = HeadDepth, std::size_t LastDepth = MaxDepth>
    constexpr void destroy_tail() noexcept {
        if constexpr (Depth == LastDepth) {
            return;
        } else {
            constexpr auto NextDepth = Depth + 1;
            destroy<NextDepth>();
            return destroy_tail<NextDepth>();
        }
    }

private: // Tail subtuple copy/move

    template <std::size_t Depth = HeadDepth>
    constexpr void copy_tail(const IterStorage& other) noexcept(
            (std::is_nothrow_copy_constructible_v<Tail> && ...)) {
        if constexpr ((std::is_nothrow_copy_constructible_v<Tail> && ...)) {
            return nothrow_copy_tail_impl(other);
        } else {
            return copy_tail_impl(other);
        }
    }

    template <std::size_t Depth = HeadDepth>
    constexpr void nothrow_copy_tail_impl(const IterStorage& other) noexcept {
        if constexpr (Depth == MaxDepth) {
            return;
        } else {
            constexpr auto NextDepth = Depth + 1;
            init<NextDepth>(other.get<NextDepth>());
            return nothrow_copy_tail_impl<NextDepth>(other);
        }
    }

    template <std::size_t Depth = HeadDepth>
    void copy_tail_impl(const IterStorage& other) {
        if constexpr (Depth == MaxDepth) {
            return;
        } else {
            constexpr auto NextDepth = Depth + 1;
            try {
                init<NextDepth>(other.get<NextDepth>());
            } catch (...) {
                destroy_tail<HeadDepth, NextDepth>();
                throw;
            }
            return copy_tail_impl<NextDepth>(other);
        }
    }

    template <std::size_t Depth = HeadDepth>
    constexpr void move_tail(IterStorage&& other) noexcept(
            (std::is_nothrow_move_constructible_v<Tail> && ...)) {
        if constexpr ((std::is_nothrow_move_constructible_v<Tail> && ...)) {
            return nothrow_move_tail_impl(std::move(other));
        } else {
            return move_tail_impl(std::move(other));
        }
    }

    template <std::size_t Depth = HeadDepth>
    constexpr void nothrow_move_tail_impl(IterStorage&& other) noexcept {
        if constexpr (Depth == MaxDepth) {
            return;
        } else {
            constexpr auto NextDepth = Depth + 1;
            init<NextDepth>(std::move(other.get<NextDepth>()));
            other.destroy<NextDepth>();
            return nothrow_move_tail_impl<NextDepth>(std::move(other));
        }
    }

    template <std::size_t Depth = HeadDepth>
    void move_tail_impl(IterStorage&& other) {
        if constexpr (Depth == MaxDepth) {
            return;
        } else {
            constexpr auto NextDepth = Depth + 1;
            try {
                init<NextDepth>(std::move(other.get<NextDepth>()));
            } catch (...) {
                destroy_tail<HeadDepth, NextDepth>();
                throw;
            }
            other.destroy<NextDepth>();
            return move_tail_impl<NextDepth>(std::move(other));
        }
    }

private: // Swap tail subtuples

    template <std::size_t Depth>
    static constexpr bool is_swap_tail_noexcept() noexcept {
        if constexpr (Depth == MaxDepth) {
            return true;
        } else {
            return (std::is_nothrow_swappable_v<Tail> && ...);
        }
    }

    template <std::size_t Depth = HeadDepth>
    static constexpr void swap_tail(IterStorage& lhs, IterStorage& rhs) noexcept(
            is_swap_tail_noexcept<Depth>()) {
        if constexpr (Depth == MaxDepth) {
            return;
        } else {
            constexpr auto NextDepth = Depth + 1;

            using std::swap;
            swap(lhs.get<NextDepth>(), rhs.get<NextDepth>());

            return swap_tail<NextDepth>(lhs, rhs);
        }
    }

private: // 

    // TODO: impl
};

} // namespace flatten::details

#endif // FLATTEN_VIEW_DETAILS_ITER_STORAGE_H
