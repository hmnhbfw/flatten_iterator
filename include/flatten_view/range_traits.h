/// \file range_traits.h
/// TODO: add file description
/// \author Pavel Tsayukov
/// \copyright MIT License

#ifndef FLATTEN_VIEW_RANGE_TRAITS_H
#define FLATTEN_VIEW_RANGE_TRAITS_H

// NOTE: support only C++17 and further so far
#if __cplusplus >= 201703L

#include <iterator>
#include <tuple>
#include <type_traits>

#if defined(__cpp_concepts) && __cpp_concepts >= 201907L
    #include <ranges>
#endif

namespace flatten {

namespace details {

namespace require = traits::require;

/// TODO: add description
template
        < typename I
        , typename S
        , typename RangeTraits
        , typename = require::AllOf
                < RangeTraits::template input_or_output_iterator<I>
                , RangeTraits::template sentinel_for<I, S>
                >
        >
class RangesAllTheWayDownTraits {
private: // Type deduction methods

    template <typename RorV, typename... RWs>
    static constexpr auto wrap_ranges_impl(std::tuple<RWs...> acc) noexcept {
        if constexpr (!RangeTraits::template range<RorV>) {
            return acc;
        } else {
            using R = RorV;
            using Iterator = decltype( RangeTraits::begin(std::declval<R&>()) );
            using Sentinel = decltype( RangeTraits::end(std::declval<R&>()) );
            using Value = std::remove_reference_t<decltype( *std::declval<Iterator&>() )>;
            using RangeWrapper = Type<PseudoRange<Iterator, Sentinel>>;

            return wrap_ranges_impl<Value>(std::tuple_cat(acc, std::tuple<RangeWrapper>{}));
        }
    }

    static constexpr auto wrap_ranges() noexcept {
        using Value = std::remove_reference_t<decltype( *std::declval<I&>() )>;
        using RangeWrapper = Type<PseudoRange<I, S>>;

        return wrap_ranges_impl<Value>(std::tuple<RangeWrapper>{});
    }

    template <typename... RWs>
    static constexpr auto unwrap_ranges(std::tuple<RWs...>) noexcept {
        return Type<std::tuple<typename RWs::type...>>{};
    }

public: // TODO: section name

    // TODO: maybe extract two methods to one later
    using Ranges = typename decltype( unwrap_ranges(wrap_ranges()) )::type;

    static constexpr std::size_t MaxDepth = std::tuple_size_v<Ranges> - 1;

private: // Noexcept checks

    template <std::size_t Depth>
    static constexpr bool is_dereference_noexcept() noexcept {
        return noexcept(*current<Depth>(std::declval<Ranges&>()));
    }

public: // Ranges getter/setter

    template <std::size_t Depth>
    static constexpr auto& get(Ranges& ranges) noexcept {
        return std::get<Depth>(ranges);
    }

    template <std::size_t Depth>
    static constexpr auto& current(Ranges& ranges) noexcept {
        return get<Depth>(ranges).begin;
    }

    template <std::size_t Depth>
    static constexpr auto& sentinel(Ranges& ranges) noexcept {
        return get<Depth>(ranges).end;
    }

    static constexpr auto& value(Ranges& ranges) noexcept(is_dereference_noexcept<MaxDepth>()) {
        return *current<MaxDepth>(ranges);
    }

    // TODO: impl: NextUp, NextDown, and similar methods

private: // TODO: section name

    template <typename IteratorTag>
    static constexpr bool is_iterator_tag() noexcept {
        return std::is_same_v<IteratorTag, std::input_iterator_tag>
                || std::is_same_v<IteratorTag, std::output_iterator_tag >
                || std::is_same_v<IteratorTag, std::forward_iterator_tag>
                || std::is_same_v<IteratorTag, std::bidirectional_iterator_tag>
                || std::is_same_v<IteratorTag, std::random_access_iterator_tag>
    #if __cplusplus >= 202002L
                || std::is_same_v<IteratorTag, std::contiguous_iterator_tag>
    #endif
                ;
    }

    template <typename IteratorTag>
    static constexpr std::size_t iterator_tag_as_number() noexcept {
        if constexpr (std::is_same_v<IteratorTag, std::input_iterator_tag>) {
            return 0;
        } else if constexpr (std::is_same_v<IteratorTag, std::output_iterator_tag>) {
            return 0;
        } else if constexpr (std::is_same_v<IteratorTag, std::forward_iterator_tag>) {
            return 1;
        } else if constexpr (std::is_same_v<IteratorTag, std::bidirectional_iterator_tag>) {
            return 2;
        } else if constexpr (std::is_same_v<IteratorTag, std::random_access_iterator_tag>) {
            return 3;
        }
    #if __cplusplus >= 202002L
        else if constexpr (std::is_same_v<IteratorTag, std::contiguous_iterator_tag>) {
            return 4;
        }
    #endif
        else {
            static_assert(is_iterator_tag<IteratorTag>(), "There's no such iterator tag.");
        }
    }

    template <typename LhsIteratorTag, typename RhsIteratorTag>
    static constexpr bool is_less_or_equal_than_tag() noexcept {
        constexpr auto lhs = iterator_tag_as_number<LhsIteratorTag>();
        constexpr auto rhs = iterator_tag_as_number<RhsIteratorTag>();
        return lhs <= rhs;
    }

    template <typename IteratorTag>
    static constexpr auto pessimize_tag() noexcept {
        if constexpr (is_less_or_equal_than_tag<std::bidirectional_iterator_tag, IteratorTag>()) {
            return Type<std::bidirectional_iterator_tag>{};
        } else {
            return Type<IteratorTag>{};
        }
    }

    template <std::size_t Depth>
    using current_iterator_tag = typename std::iterator_traits
            < std::remove_reference_t
                    < decltype( current<Depth>(std::declval<Ranges&>()) )
                    >
            >::iterator_category;

    template <std::size_t Depth, typename PreviousIteratorTag>
    static constexpr auto common_iterator_tag_impl() noexcept {
        using CurrentIteratorTag = current_iterator_tag<Depth>;

        constexpr bool is_less_or_equal = is_less_or_equal_than_tag
                < PreviousIteratorTag
                , CurrentIteratorTag
                >();

        if constexpr (Depth == 0) {
            if constexpr (is_less_or_equal) {
                return pessimize_tag<PreviousIteratorTag>();
            } else {
                return pessimize_tag<CurrentIteratorTag>();
            }
        } else {
            if constexpr (is_less_or_equal) {
                return common_iterator_tag_impl<Depth - 1, PreviousIteratorTag>();
            } else {
                return common_iterator_tag_impl<Depth - 1, CurrentIteratorTag>();
            }
        }
    }

    static constexpr auto common_iterator_tag() noexcept {
        if constexpr (MaxDepth == 0) {
            return Type<current_iterator_tag<0>>{};
        } else {
            return common_iterator_tag_impl<MaxDepth - 1, current_iterator_tag<MaxDepth>>();
        }
    }

    static constexpr auto common_difference_type() noexcept {
        if constexpr (MaxDepth == 0) {
            return Type
                    < typename std::iterator_traits<DeepestIterator>::difference_type
                    >{};
        } else {
            return Type<std::ptrdiff_t>{};
        }
    }

    using DeepestIterator = std::remove_reference_t
            < decltype( current<MaxDepth>(std::declval<Ranges&>()) )
            >;

public: // Iterator nested types

    using value_type = typename std::iterator_traits<DeepestIterator>::value_type;
    using difference_type = typename decltype( common_difference_type() )::type;
    using reference = typename std::iterator_traits<DeepestIterator>::reference;
    using pointer = typename std::iterator_traits<DeepestIterator>::pointer;
    using iterator_category = typename decltype( common_iterator_tag() )::type;

#if defined(__cpp_concepts) && __cpp_concepts >= 201907L
    // TODO: impl iterator_concept
#endif
};

} // namespace details

} // namespace flatten

#endif // __cplusplus >= 201703L

#endif // FLATTEN_VIEW_RANGE_TRAITS_H
