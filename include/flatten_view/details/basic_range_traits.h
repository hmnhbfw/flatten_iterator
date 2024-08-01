/// \file basic_range_traits.h
/// A common interface for basic range traits
/// \author Pavel Tsayukov
/// \copyright MIT License

#ifndef FLATTEN_VIEW_DETAILS_BASIC_RANGE_TRAITS_H
#define FLATTEN_VIEW_DETAILS_BASIC_RANGE_TRAITS_H

#include <flatten_view/details/type_traits.h>

#if defined(__cpp_concepts) && __cpp_concepts >= 201907L
    #include <ranges>
#endif

#if __has_include(<range/v3/iterator/concepts.hpp>) && __has_include(<range/v3/range/concepts.hpp>)
    #include <range/v3/iterator/concepts.hpp>
    #include <range/v3/range/concepts.hpp>
#endif

namespace flatten::details::traits {

struct PreConceptRangeTraits {
    template <typename R>
    static constexpr bool range = ranges::IsRange<R>;

    template <typename I>
    static constexpr bool input_or_output_iterator = iterator::IsInputOrOutputIterator<I>;

    template <typename I, typename S>
    static constexpr bool sentinel_for = iterator::IsSentinelFor<I, S>;

    static constexpr auto begin = ranges::BeginFn;
    static constexpr auto end = ranges::EndFn;
};

#if defined(__cpp_concepts) && __cpp_concepts >= 201907L

struct ConceptRangeTraits {
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

#if __has_include(<range/v3/iterator/concepts.hpp>) && __has_include(<range/v3/range/concepts.hpp>)

struct RangeV3Traits {
    template <typename R>
    static constexpr bool range = ::ranges::range<R>;

    template <typename I>
    static constexpr bool input_or_output_iterator = ::ranges::input_or_output_iterator<I>;

    template <typename I, typename S>
    static constexpr bool sentinel_for = ::ranges::sentinel_for<I, S>;

    static constexpr auto begin = ::ranges::begin;
    static constexpr auto end = ::ranges::end;
};

#endif // __has_include(<range/v3/iterator/concepts.hpp>) && __has_include(<range/v3/range/concepts.hpp>)

} // namespace flatten::details::traits

#endif // FLATTEN_VIEW_DETAILS_BASIC_RANGE_TRAITS_H
