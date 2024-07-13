/// \file flatten_iterator.h
/// TODO: add file description
/// \author Pavel Tsayukov
/// \copyright MIT License

#ifndef FLATTEN_ITERATOR_FLATTEN_ITERATOR_H
#define FLATTEN_ITERATOR_FLATTEN_ITERATOR_H

#include <iterator>
#include <tuple>
#include <type_traits>

namespace flatten_iterator {

namespace details {

namespace traits {

using std::begin, std::end;

template <typename T>
using begin_t = decltype(begin(std::declval<T&>()));

template <typename T>
using end_t = decltype(end(std::declval<T&>()));

template <typename T, typename = void>
constexpr bool is_iterable_v = false;

template <typename T>
constexpr bool is_iterable_v
        < T
        , std::void_t
                < begin_t<T>
                , end_t<T>
                , decltype(std::declval<begin_t<T>&>() != std::declval<end_t<T>&>())
                , decltype(*std::declval<begin_t<T>&>())
                , decltype(++std::declval<begin_t<T>&>())
                >
        > = true;

} // namespace traits

template <typename T>
struct Type {
    using type = T;
};

template <typename Iterator, typename Sentinel>
struct Range {
    Iterator current;
    Sentinel end;
};

// Stubs for the unused second type parameter of `std::enable_if_t`
using AnyType = int;
constexpr AnyType AnyValue = {};

template
        < typename ValueType
        , typename... RangeWrappers
        , std::enable_if_t
                < !traits::is_iterable_v<ValueType>
                , AnyType
                > = AnyValue
        >
constexpr auto TupleOfRangeWrappers(std::tuple<RangeWrappers...> acc) noexcept {
    return acc;
}

template
        < typename ValueType
        , typename... RangeWrappers
        , std::enable_if_t
                < traits::is_iterable_v<ValueType>
                , AnyType
                > = AnyValue
        >
constexpr auto TupleOfRangeWrappers(std::tuple<RangeWrappers...> acc) noexcept {
    using Iterator = traits::begin_t<ValueType>;
    using Sentinel = traits::end_t<ValueType>;
    using RangeWrapper = Type<Range<Iterator, Sentinel>>;
    using NestedValueType = std::remove_reference_t<decltype(*std::declval<Iterator&>())>;

    return TupleOfRangeWrappers<NestedValueType>(std::tuple_cat(acc, std::tuple<RangeWrapper>{}));
}

template <typename... RangeWrappers>
constexpr auto UnwrapTupleOfRangeWrappers(std::tuple<RangeWrappers...>) noexcept {
    return std::tuple<typename RangeWrappers::type...>{};
}

template <typename Iterator, typename Sentinel>
constexpr auto TupleOfRanges(Iterator&& begin, Sentinel&&) noexcept {
    using RangeWrapper = Type<Range<Iterator, Sentinel>>;
    using ValueType = std::remove_reference_t<decltype(*begin)>;

    const auto range_wrappers = TupleOfRangeWrappers<ValueType>(std::tuple<RangeWrapper>{});
    return UnwrapTupleOfRangeWrappers(range_wrappers);
}

} // namespace details

/// TODO: add description
template <typename Iterator, typename Sentinel>
class FlattenIterator {
    using Ranges =
            decltype(details::TupleOfRanges(std::declval<Iterator>(), std::declval<Sentinel>()));
    Ranges ranges_;

private: // Ranges getter/setter/traversing

    static constexpr std::size_t MaxDepth = std::tuple_size_v<Ranges>;

    template <std::size_t Depth>
    constexpr auto& Range() noexcept {
        return std::get<Depth>(ranges_);
    }

    template <std::size_t Depth>
    constexpr auto& Current() noexcept {
        return Range<Depth>().current;
    }

    template <std::size_t Depth>
    constexpr auto& End() noexcept {
        return Range<Depth>().end;
    }

    // TODO: impl NextUp, NextDown, and similar methods

public: // Nested iterator types
    // TODO: figure out types
    using value_type = void;
    using difference_type = void;
    using reference = void;
    using pointer = void;
    using iterator_category = void;

#if 0
    // TODO: figure out type, then remove the outer `if`
#if defined(__cpp_concepts) && __cpp_concepts >= 201907L
    using iterator_concept = void;
#endif
#endif

    // TODO: impl
};

} // namespace flatten_iterator

#endif // FLATTEN_ITERATOR_FLATTEN_ITERATOR_H
