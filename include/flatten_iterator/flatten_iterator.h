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

} // namespace details

/// TODO: add description
template <typename Iterator, typename Sentinel>
class FlattenIterator {
    // TODO: impl
};

} // namespace flatten_iterator

#endif // FLATTEN_ITERATOR_FLATTEN_ITERATOR_H
