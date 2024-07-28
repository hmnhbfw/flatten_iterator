/// \file flatten_view.h
/// TODO: add file description
/// \author Pavel Tsayukov
/// \copyright MIT License

#ifndef FLATTEN_VIEW_FLATTEN_VIEW_H
#define FLATTEN_VIEW_FLATTEN_VIEW_H

// NOTE: support only C++17 and further so far
#if __cplusplus >= 201703L

#include "range_traits.h"

namespace flatten {

namespace details {

/// Implementation of a sentinel over ranges of arbitrary nesting.
class Sentinel {};

/// Implementation of an iterator over ranges of arbitrary nesting.
template
        < typename I
        , typename S
        , typename RangeTraits
        , typename = require::AllOf
                < RangeTraits::template input_or_output_iterator<I>
                , RangeTraits::template sentinel_for<I, S>
                >
        >
class Iterator {
    using Traits = details::RangesAllTheWayDownTraits<I, S, RangeTraits>;

    typename Traits::Ranges ranges_;

public: // Nested iterator types

    using value_type = typename Traits::value_type;
    using difference_type = typename Traits::difference_type;
    using reference = typename Traits::reference;
    using pointer = typename Traits::pointer;
    using iterator_category = typename Traits::iterator_category;

#if defined(__cpp_concepts) && __cpp_concepts >= 201907L
    // TODO: impl iterator_concept
#endif

public: // Constructor

    constexpr Iterator(I begin, S end) { // TODO: add noexcept
        static_assert(RangeTraits::template input_or_output_iterator<Iterator>,
                      "Flatten `Iterator` must satisfy at least "
                      "`input_or_output_iterator` concept.");
        static_assert(RangeTraits::template sentinel_for<Iterator, Sentinel>,
                      "Flatten `Iterator` and `Sentinel` must satisfy "
                      "`sentinel_for` concept.");
        // TODO: impl
    }

public: // TODO: add name for section

    // TODO: impl
};

} // namespace details

/// \brief Default range traits.
///
/// Both \c flatten::view_fn and \c flatten::view<> have a template parameter
/// called by \c RangeTraits that expects a type which has to have following
/// nested variable templates, and static members:
/// \code{.cpp}
/// struct CustomRangeTraits {
///     template <typename R>
///     static constexpr bool range = /* concept or variable template */;
///
///     template <typename I>
///     static constexpr bool input_or_output_iterator = /* concept or variable template */;
///
///     template <typename I, typename S>
///     static constexpr bool sentinel_for = /* concept or variable template */;
///
///     static constexpr auto begin = /* functional object */;
///     static constexpr auto end = /* functional object */;
/// };
/// \endcode
/// \c range checks if \c R is a range, \c input_or_output_iterator checks if
/// \c I is an input or output iterator, \c sentinel_for checks if \c I and \c S
/// have the relationship and a pair of values of these types denotes the
/// \c range. \c begin and \c end are functional objects that return an iterator
/// to the first element of the range and a sentinel indicating the end of the
/// range, accordingly. These checks and objects shouldn't contradict each
/// other, otherwise the behavior is undefined.
///
/// By default, in C++20 and later \c ConceptRangeTraits is using, otherwise,
/// \c PreConceptRangeTraits that uses the internal implementation of necessary
/// concepts in terms of C++17. If you use
/// <a href="https://github.com/ericniebler/range-v3">range/v3 library</a>
/// you may use \c RangeV3Traits instead, e.g:
/// \code{.cpp}
/// auto view = flatten::view_fn<flatten::RangeV3Traits>(some_range);
/// \endcode
using DefaultRangeTraits =
#if defined(__cpp_concepts) && __cpp_concepts >= 201907L
        ConceptRangeTraits
#else
        PreConceptRangeTraits
#endif
        ;

/// \brief Enum values to control borrowing mechanism.
///
/// This enum values are needed to resolve the following situation until C++23:
/// \code{.cpp}
/// for (const auto& e : flatten::view_fn<flatten::Borrowing::DISABLE>(std::vector{1, 2, 3})) {
///     /* do something with `e` */
/// }
/// \endcode
/// Even though \c flatten::view_fn returns a temporary, since it binds to
/// a forwarding reference, its lifetime is extended until the end of the loop.
/// But \c std::vector{1,2,3} is not. It will be destroyed at the end of the
/// statement:
/// \code{.cpp}
/// auto&& /* range */ = /* range-initializer  */;
/// \endcode
/// and this leads to undefined behavior.
///
/// But if \c Borrowing::IF_NEEDED is used instead of \c Borrowing::DISABLE, —
/// and this is what happens by default, — the temporary will be moved into
/// the view object, and its lifetime will be extended as long as the view
/// object. As the enum value's name says, it won't do unnecessary borrowing
/// if \c flatten::view_fn takes an lvalue reference.
///
/// \note These enum values have no effect in C++23 and further.
enum class Borrowing : bool { DISABLE, IF_NEEDED };

namespace details {

/// To pass into \c BorrowingController when there's no actual range at hand
/// and \c Borrowing::DISABLE is using.
inline constexpr struct DummyRangeType {} DummyRange;

template <typename R = DummyRangeType, Borrowing Tag = Borrowing::DISABLE, typename = void>
class BorrowingController {
public: // Noexcept check

    static constexpr bool is_noexcept() noexcept {
        return true;
    }

public: // Dummy constructor

    constexpr BorrowingController(R&&) noexcept {}

public: // Transparent getters

    template <typename U>
    constexpr U&& transparent_get(U&& range) noexcept {
        return std::forward<U>(range);
    }

    template <typename U>
    constexpr U&& transparent_get(U&& range) const noexcept {
        return std::forward<U>(range);
    }
};

#if __cplusplus < 202302L

template <typename R>
class BorrowingController
        < R
        , Borrowing::IF_NEEDED
        , require::AllOf<!std::is_lvalue_reference_v<R>>
        > {
    R range_;

public: // Noexcept check

    static constexpr bool is_noexcept() noexcept {
        return std::is_nothrow_move_constructible_v<R>;
    }

public: // Constructor

    constexpr BorrowingController(R&& range) noexcept(is_noexcept())
            : range_(std::move(range)) {}

public: // Transparent getters

    constexpr R& transparent_get(R&&) noexcept {
        return range_;
    }

    constexpr const R& transparent_get(R&&) const noexcept {
        return range_;
    }
};

#endif // __cplusplus < 202302L

/// Implementation of a view over ranges of arbitrary nesting
template
        < typename I
        , typename S
        , typename RangeTraits
        , typename BorrowingCandidate
        , Borrowing Tag
        , typename = require::AllOf
                < RangeTraits::template input_or_output_iterator<I>
                , RangeTraits::template sentinel_for<I, S>
                >
        >
class View : public details::BorrowingController<BorrowingCandidate, Tag> {
    using FlatIterator = Iterator<I, S, RangeTraits>;

    FlatIterator flat_iterator_;

    static constexpr Sentinel flat_sentinel_;

public: // Constructors

    constexpr View(I begin, S end, RangeTraits)
            noexcept(std::is_nothrow_constructible_v<FlatIterator, I, S>)
            : details::BorrowingController<DummyRangeType, Borrowing::DISABLE>(DummyRange)
            , flat_iterator_(std::move(begin), std::move(end)) {
        static_assert(RangeTraits::template range<View>,
                      "Flatten `View` must satisfy `range` concept.");
    }

    template <typename R>
    constexpr View(R&& range, RangeTraits, Value<Tag>)
            noexcept(std::is_nothrow_constructible_v<FlatIterator, I, S>
                    && details::BorrowingController<R&&, Tag>::is_noexcept())
            : details::BorrowingController<R&&, Tag>(std::forward<R>(range))
            , flat_iterator_(RangeTraits::begin(this->transparent_get(std::forward<R>(range))),
                             RangeTraits::end(this->transparent_get(std::forward<R>(range)))) {
        static_assert(RangeTraits::template range<View>,
                      "Flatten `View` must satisfy `range` concept.");
    }

public: // Iterator/Sentinel access methods

    // NOTE: don't do the trick with `const_cast`, because don't want to copy
    // `flat_iterator_` one more time. Potentially it may be heavy.
    // TODO: do benchmarking of iteration of containers of multiple nesting.
    // If it has a significant effect, try return a wrapper over the pointer to
    // `flat_iterator_` instead of the iterator itself in `begin` methods.
    // In other cases, return `flat_iterator_` as usual. Use `if constexpr` and
    // check a size of `flat_iterator_` (find a cut-off point).
    // Additional indirection may also have a negative effect. I bet, much worse
    // then probably *one* copy of heavy `flat_iterator_` per loop against
    // multiple additional indirections per loop, as many as there're elements.
    // Do benchmarking of iteration of containers with lots of elements.

    constexpr auto begin() noexcept(std::is_nothrow_copy_constructible_v<FlatIterator>) {
        return flat_iterator_;
    }

    constexpr auto end() noexcept {
        return flat_sentinel_;
    }

    constexpr auto begin() const noexcept(std::is_nothrow_copy_constructible_v<FlatIterator>) {
        return flat_iterator_;
    }

    constexpr auto end() const noexcept {
        return flat_sentinel_;
    }

    constexpr auto cbegin() const noexcept(std::is_nothrow_copy_constructible_v<FlatIterator>) {
        return flat_iterator_;
    }

    constexpr auto cend() const noexcept {
        return flat_sentinel_;
    }

    // TODO: make `View` a full view-class in terms C++20
};

// Deduction guides

template <typename I, typename S, typename RangeTraits>
View(I, S, RangeTraits) -> View<I, S, RangeTraits, DummyRangeType, Borrowing::DISABLE>;

template <typename R, typename RangeTraits, Borrowing Tag>
View(R&& range, RangeTraits, Value<Tag>) -> View
        < decltype( RangeTraits::template begin(range) )
        , decltype( RangeTraits::template end(range) )
        , RangeTraits
        , R, Tag
        >;

} // namespace details

namespace require = details::traits::require;

/// TODO: add description
template
        < typename RangeTraits = DefaultRangeTraits
        , typename I
        , typename S
        , typename = require::AllOf
                < RangeTraits::template input_or_output_iterator<I>
                , RangeTraits::template sentinel_for<I, S>
                >
        >
constexpr auto view_fn(I begin, S end) noexcept(
        noexcept(View(std::move(begin), std::move(end), RangeTraits{}))) {
    return View(std::move(begin), std::move(end), RangeTraits{});
}

// NOTE: That's a potentially combinatorial explosion below, but until `view_fn`
// needs more than two optional template parameters, everything will be okay...

/// TODO: add description
template
        < typename RangeTraits = DefaultRangeTraits
        , typename R
        , typename = require::AllOf<RangeTraits::template range<R>>
        >
constexpr auto view_fn(R&& range) noexcept(
        noexcept(View(std::forward<R>(range), RangeTraits{}, details::Value<Borrowing::IF_NEEDED>{}))) {
    return View(std::forward<R>(range), RangeTraits{}, details::Value<Borrowing::IF_NEEDED>{});
}

/// TODO: add description
template
        < Borrowing Tag = Borrowing::IF_NEEDED
        , typename R
        , typename = require::AllOf<DefaultRangeTraits::template range<R>>
        >
constexpr auto view_fn(R&& range) noexcept(
        noexcept(View(std::forward<R>(range), DefaultRangeTraits{}, details::Value<Tag>{}))) {
    return View(std::forward<R>(range), DefaultRangeTraits{}, details::Value<Tag>{});
}

/// TODO: add description
template
        < typename RangeTraits = DefaultRangeTraits
        , Borrowing Tag = Borrowing::IF_NEEDED
        , typename R
        , typename = require::AllOf<RangeTraits::template range<R>>
        >
constexpr auto view_fn(R&& range) noexcept(
        noexcept(View(std::forward<R>(range), RangeTraits{}, details::Value<Tag>{}))) {
    return View(std::forward<R>(range), RangeTraits{}, details::Value<Tag>{});
}

} // namespace flatten

#endif // __cplusplus >= 201703L

#endif // FLATTEN_VIEW_FLATTEN_VIEW_H
