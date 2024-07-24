/// \file flatten_iterator.h
/// TODO: add file description
/// \author Pavel Tsayukov
/// \copyright MIT License

#ifndef FLATTEN_ITERATOR_FLATTEN_ITERATOR_H
#define FLATTEN_ITERATOR_FLATTEN_ITERATOR_H

#include "range_traits.h"

namespace flatten {

namespace require = details::traits::require;

/// TODO: add description
template
        < typename I
        , typename S
        , typename RangeTraits =
#if defined(__cpp_concepts) && __cpp_concepts >= 201907L
                DefaultConceptRangeTraits
#else
                DefaultPreConceptRangeTraits
#endif
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
    using iterator_concept = typename Traits::iterator_concept;
#endif

public: // Constructors

    Iterator(I begin, S end) { // TODO: add noexcept
        // TODO: impl
    }

public: // TODO: add name for section

    // TODO: impl
};

} // namespace flatten

#endif // FLATTEN_ITERATOR_FLATTEN_ITERATOR_H
