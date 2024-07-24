/// \file flatten_iterator.h
/// TODO: add file description
/// \author Pavel Tsayukov
/// \copyright MIT License

#ifndef FLATTEN_ITERATOR_FLATTEN_ITERATOR_H
#define FLATTEN_ITERATOR_FLATTEN_ITERATOR_H

#include "range_traits.h"

namespace flatten_iterator {

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
        , typename = details::traits::require::Traits
                < RangeTraits::template input_or_output_iterator<I>
                , RangeTraits::template sentinel_for<I, S>
                >
        >
class FlattenIterator {
    using Traits = details::RangesAllTheWayDownTraits<I, S, RangeTraits>;

    typename Traits::Ranges ranges_;

public: // Nested iterator types

    using value_type = typename Traits::ValueType;
    using difference_type = typename Traits::DifferenceType;
    using reference = typename Traits::Reference;
    using pointer = typename Traits::Pointer;
    using iterator_category = typename Traits::IteratorCategory;

#if defined(__cpp_concepts) && __cpp_concepts >= 201907L
    using iterator_concept = typename Traits::IteratorConcept;
#endif

public: // Constructors

    template <typename Iterator, typename Sentinel>
    FlattenIterator(Iterator begin, Sentinel end) { // TODO: add noexcept
        // TODO: impl
    }

public: // TODO: add name for section

    // TODO: impl
};

} // namespace flatten_iterator

#endif // FLATTEN_ITERATOR_FLATTEN_ITERATOR_H
