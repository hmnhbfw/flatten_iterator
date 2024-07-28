#include "common.h"

#include <flatten_view/flatten_view.h>
#include <flatten_view/generator/generator.h>

#include <gtest/gtest.h>

// TODO: add tests

#if 0 // Tests from the first version of `flatten_iterator`

std::vector<std::size_t> GenerateBottomSizes(std::size_t top_size,
                                             std::size_t min_bottom_size, std::size_t max_bottom_size) {
    std::vector<std::size_t> bottom_sizes(top_size);
    std::generate(
            bottom_sizes.begin(), bottom_sizes.end(),
            [min_bottom_size, max_bottom_size] {
                return Generator<std::size_t>::Get(min_bottom_size, max_bottom_size);
            });
    return bottom_sizes;
}

template<typename T>
std::vector<T> GenerateAnswer(const std::vector<std::size_t>& bottom_sizes) {
    std::vector<T> answer;
    const std::size_t answer_size = std::accumulate(bottom_sizes.cbegin(), bottom_sizes.cend(), std::size_t(0));
    answer.reserve(answer_size);
    for (std::size_t i = 0; i < answer_size; ++i) {
        answer.push_back(Generator<T>::Get());
    }
    return answer;
}

template<typename TopContainer, typename ValueType = typename TopContainer::value_type::value_type>
TopContainer GetTopContainer(const std::vector<ValueType>& answer,
                             const std::vector<std::size_t>& bottom_sizes) {
    using BottomContainer = typename TopContainer::value_type;

    TopContainer top_container(bottom_sizes.size(), BottomContainer{});
    auto top_iter = top_container.begin();
    for (std::size_t i = 0, j = 0; i < bottom_sizes.size(); ++i, ++top_iter) {
        *top_iter = BottomContainer(bottom_sizes[i], ValueType{});
        std::size_t bottom_size = bottom_sizes[i];
        for (auto bottom_iter = top_iter->begin(); bottom_size != 0; ++bottom_iter, --bottom_size, ++j) {
            *bottom_iter = answer[j];
        }
    }
    return top_container;
}

template<typename TopContainer, typename ValueType = typename TopContainer::value_type::value_type>
std::vector<ValueType> GetResultByRangeBasedForLoop(TopContainer& top_container, std::size_t answer_size) {
    std::vector<ValueType> result;
    result.reserve(answer_size);
    const auto flatten_container = MakeFlattenContainer(std::move(top_container));
    for (const auto& element : flatten_container) {
        result.push_back(element);
    }
    return result;
}

template<typename TopContainer>
void TestRangeBasedFor(std::size_t top_size,
                       std::size_t min_bottom_size, std::size_t max_bottom_size) {
    using ValueType = typename TopContainer::value_type::value_type;

    auto bottom_sizes = GenerateBottomSizes(top_size, min_bottom_size, max_bottom_size);
    auto answer = GenerateAnswer<ValueType>(bottom_sizes);
    auto top_container = GetTopContainer<TopContainer>(answer, bottom_sizes);
    auto result = GetResultByRangeBasedForLoop(top_container, answer.size());
    ASSERT_EQUAL(result, answer);
}

template<typename T, std::size_t TopSize, std::size_t MinBottomSize, std::size_t MaxBottomSize>
void AllTestsRangeBasedFor() {
    TestRangeBasedFor<RAContainer<RAContainer<T>>>(TopSize, MinBottomSize, MaxBottomSize);
    TestRangeBasedFor<BContainer<RAContainer<T>>>(TopSize, MinBottomSize, MaxBottomSize);
    TestRangeBasedFor<FContainer<RAContainer<T>>>(TopSize, MinBottomSize, MaxBottomSize);

    TestRangeBasedFor<RAContainer<BContainer<T>>>(TopSize, MinBottomSize, MaxBottomSize);
    TestRangeBasedFor<BContainer<BContainer<T>>>(TopSize, MinBottomSize, MaxBottomSize);
    TestRangeBasedFor<FContainer<BContainer<T>>>(TopSize, MinBottomSize, MaxBottomSize);

    TestRangeBasedFor<RAContainer<FContainer<T>>>(TopSize, MinBottomSize, MaxBottomSize);
    TestRangeBasedFor<BContainer<FContainer<T>>>(TopSize, MinBottomSize, MaxBottomSize);
    TestRangeBasedFor<FContainer<FContainer<T>>>(TopSize, MinBottomSize, MaxBottomSize);
}

template<typename TopContainer, typename ValueType = typename TopContainer::value_type::value_type>
std::vector<ValueType> GetResultByReverseOrder(TopContainer& top_container, std::size_t answer_size) {
    std::vector<ValueType> result;
    result.reserve(answer_size);
    const auto flatten_container = MakeFlattenContainer(std::move(top_container));
    auto begin = flatten_container.cbegin();
    auto end = flatten_container.cend();
    if (begin != end) {
        do {
            --end;
            result.push_back(*end);
        } while (begin != end);
    }
    return result;
}

template<typename TopContainer>
void TestReverseOrder(std::size_t top_size,
                      std::size_t min_bottom_size, std::size_t max_bottom_size) {
    using ValueType = typename TopContainer::value_type::value_type;

    auto bottom_sizes = GenerateBottomSizes(top_size, min_bottom_size, max_bottom_size);
    auto answer = GenerateAnswer<ValueType>(bottom_sizes);
    auto top_container = GetTopContainer<TopContainer>(answer, bottom_sizes);
    auto result = GetResultByReverseOrder(top_container, answer.size());
    std::reverse(answer.begin(), answer.end());
    ASSERT_EQUAL(result, answer);
}

template<typename T, std::size_t TopSize, std::size_t MinBottomSize, std::size_t MaxBottomSize>
void AllTestsReverseOrder() {
    TestReverseOrder<RAContainer<RAContainer<T>>>(TopSize, MinBottomSize, MaxBottomSize);
    TestReverseOrder<BContainer<RAContainer<T>>>(TopSize, MinBottomSize, MaxBottomSize);

    TestReverseOrder<RAContainer<BContainer<T>>>(TopSize, MinBottomSize, MaxBottomSize);
    TestReverseOrder<BContainer<BContainer<T>>>(TopSize, MinBottomSize, MaxBottomSize);
}

template<typename TopContainer, typename ValueType = typename TopContainer::value_type::value_type>
std::vector<ValueType> GetResultByMutateValue(TopContainer& top_container, std::size_t answer_size) {
    std::vector<ValueType> result;
    result.reserve(answer_size);
    auto flatten_container = MakeFlattenContainer(std::move(top_container));
    for (auto& elem : flatten_container) {
        elem = ValueType{};
    }
    for (const auto& element : flatten_container) {
        result.push_back(element);
    }
    return result;
}

template<typename TopContainer>
void TestMutateValue(std::size_t top_size,
                       std::size_t min_bottom_size, std::size_t max_bottom_size) {
    using ValueType = typename TopContainer::value_type::value_type;

    auto bottom_sizes = GenerateBottomSizes(top_size, min_bottom_size, max_bottom_size);
    auto answer = GenerateAnswer<ValueType>(bottom_sizes);
    auto top_container = GetTopContainer<TopContainer>(answer, bottom_sizes);
    auto result = GetResultByMutateValue(top_container, answer.size());
    std::for_each(answer.begin(), answer.end(), [](auto& element) { return element = ValueType{}; });
    ASSERT_EQUAL(result, answer);
}

template<typename T, std::size_t TopSize, std::size_t MinBottomSize, std::size_t MaxBottomSize>
void AllTestsMutateValue() {
    TestMutateValue<RAContainer<RAContainer<T>>>(TopSize, MinBottomSize, MaxBottomSize);
    TestMutateValue<BContainer<RAContainer<T>>>(TopSize, MinBottomSize, MaxBottomSize);
    TestMutateValue<FContainer<RAContainer<T>>>(TopSize, MinBottomSize, MaxBottomSize);

    TestMutateValue<RAContainer<BContainer<T>>>(TopSize, MinBottomSize, MaxBottomSize);
    TestMutateValue<BContainer<BContainer<T>>>(TopSize, MinBottomSize, MaxBottomSize);
    TestMutateValue<FContainer<BContainer<T>>>(TopSize, MinBottomSize, MaxBottomSize);

    TestMutateValue<RAContainer<FContainer<T>>>(TopSize, MinBottomSize, MaxBottomSize);
    TestMutateValue<BContainer<FContainer<T>>>(TopSize, MinBottomSize, MaxBottomSize);
    TestMutateValue<FContainer<FContainer<T>>>(TopSize, MinBottomSize, MaxBottomSize);
}

#endif
