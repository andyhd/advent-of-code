from itertools import groupby, islice


def by_elf(data):
    data = data.strip().splitlines()
    for key, group in groupby(data, lambda s: bool(s.strip())):
        if key:
            yield map(int, group)


def sorted_calorie_subtotals(food_items_by_elf):
    yield from reversed(sorted(map(sum, food_items_by_elf)))


def part_1(data):
    return sum(islice(sorted_calorie_subtotals(by_elf(data)), 1))


def part_2(data):
    return sum(islice(sorted_calorie_subtotals(by_elf(data)), 3))


def main():
    with open("day1-input.txt") as f:
        data = f.read()

    print(f"part_1: most calories carried {part_1(data)}")
    print(f"part_2: top three elves calories {part_2(data)}")


if __name__ == "__main__":
    main()
