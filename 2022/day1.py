"""
--- Day 1: Calorie Counting ---

Santa's reindeer typically eat regular reindeer food, but they need a lot of
magical energy to deliver presents on Christmas. For that, their favorite snack
is a special type of star fruit that only grows deep in the jungle. The Elves
have brought you on their annual expedition to the grove where the fruit grows.

To supply enough magical energy, the expedition needs to retrieve a minimum of
fifty stars by December 25th. Although the Elves assure you that the grove has
plenty of fruit, you decide to grab any fruit you see along the way, just in
case.

Collect stars by solving puzzles. Two puzzles will be made available on each day
in the Advent calendar; the second puzzle is unlocked when you complete the
first. Each puzzle grants one star. Good luck!

The jungle must be too overgrown and difficult to navigate in vehicles or access
from the air; the Elves' expedition traditionally goes on foot. As your boats
approach land, the Elves begin taking inventory of their supplies. One important
consideration is food - in particular, the number of Calories each Elf is
carrying (your puzzle input).

The Elves take turns writing down the number of Calories contained by the
various meals, snacks, rations, etc. that they've brought with them, one item
per line. Each Elf separates their own inventory from the previous Elf's
inventory (if any) by a blank line.

For example, suppose the Elves finish writing their items' Calories and end
up with the following list:
"""

example_input = """
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"""

example_food_items = example_input.strip().splitlines()

from itertools import groupby, islice


def grouped_by_elf(food_items):
    """
    This list represents the Calories of the food carried by five Elves:

    - The first Elf is carrying food with 1000, 2000, and 3000 Calories, a total
      of 6000 Calories.

      >>> list(grouped_by_elf(example_food_items))[0]
      [1000, 2000, 3000]

      >>> sum(_)
      6000

    - The second Elf is carrying one food item with 4000 Calories.

      >>> list(grouped_by_elf(example_food_items))[1]
      [4000]

    - The third Elf is carrying food with 5000 and 6000 Calories, a total of
      11000 Calories.

      >>> list(grouped_by_elf(example_food_items))[2]
      [5000, 6000]

      >>> sum(_)
      11000

    - The fourth Elf is carrying food with 7000, 8000, and 9000 Calories, a
      total of 24000 Calories.

      >>> list(grouped_by_elf(example_food_items))[3]
      [7000, 8000, 9000]

      >>> sum(_)
      24000

    - The fifth Elf is carrying one food item with 10000 Calories.

      >>> list(grouped_by_elf(example_food_items))[4]
      [10000]
    """
    for not_empty, group in groupby(food_items, bool):
        if not_empty:
            yield list(map(int, group))


def sorted_calorie_totals(food_items_by_elf):
    """
    In case the Elves get hungry and need extra snacks, they need to know which
    Elf to ask: they'd like to know how many Calories are being carried by the
    Elf carrying the most Calories. In the example above, this is 24000 (carried
    by the fourth Elf).

    >>> list(sorted_calorie_totals(grouped_by_elf(example_food_items)))[0]
    24000
    """
    yield from reversed(sorted(map(sum, food_items_by_elf)))


food_items = open("day1-input.txt").read().strip().splitlines()


def part1_answer():
    """
    Find the Elf carrying the most Calories. How many total Calories is that Elf
    carrying?
    """
    return sum(islice(sorted_calorie_totals(grouped_by_elf(food_items)), 1))


"""

## --- Part Two ---

By the time you calculate the answer to the Elves' question, they've already
realized that the Elf carrying the most Calories of food might eventually run
out of snacks.

To avoid this unacceptable situation, the Elves would instead like to know the
total Calories carried by the top three Elves carrying the most Calories. That
way, even if one of those Elves runs out of snacks, they still have two backups.

In the example above, the top three Elves are the fourth Elf (with 24000
Calories), then the third Elf (with 11000 Calories), then the fifth Elf (with
10000 Calories). The sum of the Calories carried by these three elves is 45000.
"""


def part2_answer(food_items):
    """
    Find the top three Elves carrying the most Calories. How many Calories are
    those Elves carrying in total?

    >>> part2_answer(example_food_items)
    45000
    """
    return sum(islice(sorted_calorie_totals(grouped_by_elf(food_items)), 3))


if __name__ == "__main__":
    print(f"part 1: {part1_answer()}")
    print(f"part 2: {part2_answer(food_items)}")
