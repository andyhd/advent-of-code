"""
--- Day 3: Rucksack Reorganization ---

One Elf has the important job of loading all of the rucksacks with supplies for
the jungle journey. Unfortunately, that Elf didn't quite follow the packing
instructions, and so a few items now need to be rearranged.

Each rucksack has two large compartments. All items of a given type are meant to
go into exactly one of the two compartments. The Elf that did the packing failed
to follow this rule for exactly one item type per rucksack.

The Elves have made a list of all of the items currently in each rucksack (your
puzzle input), but they need your help finding the errors. Every item type is
identified by a single lowercase or uppercase letter (that is, a and A refer to
different types of items).
"""
rucksacks = open("day3-input.txt").read().strip().splitlines()


def compartments(items):
    """
    The list of items for each rucksack is given as characters all on a single
    line.  A given rucksack always has the same number of items in each of its
    two compartments, so the first half of the characters represent items in the
    first compartment, while the second half of the characters represent items
    in the second compartment.

    >>> compartments(example_rucksacks[0])
    ['vJrwpWtwJgWr', 'hcsFMMfFFhFp']

    >>> compartments(example_rucksacks[1])
    ['jqHRNqRjqzjGDLGL', 'rsFMfFZSrLrFZsSL']

    >>> compartments(example_rucksacks[2])
    ['PmmdzqPrV', 'vPwwTWBwg']
    """
    return [items[: (half := len(items) // 2)], items[half:]]


"""
For example, suppose you have the following list of contents from six rucksacks:
"""
example_input = """
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
"""

example_rucksacks = example_input.strip().splitlines()


def appears_in_both(compartments):
    """
    - The first rucksack contains the items vJrwpWtwJgWrhcsFMMfFFhFp, which
      means its first compartment contains the items vJrwpWtwJgWr, while the
      second compartment contains the items hcsFMMfFFhFp. The only item type
      that appears in both compartments is lowercase p.

    >>> appears_in_both(compartments(example_rucksacks[0]))
    'p'

    - The second rucksack's compartments contain jqHRNqRjqzjGDLGL and
      rsFMfFZSrLrFZsSL. The only item type that appears in both compartments is
      uppercase L.

    >>> appears_in_both(compartments(example_rucksacks[1]))
    'L'

    - The third rucksack's compartments contain PmmdzqPrV and vPwwTWBwg; the only
      common item type is uppercase P.

    >>> appears_in_both(compartments(example_rucksacks[2]))
    'P'

    - The fourth rucksack's compartments only share item type v.

    >>> appears_in_both(compartments(example_rucksacks[3]))
    'v'

    - The fifth rucksack's compartments only share item type t.

    >>> appears_in_both(compartments(example_rucksacks[4]))
    't'

    - The sixth rucksack's compartments only share item type s.

    >>> appears_in_both(compartments(example_rucksacks[5]))
    's'
    """
    return set(compartments[0]).intersection(compartments[1]).pop()


def priority(item_type):
    """
    To help prioritize item rearrangement, every item type can be converted to a
    priority:

    - Lowercase item types a through z have priorities 1 through 26.
    - Uppercase item types A through Z have priorities 27 through 52.

    In the above example, the priority of the item type that appears in both
    compartments of each rucksack is 16 (p), 38 (L), 42 (P), 22 (v), 20 (t), and
    19 (s); the sum of these is 157.

    >>> [priority(appears_in_both(compartments(r))) for r in example_rucksacks]
    [16, 38, 42, 22, 20, 19]

    >>> sum(_)
    157
    """
    if item_type.islower():
        return ord(item_type) - ord("a") + 1
    if item_type.isupper():
        return ord(item_type) - ord("A") + 27


def part1_answer():
    """
    Find the item type that appears in both compartments of each rucksack. What
    is the sum of the priorities of those item types?
    """
    return sum(
        priority(appears_in_both(compartments(rucksack))) for rucksack in rucksacks
    )


"""
## --- Part Two ---

As you finish identifying the misplaced items, the Elves come to you with
another issue.

For safety, the Elves are divided into groups of three. Every Elf carries a
badge that identifies their group. For efficiency, within each group of three
Elves, the badge is the only item type carried by all three Elves.  That is, if
a group's badge is item type B, then all three Elves will have item type B
somewhere in their rucksack, and at most two of the Elves will be carrying any
other item type.

The problem is that someone forgot to put this year's updated authenticity
sticker on the badges. All of the badges need to be pulled out of the rucksacks
so the new authenticity stickers can be attached.

Additionally, nobody wrote down which item type corresponds to each group's
badges. The only way to tell which item type is the right one is by finding the
one item type that is common between all three Elves in each group.
"""


def group_badges(rucksacks):
    """
    Every set of three lines in your list corresponds to a single group, but
    each group can have a different badge item type. So, in the above example,
    the first group's rucksacks are the first three lines:

        vJrwpWtwJgWrhcsFMMfFFhFp
        jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
        PmmdzqPrVvPwwTWBwg

    And the second group's rucksacks are the next three lines:

        wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
        ttgJtRGJQctTZtZT
        CrZsJsPPZsGzwwsLwLmpwMDw

    In the first group, the only item type that appears in all three rucksacks
    is lowercase r; this must be their badges. In the second group, their badge
    item type must be Z.

    >>> list(group_badges(example_rucksacks))
    ['r', 'Z']

    Priorities for these items must still be found to organize the sticker
    attachment efforts: here, they are 18 (r) for the first group and 52 (Z) for
    the second group. The sum of these is 70.

    >>> [priority(badge) for badge in group_badges(example_rucksacks)]
    [18, 52]

    >>> sum(_)
    70
    """
    groups = zip(*([iter(rucksacks)] * 3))
    for group in groups:
        yield set(group[0]).intersection(*group[1:]).pop()


def part2_answer():
    """
    Find the item type that corresponds to the badges of each three-Elf group.
    What is the sum of the priorities of those item types?
    """
    return sum(priority(badge) for badge in group_badges(rucksacks))


if __name__ == "__main__":
    print(f"part 1: {part1_answer()}")
    print(f"part 2: {part2_answer()}")
