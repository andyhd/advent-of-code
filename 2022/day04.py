"""
--- Day 4: Camp Cleanup ---

Space needs to be cleared before the last supplies can be unloaded from the
ships, and so several Elves have been assigned the job of cleaning up sections
of the camp. Every section has a unique ID number, and each Elf is assigned a
range of section IDs.

However, as some of the Elves compare their section assignments with each other,
they've noticed that many of the assignments overlap. To try to quickly find
overlaps and reduce duplicated effort, the Elves pair up and make a big list of
the section assignments for each pair (your puzzle input).

For example, consider the following list of section assignment pairs:
"""

example_input = """
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
"""

example_assignments = example_input.strip().splitlines()


def assignment(section_range):
    start, end = map(int, section_range.split("-"))
    return list(range(start, end + 1))


def assignment_pairs(input):
    """
    For the first few pairs, this list means:

    - Within the first pair of Elves, the first Elf was assigned sections 2-4
      (sections 2, 3, and 4), while the second Elf was assigned sections 6-8
      (sections 6, 7, 8).

      >>> list(assignment_pairs(example_assignments))[0]
      [[2, 3, 4], [6, 7, 8]]

    - The Elves in the second pair were each assigned two sections.

      >>> list(map(len, list(assignment_pairs(example_assignments))[1]))
      [2, 2]

    - The Elves in the third pair were each assigned three sections: one got
      sections 5, 6, and 7, while the other also got 7, plus 8 and 9.

      >>> list(assignment_pairs(example_assignments))[2]
      [[5, 6, 7], [7, 8, 9]]

    This example list uses single-digit section IDs to make it easier to draw; your
    actual list might contain larger numbers. Visually, these pairs of section
    assignments look like this:

    .234.....  2-4
    .....678.  6-8

    .23......  2-3
    ...45....  4-5

    ....567..  5-7
    ......789  7-9

    .2345678.  2-8
    ..34567..  3-7

    .....6...  6-6
    ...456...  4-6

    .23456...  2-6
    ...45678.  4-8
    """
    for pair in input:
        yield [assignment(section_range) for section_range in pair.split(",")]


def fully_overlapping(ranges):
    """
    Some of the pairs have noticed that one of their assignments fully contains
    the other. For example, 2-8 fully contains 3-7, and 6-6 is fully contained
    by 4-6.  In pairs where one assignment fully contains the other, one Elf in
    the pair would be exclusively cleaning sections their partner will already
    be cleaning, so these seem like the most in need of reconsideration. In this
    example, there are 2 such pairs.

    >>> fully_overlapping([assignment("2-8"), assignment("3-7")])
    True

    >>> fully_overlapping([assignment("6-6"), assignment("4-6")])
    True

    >>> len(list(filter(fully_overlapping, assignment_pairs(example_assignments))))
    2
    """
    a, b = ranges
    return set(a).issubset(b) or set(b).issubset(a)


assignments = open("day04-input.txt").read().strip().splitlines()


def part1_answer():
    """
    In how many assignment pairs does one range fully contain the other?
    """
    return len(list(filter(fully_overlapping, assignment_pairs(assignments))))


"""
--- Part Two ---
"""


def overlapping(ranges):
    """
    It seems like there is still quite a bit of duplicate work planned. Instead, the
    Elves would like to know the number of pairs that overlap at all.

    In the above example, the first two pairs (2-4,6-8 and 2-3,4-5) don't overlap,

    >>> bool(overlapping([assignment("2-4"), assignment("6-8")]))
    False

    >>> bool(overlapping([assignment("2-3"), assignment("4-5")]))
    False

    while the remaining four pairs (5-7,7-9, 2-8,3-7, 6-6,4-6, and 2-6,4-8) do
    overlap:

    - 5-7,7-9 overlaps in a single section, 7.

      >>> overlapping([assignment("5-7"), assignment("7-9")])
      {7}

    - 2-8,3-7 overlaps all of the sections 3 through 7.

      >>> overlapping([assignment("2-8"), assignment("3-7")])
      {3, 4, 5, 6, 7}

    - 6-6,4-6 overlaps in a single section, 6.

      >>> overlapping([assignment("6-6"), assignment("4-6")])
      {6}

    - 2-6,4-8 overlaps in sections 4, 5, and 6.

      >>> overlapping([assignment("2-6"), assignment("4-8")])
      {4, 5, 6}

    So, in this example, the number of overlapping assignment pairs is 4.

    >>> len(list(filter(overlapping, assignment_pairs(example_assignments))))
    4
    """
    return set(ranges[0]).intersection(*ranges[1:])


def part2_answer():
    """
    In how many assignment pairs do the ranges overlap?
    """
    return len(list(filter(overlapping, assignment_pairs(assignments))))


if __name__ == "__main__":
    print(f"part 1: {part1_answer()}")
    print(f"part 2: {part2_answer()}")
