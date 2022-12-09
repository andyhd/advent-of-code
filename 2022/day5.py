"""
--- Day 5: Supply Stacks ---

The expedition can depart as soon as the final supplies have been unloaded from
the ships. Supplies are stored in stacks of marked crates, but because the
needed supplies are buried under many other crates, the crates need to be
rearranged.

The ship has a giant cargo crane capable of moving crates between stacks. To
ensure none of the crates get crushed or fall over, the crane operator will
rearrange them in a series of carefully-planned steps. After the crates are
rearranged, the desired crates will be at the top of each stack.

The Elves don't want to interrupt the crane operator during this delicate
procedure, but they forgot to ask her which crate will end up where, and they
want to be ready to unload them as soon as possible so they can embark.

They do, however, have a drawing of the starting stacks of crates and the
rearrangement procedure (your puzzle input). For example:
"""

example_input = """
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
"""

example_drawing, example_procedure = example_input.strip("\n").split("\n\n")
example_procedure = example_procedure.splitlines()


def stacks(drawing):
    """
    In this example, there are three stacks of crates. Stack 1 contains two
    crates: crate Z is on the bottom, and crate N is on top. Stack 2 contains
    three crates; from bottom to top, they are crates M, C, and D. Finally,
    stack 3 contains a single crate, P.

    >>> columns = stacks(example_drawing)
    >>> columns[0]
    ['N', 'Z']

    >>> columns[1]
    ['D', 'C', 'M']

    >>> columns[2]
    ['P']
    """
    stacks = []
    stack_width = 4
    for line in list(reversed(drawing.splitlines()))[1:]:
        for col, crate in enumerate(line[1::stack_width]):
            if col >= len(stacks):
                stacks.append([])
            if crate.isalpha():
                stacks[col].insert(0, crate)
    return stacks


def steps(procedure):
    for step in procedure:
        _, num, _, src, _, dst = step.split(" ")
        num, src, dst = map(int, (num, src, dst))
        yield num, src - 1, dst - 1


def apply(steps, stacks):
    """
    Then, the rearrangement procedure is given. In each step of the procedure, a
    quantity of crates is moved from one stack to a different stack. In the
    first step of the above rearrangement procedure, one crate is moved from
    stack 2 to stack 1, resulting in this configuration:

    [D]
    [N] [C]
    [Z] [M] [P]
     1   2   3

    >>> moves = list(steps(example_procedure))
    >>> apply(moves[0:1], stacks(example_drawing))
    [['D', 'N', 'Z'], ['C', 'M'], ['P']]

    In the second step, three crates are moved from stack 1 to stack 3. Crates
    are moved one at a time, so the first crate to be moved (D) ends up below
    the second and third crates:

            [Z]
            [N]
        [C] [D]
        [M] [P]
     1   2   3

    >>> apply(moves[1:2], _)
    [[], ['C', 'M'], ['Z', 'N', 'D', 'P']]

    Then, both crates are moved from stack 2 to stack 1. Again, because crates
    are moved one at a time, crate C ends up below crate M:

            [Z]
            [N]
    [M]     [D]
    [C]     [P]
     1   2   3

    >>> apply(moves[2:3], _)
    [['M', 'C'], [], ['Z', 'N', 'D', 'P']]

    Finally, one crate is moved from stack 1 to stack 2:

            [Z]
            [N]
            [D]
    [C] [M] [P]
     1   2   3

    >>> apply(moves[3:4], _)
    [['C'], ['M'], ['Z', 'N', 'D', 'P']]
    """
    for num, src, dst in steps:
        for _ in range(num):
            stacks[dst].insert(0, stacks[src].pop(0))
    return stacks


def top_crates(stacks):
    """
    The Elves just need to know which crate will end up on top of each stack; in
    this example, the top crates are C in stack 1, M in stack 2, and Z in stack
    3, so you should combine these together and give the Elves the message CMZ.

    >>> top_crates(apply(steps(example_procedure), stacks(example_drawing)))
    'CMZ'
    """
    return "".join([stack[0] for stack in stacks])


day5_input = open("day5-input.txt").read()
drawing, procedure = day5_input.strip("\n").split("\n\n")
procedure = procedure.splitlines()


def part1_answer():
    """
    After the rearrangement procedure completes, what crate ends up on top of
    each stack?
    """
    return top_crates(apply(steps(procedure), stacks(drawing)))


"""
--- Part Two ---

As you watch the crane operator expertly rearrange the crates, you notice the
process isn't following your prediction.

Some mud was covering the writing on the side of the crane, and you quickly wipe
it away. The crane isn't a CrateMover 9000 - it's a CrateMover 9001.
"""


def apply2(steps, stacks):
    """
    The CrateMover 9001 is notable for many new and exciting features: air
    conditioning, leather seats, an extra cup holder, and the ability to pick up
    and move multiple crates at once.

    Again considering the example above, the crates begin in the same
    configuration:

        [D]
    [N] [C]
    [Z] [M] [P]
     1   2   3

    Moving a single crate from stack 2 to stack 1 behaves the same as before:

    [D]
    [N] [C]
    [Z] [M] [P]
     1   2   3

    >>> moves = list(steps(example_procedure))
    >>> apply2(moves[0:1], stacks(example_drawing))
    [['D', 'N', 'Z'], ['C', 'M'], ['P']]

    However, the action of moving three crates from stack 1 to stack 3 means that
    those three moved crates stay in the same order, resulting in this new
    configuration:

            [D]
            [N]
        [C] [Z]
        [M] [P]
     1   2   3

    >>> apply2(moves[1:2], _)
    [[], ['C', 'M'], ['D', 'N', 'Z', 'P']]

    Next, as both crates are moved from stack 2 to stack 1, they retain their order
    as well:

            [D]
            [N]
    [C]     [Z]
    [M]     [P]
     1   2   3

    >>> apply2(moves[2:3], _)
    [['C', 'M'], [], ['D', 'N', 'Z', 'P']]

    Finally, a single crate is still moved from stack 1 to stack 2, but now it's
    crate C that gets moved:

            [D]
            [N]
            [Z]
    [M] [C] [P]
     1   2   3

    >>> apply2(moves[3:4], _)
    [['M'], ['C'], ['D', 'N', 'Z', 'P']]

    In this example, the CrateMover 9001 has put the crates in a totally different
    order: MCD.

    >>> top_crates(apply2(steps(example_procedure), stacks(example_drawing)))
    'MCD'
    """
    for num, src, dst in steps:
        stacks[dst][:0], stacks[src] = stacks[src][:num], stacks[src][num:]
    return stacks


def part2_answer():
    """
    Before the rearrangement process finishes, update your simulation so that
    the Elves know where they should stand to be ready to unload the final
    supplies.  After the rearrangement procedure completes, what crate ends up
    on top of each stack?
    """
    return top_crates(apply2(steps(procedure), stacks(drawing)))


if __name__ == "__main__":
    print(f"part 1: {part1_answer()}")
    print(f"part 2: {part2_answer()}")
