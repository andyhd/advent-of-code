def rounds(input):
    yield from map(lambda _: _.split(" "), input.strip().splitlines())


def part_1(input):
    outcomes = {
        #     R  P  S
        "A": [3, 6, 0],  # R
        "B": [0, 3, 6],  # P
        "C": [6, 0, 3],  # S
    }

    score = 0
    for opp, you in rounds(input):
        your_shape = "XYZ".index(you)
        score += 1 + your_shape
        score += outcomes[opp][your_shape]
    return score


def part_2(input):
    plays = {
        #     L  D  W
        "A": [2, 0, 1],  # R
        "B": [0, 1, 2],  # P
        "C": [1, 2, 0],  # S
    }

    score = 0
    for opp, outcome in rounds(input):
        outcome_idx = "XYZ".index(outcome)
        score += 1 + plays[opp][outcome_idx]
        score += outcome_idx * 3
    return score


def main():
    with open("day2-input.txt") as f:
        input = f.read()
    print(f"part_1: total score = {part_1(input)}")
    print(f"part_2: total score = {part_2(input)}")


if __name__ == "__main__":
    main()
