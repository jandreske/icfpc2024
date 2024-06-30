import copy
import re


def make_board(string):
    board = []
    lines = string.split('\n')
    for line in lines:
        board.append(line.split(' '))
    return board


def do_step(boards):
    old = boards[-1]
    new = copy.deepcopy(old)
    jump = 0
    submit = None
    for y in range(len(old)):
        for x in range(len(old[0])):
            match old[y][x]:
                case '.':
                    continue
                case '>':
                    if old[y][x - 1].isnumeric():
                        new[y][x + 1] = old[y][x - 1]
                        new[y][x - 1] = '.'
                case '<':
                    if old[y][x + 1].isnumeric():
                        new[y][x - 1] = old[y][x + 1]
                        new[y][x + 1] = '.'
                case '^':
                    if old[y + 1][x].isnumeric():
                        new[y - 1][x] = old[y + 1][x]
                        new[y + 1][x] = '.'
                case 'v':
                    if old[y - 1][x].isnumeric():
                        new[y + 1][x] = old[y - 1][x]
                        new[y - 1][x] = '.'
            if old[y][x - 1].isnumeric() and old[y - 1][x].isnumeric():
                match old[y][x]:
                    case '+':
                        new[y][x + 1] = str(int(old[y][x - 1]) + int(old[y - 1][x]))
                        new[y + 1][x] = str(int(old[y][x - 1]) + int(old[y - 1][x]))
                        new[y][x - 1] = '.'
                        new[y - 1][x] = '.'
                    case '-':
                        new[y][x + 1] = str(int(old[y][x - 1]) - int(old[y - 1][x]))
                        new[y + 1][x] = str(int(old[y][x - 1]) - int(old[y - 1][x]))
                        new[y][x - 1] = '.'
                        new[y - 1][x] = '.'
                    case '*':
                        new[y][x + 1] = str(int(old[y][x - 1]) * int(old[y - 1][x]))
                        new[y + 1][x] = str(int(old[y][x - 1]) * int(old[y - 1][x]))
                        new[y][x - 1] = '.'
                        new[y - 1][x] = '.'
                    case '/':
                        new[y][x + 1] = str(int(old[y][x - 1]) // int(old[y - 1][x]))
                        new[y + 1][x] = str(int(old[y][x - 1]) // int(old[y - 1][x]))
                        new[y][x - 1] = '.'
                        new[y - 1][x] = '.'
                    case '%':
                        new[y][x + 1] = str(int(old[y][x - 1]) % int(old[y - 1][x]))
                        new[y + 1][x] = str(int(old[y][x - 1]) % int(old[y - 1][x]))
                        new[y][x - 1] = '.'
                        new[y - 1][x] = '.'
                    case '=':
                        if old[y][x - 1] == old[y - 1][x]:
                            new[y][x + 1] = old[y][x - 1]
                            new[y + 1][x] = old[y - 1][x]
                            new[y][x - 1] = '.'
                            new[y - 1][x] = '.'
                    case '#':
                        if old[y][x - 1] != old[y - 1][x]:
                            new[y][x + 1] = old[y][x - 1]
                            new[y + 1][x] = old[y - 1][x]
                            new[y][x - 1] = '.'
                            new[y - 1][x] = '.'
                    case '@':
                        if old[y][x + 1].isnumeric() and old[y + 1][x].isnumeric():
                            jump = int(old[y + 1][x])
                            boards[len(boards) - 1 - jump][y - int(old[y][x + 1])][x - int(old[y][x - 1])] = old[y - 1][
                                x]

    for y in range(len(old)):
        for x in range(len(old[0])):
            if old[y][x] == 'S' and new[y][x].isnumeric():
                submit = new[y][x]
    if jump > 0:
        del boards[-jump:]
    else:
        boards.append(new)
    return submit


def show(board):
    for line in board:
        print(' '.join(line))
    print("")


program = """. 10 . 0 . . < . < 0 > . A > . > . .
A % . = . = . . 1 . . v 10 . . . v .
. . . . . . > . + S . . % . . . . .
. v . v . . . . . . . . . v . . v .
. . . . . . . . . . . . . . . . . .
. v 0 + . > . > . > S . . v . . v .
. . . . > . > . > . . . . . . 0 . .
. v . ^ 2 . 1 . . v . . . # . * v .
. . > . % . = . . . . 1 ^ . . S . .
. v . . . . . > . * . + . . . . v .
. . . 0 = . v . . . . . . < . < . .
. v . . . . . > . > . . v 10 . . . .
. . . . v 0 > . . . v . . / . > . .
. v . . . v . v 10 . . . . . . . v .
. . . . v . . . / . * . . . . < . .
. v . . . v . . . . . . . . v . . .
. . . . v . . . . . v . . . . . . .
. v . . . + . . 10 . . . . . v . . .
. . > . . . > . * . v 0 . . . . . .
. . . v . < . < . . . + . . v . . .
. . . . + . > . < . < . . . . . . .
. . . . . . . v . . . . . . v . . .
. . . . . . < . > . . . . < . > . .
. . . . 0 @ 11 . 0 @ 23 12 @ 22 . 4 @ 23
. . . . . 19 . . . 19 . . 19 . . . 19 ."""
A = 134212

program = re.sub("A", str(A), program)
start = make_board(program)
show(start)
states = [start]
for i in range(5000):
    result = do_step(states)
    # show(states[-1])
    if result is not None:
        print(f"result submitted: {result}")
        break
