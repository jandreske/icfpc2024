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
            if old[y][x - 1].lstrip('-+').isnumeric() and old[y - 1][x].lstrip('-+').isnumeric():
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
                        if old[y][x + 1].lstrip('-+').isnumeric() and old[y + 1][x].lstrip('-+').isnumeric():
                            jump = int(old[y + 1][x])
                            boards[len(boards) - 1 - jump][y - int(old[y][x + 1])][x - int(old[y][x - 1])] = old[y - 1][x]

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


program = """. . . . . . . . . . . . . . . . . . . . 1 . . . . .
. . . . . . . . . . . . . . . . . . . 2 + . . . . .
. . . . . . . . . . . . . . . . . . 0 . . . . . . .
. . . . . . . 0 . . . . . . . A > . = . + . . . . .
. . . . . . . v . < 2 > . . . . . . . . . > . > . .
. . . . . . < . * A . . v . . . . . . . v . . 5 @ 4
. . . . A = . . . v . . . . . . . . . < . > . . 5 .
. . . . . . . . v . > . / . . 0 . 14 @ -3 v 12 @ 3 . .
. . . . 0 * . . . v 2 . . > . + . . 5 . . . 5 . . .
. . . . . . . . v . % . v 0 . . . . . 10 @ 1 . . . .
. . . . 2 + . . . . . . . + . v . . . . 5 . . . . .
. . . . . S . 0 + . + . . . . . . . . . . . . . . .
. . . . . . . . . . . . . v -2 @ 9 . . . . . . . . .
. . . . . . . . . 3 @ 8 . . . 5 . . . . . . . . . .
. . . . . . . . . . 5 . 4 @ 7 . . . . . . . . . . .
. . . . . . . . . . . . . 5 . . . . . . . . . . . ."""
A = 3123

program = re.sub("A", str(A), program)
start = make_board(program)
show(start)
states = [start]
for i in range(50000):
    result = do_step(states)
    # show(states[-1])
    if result is not None:
        print(f"result submitted: {result}")
        break
