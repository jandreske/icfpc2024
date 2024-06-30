import copy

def make_board(string):
    board = []
    lines = string.split('\n')
    for line in lines:
        board.append(line.split(' '))
    return board

def do_step(boards):
    old = boards[-1]
    new = copy.deepcopy(old)
    for y in range(len(old)):
        for x in range(len(old[0])):
            match old[y][x]:
                case '.': continue
                case '>':
                    if old[y][x-1].isnumeric():
                        new[y][x+1] = old[y][x-1]
                        new[y][x - 1] = '.'
                case '<':
                    if old[y][x+1].isnumeric():
                        new[y][x-1] = old[y][x+1]
                        new[y][x + 1] = '.'
                case '^':
                    if old[y+1][x].isnumeric():
                        new[y-1][x] = old[y+1][x]
                        new[y + 1][x] = '.'
                case 'v':
                    if old[y-1][x].isnumeric():
                        new[y+1][x] = old[y-1][x]
                        new[y - 1][x] = '.'
            if old[y][x-1].isnumeric() and old[y-1][x].isnumeric():
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
                        if old[y][x-1] == old[y-1][x]:
                            new[y][x+1] = old[y][x-1]
                            new[y+1][x] = old[y-1][x]
                            new[y][x-1] = '.'
                            new[y-1][x] = '.'
                    case '#':
                        if old[y][x-1] != old[y-1][x]:
                            new[y][x+1] = old[y][x-1]
                            new[y+1][x] = old[y-1][x]
                            new[y][x-1] = '.'
                            new[y-1][x] = '.'
    boards.append(new)
    return new


def show(board):
    for line in board:
        print(' '.join(line))
    print("")


program = """. . . . 0 . . . .
. 7 > . = . . . .
. v 1 . . > . . .
. . - . . . + S .
. . . . . ^ . . .
. . v . . 1 > . .
. . . . 7 > . * .
. 1 @ 6 v 1 . . .
. . 3 . . - . v .
. . . . < . . . .
. . -1 @ 4 . 2 @ 5
. . . 3 . . . 3 ."""
start = make_board(program)
show(start)
boards = [start]
for i in range(3):
    show(do_step(boards))

