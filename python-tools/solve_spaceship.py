import submit_string

keys = [[1, 4, 7], [2, 5, 8], [3, 6, 9]]


def get_key(ax, ay):
    return keys[ax + 1][ay + 1]


def get_acc(target, position, velocity):
    a = target - position - velocity
    if a > 1:
        a = 1
    if a < -1:
        a = -1
    return a


def solve(pid):
    coordinates = []
    moves = []
    with open(f'spaceship/problems/spaceship{pid}.txt', "r") as file:
        for line in file.readlines():
            if not line.isspace():
                coordinates.append((int(line.split()[0]), int(line.split()[1])))
    vx = 0
    vy = 0
    posx = 0
    posy = 0
    for point in coordinates:
        while not (point[0] == posx and point[1] == posy):
            ax = get_acc(point[0], posx, vx)
            ay = get_acc(point[1], posy, vy)
            moves.append(str(get_key(ax, ay)))
            vx += ax
            vy += ay
            posx += vx
            posy += vy
    solution = ''.join(moves)
    with open(f'spaceship/solutions/spaceship{pid}.txt', "w") as file:
        file.write(solution)
    return solution


def solve_and_send(pid):
    solution = solve(pid)
    answer = submit_string.submit(f"solve spaceship{pid} {solution}")
    with open(f'spaceship/solutions/spaceship{pid}.txt', "a") as file:
        file.write(f"\n{answer}")
    return answer


print(solve_and_send(5))
