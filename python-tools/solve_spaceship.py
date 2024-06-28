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


def get_coords_sorted(coordinates, strategy):
    match strategy:
        case 0:
            return coordinates
        case 1:
            return sorted(coordinates)
        case 2:
            return sorted(coordinates, key=lambda x: (x[1], x[0]))
        case 3:
            return sorted(coordinates, key=lambda x: abs(x[0]) + abs(x[1]))


def solve(pid, strategy):
    coordinates = []
    moves = []
    with open(f'spaceship/problems/spaceship{pid}.txt', "r") as file:
        for line in file.readlines():
            if not line.isspace():
                coordinates.append((int(line.split()[0]), int(line.split()[1])))
    coordinates = get_coords_sorted(coordinates, strategy)
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
    return solution


def solve_and_send(pid):
    # 0 -> in given order
    # 1 -> sort by x, then by y
    # 2 -> sort by y, then by x
    # 3 -> sort from near to far
    best = ''
    for strategy in range(0, 4):
        solution = solve(pid, strategy)
        print(f"problem {pid} with strategy {strategy}: {len(solution)} moves")
        if best == '' or len(solution) < len(best):
            best = solution
    with open(f'spaceship/solutions/spaceship{pid}.txt', "w") as file:
        file.write(best)
    answer = submit_string.submit(f"solve spaceship{pid} {best}")
    with open(f'spaceship/solutions/spaceship{pid}.txt', "a") as file:
        file.write(f"\n{answer}")
    return answer


for pid in range(1, 12):
    print(solve_and_send(pid))
