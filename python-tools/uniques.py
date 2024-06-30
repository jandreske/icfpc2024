
def been_here(b, x, y):
    s = b % 10
    if s == 0:
        return False
    if s % 2 == 1:
        y = y + s - 2
    if s % 2 == 0:
        x = x + s - 3
    if x == 0 and y == 0:
        return True
    return been_here(b // 10, x, y)


def count(a, b, c):
    s = a % 10
    if s == 0:
        return c
    b = b * 10 + s
    if not been_here(b, 0, 0):
        c = c + 1
    return count(a // 10, b, c)


print(f"33321411: {count(33321411, 0, 1)}")
print(f"13: {count(13, 0, 1)}")
print(f"13131311: {count(13131311, 0, 1)}")
print(f"13112341: {count(13112341, 0, 1)}")

