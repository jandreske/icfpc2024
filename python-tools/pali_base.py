

def pali(a, b, c, d):
    if a == c:
        return True
    if d == 0:
        return False
    return pali(a, b, c * b + d % b, d // b)


def base(a, b):
    if pali(a, b, 0, a):
        return b
    return base(a, b+1)


print(base(64064, 2))
