
def calc(a, e, s):
    if e == 1:
        return s*a/10**9
    return calc(a, e-2, 10**9 - (s*a*a)/(10**18*e*(e-1)))


print(calc(1047197551, 11, 10**9))
print(calc(-1168378317, 11, 10**9))
