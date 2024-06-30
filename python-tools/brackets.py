
def check_balance(a, b):
    x = a % 10
    if x == 0 and b == 0:
        return 1
    if x == 0:
        return 0
    if x % 2 == 1:
        if x + 1 != b % 10:
            return 0
        b = b // 10
    if x % 2 == 0:
        b = b * 10 + x
    return check_balance(a // 10, b)


print(f"134212: {check_balance(134212, 0)}")
print(f"1132: {check_balance(1132, 0)}")

