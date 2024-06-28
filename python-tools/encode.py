import sys

charmap = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"


def encode(word):
    return ''.join(list(map(lambda x: chr(charmap.index(x) + 33), word)))


if __name__ == "__main__":
    print(f"{sys.argv[1]}: {encode(sys.argv[1])}")
