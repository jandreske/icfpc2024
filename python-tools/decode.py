import sys

charmap = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"


def decode(word):
    return ''.join(list(map(lambda x: charmap[ord(x) - 33], word)))


if __name__ == "__main__":
    print(f"{sys.argv[1]}: {decode(sys.argv[1])}")
