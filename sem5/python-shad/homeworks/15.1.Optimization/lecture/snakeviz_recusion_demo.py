import cProfile as profile


def fib(n):
    if n <= 1:
        return n
    return fib(n - 1) + fib(n - 2)


def main():
    print(fib(30))


if __name__ == '__main__':
    pr = profile.Profile()
    pr.enable()

    main()

    pr.create_stats()
    pr.dump_stats('recursion.pstats')