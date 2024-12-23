import cProfile as profile
import pstats
import io

from typing import Callable

def reverse_0(s: str) -> str:
    reversed_output = ''
    s_length = len(s)
    for i in range(s_length-1, 0-1, -1):
        reversed_output = reversed_output + s[i]
    return reversed_output


def reverse_1(s: str) -> str:
    reversed_output = ''
    for c in s:
        reversed_output = c + reversed_output
    return reversed_output


def reverse_2(s: str) -> str:
    s_length = len(s)
    s_list = list(s)
    j = s_length-1
    for i in range(s_length-1):
        swap_var = s_list[j]
        s_list[j] = s_list[i] 
        s_list[i] = swap_var
        j=j-1
        if (j<i):
            break
    return ''.join(s_list)


def reverse_3(s: str) -> str:
    s_list = list(s)
    s_list.reverse()
    return ''.join(s_list)


def reverse_4(s: str) -> str:
    return ''.join(reversed(s))


def reverse_5(s: str) -> str:
    return s[::-1]

def test(s: str, func: Callable[[str], str]):
	for i in range(10000):
		func(s)

def main():
	s = "abcdefghijklmnopqrstuvwxyz"
	test(s, reverse_0)
	test(s, reverse_1)
	test(s, reverse_2)
	test(s, reverse_3)
	test(s, reverse_4)
	test(s, reverse_5)


if __name__ == '__main__':
	pr = profile.Profile()
	pr.enable()
	
	main()
	
	pr.create_stats()
	s = io.StringIO()
	sortby = pstats.SortKey.CUMULATIVE
	ps = pstats.Stats(pr, stream=s).sort_stats(sortby)
	ps.print_stats()
	print(s.getvalue())