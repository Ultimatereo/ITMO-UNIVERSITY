#include "return_codes.h"
#include <cstdlib>
#include <ctime>

template< typename T >
static void swap(T *mas, size_t i, size_t j)
{
	T temp = mas[i];
	mas[i] = mas[j];
	mas[j] = temp;
}
template< typename T, bool descending >
static size_t partition(T *mas, size_t l, size_t r)
{
	srand(time(nullptr));
	size_t pivot = l + rand() % (r - l);
	T value = mas[pivot];
	size_t i = l;
	size_t j = r;
	while (i <= j)
	{
		if (descending)
		{
			while (mas[i] > value)
			{
				i++;
			}
			while (value > mas[j])
			{
				j--;
			}
		}
		else
		{
			while (mas[i] < value)
			{
				i++;
			}
			while (value < mas[j])
			{
				j--;
			}
		}
		if (i >= j)
		{
			break;
		}
		swap(mas, i++, j--);
	}
	return j;
}
template< typename T, bool descending >
void quicksort(T *mas, size_t l, size_t r)
{
	while (l < r)
	{
		size_t q = partition< T, descending >(mas, l, r);
		if (q - l < r - q)
		{
			quicksort< T, descending >(mas, l, q);
			l = q + 1;
		}
		else
		{
			quicksort< T, descending >(mas, q + 1, r);
			r = q;
		}
	}
}