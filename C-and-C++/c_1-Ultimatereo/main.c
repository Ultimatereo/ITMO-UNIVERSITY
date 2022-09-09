#include "return_codes.h"

#include <malloc.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>

void dump(float *mas, int n);
float findMin(float *mas, int n);
void expand(float *mas, int n, float eps, bool logs);
// float findEps(const float *mas, int n);
int gauss(float *mas, int n, bool logs)
{
	const float eps = 0.0001f;
	//	const float eps = findEps(mas, n);

	float min = findMin(mas, n);
	if (logs)
	{
		printf("Abs of minimum element which abs is not 0: %g.\n", min);
	}
	if (min < eps)
	{
		expand(mas, n, eps, logs);
		if (logs)
		{
			printf("The array has been expanded!\n");
			dump(mas, n);
		}
	}
	float max;
	int indexMax;
	int i = 0, j = 0;
	while (i < n && j < n)
	{
		max = 0.0f;
		for (int k = i; k < n; k++)
		{
			if (fabsf(mas[k * (n + 1) + j]) > max)
			{
				indexMax = k;
				max = fabsf(mas[k * (n + 1) + j]);
			}
		}
		// finding max in the certain column
		if (max < eps)	  // the whole column is zeros
		{
			for (int k = i; k < n; k++)
			{
				mas[k * (n + 1) + j] = 0.0f;
			}
			j++;
			continue;
		}
		for (int k = j; k <= n; k++)
		{
			float temp = mas[i * (n + 1) + k];
			mas[i * (n + 1) + k] = mas[indexMax * (n + 1) + k];
			mas[indexMax * (n + 1) + k] = temp;
		}	 // swap of the strings
		for (int k = i; k < n; k++)
		{
			float temp = mas[k * (n + 1) + j];
			if (fabsf(temp) < eps)
			{
				continue;
			}
			for (int t = j; t <= n; t++)
			{
				mas[k * (n + 1) + t] /= temp;
			}
			if (k == i)
			{
				continue;
			}
			mas[k * (n + 1) + j] = 0.0f;
			for (int t = j + 1; t <= n; t++)
			{
				mas[k * (n + 1) + t] -= mas[i * (n + 1) + t];
			}
		}
		if (logs)
		{
			dump(mas, n);
		}
		i++;
		j++;
	}
	if (logs)
	{
		dump(mas, n);
	}
	int rankA1 = 0, rankA2 = 0;
	for (int k = 0; k < n; k++)
	{
		int count = 0;
		for (int t = 0; t < n; t++)
		{
			if (fabsf(mas[k * (n + 1) + t]) > eps)
			{
				count++;
			}
		}
		if (count > 0)
		{
			rankA1++;
		}
		if (fabsf(mas[k * (n + 1) + n]) > eps)
		{
			count++;
		}
		if (count > 0)
		{
			rankA2++;
		}
	}
	if (logs)
	{
		printf("Ranks of matrix and extended matrix: %i %i.\n", rankA1, rankA2);
	}
	if (rankA1 != rankA2)
	{
		return -1;
	}
	if (rankA1 == rankA2 && rankA1 != n)
	{
		return 1;
	}
	for (int k = n - 2; k >= 0; k--)
	{
		float c = 0.0f;
		for (int t = n - 1; t > k; t--)
		{
			if (logs)
			{
				printf("Current answer: %lf. And numbers that we need to multiply and to subtract from the current "
					   "answer:"
					   " %lf %lf.\n",
					   mas[k * (n + 1) + n],
					   mas[t * (n + 1) + n],
					   mas[k * (n + 1) + t]);
			}
			// Kahan summation algorithm
			float input = -mas[t * (n + 1) + n] * mas[k * (n + 1) + t];	   // This is what we need to add to k-th root
			float y = input - c;
			float tt = mas[k * (n + 1) + n] + y;	// Lots of digits have been lost!
			c = (tt - mas[k * (n + 1) + n]) - y;	// Here are lost digits!
			mas[k * (n + 1) + n] = tt;
		}
	}
	return 0;
}
void expand(float *mas, int n, float eps, bool logs)
{
	for (int i = 0; i < n; i++)
	{
		float floatMin = 65504.0f;
		for (int j = 0; j <= n; j++)
		{
			if (mas[i * (n + 1) + j] != 0)
			{
				floatMin = mas[i * (n + 1) + j] < floatMin ? mas[i * (n + 1) + j] : floatMin;
			}
		}
		if (floatMin > eps)
		{
			continue;
		}
		float mul = 2 * (eps / floatMin);
		if (logs)
		{
			printf("Mul that we are using to expand %d string of matrix: %f.\n", i + 1, mul);
		}
		for (int j = 0; j <= n; j++)
		{
			mas[i * (n + 1) + j] *= mul;
		}
	}
}
float findMin(float *mas, int n)
{
	float minElement = 1;
	for (int i = 0; i < n * (n + 1); i++)
	{
		if (fabsf(mas[i]) > 0)
		{
			minElement = fabsf(mas[i]) < minElement ? fabsf(mas[i]) : minElement;
		}
	}
	return minElement;
}
// float findEps(const float *mas, int n)
//{
//	float minDiff = mas[0];
//	float minElement = fabsf(mas[1] - mas[0]);
////	printf("%g\n", minDiff);
//	for (int i = 0; i < n * (n + 1); i++) {
//		if (mas[i] != 0)
//		{
//			minElement = fabsf(mas[i]) < minElement ? fabsf(mas[i]) : minElement;
//		}
//		for (int j = 0; j < n * (n + 1); j++) {
//			if (mas[i] != mas[j])
//			{
//				//printf("%g\n", fabsf(mas[i] - mas[j]));
//				minDiff = fabsf(mas[i] - mas[j]) < minDiff ? fabsf(mas[i] - mas[j]) : minDiff;
//			}
//		}
//	}
//	printf("%g %g\n", minDiff, minElement);
//	return (minDiff < minElement ? minDiff : minElement) / 1024;
//}
void dump(float *mas, int n)
{
	for (int i = 0; i < n; i++)
	{
		for (int j = 0; j <= n; j++)
		{
			printf("%12g", mas[i * (n + 1) + j]);
		}
		printf("\n");
	}
	printf("\n");
}
bool logs = false;
int main(int argc, char **argv)
{
	if (argc != 3)
	{
		printf("Input doesn't have exactly 3 arguments as it was asked!\n"
			   "Format of argument from command line: lab1 <input_file_name> "
			   "<output_file_name>\n");
		return ERROR_INVALID_PARAMETER;
	}
	FILE *in = fopen(argv[1], "r");
	if (in == NULL)
	{
		printf("It was impossible to open input file \"%s\"!\n", argv[1]);
		return ERROR_FILE_NOT_FOUND;
	}
	int N;
	fscanf(in, "%i", &N);
	float *mas = malloc(sizeof(float) * N * (N + 1));
	if (mas == NULL)
	{
		fclose(in);
		printf("It was impossible to allocate memory for array to store numbers from input file!");
		return ERROR_MEMORY;
	}
	for (int i = 0; i < N; i++)
	{
		for (int j = 0; j <= N; j++)
		{
			fscanf(in, "%f", &mas[i * (N + 1) + j]);
		}
	}
	fclose(in);
	int result = gauss(mas, N, logs);	 // 0 - There is only one solution, 1 - many solutions, -1 - no solutions
	FILE *out = fopen(argv[2], "w");
	if (out == NULL)
	{
		free(mas);
		printf("It was impossible to open output file \"%s\"!\n", argv[2]);
		return ERROR_NOT_FOUND;
	}
	if (result == -1)
	{
		fprintf(out, "no solution");
	}
	else if (result == 1)
	{
		fprintf(out, "many solutions");
	}
	else
	{
		for (int i = 0; i < N; i++)
		{
			fprintf(out, "%g\n", mas[i * (N + 1) + N]);
		}
	}
	free(mas);
	fclose(out);
	return ERROR_SUCCESS;
}
