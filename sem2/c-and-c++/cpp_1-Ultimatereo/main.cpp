#include "phonebook.h"
#include "quicksort.h"
#include "return_codes.h"
#include <cstdio>

static void dump(const char *mas, size_t n)
{
	for (size_t i = 0; i < n; i++)
	{
		printf("%02X ", (unsigned char)mas[i]);
	}
	printf("\n");
}
static void dump(int *mas, size_t n)
{
	for (size_t i = 0; i < n; i++)
	{
		printf("%d ", mas[i]);
	}
	printf("\n");
}
static void dump(float *mas, size_t n)
{
	for (size_t i = 0; i < n; i++)
	{
		printf("%g ", mas[i]);
	}
	printf("\n");
}

int read21String(FILE *in, char *pString, char *buffer);
static bool isEqual(const char *mas1, const char *mas2, size_t n)
{
	size_t eq = 0;
	for (size_t i = 0; i < n; i++)
	{
		if (mas1[i] == mas2[i])
		{
			eq++;
		}
	}
	return eq == n;
}
template< typename T >
static int readAndParse(bool descending, size_t size, FILE *in, bool logs, char *stringFormat, char *outName)
{
	int check;
	auto *mas = (T *)malloc(sizeof(T) * size);
	if (mas == nullptr)
	{
		fclose(in);
		fprintf(stderr, "It was impossible to allocate memory for array to store numbers from input file!");
		return ERROR_MEMORY;
	}
	for (size_t i = 0; i < size; i++)
	{
		check = fscanf(in, stringFormat, &mas[i]);	  // reading data into array
		if (check != 1)
		{
			fclose(in);
			fprintf(stderr, "Data in input file is invalid!");
			return ERROR_INVALID_DATA;
		}
	}
	fclose(in);
	if (descending)
	{
		quicksort< T, true >(mas, 0, size - 1);
	}
	else
	{
		quicksort< T, false >(mas, 0, size - 1);
	}
	FILE *out = fopen(outName, "w");
	if (out == nullptr)
	{
		free(mas);
		printf("It was impossible to open output file \"%s\"!\n", outName);
		return ERROR_NOT_FOUND;
	}
	for (size_t i = 0; i < size; i++)
	{
		fprintf(out, stringFormat, mas[i]);	   // writing data to stream
		fprintf(out, "\n");
	}
	free(mas);
	fclose(out);
	return ERROR_SUCCESS;
}
template<>
int readAndParse< phone_book >(bool descending, size_t size, FILE *in, bool logs, char *stringFormat, char *outName)
{
	int check = 0;
	auto *mas = (phone_book *)malloc(sizeof(phone_book) * size);
	char buffer[21];
	if (mas == nullptr)
	{
		fclose(in);
		fprintf(stderr, "It was impossible to allocate memory for array to store numbers from input file!");
		return ERROR_MEMORY;
	}
	for (size_t i = 0; i < size; i++)
	{
		check = read21String(in, mas[i].surname, buffer);
		if (check != 0)
		{
			return check;
		}
		check = read21String(in, mas[i].name, buffer);
		if (check != 0)
		{
			return check;
		}
		check = read21String(in, mas[i].middleName, buffer);
		if (check != 0)
		{
			return check;
		}
		check = fscanf(in, "%llu", &mas[i].number);	   // reading data into array
		if (check != 1)
		{
			fclose(in);
			fprintf(stderr, "Data in input file is invalid!");
			return ERROR_INVALID_DATA;
		}
	}
	fclose(in);
	if (descending)
	{
		quicksort< phone_book, true >(mas, 0, size - 1);
	}
	else
	{
		quicksort< phone_book, false >(mas, 0, size - 1);
	}
	FILE *out = fopen(outName, "w");
	if (out == nullptr)
	{
		free(mas);
		printf("It was impossible to open output file \"%s\"!\n", outName);
		return ERROR_NOT_FOUND;
	}
	for (size_t i = 0; i < size; i++)
	{
		fprintf(out, "%s %s %s %llu", mas[i].surname, mas[i].name, mas[i].middleName, mas[i].number);
		fprintf(out, "\n");
	}
	free(mas);
	fclose(out);
	return ERROR_SUCCESS;
}
int read21String(FILE *in, char *pString, char *buffer)
{
	int check = fscanf(in, "%s", buffer);
	if (check != 1)
	{
		fprintf(stderr, "Surnames, names and middle names are supposed to be within 20 characters!");
		return ERROR_INVALID_DATA;
	}
	bool thereIsNull = false;
	for (int i = 0; i < 21; i++)
	{
		pString[i] = buffer[i];
		if (buffer[i] == 0)
		{
			thereIsNull = true;
			break;
		}
	}
	if (!thereIsNull)
	{
		fprintf(stderr, "Surnames, names and middle names are supposed to be within 20 characters!");
		return ERROR_INVALID_DATA;
	}
	return 0;
}
int main(int argc, char *argv[])
{
	bool logs = false;
	if (argc != 3)
	{
		fprintf(stderr, "Invalid number of parameters! Expected: 3, got: %d", argc);
		return ERROR_INVALID_PARAMETER;
	}

	FILE *in = fopen(argv[1], "r");
	if (!in)
	{
		fprintf(stderr, "Input file not found");
		return ERROR_FILE_NOT_FOUND;
	}
	char buffer[32];
	int check = fscanf(in, "%s", buffer);
	if (logs)
	{
		printf("check: %d\n", check);
		printf("buffer: %s\n", buffer);
		dump(buffer, 16);
	}
	if (check != 1)
	{
		fclose(in);
		fprintf(stderr, "Data in input file is invalid!");
		return ERROR_INVALID_DATA;
	}
	char dataMode = 0;
	if (isEqual("phonebook", buffer, 10))
	{
		dataMode = 'P';
	}
	else if (isEqual("int", buffer, 4))
	{
		dataMode = 'I';
	}
	else if (isEqual("float", buffer, 6))
	{
		dataMode = 'F';
	}
	else
	{
		fclose(in);
		fprintf(stderr, "Format in the first string %s is not supported", buffer);
		return ERROR_INVALID_DATA;
	}
	if (logs)
	{
		printf("dataMode: %c\n", dataMode);
	}
	check = fscanf(in, "%s", buffer);
	if (logs)
	{
		printf("check: %d\n", check);
		printf("buffer: %s\n", buffer);
		dump(buffer, 16);
	}
	if (check != 1)
	{
		fclose(in);
		fprintf(stderr, "Data in input file is invalid!");
		return ERROR_INVALID_DATA;
	}
	bool descending = false;
	if (isEqual("descending", buffer, 11))
	{
		descending = true;
	}
	else if (!isEqual("ascending", buffer, 10))
	{
		fclose(in);
		fprintf(stderr, "Mode in the second string %s is not supported", buffer);
		return ERROR_INVALID_DATA;
	}
	size_t size = 0;
	check = fscanf(in, "%zu", &size);
	if (check != 1)
	{
		fclose(in);
		fprintf(stderr, "Data in input file is invalid!");
		return ERROR_INVALID_DATA;
	}
	if (logs)
	{
		printf("check: %d\n", check);
		printf("size: %zu\n", size);
	}
	if (size == 0)
	{
		fclose(in);
		FILE *out = fopen(argv[2], "w");
		if (out == nullptr)
		{
			printf("It was impossible to open output file \"%s\"!\n", argv[2]);
			return ERROR_NOT_FOUND;
		}
		fclose(out);
		return ERROR_SUCCESS;
	}
	if (dataMode == 'I')
	{
		char stringFormat[] = { '%', 'd', 0 };
		check = readAndParse< int >(descending, size, in, logs, stringFormat, argv[2]);
		if (check != 0)
		{
			return check;
		}
	}
	else if (dataMode == 'F')
	{
		char stringFormat[] = { '%', 'g', 0 };
		check = readAndParse< float >(descending, size, in, logs, stringFormat, argv[2]);
		if (check != 0)
		{
			return check;
		}
	}
	else
	{
		char stringFormat[] = { '%', 's', ' ', '%', 's', ' ', '%', 's', ' ', '%', 'd', 0 };
		check = readAndParse< phone_book >(descending, size, in, logs, stringFormat, argv[2]);
		if (check != 0)
		{
			return check;
		}
	}
	return ERROR_SUCCESS;
}