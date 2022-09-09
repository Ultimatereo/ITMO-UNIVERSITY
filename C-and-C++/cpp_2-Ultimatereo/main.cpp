#include "LN.h"
#include "return_codes.h"

#include <cmath>
#include <fstream>
#include <iostream>
#include <stack>
bool isNumber(const char *buffer);
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

int main(int argc, char *argv[])
{
	bool logs = false;
	if (logs)
	{
		LN b = LN(100);
		LN c = LN(5);
		LN d = b / c;
		std::cout << "YOOOOO " << (std::string)d << "\n";
		//		LN a = LN(1000'000'000);
		//		LN b = a - LN(1001);
		//		std::string aStr =  (std::string) a;
		//		std::cout << aStr << "\n";
	}
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
	char buffer[1024];
	std::stack< LN > stackOfOperands;
	while (true)
	{
		int check = fscanf(in, "%s", buffer);
		if (logs)
		{
			printf("------------\n");
			printf("check: %d\n", check);
			printf("buffer: %s\n", buffer);
			printf("------------\n");
		}
		if (check != 1)
		{
			break;
		}
		if (isNumber(buffer))
		{
			LN number(buffer);
			//			if (logs) {
			//				auto numberStr = (std::string) number;
			//				std::cout << numberStr << "\n";
			//			}
			stackOfOperands.push(number);
		}
		else
		{
			LN b = stackOfOperands.top();
			stackOfOperands.pop();
			if (isEqual(buffer, "_", 2))
			{
				stackOfOperands.emplace(-b);
			}
			else if (isEqual(buffer, "~", 2))
			{
				stackOfOperands.emplace(b.sqrt());
			}
			else
			{
				LN a = stackOfOperands.top();
				stackOfOperands.pop();
				if (isEqual(buffer, "+", 2))
				{
					stackOfOperands.emplace(a + b);
				}
				else if (isEqual(buffer, "-", 2))
				{
					stackOfOperands.emplace(a - b);
				}
				else if (isEqual(buffer, "*", 2))
				{
					stackOfOperands.emplace(a * b);
				}
				else if (isEqual(buffer, "/", 2))
				{
					stackOfOperands.emplace(a / b);
				}
				else if (isEqual(buffer, "%", 2))
				{
					stackOfOperands.emplace(a % b);
				}
				else if (isEqual(buffer, "<", 2))
				{
					stackOfOperands.emplace(a < b ? 1 : 0);
				}
				else if (isEqual(buffer, ">", 2))
				{
					stackOfOperands.emplace(a > b ? 1 : 0);
				}
				else if (isEqual(buffer, "<=", 3))
				{
					stackOfOperands.emplace(a <= b ? 1 : 0);
				}
				else if (isEqual(buffer, ">=", 3))
				{
					stackOfOperands.emplace(a >= b ? 1 : 0);
				}
				else if (isEqual(buffer, "==", 3))
				{
					stackOfOperands.emplace(a == b ? 1 : 0);
				}
				else if (isEqual(buffer, "!=", 3))
				{
					stackOfOperands.emplace(a != b ? 1 : 0);
				}
			}
		}
	}
	fclose(in);
	std::ofstream out;
	out.open(argv[2]);
	if (!out.is_open())
	{
		printf("It was impossible to open output file \"%s\"!\n", argv[2]);
		return ERROR_NOT_FOUND;
	}
	while (!stackOfOperands.empty())
	{
		auto str = std::string(stackOfOperands.top());
		out << str << std::endl;
		stackOfOperands.pop();
	}
	out.close();
}
bool isNumber(const char *buffer)
{
	size_t i = 0;
	if (buffer[i] == '-')
	{
		i++;
		if (buffer[i] == 0)
		{
			return false;
		}
	}
	while (true)
	{
		if (buffer[i] == 0)
		{
			break;
		}
		if (!('0' <= buffer[i] && buffer[i] <= '9'))
		{
			return false;
		}
		i++;
	}
	return true;
}
