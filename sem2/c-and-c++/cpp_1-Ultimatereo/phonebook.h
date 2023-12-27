#include <cstring>

struct phone_book
{
	char surname[21];
	char name[21];
	char middleName[21];
	unsigned long long number;
	bool operator<(const phone_book &b) const;
	bool operator>(const phone_book &b) const;
};