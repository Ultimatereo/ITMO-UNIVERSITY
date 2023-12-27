#include "phonebook.h"
bool phone_book::operator>(const phone_book &b) const
{
	if ((strcmp(surname, b.surname) > 0) || (strcmp(surname, b.surname) == 0 && strcmp(name, b.name) > 0) ||
		(strcmp(surname, b.surname) == 0 && strcmp(name, b.name) == 0 && strcmp(middleName, b.middleName) > 0) ||
		(strcmp(surname, b.surname) == 0 && strcmp(name, b.name) == 0 && strcmp(middleName, b.middleName) == 0 && number > b.number))
	{
		return true;
	}
	return false;
}

bool phone_book::operator<(const phone_book &b) const
{
	if ((strcmp(surname, b.surname) < 0) || (strcmp(surname, b.surname) == 0 && strcmp(name, b.name) < 0) ||
		(strcmp(surname, b.surname) == 0 && strcmp(name, b.name) == 0 && strcmp(middleName, b.middleName) < 0) ||
		(strcmp(surname, b.surname) == 0 && strcmp(name, b.name) == 0 && strcmp(middleName, b.middleName) == 0 && number < b.number))
	{
		return true;
	}
	return false;
}