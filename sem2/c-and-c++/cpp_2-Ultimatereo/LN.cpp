#include "LN.h"

#include <cstring>
#include <typeinfo>

LN::LN(long long a)
{
	isNegative = a < 0;
	isNaN = false;
	if (a == 0)
	{
		digits = { 0 };
		return;
	}
	unsigned long long num = a > 0 ? a : -a;
	unsigned long long base = 1'000'000'000;
	digits = {};
	while (num != 0)
	{
		unsigned int last = num % base;
		digits.push_back(last);
		num /= base;
	}
	//	for (auto const &c : digits) {
	//		std::cout << c << ' ';
	//	}
	//	std::cout << "\n";
}

LN::LN(const char *a)
{
	size_t length = strlen(a);
	isNegative = false;
	isNaN = false;
	digits = {};
	if (length == 3 && a[0] == 'N' && a[1] == 'a' && a[2] == 'N')
	{
		isNaN = true;
		return;
	}
	if (length > 0)
	{
		size_t index = 0;
		if (a[0] == '-')
		{
			isNegative = true;
			index++;
		}
		LN ln2(0ll);
		LN base = LN(10ll);
		for (size_t i = index; i < length; i++)
		{
			ln2 = ln2 * base + LN((long long)(a[i] - '0'));
		}
		digits = ln2.digits;
	}
	else
	{
		isNaN = true;
	}
}

LN::LN(const std::string_view &a)
{
	size_t length = a.length();
	isNegative = false;
	isNaN = false;
	digits = {};
	if (length == 3 && a[0] == 'N' && a[1] == 'a' && a[2] == 'N')
	{
		isNaN = true;
		return;
	}
	if (length > 0)
	{
		size_t index = 0;
		if (a[0] == '-')
		{
			isNegative = true;
			index++;
		}
		LN ln2(0ll);
		for (size_t i = length - 1; i >= index; i--)
		{
			ln2 = ln2 * LN(10ll) + LN((long long)(a[i] - '0'));
		}
		digits = ln2.digits;
	}
	else
	{
		isNaN = true;
	}
}

LN::LN(const LN &that)
{
	digits = that.digits;
	isNegative = that.isNegative;
	isNaN = that.isNaN;
}

LN::LN(LN &&that) noexcept
{
	digits = that.digits;
	isNegative = that.isNegative;
	isNaN = that.isNaN;
	that.digits = {};
}

LN::~LN() = default;

LN &LN::operator=(const LN &that)
{
	if (this == &that)
	{
		return *this;
	}
	digits = that.digits;
	isNegative = that.isNegative;
	isNaN = that.isNaN;
	return *this;
}

LN &LN::operator=(LN &&that) noexcept
{
	digits = that.digits;
	isNegative = that.isNegative;
	isNaN = that.isNaN;
	that.digits = {};
	return *this;
}

int LN::compareTo(const LN &that) const
{
	if (isNaN || that.isNaN)
	{
		return 404;
	}
	if (1 == digits.size() && 1 == that.digits.size() && digits[0] == that.digits[0] && digits[0] == 0)
	{
		return 0;
	}
	if (isNegative && !(that.isNegative))
	{
		return -1;
	}
	if (!isNegative && (that.isNegative))
	{
		return 1;
	}
	int res = compareToByAbs(that);
	return isNegative ? -res : res;
}

int LN::compareToByAbs(const LN &that) const
{
	if (digits.size() > that.digits.size())
	{
		return 1;
	}
	if (digits.size() < that.digits.size())
	{
		return -1;
	}
	for (size_t i = 0; i < digits.size(); i++)
	{
		size_t index = digits.size() - i - 1;
		if (digits[index] == that.digits[index])
		{
			continue;
		}
		return digits[index] > that.digits[index] ? 1 : -1;
	}
	return 0;
}

bool operator<(const LN &a, const LN &b)
{
	if (a.isNaN || b.isNaN)
	{
		return false;
	}
	return a.compareTo(b) < 0;
}
bool operator<=(const LN &a, const LN &b)
{
	if (a.isNaN || b.isNaN)
	{
		return false;
	}
	return a.compareTo(b) <= 0;
}
bool operator>(const LN &a, const LN &b)
{
	if (a.isNaN || b.isNaN)
	{
		return false;
	}
	return a.compareTo(b) > 0;
}
bool operator>=(const LN &a, const LN &b)
{
	if (a.isNaN || b.isNaN)
	{
		return false;
	}
	return a.compareTo(b) >= 0;
}
bool operator==(const LN &a, const LN &b)
{
	if (a.isNaN || b.isNaN)
	{
		return false;
	}
	return a.compareTo(b) == 0;
}
bool operator!=(const LN &a, const LN &b)
{
	if (a.isNaN || b.isNaN)
	{
		return false;
	}
	return a.compareTo(b) != 0;
}

void LN::sumImpl(const LN &that)
{
	size_t sizeMax = digits.size() > that.digits.size() ? digits.size() : that.digits.size();
	unsigned long long res;
	unsigned int carry = 0;
	unsigned long long base = 1'000'000'000;
	for (size_t i = 0; i < sizeMax || carry; i++)
	{
		if (i >= digits.size())
		{
			digits.push_back(0);
		}
		res = (long long)digits[i] + carry + (i < that.digits.size() ? that.digits[i] : 0);
		carry = res >= base ? 1 : 0;
		digits[i] = carry == 1 ? res - base : res;
	}
	while (digits.size() > 1 && digits.back() == 0)
	{
		digits.pop_back();
	}
}

void LN::subImpl(const LN &that)
{	 // this >= that
	unsigned int carry = 0;
	long long res;
	unsigned long long base = 1'000'000'000;
	int compRes = compareToByAbs(that);
	if (compRes == 0)
	{
		digits = { 0 };
	}
	else if (compRes > 0)
	{
		for (size_t i = 0; i < that.digits.size() || carry; i++)
		{
			res = digits[i] - (carry + (i < that.digits.size() ? that.digits[i] : 0));
			carry = res < 0 ? 1 : 0;
			digits[i] = carry == 1 ? res + base : res;
		}
		while (digits.size() > 1 && digits.back() == 0)
		{
			digits.pop_back();
		}
	}
	else
	{
		isNegative = !isNegative;
		std::deque< unsigned int > temp = that.digits;
		for (size_t i = 0; i < digits.size() || carry; i++)
		{
			res = temp[i] - (carry + (i < digits.size() ? digits[i] : 0));
			carry = res < 0 ? 1 : 0;
			temp[i] = carry == 1 ? res + base : res;
		}
		while (temp.size() > 1 && temp.back() == 0)
		{
			temp.pop_back();
		}
		digits = temp;
	}
}
LN operator+(const LN &a, const LN &b)
{
	LN res(a);
	res += b;
	return res;
}
LN operator-(const LN &a, const LN &b)
{
	LN res(a);
	res -= b;
	return res;
}
LN operator*(const LN &a, const LN &b)
{
	LN res(a);
	res *= b;
	return res;
}
LN operator/(const LN &a, const LN &b)
{
	LN res(a);
	res /= b;
	return res;
}
LN operator%(const LN &a, const LN &b)
{
	if (a.isNaN || b.isNaN || !b)
	{
		return LN("NaN");
	}

	return a - ((a / b) * b);
}
LN operator-(const LN &that)
{
	LN res(that);
	res.isNegative = !res.isNegative;
	return res;
}

LN &LN::operator+=(const LN &that)
{
	if (isNaN || that.isNaN)
	{
		isNaN = true;
		return *this;
	}
	if (isNegative == that.isNegative)
	{
		sumImpl(that);
	}
	else
	{
		subImpl(that);
	}
	return *this;
}
LN &LN::operator-=(const LN &that)
{
	if (isNaN || that.isNaN)
	{
		isNaN = true;
		return *this;
	}
	if (isNegative == that.isNegative)
	{
		subImpl(that);
	}
	else
	{
		sumImpl(that);
	}
	return *this;
}
LN &LN::operator*=(const LN &that)
{
	isNegative = isNegative != that.isNegative;
	unsigned int carry = 0;
	unsigned long long res = 0;
	std::deque< unsigned int > c(digits.size() + that.digits.size(), 0);
	size_t i = 0;
	unsigned long long cur = 0;
	unsigned long long base = 1'000'000'000;
	while (i < digits.size())
	{
		size_t j = 0;
		while (j < that.digits.size() || carry)
		{
			if (j < that.digits.size())
			{
				cur = (long long)c[i + j] + (long long)digits[i] * that.digits[j] + carry;
			}
			else
			{
				cur = c[i + j] + carry;
			}
			c[i + j] = cur % base;
			carry = cur / base;
			j++;
		}
		i++;
	}
	while (c.size() > 1 && c.back() == 0)
	{
		c.pop_back();
	}
	digits = c;
	return *this;
}
LN &LN::operator/=(const LN &that)
{
	if (!that)
	{
		isNaN = true;
	}
	if (!*this)
	{
		digits = { 0 };
		return *this;
	}
	isNegative = (isNegative != that.isNegative);
	if (digits.size() == 1 && that.digits.size() == 1)
	{
		digits[0] = digits[0] / that.digits[0];
		return *this;
	}
	int res = compareToByAbs(that);
	if (res == 0)
	{
		digits = { 1 };
	}
	else if (res > 0)
	{
		long long base = 1'000'000'000;
		LN a(*this);
		LN b(that);
		while (b.digits.back() < 2147483648)
		{
			b *= LN(2);
			a *= LN(2);
		}
		size_t n = b.digits.size();
		size_t m = a.digits.size() - b.digits.size();
		LN q(0ll);
		if (a >= b.addZeros(m))
		{
			q.digits[0] = 1;
			a -= b.addZeros(m);
		}
		for (size_t j = m; j >= 1; j--)
		{
			long long cur = ((long long)a.digits[n + j - 1] * base + a.digits[n + j - 2]) / b.digits[n - 1];
			cur = cur < base - 1 ? cur : base - 1;
			a -= (LN(cur) * b).addZeros(j - 1);
			while (a < LN(0ll))
			{
				cur--;
				a += b.addZeros(j - 1);
			}
			if (j - 1 == 0)
			{
				q.digits[0] = cur;
			}
			else
			{
				q.digits.push_back(cur);
			}
		}
		digits = q.digits;
	}
	else
	{
		digits = { 0 };
	}
	return *this;
}
LN &LN::operator%=(const LN &that)
{
	*this = *this % that;
	return *this;
}
LN &LN::operator-()
{
	isNegative = !isNegative;
	return *this;
}

LN::operator long long() const
{
	if (isNaN)
	{
		throw std::bad_cast();
	}
	if (!(*this))
	{
		return 0ll;
	}
	long long mul = isNegative ? -1 : 1;
	long long res = digits[0];
	long long lastRes = res;
	long long base = 1'000'000'000;
	for (size_t i = 1; i < digits.size(); i++)
	{
		res += digits[i] * mul * base;
		if ((!isNegative && res < lastRes) || (isNegative && res > lastRes))
		{
			throw std::bad_cast();
		}
		base *= base;
		lastRes = res;
	}
	return res;
}
LN::operator bool() const
{
	if (isNaN)
	{
		return true;
	}
	size_t count = 0;
	for (size_t i = 0; i < digits.size(); i++)
	{
		if (digits[i] == 0)
		{
			count++;
		}
	}
	return count != digits.size();
}
LN::operator std::string() const
{
	if (isNaN)
	{
		return "NaN";
	}
	if (!(*this))
	{
		return "0";
	}
	std::string res = isNegative ? "-" : "";
	res.append(std::to_string(digits[digits.size() - 1]));
	for (size_t i = digits.size() - 1; i >= 1; i--)
	{
		auto curres = std::to_string(digits[i - 1]);
		res.append(std::string(9 - curres.length(), '0')).append(curres);
	}
	return res;
}
LN operator""_ln(const char *s)
{
	return LN(s);
}
LN LN::sqrt() const
{
	return LN(0ll);
}
LN LN::addZeros(size_t a) const
{
	LN b(*this);
	for (size_t i = 0; i < a; i++)
	{
		b.digits.push_front(0);
	}
	return b;
}
