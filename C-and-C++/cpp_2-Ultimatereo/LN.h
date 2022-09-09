#ifndef LN_ULTIMATEREO
#define LN_ULTIMATEREO
#include <string_view>

#include <deque>
#include <string>
class LN
{
  public:
	// Constructors
	explicit LN(long long a = 0);
	explicit LN(const char *a);
	explicit LN(const std::string_view &s);
	LN(const LN &that);
	LN(LN &&that) noexcept;
	// Destructor
	~LN();
	// Operators
	LN &operator=(const LN &that);
	LN &operator=(LN &&that) noexcept;

	friend LN operator+(const LN &a, const LN &b);
	friend LN operator-(const LN &a, const LN &b);
	friend LN operator*(const LN &a, const LN &b);
	friend LN operator/(const LN &a, const LN &b);
	friend LN operator%(const LN &a, const LN &b);
	friend LN operator-(const LN &a);
	LN &operator+=(const LN &a);
	LN &operator-=(const LN &a);
	LN &operator*=(const LN &a);
	LN &operator/=(const LN &a);
	LN &operator%=(const LN &a);
	LN &operator-();

	friend bool operator<(const LN &a, const LN &b);
	friend bool operator<=(const LN &a, const LN &b);
	friend bool operator>(const LN &a, const LN &b);
	friend bool operator>=(const LN &a, const LN &b);
	friend bool operator==(const LN &a, const LN &b);
	friend bool operator!=(const LN &a, const LN &b);

	explicit operator long long() const;
	explicit operator bool() const;
	explicit operator std::string() const;
	friend LN operator""_ln(const char *s);
	[[nodiscard]] LN sqrt() const;

  private:
	bool isNegative, isNaN;
	std::deque< unsigned int > digits;
	[[nodiscard]] int compareTo(const LN &that) const;
	[[nodiscard]] int compareToByAbs(const LN &that) const;
	void sumImpl(const LN &that);
	void subImpl(const LN &that);
	LN addZeros(size_t a) const;
};
#endif