#include <algorithm>
#include <cassert>
#include <chrono>
#include <cmath>
#include <iostream>
#include <random>
#include <vector>

using namespace std;
using VecInt = vector<int>;
using PairVecInt = pair<VecInt, VecInt>;

namespace Utility {
    mt19937 generator((random_device())());
    VecInt elemToPower;
    VecInt powerToElem;
    int n, primitivePoly, delta, m, k;

    double nextGaussian(const double mean, const double sigma) {
        static default_random_engine generator(chrono::system_clock::now().time_since_epoch().count());
        normal_distribution normalDistribution(mean, sigma);
        return normalDistribution(generator);
    }

    int elemMultiply(const int a, const int b) {
        if (a == 0 || b == 0) {
            return 0;
        }
        return powerToElem[elemToPower[a] + elemToPower[b]];
    }

    int elemInverse(const int a) {
        return powerToElem[(n - elemToPower[a]) % n];
    }

    void normalizePolynomial(VecInt &v) {
        v.erase(std::find_if(v.rbegin(), v.rend(), [](const int coef) { return coef != 0; }).base(), v.end());
    }


    void printVector(const VecInt &a) {
        for (auto it = a.begin(); it != a.end(); ++it) {
            cout << *it;
            if (it + 1 != a.end()) cout << " ";
        }
        cout << endl;
    }


    int evaluatePolynomial(const VecInt &poly, int x) {
        int result = poly.back();
        for (int j = static_cast<int>(poly.size()) - 2; j >= 0 && j < poly.size(); --j) {
            result = elemMultiply(x, result) ^ poly[j];
        }
        return result;
    }
}// namespace Utility

class Decoder {
public:
    Decoder(int n, int primitivePoly, int delta) {
        Utility::n = n;
        Utility::primitivePoly = primitivePoly;
        Utility::delta = delta;
        Utility::m = static_cast<int>(log2(n + 1));
        Utility::k = n - delta + 1;

        Utility::elemToPower.resize(n + 1);
        Utility::powerToElem.resize(2 * n);

        for (int i = 0, elem = 1; i < n; ++i) {
            Utility::elemToPower[elem] = i;
            Utility::powerToElem[i] = elem;
            elem <<= 1;
            if (static_cast<int>(floor(log2(elem))) == Utility::m) {
                elem ^= primitivePoly;
            }
        }
        for (int i = 0; i < n; ++i) {
            Utility::powerToElem[n + i] = Utility::powerToElem[i];
        }

        g = {1};
        for (int i = 0; i < delta - 1; ++i) {
            g = multiplyPolynomials(g, {Utility::powerToElem[i + 1], 1});
        }
    }

    VecInt encode(const VecInt &a) const {
        VecInt c(Utility::n - Utility::k + 1);
        c[Utility::n - Utility::k] = 1;
        c = multiplyPolynomials(c, a);
        c = addPolynomials(c, dividePolynomials(c, g).second);
        c.resize(Utility::n);
        return c;
    }

    static VecInt decode(const VecInt &y) {
        int t = (Utility::delta - 1) / 2;
        VecInt syndromes(Utility::delta - 1);
        for (int i = 0; i < syndromes.size(); ++i) {
            syndromes[i] = Utility::evaluatePolynomial(y, Utility::powerToElem[i + 1]);
        }

        VecInt a(Utility::delta);
        a.back() = 1;
        VecInt b = syndromes;
        VecInt ua{0}, ub{1};

        while (degreeOfPolynomial(b) >= t) {
             auto [q, r] = dividePolynomials(a, b);
            VecInt ui = addPolynomials(ua, multiplyPolynomials(q, ub));
             a = b;
             b = r;
             ua = ub;
             ub = ui;
        }


        VecInt lambda = ub;
        VecInt gamma = b;
        VecInt decoded = y;

        int normCoeff = lambda[0];
        if (normCoeff > 1) {
            normCoeff = Utility::elemInverse(normCoeff);
             for (int &i: lambda) {
                 i = Utility::elemMultiply(i, normCoeff);
            }
             for (int &i: gamma) {
                 i = Utility::elemMultiply(i, normCoeff);
            }
        }

        VecInt lambdaDerivative(lambda.size() - 1);
        for (int i_l = 1; i_l < lambda.size(); ++i_l) {
            for (int j = 0; j < i_l; ++j){
               lambdaDerivative[i_l - 1] ^= lambda[i_l];
            }
        }

        for (int i_d = 0; i_d < decoded.size(); ++i_d) {
             int a_i_inv = Utility::elemInverse(Utility::powerToElem[i_d]);
            if (Utility::evaluatePolynomial(lambda, a_i_inv) == 0) {
                int num = Utility::evaluatePolynomial(gamma, a_i_inv);
                int den = Utility::evaluatePolynomial(lambdaDerivative, a_i_inv);
                int y_i = Utility::elemMultiply(num, Utility::elemInverse(den));
                decoded[i_d] ^= y_i;
             }
        }
        return decoded;
    }

    static int elemMultiply(const int a, const int b) {
        return Utility::elemMultiply(a, b);
    }

    static int elemInverse(const int a) {
        return Utility::elemInverse(a);
    }

private:
    static VecInt addPolynomials(const VecInt &a, const VecInt &b) {
        if (a.size() < b.size()) {
            return addPolynomials(b, a);
        }
        std::vector<int> res = a;
        for (int i = 0; i < b.size(); ++i) {
            res[i] ^= b[i];
        }
        return res;
    }

    static VecInt multiplyPolynomials(const VecInt &a, const VecInt &b) {
        VecInt result(a.size() + b.size());
        for (size_t i = 0; i < a.size(); ++i) {
            for (size_t j = 0; j < b.size(); ++j) {
                result[i + j] ^= Utility::elemMultiply(a[i], b[j]);
            }
        }
        Utility::normalizePolynomial(result);
        return result;
    }
    static PairVecInt dividePolynomials(VecInt a, VecInt b) {
         VecInt remainder = a;
         VecInt quotient(a.size());

         Utility::normalizePolynomial(b);

         while (remainder.size() >= b.size() && (remainder.size() > 1 || remainder.back() > 0)) {
             int leadingCoeff = Utility::elemMultiply(remainder.back(), Utility::elemInverse(b.back()));
             int degreeDiff = static_cast<int>(remainder.size()) - static_cast<int>(b.size());

             quotient[degreeDiff] = leadingCoeff;

             for (size_t i = 0; i < b.size(); ++i) {
                 remainder[degreeDiff + i] ^= Utility::elemMultiply(leadingCoeff, b[i]);
             }

             Utility::normalizePolynomial(remainder);
         }

         Utility::normalizePolynomial(quotient);
         return {quotient, remainder};
    }

    static int degreeOfPolynomial(const VecInt &poly) {
        int degree = 0;
        for (int i = static_cast<int>(poly.size()) - 1; i >= 0 && i < poly.size(); --i) {
            if (poly[i]) {
                degree = i;
                break;
            }
        }
        return degree;
    }

public:
    VecInt g;
};

long double simulate(const Decoder &decoder, long double noise, int iterationsCount, int maxErrors) {
    uniform_real_distribution<> noiseDist(0, 1);
    uniform_int_distribution<> elemDist(1, Utility::n);

    int iterations = 0;
    int errors = 0;

    while (iterations < iterationsCount && errors < maxErrors) {
        ++iterations;

        std::vector<int> seq(Utility::k);
        for (int &i: seq) i = elemDist(Utility::generator);

        VecInt encoded = decoder.encode(seq);

        VecInt noised = encoded;
        for (int &value: noised) {
            if (noiseDist(Utility::generator) < noise) {
                value ^= elemDist(Utility::generator);
            }
        }

        VecInt decoded = Decoder::decode(noised);

        if (decoded != encoded) {
            ++errors;
        }
    }

    return static_cast<long double>(errors) / iterations;
}


int main() {
    freopen("input.txt", "r", stdin);
    freopen("output.txt", "w", stdout);

    cin >> Utility::n >> Utility::primitivePoly >> Utility::delta;

    Decoder decoder(Utility::n, Utility::primitivePoly, Utility::delta);
    cout << Utility::k << endl;
    Utility::printVector(decoder.g);


    for (string command; cin >> command;) {
        switch (command[0]) {
            case 'E': {
                VecInt a(Utility::k);
                for (int &i: a) cin >> i;
                VecInt res = decoder.encode(a);
                for (const int &i : res) {
                    cout << i << (&i == &res.back() ? "" : " ");
                }
                cout << endl;
                continue;
            }
            case 'D': {
                VecInt y(Utility::n);
                for (int &i: y) cin >> i;
                if (VecInt x = Decoder::decode(y); x.empty()) {
                    cout << "ERROR" << endl;
                } else {
                    for (auto it = x.begin(); it != x.end(); ++it) cout << *it << (it + 1 == x.end() ? "" : " ");
                    cout << endl;
                }
                continue;
            }
            case 'S': {
                long double noise;
                int iterationsCount, maxErrors;
                cin >> noise >> iterationsCount >> maxErrors;
                cout << simulate(decoder, noise, iterationsCount, maxErrors) << endl;
            }
            default: {
            }
        }
    }
    return 0;
}