#include <cstdio>
#include <cstring>
#include <cassert>
#include <tuple>


using namespace std;

using ti = tuple<int, int, int>;
ti gcd(int a, int b) {
	int aa = a;
	int bb = b;
	int u = 1, v = 0, s = 0, t = 1, r = 0;
	while (a % 2 == 0 && b % 2 == 0) {
		a /= 2;
		b /= 2;
		r++;
	}
	int alpha = a;
	int beta = b;
	while (a % 2 == 0) {
		a /= 2;
		if (u % 2 == 0 && v % 2 == 0) {
			u /= 2, v /= 2;
		} else {
			assert((u + beta) % 2 == 0);
			assert((v - alpha) % 2 == 0);
			u = (u + beta) / 2;
			v = (v - alpha) / 2;
		}
	}
	while (a != b) {
		if (!(b & 1)) {
			b /= 2;
			if (!(s & 1) && !(t & 1)) {
				s /= 2;
				t /= 2;
			} else {
				assert((s + beta) % 2 == 0);
				assert((t - alpha) % 2 == 0);
				s = (s + beta) / 2;
				t = (t - alpha) / 2;
			}
		} else if (b < a) {
			swap(a, b);
			swap(u, s);
			swap(v, t);
		} else {
			b -= a;
			s -= u;
			t -= v;
		}
	}
	printf("%d = %d*%d%+d*%d\n", a << r, aa, s, bb, t);
	assert(aa * s + bb * t == a << r);
	return ti(a << r, s, t);
}

int main() {
	for (int i = 1; i <= 10; i++) {
		for (int j = 1; j <= 10; j++) {
		gcd(i, j);
		}
	}
}
