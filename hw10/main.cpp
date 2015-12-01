#include <cstdio>
#include <random>

std::default_random_engine gent;
const int Q1 = (int)1e9 + 7;
const int Q2 = (int)1e9 + 9;

int r(int a, int b) {
	std::uniform_int_distribution<int> d(a, b);
	return d(gent);
}

int main() {
	for (int i = 0; i < 100; i++) printf("%d%c", r(0, Q1), " \n"[i + 1 == 100]);
	for (int i = 0; i < 100; i++) printf("%d%c", r(0, Q2), " \n"[i + 1 == 100]);
}
