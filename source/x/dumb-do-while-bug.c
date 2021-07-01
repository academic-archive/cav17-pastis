int g();

void f(int n) {
	int k;

	if (g()) {

	k = n;
	do {
		k -= 8;
	} while (k >= 0);

	}
}
