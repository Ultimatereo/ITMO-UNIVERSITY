#include <cstdint>
#include <cstdio>
#include <cmath>
int INTMAX = 16714589 + 1000;
//static inline uint64_t lg_down(uint64_t x) { return 63U - __builtin_clzl(x); }
//static inline uint64_t lg_up(uint64_t x) { return lg_down(x - 1) + 1; }
int min(int a, int b) { return a < b ? a : b; }
int max(int a, int b) { return a > b ? a : b; }

int main() {
  bool logs = false;
  int n, m, u, v;
  scanf("%d", &n);
  int a[n];
  scanf("%d", &m);
  scanf("%d", &a[0]);
  scanf("%d", &u);
  scanf("%d", &v);
  for (int i = 0; i < n - 1; i++) {
    a[i + 1] = (23 * a[i] + 21563) % 16714589;
  }
  int logn = ceil(log2(n)) + 2;
  int mn[logn + 10][n * 2 + 10];
  for (int i = 0; i < n; i++) {
    mn[0][i] = a[i];
  }
  for (int k = 0; k < logn - 1; k++) {
    for (int i = 0; i + (1 << k) < n; i++) {
      mn[k + 1][i] = min(mn[k][i], mn[k][i + (1 << k)]);
    }
  }
  if (logs) {
    for (int i = 0; i < logn; i++) {
      printf("k = %d ", i);
      for (int j = 0; j < n; j++) {
        printf("%10d", mn[i][j]);
      }
      printf("\n");
    }
  }
  int r = max(u, v);
  int l = min(u, v);
  long k = (long) (log2(r - l + 1));
  int res = min(mn[k][l - 1], mn[k][r - (1 << k)]);
  if (logs) {
    printf("l : %d, r : %d, k : %d\n", l, r, k);
    printf("u : %d, v : %d, r : %d\n", u, v, res);
  }
  for (int i = 1; i < m; i++) {
    u = ((17 * u + 751 + res + 2*i) % n) + 1;
    v = ((13 * v + 593 + res + 5*i) % n) + 1;
    r = max(u, v);
    l = min(u, v);
    k = (long) (log2(r - l + 1));
    res = min(mn[k][l - 1], mn[k][r - (1 << k)]);
    if (logs) {
      printf("l : %d, r : %d, k : %d\n", l, r, k);
      printf("u : %d, v : %d, r : %d\n", u, v, res);
    }
  }
  printf("%d %d %d", u, v, res);
}