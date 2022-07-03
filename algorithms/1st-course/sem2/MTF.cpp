#include <ext/rope>
#include <iostream>
#include <fstream>
using namespace __gnu_cxx;
using namespace std;

int main()
{
//  ifstream in("k.txt");
//  ofstream out("ksmart.txt");
  int n, m;
  scanf("%d %d", &n, &m);
//  in >> n >> m;
  rope<int> c;
  for (int i = 1; i <= n; i++) {
    c.push_back(i);
  }
  int l, r;
  for (int i = 0; i < m; i++) {
    scanf("%d %d", &l, &r);
//    in >> l >> r;
    rope<int> leftC = c.substr(l - 1, r - l + 1);
    c.erase(l - 1, r - l + 1);
    c.insert(0, leftC);
//    for (int i = 0; i < n; i++) {
//      printf("%c ", c[i]);
//    }
//    printf("\n");
  }
  for (int i = 0; i < n; i++) {
    printf("%d ", c[i]);
//    out << c[i] << " ";
  }
//  in.close();
//  out.close();
//  printf("\n");
}