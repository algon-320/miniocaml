#include <stdio.h>
int tarai(int x, int y, int z) {
  if (x <= y) return y;
  return tarai(tarai(x-1, y, z), tarai(y-1, z, x), tarai(z-1, x, y));
}
int main() {
  int x, y, z;
  scanf("%d", &x);
  scanf("%d", &y);
  scanf("%d", &z);
  printf("%d\n", tarai(x, y, z));
  return 0;
}
