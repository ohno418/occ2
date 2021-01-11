#include "test.h"

int main() {
  ASSERT(97, 'a');
  ASSERT(10, '\n');

  printf("OK\n");
  return 0;
}
