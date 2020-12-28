#include "occ.h"

int main(int argc, char **argv) {
  if (argc != 2)
    error("%s: invalid number of arguments", argv[0]);

  // Tokenize and parse.
  Token *tok = tokenize(argv[1]);
  Obj *prog = parse(tok);

  // Traverse the AST to emit assembly.
  codegen(prog);

  return 0;
}

//
// debug tools
//

// printf("===== token debug =====\n");
// int i = 0;
// for (Token *t = tok; t->kind != TK_EOF; t = t->next) {
//   printf("token %d\n", i);
//   printf("  kind: %d\n", t->kind);
//   printf("  val: %d\n", t->val);
//   printf("  loc: %s\n", t->loc);
//   printf("  len: %d\n", t->len);
//   i++;
// }
// printf("===== debug end =====\n");
