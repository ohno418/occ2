// Token debug
printf("===== token debug =====\n");
int i = 0;
for (Token *t = tok; t->kind != TK_EOF; t = t->next) {
  printf("token %d\n", i);
  printf("  kind: %d\n", t->kind);
  printf("  val: %d\n", t->val);
  printf("  loc: %s\n", t->loc);
  printf("  len: %d\n", t->len);
  i++;
}
printf("===== debug end =====\n");
