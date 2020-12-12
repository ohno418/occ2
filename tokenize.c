#include "occ.h"

// Input string
static char *current_input;

// Report an error and exit.
void error(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  exit(1);
}

void error_at(char *loc, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  int pos = loc - current_input;
  fprintf(stderr, "%s\n", current_input);
  fprintf(stderr, "%*s^ ", pos, "");
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  exit(1);
}

void error_tok(Token *tok, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  int pos = tok->loc - current_input;
  fprintf(stderr, "%s\n", current_input);
  fprintf(stderr, "%*s^ ", pos, "");
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  exit(1);
}

static Token *new_token(TokenKind kind, char *loc, int len) {
  Token *tok = calloc(1, sizeof(Token));
  tok->kind = kind;
  tok->loc = loc;
  tok->len = len;
  return tok;
}

bool equal(Token *tok, char *op) {
  return tok->len == strlen(op) && memcmp(tok->loc, op, tok->len) == 0;
}

Token *skip(Token *tok, char *s) {
  if (!equal(tok, s))
    error_tok(tok, "expected '%s'", s);
  return tok->next;
}

static bool starts_with(char *p, char *q) {
  return strncmp(p, q, strlen(q)) == 0;
}

static int read_punct_len(char *p) {
  if (starts_with(p, "==") || starts_with(p, "!=") ||
      starts_with(p, "<=") || starts_with(p, ">="))
    return 2;

  return ispunct(*p) ? 1 : 0;
}

// Tokenize `current_input` and returns new tokens.
Token *tokenize(char *p) {
  current_input = p;

  Token head;
  Token *cur = &head;

  while (*p) {
    // Skip whitespace characters.
    if (isspace(*p)) {
      p++;
      continue;
    }

    // Numeric literal
    if (isdigit(*p)) {
      char *q = p;
      cur = cur->next = new_token(TK_NUM, p, 0);
      cur->val = strtol(p, &p, 10);
      cur->len = p - q;
      continue;
    }

    // Puctuators
    int puct_len = read_punct_len(p);
    if (puct_len) {
      cur = cur->next = new_token(TK_PUNCT, p, puct_len);
      p += puct_len;
      continue;
    }

    error_at(p, "invalid token");
  }

  cur = cur->next = new_token(TK_EOF, p, 0);
  return head.next;
}

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