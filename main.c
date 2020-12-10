#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
  TK_PUNCT, // Puctuators
  TK_NUM,   // Numeric literals
  TK_EOF,   // End-of-file markers
} TokenKind;

typedef struct Token Token;
struct Token {
  TokenKind kind;
  Token *next;
  int val;   // If token is TK_NUM, its value
  char *loc; // Token location
  int len;   // Token length
};

// Input string
static char *current_input;

// Report an error and exit.
static void error(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  exit(1);
}

static void error_at(char *loc, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  int pos = loc - current_input;
  fprintf(stderr, "%s\n", current_input);
  fprintf(stderr, "%*s^ ", pos, "");
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  exit(1);
}

static void error_tok(Token *tok, char *fmt, ...) {
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

// Tokenize `current_input` and returns new tokens.
static Token *tokenize(void) {
  char *p = current_input;
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
      int num = strtol(p, &p, 10);
      cur = cur->next = new_token(TK_NUM, p, p - q);
      cur->val = num;
      cur->len = p - q;
      continue;
    }

    // Puctuator
    if (*p == '+' || *p == '-') {
      cur = cur->next = new_token(TK_PUNCT, p, 1);
      p++;
      continue;
    }

    error_at(p, "invalid token");
  }

  cur = cur->next = new_token(TK_EOF, p, 0);
  return head.next;
}

// Surely get an number from the given token.
static int get_number(Token *tok) {
  if (tok->kind != TK_NUM)
    error_tok(tok, "expected a number");
  return tok->val;
}

static bool equal(Token *tok, char *op) {
  return tok->len == strlen(op) && memcmp(tok->loc, op, tok->len) == 0;
}

int main(int argc, char **argv) {
  if (argc != 2)
    error("%s: invalid number of arguments", argv[0]);

  current_input = argv[1];
  Token *tok = tokenize();

  printf(".intel_syntax noprefix\n");
  printf(".global main\n");
  printf("\n");
  printf("main:\n");

  // The first token must be a number.
  printf("  mov rax, %d\n", get_number(tok));
  tok = tok->next;

  while (tok->kind != TK_EOF) {
    if (equal(tok, "+")) {
      printf("  add rax, %d\n", get_number(tok->next));
      tok = tok->next->next;
      continue;
    }

    if (equal(tok, "-")) {
      printf("  sub rax, %d\n", get_number(tok->next));
      tok = tok->next->next;
      continue;
    }

    error_tok(tok, "invalid token");
  }

  printf("  ret\n");
  return 0;
}
