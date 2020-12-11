#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//
// Tokenizer
//

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

static bool equal(Token *tok, char *op) {
  return tok->len == strlen(op) && memcmp(tok->loc, op, tok->len) == 0;
}

static Token *skip(Token *tok, char *s) {
  if (!equal(tok, s))
    error_tok(tok, "expected '%s'", s);
  return tok->next;
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
      cur = cur->next = new_token(TK_NUM, p, 0);
      cur->val = strtol(p, &p, 10);
      cur->len = p - q;
      continue;
    }

    // Puctuators
    if (ispunct(*p)) {
      cur = cur->next = new_token(TK_PUNCT, p, 1);
      p++;
      continue;
    }

    error_at(p, "invalid token");
  }

  cur = cur->next = new_token(TK_EOF, p, 0);
  return head.next;
}

//
// Parser
//

typedef enum {
  ND_ADD, // +
  ND_SUB, // -
  ND_MUL, // *
  ND_DIV, // /
  ND_NUM, // Integer
} NodeKind;

typedef struct Node Node;
struct Node {
  NodeKind kind;
  Node *lhs; // Left-hand side
  Node *rhs; // Right-hand side
  int val;   // Used if kind is ND_NUM
};

static Node *new_node(NodeKind kind) {
  Node *node = calloc(1, sizeof(Node));
  node->kind = kind;
  return node;
}

static Node *new_num(int val) {
  Node *node = new_node(ND_NUM);
  node->val = val;
  return node;
}

static Node *new_binary(NodeKind kind, Node *lhs, Node *rhs) {
  Node *node = new_node(kind);
  node->lhs = lhs;
  node->rhs = rhs;
  return node;
}

static Node *expr(Token **rest, Token *tok);
static Node *mul(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);

// expr = mul ("+" mul | "-" mul)*
static Node *expr(Token **rest, Token *tok) {
  Node *node = mul(&tok, tok);

  for (;;) {
    if (equal(tok, "+")) {
      tok = tok->next;
      node = new_binary(ND_ADD, node, mul(&tok, tok));
      continue;
    }

    if (equal(tok, "-")) {
      tok = tok->next;
      node = new_binary(ND_SUB, node, mul(&tok, tok));
      continue;
    }

    *rest = tok;
    return node;
  }
}

// mul = primary ("*" primary | "/" primary)*
static Node *mul(Token **rest, Token *tok) {
  Node *node = primary(&tok, tok);

  for (;;) {
    if (equal(tok, "*")) {
      tok = tok->next;
      node = new_binary(ND_MUL, node, primary(&tok, tok));
      continue;
    }

    if (equal(tok, "/")) {
      tok = tok->next;
      node = new_binary(ND_DIV, node, primary(&tok, tok));
      continue;
    }

    *rest = tok;
    return node;
  }
}

// primary = "(" expr ")" | num
static Node *primary(Token **rest, Token *tok) {
  if (equal(tok, "(")) {
    tok = tok->next;
    Node *node = expr(&tok, tok);
    *rest = skip(tok, ")");
    return node;
  }

  if (tok->kind == TK_NUM) {
    Node *node = new_num(tok->val);
    *rest = tok->next;
    return node;
  }

  error_tok(tok, "expected an expression");
}

//
// Code generator
//

static void push(void) {
  printf("  push rax\n");
}

static void pop(char *arg) {
  printf("  pop %s\n", arg);
}

static void gen_expr(Node *node) {
  if (node->kind == ND_NUM) {
    printf("  mov rax, %d\n", node->val);
    return;
  }

  gen_expr(node->rhs);
  push();              // rhs on rax
  gen_expr(node->lhs);
  pop("rdi");          // lhs on rdi

  switch (node->kind) {
  case ND_ADD:
    printf("  add rax, rdi\n");
    return;
  case ND_SUB:
    printf("  sub rax, rdi\n");
    return;
  case ND_MUL:
    printf("  imul rax, rdi\n");
    return;
  case ND_DIV:
    printf("  cqo\n");
    printf("  idiv rdi\n");
    return;
  default:
    error("unknown node");
  }
}

int main(int argc, char **argv) {
  if (argc != 2)
    error("%s: invalid number of arguments", argv[0]);

  current_input = argv[1];
  Token *tok = tokenize();
  Node *node = expr(&tok, tok);

  if (tok->kind != TK_EOF)
    error_tok(tok, "extra token");

  printf(".intel_syntax noprefix\n");
  printf(".global main\n");
  printf("\n");
  printf("main:\n");

  // Traverse the AST to emit assembly.
  gen_expr(node);
  printf("  ret\n");

  return 0;
}
