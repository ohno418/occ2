#define _POSIX_C_SOURCE 200809L
#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//
// tokenize.c
//

typedef enum {
  TK_IDENT,   // Identifiers
  TK_PUNCT,   // Puctuators
  TK_KEYWORD, // Keywords
  TK_NUM,     // Numeric literals
  TK_EOF,     // End-of-file markers
} TokenKind;

typedef struct Token Token;
struct Token {
  TokenKind kind;
  Token *next;
  int val;   // If token is TK_NUM, its value
  char *loc; // Token location
  int len;   // Token length
};

void error(char *fmt, ...);
void error_at(char *loc, char *fmt, ...);
void error_tok(Token *tok, char *fmt, ...);
bool equal(Token *tok, char *op);
Token *skip(Token *tok, char *s);
Token *tokenize(char *p);

//
// parse.c
//

typedef struct Node Node;

// Local varialbe
typedef struct Obj Obj;
struct Obj {
  Obj *next;
  char *name;
  int offset;
};

typedef enum {
  ND_ADD,       // +
  ND_SUB,       // -
  ND_MUL,       // *
  ND_DIV,       // /
  ND_NEG,       // unary -
  ND_EQ,        // ==
  ND_NE,        // !=
  ND_LT,        // <
  ND_LE,        // <=
  ND_ASSIGN,    // =
  ND_RETURN,    // "return"
  ND_IF,        // "if"
  ND_BLOCK,     // { ... }
  ND_EXPR_STMT, // Expression statement
  ND_VAR,       // Varialbe
  ND_NUM,       // Integer
} NodeKind;

// AST node
struct Node {
  NodeKind kind;
  Node *next;

  Node *lhs; // Left-hand side
  Node *rhs; // Right-hand side

  // Block
  Node *body;

  // "if" statement
  Node *cond;
  Node *then;
  Node *els;

  Obj *var;  // Used if kind == ND_VAR
  int val;   // Used if kind == ND_NUM
};

typedef struct Function Function;
struct Function {
  Node *body;
  Obj *locals;
  int stack_size;
};

Function *parse(Token *tok);

//
// codegen.c
//

void codegen(Function *prog);
