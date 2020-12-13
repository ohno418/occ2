#include "occ.h"

static int depth;

static void push(void) {
  printf("  push rax\n");
  depth++;
}

static void pop(char *arg) {
  printf("  pop %s\n", arg);
  depth--;
}

// Compute the absolute address of a given node.
static void gen_addr(Node *node) {
  if (node->kind == ND_VAR) {
    int offset = (node->name - 'a' + 1) * 8;
    printf("  lea rax, [rbp-%d]\n", offset);
    return;
  }

  error("not a lvalue");
}

static void gen_expr(Node *node) {
  switch (node->kind) {
  case ND_NUM:
    printf("  mov rax, %d\n", node->val);
    return;
  case ND_NEG:
    gen_expr(node->lhs);
    printf("  neg rax\n");
    return;
  case ND_VAR:
    gen_addr(node);
    printf("  mov rax, [rax]\n");
    return;
  case ND_ASSIGN:
    gen_addr(node->lhs);
    push();
    gen_expr(node->rhs);
    pop("rdi");
    printf("  mov [rdi], rax\n");
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
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
    printf("  cmp rax, rdi\n");

    if (node->kind == ND_EQ)
      printf("  sete al\n");
    else if (node->kind == ND_NE)
      printf("  setne al\n");
    else if (node->kind == ND_LT)
      printf("  setl al\n");
    else if (node->kind == ND_LE)
      printf("  setle al\n");

    printf("  movzb rax, al\n");
    return;
  }

  error("invalid expression");
}

static void gen_stmt(Node *node) {
  if (node->kind == ND_EXPR_STMT) {
    gen_expr(node->lhs);
    return;
  }

  error("invalid statement");
}

void codegen(Function *prog) {
  printf(".intel_syntax noprefix\n");
  printf(".global main\n");
  printf("main:\n");

  // Prologue
  printf("  push rbp\n");
  printf("  mov rbp, rsp\n");
  printf("  sub rsp, 208\n");

  for (Node *n = prog->body; n; n = n->next) {
    gen_stmt(n);
    assert(depth == 0);
  }

  // Epilogue
  printf("  mov rsp, rbp\n");
  printf("  pop rbp\n");
  printf("  ret\n");
}
