#include "occ.h"

static int depth;
// x86_64 calling convention
// (https://en.wikipedia.org/wiki/X86_calling_conventions#x86-64_calling_conventions)
static char *argreg8[] = {"dil", "sil", "dl", "cl", "r8b", "r9b"};
static char *argreg16[] = {"di", "si", "dx", "cx", "r8w", "r9w"};
static char *argreg32[] = {"edi", "esi", "edx", "ecx", "r8d", "r9d"};
static char *argreg64[] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};
static Obj *current_fn;

static void gen_stmt(Node *node);
static void gen_expr(Node *node);

static int count(void) {
  static int i = 1;
  return i++;
}

static void push(void) {
  printf("  push rax\n");
  depth++;
}

static void pop(char *arg) {
  printf("  pop %s\n", arg);
  depth--;
}

// Round up `n` to the nearest multiple of `align`, For example,
// align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
int align_to(int n, int align) {
  return (n + align - 1) / align * align;
}

// Compute the absolute address of a given node.
static void gen_addr(Node *node) {
  switch (node->kind) {
  case ND_VAR:
    if (node->var->is_local)
      printf("  lea rax, [rbp-%d]\n", -node->var->offset);
    else
      printf("  lea rax, %s[rip]\n", node->var->name);
    return;
  case ND_DEREF:
    gen_expr(node->lhs);
    return;
  case ND_COMMA:
    gen_expr(node->lhs);
    gen_addr(node->rhs);
    return;
  case ND_MEMBER:
    gen_addr(node->lhs);
    printf("  add rax, %d\n", node->member->offset);
    return;
  }

  error_tok(node->tok, "not a lvalue");
}

// Load a value from where RAX is pointing to.
static void load(Type *ty) {
  if (ty->kind == TY_ARRAY)
    return;

  if (ty->size == 1)
    printf("  movsbq rax, [rax]\n");
  else if (ty->size == 2)
    printf("  movswq rax, [rax]\n");
  else if (ty->size == 4)
    printf("  movsxd rax, [rax]\n");
  else
    printf("  mov rax, [rax]\n");
}

// Store RAX to an address that the stack top is pointing to.
static void store(Type *ty) {
  pop("rdi");

  if (ty->size == 1)
    printf("  mov [rdi], al\n");
  else if (ty->size == 2)
    printf("  mov [rdi], ax\n");
  else if (ty->size == 4)
    printf("  mov [rdi], eax\n");
  else
    printf("  mov [rdi], rax\n");
}

enum { I8, I16, I32, I64 };

static int get_type_id(Type *ty) {
  switch (ty->kind) {
  case TY_CHAR:
    return I8;
  case TY_SHORT:
    return I16;
  case TY_INT:
    return I32;
  }
  return I64;
}

// The table for type casts
static char i32i8[] = "movsbl eax, al";
static char i32i16[] = "movswl eax, ax";
static char i32i64[] = "movsxd rax, eax";

static char *cast_table[][10] = {
  {NULL,  NULL,   NULL, i32i64}, // i8
  {i32i8, NULL,   NULL, i32i64}, // i16
  {i32i8, i32i16, NULL, i32i64}, // i32
  {i32i8, i32i16, NULL, NULL},   // i64
};

static void cmp_zero(Type *ty) {
  if (is_integer(ty) && ty->size <= 4)
    printf("  cmp eax, 0\n");
  else
    printf("  cmp rax, 0\n");
}

static void cast(Type *from, Type *to) {
  if (to->kind == TY_VOID)
    return;

  if (to->kind == TY_BOOL) {
    cmp_zero(from);
    printf("  setne al\n");
    printf("  movzx rax, al\n");
    return;
  }

  int t1 = get_type_id(from);
  int t2 = get_type_id(to);

  if (cast_table[t1][t2])
    printf("  %s\n", cast_table[t1][t2]);
}

static void gen_expr(Node *node) {
  printf("  .loc 1 %d\n", node->tok->line_no);

  switch (node->kind) {
  case ND_NUM:
    printf("  mov rax, %ld\n", node->val);
    return;
  case ND_NEG:
    gen_expr(node->lhs);
    printf("  neg rax\n");
    return;
  case ND_VAR:
  case ND_MEMBER:
    gen_addr(node);
    load(node->ty);
    return;
  case ND_DEREF:
    gen_expr(node->lhs);
    load(node->ty);
    return;
  case ND_ADDR:
    gen_addr(node->lhs);
    return;
  case ND_ASSIGN:
    gen_addr(node->lhs);
    push();
    gen_expr(node->rhs);
    store(node->ty);
    return;
  case ND_COMMA:
    gen_expr(node->lhs);
    gen_expr(node->rhs);
    return;
  case ND_CAST:
    gen_expr(node->lhs);
    cast(node->lhs->ty, node->ty);
    return;
  case ND_FUNCALL: {
    int nargs = 0;
    for (Node *arg = node->args; arg; arg = arg->next) {
      gen_expr(arg);
      push();
      nargs++;
    }

    for (int i = nargs - 1; i >= 0; i--)
      pop(argreg64[i]);

    printf("  mov rax, 0\n");
    printf("  call %s\n", node->funcname);
    return;
  }
  case ND_STMT_EXPR:
    for (Node *stmt = node->body; stmt; stmt = stmt->next)
      gen_stmt(stmt);
    return;
  case ND_NOT:
    gen_expr(node->lhs);
    printf("cmp rax, 0\n");
    printf("sete al\n");
    printf("movzx rax, al\n");
    return;
  case ND_BITNOT:
    gen_expr(node->lhs);
    printf("not rax\n");
    return;
  case ND_LOGAND: {
    int c = count();
    gen_expr(node->lhs);
    printf("  cmp rax, 0\n");
    printf("  je .L.false.%d\n", c);
    gen_expr(node->rhs);
    printf("  cmp rax, 0\n");
    printf("  je .L.false.%d\n", c);
    printf("  mov rax, 1\n");
    printf("  jmp .L.end.%d\n", c);
    printf(".L.false.%d:\n", c);
    printf("  mov rax, 0\n");
    printf(".L.end.%d:\n", c);
    return;
  }
  case ND_LOGOR: {
    int c = count();
    gen_expr(node->lhs);
    printf("  cmp rax, 0\n");
    printf("  jne .L.true.%d\n", c);
    gen_expr(node->rhs);
    printf("  cmp rax, 0\n");
    printf("  jne .L.true.%d\n", c);
    printf("  mov rax, 0\n");
    printf("  jmp .L.end.%d\n", c);
    printf(".L.true.%d:\n", c);
    printf("  mov rax, 1\n");
    printf(".L.end.%d:\n", c);
    return;
  }
  }

  gen_expr(node->rhs);
  push();              // rhs on rax
  gen_expr(node->lhs);
  pop("rdi");          // lhs on rdi

  char *ax, *di;
  if (node->lhs->ty->kind == TY_LONG || node->lhs->ty->base) {
    ax = "rax";
    di = "rdi";
  } else {
    ax = "eax";
    di = "edi";
  }

  switch (node->kind) {
  case ND_ADD:
    printf("  add %s, %s\n", ax, di);
    return;
  case ND_SUB:
    printf("  sub %s, %s\n", ax, di);
    return;
  case ND_MUL:
    printf("  imul %s, %s\n", ax, di);
    return;
  case ND_DIV:
    if (node->lhs->ty->size == 8)
      printf("  cqo\n");
    else
      printf("  cdq\n");
    printf("  idiv %s\n", di);
    return;
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
    printf("  cmp %s, %s\n", ax, di);

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

  error_tok(node->tok, "invalid expression");
}

static void gen_stmt(Node *node) {
  printf("  .loc 1 %d\n", node->tok->line_no);

  switch (node->kind) {
  case ND_IF: {
    int c = count();
    gen_expr(node->cond);
    printf("  cmp rax, 0\n");
    printf("  je .L.else.%d\n", c);
    gen_stmt(node->then);
    printf("  jmp .L.end.%d\n", c);
    printf(".L.else.%d:\n", c);
    if (node->els)
      gen_stmt(node->els);
    printf(".L.end.%d:\n", c);
    return;
  }
  case ND_FOR: {
    int c = count();
    if (node->init)
      gen_stmt(node->init);
    printf(".L.begin.%d:\n", c);
    if (node->cond) {
      gen_expr(node->cond);
      printf("  cmp rax, 0\n");
      printf("  je .L.end.%d\n", c);
    }
    gen_stmt(node->then);
    if (node->inc)
      gen_expr(node->inc);
    printf("  jmp .L.begin.%d\n", c);
    printf(".L.end.%d:\n", c);
    return;
  }
  case ND_BLOCK:
    for (Node *n = node->body; n; n = n->next)
      gen_stmt(n);
    return;
  case ND_GOTO:
    printf("  jmp %s\n", node->unique_label);
    return;
  case ND_LABEL:
    printf("%s:\n", node->unique_label);
    gen_stmt(node->lhs);
    return;
  case ND_RETURN:
    gen_expr(node->lhs);
    printf("  jmp .L.return.%s\n", current_fn->name);
    return;
  case ND_EXPR_STMT:
    gen_expr(node->lhs);
    return;
  }

  error_tok(node->tok, "invalid statement");
}

// Assign offsets to local varialbes.
static void assign_lvar_offsets(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function)
      continue;

    int offset = 0;
    for (Obj *var = fn->locals; var; var = var->next) {
      offset += var->ty->size;
      offset = align_to(offset, var->ty->align);
      var->offset = -offset;
    }
    fn->stack_size = align_to(offset, 16);
  }
}

static void emit_data(Obj *prog) {
  for (Obj *var = prog; var; var = var->next) {
    if (var->is_function)
      continue;

    printf("  .data\n");
    printf("  .globl %s\n", var->name);
    printf("%s:\n", var->name);

    if (var->init_data) {
      for (int i = 0; i < var->ty->size; i++)
        printf("  .byte %d\n", var->init_data[i]);
    } else {
      printf("  .zero %d\n", var->ty->size);
    }
  }
}

static void store_gp(int r, int offset, int sz) {
  switch (sz) {
  case 1:
    printf("  mov [rbp-%d], %s\n", offset, argreg8[r]);
    return;
  case 2:
    printf("  mov [rbp-%d], %s\n", offset, argreg16[r]);
    return;
  case 4:
    printf("  mov [rbp-%d], %s\n", offset, argreg32[r]);
    return;
  case 8:
    printf("  mov [rbp-%d], %s\n", offset, argreg64[r]);
    return;
  default:
    error("internal error");
  }
}

static void emit_text(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition)
      continue;

    if (fn->is_static)
      printf("  .local %s\n", fn->name);
    else
      printf("  .globl %s\n", fn->name);
    printf("  .text\n");
    printf("%s:\n", fn->name);
    current_fn = fn;

    // Prologue
    printf("  push rbp\n");
    printf("  mov rbp, rsp\n");
    printf("  sub rsp, %d\n", fn->stack_size);

    // Save passed-by-register arguments to the stack.
    int i = 0;
    for (Obj *var = fn->params; var; var = var->next)
      store_gp(i++, -var->offset, var->ty->size);

    // Emit code
    gen_stmt(fn->body);
    assert(depth == 0);

    // Epilogue
    printf(".L.return.%s:\n", fn->name);
    printf("  mov rsp, rbp\n");
    printf("  pop rbp\n");
    printf("  ret\n");
  }
}

void codegen(Obj *prog) {
  assign_lvar_offsets(prog);
  printf("  .intel_syntax noprefix\n");
  emit_data(prog);
  emit_text(prog);
}
