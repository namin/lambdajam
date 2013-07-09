struct Cons ;
struct Int ;
struct Boolean ;
struct Closure ;
union Value ;

enum Tag { VOID, INT, BOOLEAN, CLOSURE, CELL, ENV, CONS, SYMBOL, NIL } ;

typedef union Value (*Lambda)()  ;

struct Int {
  enum Tag t ;
  int value ;
} ;

struct Boolean {
  enum Tag t ;
  unsigned int value ;
} ;

struct Closure {
  enum Tag t ;
  Lambda lam ;
  void* env ;
} ;

struct Env {
  enum Tag t ;
  void* env ;
} ;

struct Cell {
  enum Tag t ;
  union Value* addr ;
} ;

struct Cons {
  enum Tag t ;
  union Value* car;
  union Value* cdr;
} ;

struct Symbol {
  enum Tag t ;
  char* name;
} ;

union Value {
  enum Tag t ;
  struct Int z ;
  struct Boolean b ;
  struct Closure clo ;
  struct Env env ;
  struct Cell cell ;
  struct Cons cons ;
  struct Symbol sym ;
} ;

typedef union Value Value ;

static Value MakeClosure(Lambda lam, Value env) {
  Value v ;
  v.t = CLOSURE ;
  v.clo.lam = lam ;
  v.clo.env = env.env.env ;
  return v ;
}

static Value MakeInt(int n) {
  Value v ;
  v.t = INT ;
  v.z.value = n ;
  return v ;
}

static Value MakeBoolean(unsigned int b) {
  Value v ;
  v.t = BOOLEAN ;
  v.b.value = !!b ;
  return v ;
}

static Value MakePrimitive(Lambda prim) {
  Value v ;
  v.t = CLOSURE ;
  v.clo.lam = prim ;
  v.clo.env = NULL ;
  return v ;
}

static Value MakeEnv(void* env) {
  Value v ;
  v.t = ENV ;
  v.env.env = env ;
  return v ;
}


static Value NewCell(Value initialValue) {
  Value v ;
  v.t = CELL ;
  v.cell.addr = malloc(sizeof(Value)) ;
  *v.cell.addr = initialValue ;
  return v ;
}

static Value MakeNil() {
  Value v ;
  v.t = NIL ;
  return v;
}

static Value MakeSymbol(char* name) {
  Value v;
  v.t = SYMBOL ;
  v.sym.name = name ;
  return v;
}

static Value MakeCons(Value a, Value d) {
  Value v ;
  v.t = CONS ;
  v.cons.car = malloc(sizeof(Value));
  v.cons.cdr = malloc(sizeof(Value));
  *v.cons.car = a;
  *v.cons.cdr = d;
  return v ;
}

static void print_value(Value v, int dot);

static void print_value_ln(Value a) {
  print_value(a, 0);
  printf("\n");
}

static void print_value(Value v, int dot) {
  if (dot && v.t!=CONS && v.t!=NIL)
    printf(" . ");
  switch (v.t) {
  case INT:
    printf("%i",v.z.value);
    break;
  case BOOLEAN:
    printf("%s",v.b.value ? "#t" : "#f");
    break;
  case CONS:
    if (!dot) printf("(");
    print_value(*v.cons.car, 0);
    if ((*v.cons.cdr).t!=NIL) printf(" ");
    print_value(*v.cons.cdr, 1);
    if (!dot) printf(")");
    break;
  case SYMBOL:
    printf("%s", v.sym.name);
    break;
  case NIL:
    if (!dot) printf("()");
    break;
  case CLOSURE:
    printf("#<procedure>");
    break;
  default:
    printf("#<?>");
    break;
  }
}

extern Value __nil;
extern Value __is_symbol;
extern Value __is_pair;
extern Value __cons;
extern Value __car;
extern Value __cdr;
extern Value __is_eq;
extern Value __or;
extern Value __is_proc;
extern Value __lt;
extern Value __sum ;
extern Value __difference ;
extern Value __product ;
extern Value __display ;
extern Value __numEqual ;
