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
  enum Tag t;
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
  v.clo.t = CLOSURE ;
  v.clo.lam = lam ;
  v.clo.env = env.env.env ;
  return v ;
}

static Value MakeInt(int n) {
  Value v ;
  v.z.t = INT ;
  v.z.value = n ;
  return v ;
}

static Value MakeBoolean(unsigned int b) {
  Value v ;
  v.b.t = BOOLEAN ;
  v.b.value = b ;
  return v ;
}

static Value MakePrimitive(Lambda prim) {
  Value v ;
  v.clo.t = CLOSURE ;
  v.clo.lam = prim ;
  v.clo.env = NULL ;
  return v ;
}

static Value MakeEnv(void* env) {
  Value v ;
  v.env.t = ENV ;
  v.env.env = env ;
  return v ;
}


static Value NewCell(Value initialValue) {
  Value v ;
  v.cell.t = CELL ;
  v.cell.addr = malloc(sizeof(Value)) ;
  *v.cell.addr = initialValue ;
  return v ;
}

static Value MakeNil() {
  Value v ;
  v.t = NIL ;
  return v;
}
static Value SYMTAB[100];
int SYMTAB_N = 0;
static Value MakeSymbol(char* name) {
  int i;
  for (i=0; i<SYMTAB_N; i++) {
    if (strcmp(SYMTAB[i].sym.name, name)==0)
      return SYMTAB[i];
  }
  Value v ;
  v.sym.t = SYMBOL ;
  v.sym.name = name;
  SYMTAB[i++] = v;
  return v;
}

static Value MakeCons(Value a, Value d) {
  Value v ;
  v.cons.t = CONS ;
  v.cons.car = malloc(sizeof(Value));
  v.cons.cdr = malloc(sizeof(Value));
  *v.cons.car = a;
  *v.cons.cdr = d;
  return v ;
}

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
