{$ifopt c-}
{$error 'Must enable assertions'}
{$endif}

const
  BASE10 = 10;
  BASE16 = 16;
  Z_FMT = '%Zd';
  Q_FMT = '%Qd';

function alloc_func(alloc_size: sizeuint): pointer; cdecl;
begin
  result := getmem(alloc_size);
end;

procedure free_proc(p: pointer; size: sizeuint); cdecl;
begin
  assert(size = size); // hint off
  freemem(p);
end;

function reallocate_func(p: pointer; old_size, new_size: sizeuint): pointer; cdecl;
begin
  assert(old_size = old_size); // hint off
  result := reallocmem(p, new_size);
end;

function rnd_test(var state: randstate_t): boolean;
const N = 1000000;
var r1, r2, r3: valuint;
begin
  r1 := mp_urandomm_ui(state, N);
  r2 := mp_urandomm_ui(state, N);
  r3 := mp_urandomm_ui(state, N);
  result := (r1 < N) and (r2 < N) and (r3 < N) and ((r1 <> r2) or (r1 <> r3) or (r2 <> r3));
end;

{ TTestGmpBinding }

procedure TTestGmpBinding.mp_set_memory_functions_test;
var
  af0, af: alloc_func_t;
  rf0, rf: reallocate_func_t;
  fp0, fp: free_proc_t;
begin
  mp_get_memory_functions(@af0, @rf0, @fp0);
  try
    mp_set_memory_functions(@alloc_func, @reallocate_func, @free_proc);
    mp_get_memory_functions(@af, @rf, @fp);
    assert(af = @alloc_func);
    assert(rf = @reallocate_func);
    assert(fp = @free_proc);
  finally
    mp_set_memory_functions(af0, rf0, fp0);
    mp_get_memory_functions(@af, @rf, @fp);
    assert(af = af0);
    assert(rf = rf0);
    assert(fp = fp0);
  end;
end;

procedure TTestGmpBinding.mp_get_memory_functions_test;
var
  af: alloc_func_t;
  fp: free_proc_t;
  rf: reallocate_func_t;
begin
  af := nil;
  fp := nil;
  rf := nil;
  mp_get_memory_functions(nil, nil, nil);
  assert(af = nil);
  assert(fp = nil);
  assert(rf = nil);

  mp_get_memory_functions(@af, nil, nil);
  assert(af <> nil);
  assert(fp = nil);
  assert(rf = nil);

  af := nil;
  mp_get_memory_functions(nil, @rf, nil);
  assert(af = nil);
  assert(fp = nil);
  assert(rf <> nil);

  rf := nil;
  mp_get_memory_functions(nil, nil, @fp);
  assert(af = nil);
  assert(fp <> nil);
  assert(rf = nil);
end;

procedure TTestGmpBinding.mp_randinit_default_test;
var state: randstate_t;
begin
  mp_randinit_default(state);
  assert(rnd_test(state));
  mp_randclear(state);
end;

procedure TTestGmpBinding.mp_randinit_mt_test;
var state: randstate_t;
begin
  mp_randinit_mt(state);
  assert(rnd_test(state));
  mp_randclear(state);
end;

procedure TTestGmpBinding.mp_randinit_lc_2exp_test;
const
  A_ =  3;
  C = 5;
  M2EXP = 8;
var
  state: randstate_t;
  a: mpz_t;
begin
  mpz_init_set_ui(a, A_);
  mp_randinit_lc_2exp(state, a, C, M2EXP);
  assert(rnd_test(state));
  mp_randclear(state);
  mpz_clear(a);
end;

procedure TTestGmpBinding.mp_randinit_lc_2exp_size_test;
var state: randstate_t;
begin
  assert(longbool(mp_randinit_lc_2exp_size(state, 0)));
  assert(rnd_test(state));
  mp_randclear(state);
end;

procedure TTestGmpBinding.mp_randinit_set_test;
var rop, op: randstate_t;
begin
  mp_randinit_default(op);
  mp_randinit_set(rop, op);
  assert(rnd_test(rop));
  mp_randclear(rop);
  mp_randclear(op);
end;

procedure TTestGmpBinding.mp_randclear_test;
var state: randstate_t;
begin
  mp_randinit_default(state);
  mp_randclear(state);
end;

procedure TTestGmpBinding.mp_randseed_test;
var
  state: randstate_t;
  seed: mpz_t;
begin
  mp_randinit_default(state);
  mpz_init_set_ui(seed, 0);
  mp_randseed(state, seed);
  mp_randclear(state);
  mpz_clear(seed);
end;

procedure TTestGmpBinding.mp_randseed_ui_test;
var state: randstate_t;
begin
  mp_randinit_default(state);
  mp_randseed_ui(state, 0);
  mp_randclear(state);
end;

procedure TTestGmpBinding.mp_urandomb_ui_test;
const
  B = 8 * (sizeof(valuint) - 1);
  N = 1 shl B;
var
  state: randstate_t;
  r1, r2, r3: valuint;
begin
  mp_randinit_default(state);
  r1 := mp_urandomb_ui(state, B);
  r2 := mp_urandomb_ui(state, B);
  r3 := mp_urandomb_ui(state, B);
  assert((r1 < N) and (r2 < N) and (r3 < N) and ((r1 <> r2) or (r1 <> r3) or (r2 <> r3)));
  mp_randclear(state);
end;

procedure TTestGmpBinding.mp_urandomm_ui_test;
const  N = high(valuint);
var
  state: randstate_t;
  r1, r2, r3: valuint;
begin
  mp_randinit_default(state);
  r1 := mp_urandomm_ui(state, N);
  r2 := mp_urandomm_ui(state, N);
  r3 := mp_urandomm_ui(state, N);
  assert((r1 < N) and (r2 < N) and (r3 < N) and ((r1 <> r2) or (r1 <> r3) or (r2 <> r3)));
  mp_randclear(state);
end;

procedure TTestGmpBinding.mp_asprintf_test;
const
  N = 123456;
  S = '123456';
var
  z: mpz_t;
  p: pchar;
begin
  mpz_init_set_ui(z, N);
  assert(mp_asprintf(p, Z_FMT, [@z]) = length(S));
  assert(p = S);
  freemem(p);
  mpz_clear(z);
end;

procedure TTestGmpBinding.mp_asprintf_test2;
const
  N = 123456;
  S = '123456';
var
  z: mpz_t;
  p: pchar;
begin
  mpz_init_set_ui(z, N);
  assert(mp_asprintf(p, Z_FMT, @z) = length(S));
  assert(p = S);
  freemem(p);
  mpz_clear(z);
end;

procedure TTestGmpBinding.mp_snprintf_test;
const
  N = 123456;
  S = '123456';
var
  z: mpz_t;
  buf: pchar;
begin
  mpz_init_set_ui(z, N);
  getmem(buf, length(S) + 1);
  assert(mp_snprintf(buf, length(S) + 1, Z_FMT, [@z]) = length(S));
  assert(buf = S);
  freemem(buf);
  mpz_clear(z);
end;

procedure TTestGmpBinding.mp_snprintf_test2;
const
  N = 123456;
  S = '123456';
var
  z: mpz_t;
  buf: pchar;
begin
  mpz_init_set_ui(z, N);
  getmem(buf, length(S) + 1);
  assert(mp_snprintf(buf, length(S) + 1, Z_FMT, @z) = length(S));
  assert(buf = S);
  freemem(buf);
  mpz_clear(z);
end;

procedure TTestGmpBinding.mp_sprintf_test;
const
  N = 123456;
  S = '123456';
var
  z: mpz_t;
  buf: pchar;
begin
  mpz_init_set_ui(z, N);
  getmem(buf, length(S) + 1);
  assert(mp_sprintf(buf, Z_FMT, [@z]) = length(S));
  assert(buf = S);
  freemem(buf);
  mpz_clear(z);
end;

procedure TTestGmpBinding.mp_sprintf_test2;
const
  N = 123456;
  S = '123456';
var
  z: mpz_t;
  buf: pchar;
begin
  mpz_init_set_ui(z, N);
  getmem(buf, length(S) + 1);
  assert(mp_sprintf(buf, Z_FMT, @z) = length(S));
  assert(buf = S);
  freemem(buf);
  mpz_clear(z);
end;

procedure TTestGmpBinding.mp_sscanf_test;
const
  N = 3;
  D = 2;
var q: mpq_t;
begin
  mpq_init(q);
  assert(mp_sscanf(pchar(inttostr(N) + '/' + inttostr(D)), Q_FMT, [@q]) = 1);
  assert(mpq_cmp_ui(q, N, D) = 0);
  mpq_clear(q);
end;

procedure TTestGmpBinding.mp_sscanf_test2;
const
  N = 3;
  D = 2;
var q: mpq_t;
begin
  mpq_init(q);
  assert(mp_sscanf(pchar(inttostr(N) + '/' + inttostr(D)), Q_FMT, @q) = 1);
  assert(mpq_cmp_ui(q, N, D) = 0);
  mpq_clear(q);
end;

procedure TTestGmpBinding.mpz_init_test;
var integer_: mpz_t;
begin
  mpz_init(integer_);
  assert(mpz_get_si(integer_) = 0);
  mpz_clear(integer_);
end;

procedure TTestGmpBinding.mpz_clear_test;
const N = 1000000;
var integer_: mpz_t;
begin
  mpz_init2(integer_, N);
  assert(mpz_get_si(integer_) = 0);
  mpz_clear(integer_);
end;

procedure TTestGmpBinding.mpz_init2_test;
const N = 1000000;
var integer_: mpz_t;
begin
  mpz_init2(integer_, N);
  assert(mpz_get_si(integer_) = 0);
  mpz_clear(integer_);
end;

procedure TTestGmpBinding.mpz_init_set_si_test;
const N = -1000000;
var rop: mpz_t;
begin
  mpz_init_set_si(rop, N);
  assert(mpz_get_si(rop) = N);
  mpz_clear(rop);
end;

procedure TTestGmpBinding.mpz_init_set_ui_test;
const N = 1000000;
var rop: mpz_t;
begin
  mpz_init_set_ui(rop, N);
  assert(mpz_get_ui(rop) = N);
  mpz_clear(rop);
end;

procedure TTestGmpBinding.mpz_init_set_d_test;
const N = -1000000;
var rop: mpz_t;
begin
  mpz_init_set_d(rop, N);
  assert(mpz_get_si(rop) = N);
  mpz_clear(rop);
end;

procedure TTestGmpBinding.mpz_init_set_test;
const N = -1000000;
var rop, op: mpz_t;
begin
  mpz_init_set_si(op, N);
  mpz_init_set(rop, op);
  assert(mpz_get_si(rop) = N);
  mpz_clear(rop);
  mpz_clear(op);
end;

procedure TTestGmpBinding.mpz_init_set_str_test;
const N = 1000000;
var rop: mpz_t;
begin
  assert(mpz_init_set_str(rop, pchar(inttostr(N)), BASE10) = 0);
  assert(mpz_get_si(rop) = N);
  mpz_clear(rop);
end;

procedure TTestGmpBinding.mpz_realloc_test;
const N = 1000;
var integer_: mpz_t;
begin
  mpz_init(integer_);
  mpz_realloc(integer_, N);
  mpz_clear(integer_)
end;

procedure TTestGmpBinding.mpz_realloc2_test;
const N = 1000;
var integer_: mpz_t;
begin
  mpz_init(integer_);
  mpz_realloc2(integer_, N);
  mpz_clear(integer_)
end;

procedure TTestGmpBinding.mpz_getlimbn_test;
const N = 1234;
var op: mpz_t;
begin
  mpz_init_set_ui(op, N);
  assert(mpz_getlimbn(op, 0) = N);
  mpz_clear(op);
end;

procedure TTestGmpBinding.mpz_export_test;
type t = word;
const
  N = 1000000;
  BUF_HIGH = sizeof(N) div sizeof(t);
var
  export_op, import_op: mpz_t;
  buf: array[0..BUF_HIGH] of t;
  count: sizeuint;
begin
  mpz_init_set_si(export_op, N);
  mpz_init(import_op);
  mpz_export(buf, count, -1, sizeof(t), 0, 0, export_op);
  mpz_import(import_op, count, -1, sizeof(t), 0, 0, buf);
  assert(mpz_get_si(export_op) =mpz_get_si(import_op));
  mpz_clear(export_op);
  mpz_clear(import_op);
end;

procedure TTestGmpBinding.mpz_import_test;
type t = word;
const
  N = 1000000;
  BUF_HIGH = sizeof(N) div sizeof(t);
var
  export_op, import_op: mpz_t;
  buf: array[0..BUF_HIGH] of t;
  count: sizeuint;
begin
  mpz_init_set_si(export_op, N);
  mpz_init(import_op);
  mpz_export(buf, count, -1, sizeof(t), 0, 0, export_op);
  mpz_import(import_op, count, -1, sizeof(t), 0, 0, buf);
  assert(mpz_get_si(export_op) =mpz_get_si(import_op));
  mpz_clear(export_op);
  mpz_clear(import_op);
end;

procedure TTestGmpBinding.mpz_swap_test;
const
  ROP_1 = -1000;
  ROP_2 = 1000000;
var rop1, rop2: mpz_t;
begin
  mpz_init_set_si(rop1, ROP_1);
  mpz_init_set_si(rop2, ROP_2);
  mpz_swap(rop1, rop2);
  assert(mpz_get_si(rop1) = ROP_2);
  assert(mpz_get_si(rop2) = ROP_1);
  mpz_clear(rop1);
  mpz_clear(rop2);
end;

procedure TTestGmpBinding.mpz_set_test;
const OP_ = 1000000;
var rop, op: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op, OP_);
  mpz_set(rop, op);
  assert(mpz_get_si(rop) = OP_);
  mpz_clear(rop);
  mpz_clear(op);
end;

procedure TTestGmpBinding.mpz_set_d_test;
const OP_ = 1000000;
var rop: mpz_t;
begin
  mpz_init(rop);
  mpz_set_d(rop, OP_);
  assert(mpz_get_si(rop) = OP_);
  mpz_clear(rop);
end;

procedure TTestGmpBinding.mpz_set_f_test;
const OP_ = -10;
var
  op: mpf_t;
  rop: mpz_t;
begin
  mpf_init_set_si(op, OP_);
  mpz_init(rop);
  mpz_set_f(rop, op);
  assert(mpz_get_si(rop) = OP_);
  mpz_clear(rop);
  mpf_clear(op);
end;

procedure TTestGmpBinding.mpz_set_q_test;
const OP_ = -10;
var
  op: mpq_t;
  rop: mpz_t;
begin
  mpq_init(op);
  mpz_init(rop);
  mpq_set_si(op, OP_, 1);
  mpz_set_q(rop, op);
  assert(mpz_get_si(rop) = OP_);
  mpz_clear(rop);
  mpq_clear(op);
end;

procedure TTestGmpBinding.mpz_set_si_test;
const OP_ = -1000000;
var rop: mpz_t;
begin
  mpz_init(rop);
  mpz_set_si(rop, OP_);
  assert(mpz_get_si(rop) = OP_);
  mpz_clear(rop);
end;

procedure TTestGmpBinding.mpz_set_str_test;
const OP_ = -1000000;
var rop: mpz_t;
begin
  mpz_init(rop);
  assert(mpz_set_str(rop, pchar(inttostr(OP_)), BASE10) = 0);
  assert(mpz_get_si(rop) = OP_);
  mpz_clear(rop);
end;

procedure TTestGmpBinding.mpz_set_ui_test;
const OP_ = 1000000;
var rop: mpz_t;
begin
  mpz_init(rop);
  mpz_set_ui(rop, OP_);
  assert(mpz_get_si(rop) = OP_);
  mpz_clear(rop);
end;

procedure TTestGmpBinding.mpz_get_d_test;
const N = -1000;
var op: mpz_t;
begin
  mpz_init_set_si(op, N);
  assert(mpz_get_d(op) = N);
  mpz_clear(op)
end;

procedure TTestGmpBinding.mpz_get_d_2exp_test;
const
  N_LG2 = 4;
  N = 1 shl N_LG2;
var
  op: mpz_t;
  exp: mp_exp_t;
begin
  mpz_init_set_si(op, N);
  assert(mpz_get_d_2exp(exp, op) = 1 / 2);
  assert(exp = N_LG2 + 1);
  mpz_clear(op)
end;

procedure TTestGmpBinding.mpz_get_si_test;
const N = -1000000;
var op: mpz_t;
begin
  mpz_init_set_si(op, N);
  assert(mpz_get_si(op) = N);
  mpz_clear(op)
end;

procedure TTestGmpBinding.mpz_get_str_test;
const N = -1000000;
var
  op: mpz_t;
  p: pchar;
  s0, s1: string;
begin
  mpz_init_set_si(op, N);
  s0 := inttostr(N);
  p :=mpz_get_str(nil, BASE10, op);
  assert(s0 = p);
  freemem(p);
  setlength(s1, length(s0));
  assert(pointer(mpz_get_str(pchar(s1), BASE10, op)) = pointer(s1));
  assert(s0 = s1);
  mpz_clear(op)
end;

procedure TTestGmpBinding.mpz_get_ui_test;
const N = 1000000;
var op: mpz_t;
begin
  mpz_init_set_ui(op, N);
  assert(mpz_get_ui(op) = N);
  mpz_clear(op)
end;

procedure TTestGmpBinding.mpz_add_test;
const
  OP_1 = -1000;
  OP_2 = 1000000;
var rop, op1, op2: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op1, OP_1);
  mpz_init_set_si(op2, OP_2);
  mpz_add(rop, op1, op2);
  assert(mpz_get_si(rop) = OP_1 + OP_2);
  mpz_clear(rop);
  mpz_clear(op1);
  mpz_clear(op2);
end;

procedure TTestGmpBinding.mpz_add_ui_test;
const
  OP_1 = -1000;
  OP_2 = 1000000;
var rop, op1: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op1, OP_1);
  mpz_add_ui(rop, op1, OP_2);
  assert(mpz_get_si(rop) = OP_1 + OP_2);
  mpz_clear(rop);
  mpz_clear(op1);
end;

procedure TTestGmpBinding.mpz_sub_test;
const
  OP_1 = -1000;
  OP_2 = 1000000;
var rop, op1, op2: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op1, OP_1);
  mpz_init_set_si(op2, OP_2);
  mpz_sub(rop, op1, op2);
  assert(mpz_get_si(rop) = OP_1 - OP_2);
  mpz_clear(rop);
  mpz_clear(op1);
  mpz_clear(op2);
end;

procedure TTestGmpBinding.mpz_sub_ui_test;
const
  OP_1 = -1000;
  OP_2 = 1000000;
var rop, op1: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op1, OP_1);
  mpz_sub_ui(rop, op1, OP_2);
  assert(mpz_get_si(rop) = OP_1 - OP_2);
  mpz_clear(rop);
  mpz_clear(op1);
end;

procedure TTestGmpBinding.mpz_ui_sub_test;
const
  OP_1 = 1000;
  OP_2 = 1000000;
var rop, op2: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op2, OP_2);
  mpz_ui_sub(rop, OP_1, op2);
  assert(mpz_get_si(rop) = OP_1 - OP_2);
  mpz_clear(rop);
  mpz_clear(op2);
end;

procedure TTestGmpBinding.mpz_mul_test;
const
  OP_1 = -10;
  OP_2 = 1000;
var rop, op1, op2: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op1, OP_1);
  mpz_init_set_si(op2, OP_2);
  mpz_mul(rop, op1, op2);
  assert(mpz_get_si(rop) = OP_1 * OP_2);
  mpz_clear(rop);
  mpz_clear(op1);
  mpz_clear(op2);
end;

procedure TTestGmpBinding.mpz_mul_si_test;
const
  OP_1 = 10;
  OP_2 = -1000;
var rop, op1: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op1, OP_1);
  mpz_mul_si(rop, op1, OP_2);
  assert(mpz_get_si(rop) = OP_1 * OP_2);
  mpz_clear(rop);
  mpz_clear(op1);
end;

procedure TTestGmpBinding.mpz_mul_ui_test;
const
  OP_1 = -10;
  OP_2 = 1000;
var rop, op1: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op1, OP_1);
  mpz_mul_ui(rop, op1, OP_2);
  assert(mpz_get_si(rop) = OP_1 * OP_2);
  mpz_clear(rop);
  mpz_clear(op1);
end;

procedure TTestGmpBinding.mpz_addmul_test;
const
  ROP_ = 10;
  OP_1 = -100;
  OP_2 = 1000;
var rop, op1, op2: mpz_t;
begin
  mpz_init_set_si(rop, ROP_);
  mpz_init_set_si(op1, OP_1);
  mpz_init_set_si(op2, OP_2);
  mpz_addmul(rop, op1, op2);
  assert(mpz_get_si(rop) = ROP_ + OP_1 * OP_2);
  mpz_clear(rop);
  mpz_clear(op1);
  mpz_clear(op2);
end;

procedure TTestGmpBinding.mpz_addmul_ui_test;
const
  ROP_ = 10;
  OP_1 = -100;
  OP_2 = 1000;
var rop, op1: mpz_t;
begin
  mpz_init_set_si(rop, ROP_);
  mpz_init_set_si(op1, OP_1);
  mpz_addmul_ui(rop, op1, OP_2);
  assert(mpz_get_si(rop) = ROP_ + OP_1 * OP_2);
  mpz_clear(rop);
  mpz_clear(op1);
end;

procedure TTestGmpBinding.mpz_submul_test;
const
  ROP_ = 10;
  OP_1 = -100;
  OP_2 = 1000;
var rop, op1, op2: mpz_t;
begin
  mpz_init_set_si(rop, ROP_);
  mpz_init_set_si(op1, OP_1);
  mpz_init_set_si(op2, OP_2);
  mpz_submul(rop, op1, op2);
  assert(mpz_get_si(rop) = ROP_ - OP_1 * OP_2);
  mpz_clear(rop);
  mpz_clear(op1);
  mpz_clear(op2);
end;

procedure TTestGmpBinding.mpz_submul_ui_test;
const
  ROP_ = 10;
  OP_1 = -100;
  OP_2 = 1000;
var rop, op1: mpz_t;
begin
  mpz_init_set_si(rop, ROP_);
  mpz_init_set_si(op1, OP_1);
  mpz_submul_ui(rop, op1, OP_2);
  assert(mpz_get_si(rop) = ROP_ - OP_1 * OP_2);
  mpz_clear(rop);
  mpz_clear(op1);
end;

procedure TTestGmpBinding.mpz_mul_2exp_test;
const
  OP_1 = 10;
  OP_2 = 10;
var rop, op1: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op1, OP_1);
  mpz_mul_2exp(rop, op1, OP_2);
  assert(mpz_get_si(rop) = OP_1 * (1 shl OP_2));
  mpz_clear(rop);
  mpz_clear(op1);
end;

procedure TTestGmpBinding.mpz_neg_test;
const OP_ = 1000;
var rop, op: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op, OP_);
  mpz_neg(rop, op);
  assert(mpz_get_si(rop) = -OP_);
  mpz_clear(rop);
  mpz_clear(op);
end;

procedure TTestGmpBinding.mpz_abs_test;
const OP_ = -1000;
var rop, op: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op, OP_);
  mpz_abs(rop, op);
  assert(mpz_get_si(rop) = abs(OP_));
  mpz_clear(rop);
  mpz_clear(op);
end;

procedure TTestGmpBinding.mpz_cdiv_q_test;
const
  N_ = -17;
  D_ = 4;
var q, n, d: mpz_t;
begin
  mpz_init(q);
  mpz_init_set_si(n, N_);
  mpz_init_set_si(d, D_);
  mpz_cdiv_q(q, n, d);
  assert(mpz_get_si(q) = ceil(N_ / D_));
  mpz_clear(q);
  mpz_clear(n);
  mpz_clear(d);
end;

procedure TTestGmpBinding.mpz_fdiv_q_test;
const
  N_ = -17;
  D_ = 4;
var q, n, d: mpz_t;
begin
  mpz_init(q);
  mpz_init_set_si(n, N_);
  mpz_init_set_si(d, D_);
  mpz_fdiv_q(q, n, d);
  assert(mpz_get_si(q) = floor(N_ / D_));
  mpz_clear(q);
  mpz_clear(n);
  mpz_clear(d);
end;

procedure TTestGmpBinding.mpz_tdiv_q_test;
const
  N_ = -17;
  D_ = 4;
var q, n, d: mpz_t;
begin
  mpz_init(q);
  mpz_init_set_si(n, N_);
  mpz_init_set_si(d, D_);
  mpz_tdiv_q(q, n, d);
  assert(mpz_get_si(q) = trunc(N_ / D_));
  mpz_clear(q);
  mpz_clear(n);
  mpz_clear(d);
end;

procedure TTestGmpBinding.mpz_cdiv_q_2exp_test;
const
  N_ = -17;
  B_ = 2;
  D_ = 1 shl B_;
var q, n: mpz_t;
begin
  mpz_init(q);
  mpz_init_set_si(n, N_);
  mpz_cdiv_q_2exp(q, n, B_);
  assert(mpz_get_si(q) = ceil(N_ / D_));
  mpz_clear(q);
  mpz_clear(n);
end;

procedure TTestGmpBinding.mpz_fdiv_q_2exp_test;
const
  N_ = -17;
  B_ = 2;
  D_ = 1 shl B_;
var q, n: mpz_t;
begin
  mpz_init(q);
  mpz_init_set_si(n, N_);
  mpz_fdiv_q_2exp(q, n, B_);
  assert(mpz_get_si(q) = floor(N_ / D_));
  mpz_clear(q);
  mpz_clear(n);
end;

procedure TTestGmpBinding.mpz_tdiv_q_2exp_test;
const
  N_ = -17;
  B_ = 2;
  D_ = 1 shl B_;
var q, n: mpz_t;
begin
  mpz_init(q);
  mpz_init_set_si(n, N_);
  mpz_tdiv_q_2exp(q, n, B_);
  assert(mpz_get_si(q) = trunc(N_ / D_));
  mpz_clear(q);
  mpz_clear(n);
end;

procedure TTestGmpBinding.mpz_cdiv_q_ui_test;
const
  N_ = -17;
  D_ = 4;
var
  q, n: mpz_t;
  ur: valuint;
begin
  mpz_init(q);
  mpz_init_set_si(n, N_);
  ur :=mpz_cdiv_q_ui(q, n, D_);
  assert(mpz_get_si(q) = ceil(N_ / D_));
  assert(ur - abs(N_ -mpz_get_si(q) * D_) = 0);
  mpz_clear(q);
  mpz_clear(n);
end;

procedure TTestGmpBinding.mpz_fdiv_q_ui_test;
const
  N_ = -17;
  D_ = 4;
var
  q, n: mpz_t;
  ur: valuint;
begin
  mpz_init(q);
  mpz_init_set_si(n, N_);
  ur :=mpz_fdiv_q_ui(q, n, D_);
  assert(mpz_get_si(q) = floor(N_ / D_));
  assert(ur - abs(N_ -mpz_get_si(q) * D_) = 0);
  mpz_clear(q);
  mpz_clear(n);
end;

procedure TTestGmpBinding.mpz_tdiv_q_ui_test;
const
  N_ = -17;
  D_ = 4;
var
  q, n: mpz_t;
  ur: valuint;
begin
  mpz_init(q);
  mpz_init_set_si(n, N_);
  ur :=mpz_tdiv_q_ui(q, n, D_);
  assert(mpz_get_si(q) = trunc(N_ / D_));
  assert(ur - abs(N_ -mpz_get_si(q) * D_) = 0);
  mpz_clear(q);
  mpz_clear(n);
end;

procedure TTestGmpBinding.mpz_cdiv_qr_test;
const
  N_ = -17;
  D_ = 4;
var q, r, n, d: mpz_t;
begin
  mpz_init(q);
  mpz_init(r);
  mpz_init_set_si(n, N_);
  mpz_init_set_si(d, D_);
  mpz_cdiv_qr(q, r, n, d);
  assert(mpz_get_si(q) = ceil(N_ / D_));
  assert(mpz_get_si(q) * D_ + mpz_get_si(r) = N_);
  mpz_clear(q);
  mpz_clear(r);
  mpz_clear(n);
  mpz_clear(d);
end;

procedure TTestGmpBinding.mpz_fdiv_qr_test;
const
  N_ = -17;
  D_ = 4;
var q, r, n, d: mpz_t;
begin
  mpz_init(q);
  mpz_init(r);
  mpz_init_set_si(n, N_);
  mpz_init_set_si(d, D_);
  mpz_fdiv_qr(q, r, n, d);
  assert(mpz_get_si(q) = floor(N_ / D_));
  assert(mpz_get_si(q) * D_ + mpz_get_si(r) = N_);
  mpz_clear(q);
  mpz_clear(r);
  mpz_clear(n);
  mpz_clear(d);
end;

procedure TTestGmpBinding.mpz_tdiv_qr_test;
const
  N_ = -17;
  D_ = 4;
var q, r, n, d: mpz_t;
begin
  mpz_init(q);
  mpz_init(r);
  mpz_init_set_si(n, N_);
  mpz_init_set_si(d, D_);
  mpz_tdiv_qr(q, r, n, d);
  assert(mpz_get_si(q) = trunc(N_ / D_));
  assert(mpz_get_si(q) * D_ + mpz_get_si(r) = N_);
  mpz_clear(q);
  mpz_clear(r);
  mpz_clear(n);
  mpz_clear(d);
end;

procedure TTestGmpBinding.mpz_cdiv_qr_ui_test;
const
  N_ = -17;
  D_ = 4;
var
  q, r, n: mpz_t;
  ur: valuint;
begin
  mpz_init(q);
  mpz_init(r);
  mpz_init_set_si(n, N_);
  ur :=mpz_cdiv_qr_ui(q, r, n, D_);
  assert(mpz_get_si(q) = ceil(N_ / D_));
  assert(mpz_get_si(q) * D_ + mpz_get_si(r) = N_);
  assert(ur - abs(mpz_get_si(r)) = 0);
  mpz_clear(q);
  mpz_clear(r);
  mpz_clear(n);
end;

procedure TTestGmpBinding.mpz_fdiv_qr_ui_test;
const
  N_ = -17;
  D_ = 4;
var
  q, r, n: mpz_t;
  ur: valuint;
begin
  mpz_init(q);
  mpz_init(r);
  mpz_init_set_si(n, N_);
  ur :=mpz_fdiv_qr_ui(q, r, n, D_);
  assert(mpz_get_si(q) = floor(N_ / D_));
  assert(mpz_get_si(q) * D_ + mpz_get_si(r) = N_);
  assert(ur - abs(mpz_get_si(r)) = 0);
  mpz_clear(q);
  mpz_clear(r);
  mpz_clear(n);
end;

procedure TTestGmpBinding.mpz_tdiv_qr_ui_test;
const
  N_ = -17;
  D_ = 4;
var
  q, r, n: mpz_t;
  ur: valuint;
begin
  mpz_init(q);
  mpz_init(r);
  mpz_init_set_si(n, N_);
  ur :=mpz_tdiv_qr_ui(q, r, n, D_);
  assert(mpz_get_si(q) = trunc(N_ / D_));
  assert(mpz_get_si(q) * D_ + mpz_get_si(r) = N_);
  assert(ur - abs(mpz_get_si(r)) = 0);
  mpz_clear(q);
  mpz_clear(r);
  mpz_clear(n);
end;

procedure TTestGmpBinding.mpz_cdiv_r_test;
const
  N_ = -17;
  D_ = 4;
var r, n, d: mpz_t;
begin
  mpz_init(r);
  mpz_init_set_si(n, N_);
  mpz_init_set_si(d, D_);
  mpz_cdiv_r(r, n, d);
  assert(ceil(N_ / D_) * D_ + mpz_get_si(r) = N_);
  mpz_clear(r);
  mpz_clear(n);
  mpz_clear(d);
end;

procedure TTestGmpBinding.mpz_fdiv_r_test;
const
  N_ = -17;
  D_ = 4;
var r, n, d: mpz_t;
begin
  mpz_init(r);
  mpz_init_set_si(n, N_);
  mpz_init_set_si(d, D_);
  mpz_fdiv_r(r, n, d);
  assert(floor(N_ / D_) * D_ + mpz_get_si(r) = N_);
  mpz_clear(r);
  mpz_clear(n);
  mpz_clear(d);
end;

procedure TTestGmpBinding.mpz_tdiv_r_test;
const
  N_ = -17;
  D_ = 4;
var r, n, d: mpz_t;
begin
  mpz_init(r);
  mpz_init_set_si(n, N_);
  mpz_init_set_si(d, D_);
  mpz_tdiv_r(r, n, d);
  assert(trunc(N_ / D_) * D_ + mpz_get_si(r) = N_);
  mpz_clear(r);
  mpz_clear(n);
  mpz_clear(d);
end;

procedure TTestGmpBinding.mpz_cdiv_r_2exp_test;
const
  N_ = -17;
  B_ = 2;
  D_ = 1 shl B_;
var r, n: mpz_t;
begin
  mpz_init(r);
  mpz_init_set_si(n, N_);
  mpz_cdiv_r_2exp(r, n, B_);
  assert(ceil(N_ / D_) * D_ + mpz_get_si(r) = N_);
  mpz_clear(r);
  mpz_clear(n);
end;

procedure TTestGmpBinding.mpz_fdiv_r_2exp_test;
const
  N_ = -17;
  B_ = 2;
  D_ = 1 shl B_;
var r, n: mpz_t;
begin
  mpz_init(r);
  mpz_init_set_si(n, N_);
  mpz_fdiv_r_2exp(r, n, B_);
  assert(floor(N_ / D_) * D_ + mpz_get_si(r) = N_);
  mpz_clear(r);
  mpz_clear(n);
end;

procedure TTestGmpBinding.mpz_tdiv_r_2exp_test;
const
  N_ = -17;
  B_ = 2;
  D_ = 1 shl B_;
var r, n: mpz_t;
begin
  mpz_init(r);
  mpz_init_set_si(n, N_);
  mpz_tdiv_r_2exp(r, n, B_);
  assert(trunc(N_ / D_) * D_ + mpz_get_si(r) = N_);
  mpz_clear(r);
  mpz_clear(n);
end;

procedure TTestGmpBinding.mpz_cdiv_r_ui_test;
const
  N_ = -17;
  D_ = 4;
var
  r, n: mpz_t;
  ur: valuint;
begin
  mpz_init(r);
  mpz_init_set_si(n, N_);
  ur :=mpz_cdiv_r_ui(r, n, D_);
  assert(ceil(N_ / D_) * D_ + mpz_get_si(r) = N_);
  assert(ur - abs(mpz_get_si(r)) = 0);
  mpz_clear(r);
  mpz_clear(n);
end;

procedure TTestGmpBinding.mpz_fdiv_r_ui_test;
const
  N_ = -17;
  D_ = 4;
var
  r, n: mpz_t;
  ur: valuint;
begin
  mpz_init(r);
  mpz_init_set_si(n, N_);
  ur :=mpz_fdiv_r_ui(r, n, D_);
  assert(floor(N_ / D_) * D_ + mpz_get_si(r) = N_);
  assert(ur - abs(mpz_get_si(r)) = 0);
  mpz_clear(r);
  mpz_clear(n);
end;

procedure TTestGmpBinding.mpz_tdiv_r_ui_test;
const
  N_ = -17;
  D_ = 4;
var
  r, n: mpz_t;
  ur: valuint;
begin
  mpz_init(r);
  mpz_init_set_si(n, N_);
  ur :=mpz_tdiv_r_ui(r, n, D_);
  assert(trunc(N_ / D_) * D_ + mpz_get_si(r) = N_);
  assert(ur - abs(mpz_get_si(r)) = 0);
  mpz_clear(r);
  mpz_clear(n);
end;

procedure TTestGmpBinding.mpz_cdiv_ui_test;
const
  N_ = -17;
  D_ = 4;
var n: mpz_t;
begin
  mpz_init_set_si(n, N_);
  assert(mpz_cdiv_ui(n, D_) - abs(N_ - ceil(N_ / D_) * D_) = 0);
  mpz_clear(n);
end;

procedure TTestGmpBinding.mpz_fdiv_ui_test;
const
  N_ = -17;
  D_ = 4;
var n: mpz_t;
begin
  mpz_init_set_si(n, N_);
  assert(mpz_fdiv_ui(n, D_) - abs(N_ - floor(N_ / D_) * D_) = 0);
  mpz_clear(n);
end;

procedure TTestGmpBinding.mpz_tdiv_ui_test;
const
  N_ = -17;
  D_ = 4;
var n: mpz_t;
begin
  mpz_init_set_si(n, N_);
  assert(mpz_tdiv_ui(n, D_) = abs(N_ - trunc(N_ / D_) * D_));
  mpz_clear(n);
end;

procedure TTestGmpBinding.mpz_mod_test;
const
  N_ = 17;
  D_ = -4;
var r, n, d: mpz_t;
begin
  mpz_init(r);
  mpz_init_set_si(n, N_);
  mpz_init_set_si(d, D_);
  mpz_mod(r, n, d);
  assert(mpz_get_si(r) = N_ mod abs(D_));
  mpz_clear(r);
  mpz_clear(n);
  mpz_clear(d);
end;

procedure TTestGmpBinding.mpz_divexact_test;
const
  N_ = -16;
  D_ = 4;
var q, n, d: mpz_t;
begin
  mpz_init(q);
  mpz_init_set_si(n, N_);
  mpz_init_set_si(d, D_);
  mpz_divexact(q, n, d);
  assert(mpz_get_si(q) * D_ = N_);
  mpz_clear(q);
  mpz_clear(n);
  mpz_clear(d);
end;

procedure TTestGmpBinding.mpz_divexact_ui_test;
const
  N_ = -16;
  D_ = 4;
var q, n: mpz_t;
begin
  mpz_init(q);
  mpz_init_set_si(n, N_);
  mpz_divexact_ui(q, n, D_);
  assert(mpz_get_si(q) * D_ = N_);
  mpz_clear(q);
  mpz_clear(n);
end;

procedure TTestGmpBinding.mpz_divisible_p_test;
const
  N_ = -16;
  D_ = 4;
var n, d: mpz_t;
begin
  mpz_init_set_si(n, N_);
  mpz_init_set_si(d, D_);
  assert(mpz_divisible_p(n, d) <> 0);
  mpz_set_si(n,mpz_get_si(n) + 1);
  assert(mpz_divisible_p(n, d) = 0);
  mpz_clear(n);
  mpz_clear(d);
end;

procedure TTestGmpBinding.mpz_divisible_ui_p_test;
const
  N_ = -16;
  D_ = 4;
var n: mpz_t;
begin
  mpz_init_set_si(n, N_);
  assert(mpz_divisible_ui_p(n, D_) <> 0);
  mpz_set_si(n,mpz_get_si(n) + 1);
  assert(mpz_divisible_ui_p(n, D_) = 0);
  mpz_clear(n);
end;

procedure TTestGmpBinding.mpz_divisible_2exp_p_test;
const
  N_ = -16;
  B_ = 2;
var n: mpz_t;
begin
  mpz_init_set_si(n, N_);
  assert(mpz_divisible_2exp_p(n, B_) <> 0);
  mpz_set_si(n,mpz_get_si(n) + 1);
  assert(mpz_divisible_ui_p(n, B_) = 0);
  mpz_clear(n);
end;

procedure TTestGmpBinding.mpz_congruent_p_test;
const
  N_ = 23;
  C_ = 6;
  D_ = 17;
var n, c, d: mpz_t;
begin
  mpz_init_set_si(n, N_);
  mpz_init_set_si(c, C_);
  mpz_init_set_si(d, D_);
  assert(mpz_congruent_p(n, c, d) <> 0);
  mpz_set_si(n,mpz_get_si(n) + 1);
  assert(mpz_congruent_p(n, c, d) = 0);
  mpz_clear(n);
  mpz_clear(c);
  mpz_clear(d);
end;

procedure TTestGmpBinding.mpz_congruent_2exp_p_test;
const
  N_ = 23;
  C_ = 7;
  B_ = 4;
var n, c: mpz_t;
begin
  mpz_init_set_si(n, N_);
  mpz_init_set_si(c, C_);
  assert(mpz_congruent_2exp_p(n, c, B_) <> 0);
  mpz_set_si(n,mpz_get_si(n) + 1);
  assert(mpz_congruent_2exp_p(n, c, B_) = 0);
  mpz_clear(n);
  mpz_clear(c);
end;

procedure TTestGmpBinding.mpz_congruent_ui_p_test;
const
  N_ = 23;
  C_ = 6;
  D_ = 17;
var n: mpz_t;
begin
  mpz_init_set_si(n, N_);
  assert(mpz_congruent_ui_p(n, C_, D_) <> 0);
  mpz_set_si(n,mpz_get_si(n) + 1);
  assert(mpz_congruent_ui_p(n, C_, D_) = 0);
  mpz_clear(n);
end;

procedure TTestGmpBinding.mpz_powm_test;
const
  N_EXP = 3;
  N_MOD = 16;
  ROP_ = 8;
var rop, base, exp, mod_: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(base, BASE10);
  mpz_init_set_si(exp, N_EXP);
  mpz_init_set_si(mod_, N_MOD);
  mpz_powm(rop, base, exp, mod_);
  assert(mpz_get_si(rop) = ROP_);
  mpz_clear(rop);
  mpz_clear(base);
  mpz_clear(exp);
  mpz_clear(mod_);
end;

procedure TTestGmpBinding.mpz_powm_ui_test;
const
  N_EXP = 3;
  N_MOD = 16;
  ROP_ = 8;
var rop, base, mod_: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(base, BASE10);
  mpz_init_set_si(mod_, N_MOD);
  mpz_powm_ui(rop, base, N_EXP, mod_);
  assert(mpz_get_si(rop) = ROP_);
  mpz_clear(rop);
  mpz_clear(base);
  mpz_clear(mod_);
end;

procedure TTestGmpBinding.mpz_pow_ui_test;
const
  BASE_ = 10;
  EXP = 3;
var rop, base: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(base, BASE_);
  mpz_pow_ui(rop, base, EXP);
  assert(mpz_get_si(rop) = BASE_ ** EXP);
  mpz_clear(rop);
  mpz_clear(base);
end;

procedure TTestGmpBinding.mpz_ui_pow_ui_test;
const
  BASE_ = 10;
  EXP = 3;
var rop: mpz_t;
begin
  mpz_init(rop);
  mpz_ui_pow_ui(rop, BASE_, EXP);
  assert(mpz_get_si(rop) = BASE_ ** EXP);
  mpz_clear(rop);
end;

procedure TTestGmpBinding.mpz_root_test;
const
  ROOT = 5;
  N_ = 3;
var rop, op: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op, ROOT ** N_);
  assert(mpz_root(rop, op, N_) <> 0);
  assert(mpz_get_si(rop) = ROOT);
  mpz_set_si(op, ROOT ** N_ + 1);
  assert(mpz_root(rop, op, N_) = 0);
  assert(mpz_get_si(rop) = ROOT);
  mpz_clear(rop);
  mpz_clear(op);
end;

procedure TTestGmpBinding.mpz_rootrem_test;
const
  ROOT_ = 5;
  N_ = 3;
  REM_ = 1;
var root, rem, u: mpz_t;
begin
  assert(REM_ < ROOT_);
  mpz_init(root);
  mpz_init(rem);
  mpz_init_set_si(u, ROOT_ ** N_);
  mpz_rootrem(root, rem, u, N_);
  assert(mpz_get_si(root) = ROOT_);
  assert(mpz_get_si(rem) = 0);
  mpz_set_si(u, ROOT_ ** N_ + REM_);
  mpz_rootrem(root, rem, u, N_);
  assert(mpz_get_si(root) = ROOT_);
  assert(mpz_get_si(rem) = REM_);
  mpz_clear(root);
  mpz_clear(rem);
  mpz_clear(u);
end;

procedure TTestGmpBinding.mpz_sqrt_test;
const ROOT = 5;
var rop, op: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op, ROOT ** 2);
  mpz_sqrt(rop, op);
  assert(mpz_get_si(rop) = ROOT);
  mpz_clear(rop);
  mpz_clear(op);
end;

procedure TTestGmpBinding.mpz_sqrtrem_test;
const
  ROOT = 5;
  REM = 1;
var rop1, rop2, op: mpz_t;
begin
  mpz_init(rop1);
  mpz_init(rop2);
  mpz_init_set_si(op, ROOT ** 2 + REM);
  mpz_sqrtrem(rop1, rop2, op);
  assert(mpz_get_si(rop1) = ROOT);
  assert(mpz_get_si(rop2) = REM);
  mpz_clear(rop1);
  mpz_clear(rop2);
  mpz_clear(op);
end;

procedure TTestGmpBinding.mpz_perfect_power_p_test;
const
  ROOT = 3;
  EXP = 3;
var op: mpz_t;
begin
  mpz_init_set_si(op, ROOT ** EXP);
  assert(mpz_perfect_power_p(op) <> 0);
  mpz_set_si(op, ROOT ** EXP + 1);
  assert(mpz_perfect_power_p(op) = 0);
  mpz_clear(op);
end;

procedure TTestGmpBinding.mpz_perfect_square_p_test;
const ROOT = 3;
var op: mpz_t;
begin
  mpz_init_set_si(op, ROOT ** 2);
  assert(mpz_perfect_square_p(op) <> 0);
  mpz_set_si(op, ROOT ** 2 or 2);
  assert(mpz_perfect_square_p(op) = 0);
  mpz_clear(op);
end;

procedure TTestGmpBinding.mpz_probab_prime_p_test;
const
  N_ = 127;
  REPS = 5;
var n: mpz_t;
begin
  mpz_init_set_si(n, N_);
  assert(mpz_probab_prime_p(n, REPS) = 2);
  mpz_set_si(n, N_ + 1);
  assert(mpz_probab_prime_p(n, REPS) = 0);
  mpz_clear(n);
end;

procedure TTestGmpBinding.mpz_nextprime_test;
const
  PRIME3 = 5;
  PRIME4 = 7;
var rop, op: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op, PRIME3);
  mpz_nextprime(rop, op);
  assert(mpz_get_si(rop) = PRIME4);
  mpz_clear(rop);
  mpz_clear(op);
end;

procedure TTestGmpBinding.mpz_gcd_test;
const
  OP_1 = 42;
  OP_2 = 56;
  ROP_ = 14;
var rop, op1, op2: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op1, OP_1);
  mpz_init_set_si(op2, OP_2);
  mpz_gcd(rop, op1, op2);
  assert(mpz_get_si(rop) = ROP_);
  mpz_clear(rop);
  mpz_clear(op1);
  mpz_clear(op2);
end;

procedure TTestGmpBinding.mpz_gcd_ui_test;
const
  OP_1 = 42;
  OP_2 = 56;
  ROP_ = 14;
var rop, op1: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op1, OP_1);
  assert(mpz_gcd_ui(rop, op1, OP_2) = ROP_);
  assert(mpz_get_si(rop) = ROP_);
  mpz_clear(rop);
  mpz_clear(op1);
end;

procedure TTestGmpBinding.mpz_gcdext_test;
const
  A_ = 42;
  B_ = 56;
  G_ = 14;
var g, s, t, a, b: mpz_t;
begin
  mpz_init(g);
  mpz_init(s);
  mpz_init(t);
  mpz_init_set_si(a, A_);
  mpz_init_set_si(b, B_);
  mpz_gcdext(g, s, t, a, b);
  assert(mpz_get_si(g) = G_);
  assert(A_ *mpz_get_si(s) + B_ *mpz_get_si(t) = G_);
  mpz_clear(g);
  mpz_clear(s);
  mpz_clear(t);
  mpz_clear(a);
  mpz_clear(b);
end;

procedure TTestGmpBinding.mpz_lcm_test;
const
  OP_1 = 21;
  OP_2 = 6;
  ROP_ = 42;
var rop, op1, op2: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op1, OP_1);
  mpz_init_set_si(op2, OP_2);
  mpz_lcm(rop, op1, op2);
  assert(mpz_get_si(rop) = ROP_);
  mpz_clear(rop);
  mpz_clear(op1);
  mpz_clear(op2);
end;

procedure TTestGmpBinding.mpz_lcm_ui_test;
const
  OP_1 = 21;
  OP_2 = 6;
  ROP_ = 42;
var rop, op1: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op1, OP_1);
  mpz_lcm_ui(rop, op1, OP_2);
  assert(mpz_get_si(rop) = ROP_);
  mpz_clear(rop);
  mpz_clear(op1);
end;

procedure TTestGmpBinding.mpz_invert_test;
const
  OP_1 = 256;
  OP_2 = 337;
  ROP_ = 104;
var rop, op1, op2: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op1, OP_1);
  mpz_init_set_si(op2, OP_2);
  assert(mpz_invert(rop, op1, op2) <> 0);
  assert(mpz_get_si(rop) = ROP_);
  mpz_clear(rop);
  mpz_clear(op1);
  mpz_clear(op2);
end;

procedure TTestGmpBinding.mpz_jacobi_test;
const
  B0 = 11;
  A0 = 13 * B0;
  AM1 = 1001;
  BM1 = 9907;
  A1 = 4;
  B1 = 7;
var a, b: mpz_t;
begin
  mpz_init_set_si(a, A0);
  mpz_init_set_si(b, B0);
  assert(mpz_jacobi(a, b) = 0);
  mpz_set_si(a, AM1);
  mpz_set_si(b, BM1);
  assert(mpz_jacobi(a, b) = -1);
  mpz_set_si(a, A1);
  mpz_set_si(b, B1);
  assert(mpz_jacobi(a, b) = 1);
  mpz_clear(a);
  mpz_clear(b);
end;

procedure TTestGmpBinding.mpz_kronecker_si_test;
const
  A_ = 8;
  B0 = 6;
  BM1 = 5;
  B1 = 7;
var a: mpz_t;
begin
  mpz_init_set_si(a, A_);
  assert(mpz_kronecker_si(a, B1) = 1);
  assert(mpz_kronecker_si(a, B0) = 0);
  assert(mpz_kronecker_si(a, BM1) = -1);
  mpz_clear(a);
end;

procedure TTestGmpBinding.mpz_kronecker_ui_test;
const
  A_ = 8;
  B0 = 6;
  BM1 = 5;
  B1 = 7;
var a: mpz_t;
begin
  mpz_init_set_si(a, A_);
  assert(mpz_kronecker_ui(a, B1) = 1);
  assert(mpz_kronecker_ui(a, B0) = 0);
  assert(mpz_kronecker_ui(a, BM1) = -1);
  mpz_clear(a);
end;

procedure TTestGmpBinding.mpz_si_kronecker_test;
const
  A_ = 8;
  B0 = 6;
  BM1 = 5;
  B1 = 7;
var b: mpz_t;
begin
  mpz_init_set_si(b, B1);
  assert(mpz_si_kronecker(A_, b) = 1);
  mpz_set_si(b, B0);
  assert(mpz_si_kronecker(A_, b) = 0);
  mpz_set_si(b, BM1);
  assert(mpz_si_kronecker(A_, b) = -1);
  mpz_clear(b);
end;

procedure TTestGmpBinding.mpz_ui_kronecker_test;
const
  A_ = 8;
  B0 = 6;
  BM1 = 5;
  B1 = 7;
var b: mpz_t;
begin
  mpz_init_set_si(b, B1);
  assert(mpz_ui_kronecker(A_, b) = 1);
  mpz_set_si(b, B0);
  assert(mpz_ui_kronecker(A_, b) = 0);
  mpz_set_si(b, BM1);
  assert(mpz_ui_kronecker(A_, b) = -1);
  mpz_clear(b);
end;

procedure TTestGmpBinding.mpz_remove_test;
const
  F_ = 5;
  OP_ = 3 * F_ * 7;
  ROP_ = OP_ div F_;
var rop, op, f: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op, OP_);
  mpz_init_set_si(f, F_);
  assert(mpz_remove(rop, op, f) = 1);
  assert(mpz_get_si(rop) = ROP_);
  mpz_clear(rop);
  mpz_clear(op);
  mpz_clear(f);
end;

procedure TTestGmpBinding.mpz_fac_ui_test;
const
  OP = 4;
  ROP_ = 2 * 3 * 4;
var rop: mpz_t;
begin
  mpz_init(rop);
  mpz_fac_ui(rop, OP);
  assert(mpz_get_si(rop) = ROP_);
  mpz_clear(rop);
end;

procedure TTestGmpBinding.mpz_bin_ui_test;
const
  N_ = 7;
  K = 3;
  ROP_ = 35;
var rop, n: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(n, N_);
  mpz_bin_ui(rop, n, K);
  assert(mpz_get_si(rop) = ROP_);
  mpz_clear(rop);
  mpz_clear(n);
end;

procedure TTestGmpBinding.mpz_bin_uiui_test;
const
  N = 7;
  K = 3;
  ROP_ = 35;
var rop: mpz_t;
begin
  mpz_init(rop);
  mpz_bin_uiui(rop, N, K);
  assert(mpz_get_si(rop) = ROP_);
  mpz_clear(rop);
end;

procedure TTestGmpBinding.mpz_fib_ui_test;
const
  N = 10;
  FN_ = 55;
var fn: mpz_t;
begin
  mpz_init(fn);
  mpz_fib_ui(fn, N);
  assert(mpz_get_si(fn) = FN_);
  mpz_clear(fn);
end;

procedure TTestGmpBinding.mpz_fib2_ui_test;
const
  N = 10;
  FN_ = 55;
  FNSUB1_ = 34;
var fn, fnsub1: mpz_t;
begin
  mpz_init(fn);
  mpz_init(fnsub1);
  mpz_fib2_ui(fn, fnsub1, N);
  assert(mpz_get_si(fn) = FN_);
  assert(mpz_get_si(fnsub1) = FNSUB1_);
  mpz_clear(fn);
  mpz_clear(fnsub1);
end;

procedure TTestGmpBinding.mpz_lucnum_ui_test;
const
  N = 10;
  LN_ = 123;
var ln: mpz_t;
begin
  mpz_init(ln);
  mpz_lucnum_ui(ln, N);
  assert(mpz_get_si(ln) = LN_);
  mpz_clear(ln);
end;

procedure TTestGmpBinding.mpz_lucnum2_ui_test;
const
  N = 10;
  LN_ = 123;
  LNSUB1_ = 76;
var ln, lnsub1: mpz_t;
begin
  mpz_init(ln);
  mpz_init(lnsub1);
  mpz_lucnum2_ui(ln, lnsub1, N);
  assert(mpz_get_si(ln) = LN_);
  assert(mpz_get_si(lnsub1) = LNSUB1_);
  mpz_clear(ln);
  mpz_clear(lnsub1);
end;

procedure TTestGmpBinding.mpz_cmp_test;
const
  OP_1 = -10;
  OP_2 = OP_1 - 1;
var op1, op2: mpz_t;
begin
  mpz_init_set_si(op1, OP_1);
  mpz_init_set_si(op2, OP_2);
  assert(mpz_cmp(op1, op2) > 0);
  mpz_set_si(op2, OP_1);
  assert(mpz_cmp(op1, op2) = 0);
  mpz_set_si(op2, OP_1 + 1);
  assert(mpz_cmp(op1, op2) < 0);
  mpz_clear(op1);
  mpz_clear(op2);
end;

procedure TTestGmpBinding.mpz_cmp_d_test;
const
  OP_1 = -10;
var op1: mpz_t;
begin
  mpz_init_set_si(op1, OP_1);
  assert(mpz_cmp_d(op1, OP_1 - 1) > 0);
  assert(mpz_cmp_d(op1, OP_1) = 0);
  assert(mpz_cmp_d(op1, OP_1 + 1) < 0);
  mpz_clear(op1);
end;

procedure TTestGmpBinding.mpz_cmp_si_test;
const
  OP_1 = -10;
var op1: mpz_t;
begin
  mpz_init_set_si(op1, OP_1);
  assert(mpz_cmp_si(op1, OP_1 - 1) > 0);
  assert(mpz_cmp_si(op1, OP_1) = 0);
  assert(mpz_cmp_si(op1, OP_1 + 1) < 0);
  mpz_clear(op1);
end;

procedure TTestGmpBinding.mpz_cmp_ui_test;
const
  OP_1 = 10;
var op1: mpz_t;
begin
  mpz_init_set_si(op1, OP_1);
  assert(mpz_cmp_ui(op1, OP_1 - 1) > 0);
  assert(mpz_cmp_ui(op1, OP_1) = 0);
  assert(mpz_cmp_ui(op1, OP_1 + 1) < 0);
  mpz_clear(op1);
end;

procedure TTestGmpBinding.mpz_cmpabs_test;
const
  OP_1 = -10;
  OP_2 = -OP_1 - 1;
var op1, op2: mpz_t;
begin
  mpz_init_set_si(op1, OP_1);
  mpz_init_set_si(op2, -OP_2);
  assert(mpz_cmpabs(op1, op2) > 0);
  mpz_set_si(op2, -OP_1);
  assert(mpz_cmpabs(op1, op2) = 0);
  mpz_set_si(op2, -OP_1 + 1);
  assert(mpz_cmpabs(op1, op2) < 0);
  mpz_clear(op1);
  mpz_clear(op2);
end;

procedure TTestGmpBinding.mpz_cmpabs_d_test;
const OP_1 = -10;
var op1: mpz_t;
begin
  mpz_init_set_si(op1, OP_1);
  assert(mpz_cmpabs_d(op1, -OP_1 - 1) > 0);
  assert(mpz_cmpabs_d(op1, -OP_1) = 0);
  assert(mpz_cmpabs_d(op1, -OP_1 + 1) < 0);
  mpz_clear(op1);
end;

procedure TTestGmpBinding.mpz_cmpabs_ui_test;
const OP_1 = -10;
var op1: mpz_t;
begin
  mpz_init_set_si(op1, OP_1);
  assert(mpz_cmpabs_ui(op1, -OP_1 - 1) > 0);
  assert(mpz_cmpabs_ui(op1, -OP_1) = 0);
  assert(mpz_cmpabs_ui(op1, -OP_1 + 1) < 0);
  mpz_clear(op1);
end;

procedure TTestGmpBinding.mpz_and_test;
const
  OP_1 = $c;
  OP_2 = $a;
var rop, op1, op2: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op1, OP_1);
  mpz_init_set_si(op2, OP_2);
  mpz_and(rop, op1, op2);
  assert(mpz_get_si(rop) = OP_1 and OP_2);
  mpz_clear(rop);
  mpz_clear(op1);
  mpz_clear(op2);
end;

procedure TTestGmpBinding.mpz_ior_test;
const
  OP_1 = $c;
  OP_2 = $a;
var rop, op1, op2: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op1, OP_1);
  mpz_init_set_si(op2, OP_2);
  mpz_ior(rop, op1, op2);
  assert(mpz_get_si(rop) = OP_1 or OP_2);
  mpz_clear(rop);
  mpz_clear(op1);
  mpz_clear(op2);
end;

procedure TTestGmpBinding.mpz_xor_test;
const
  OP_1 = $c;
  OP_2 = $a;
var rop, op1, op2: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op1, OP_1);
  mpz_init_set_si(op2, OP_2);
  mpz_xor(rop, op1, op2);
  assert(mpz_get_si(rop) = OP_1 xor OP_2);
  mpz_clear(rop);
  mpz_clear(op1);
  mpz_clear(op2);
end;

procedure TTestGmpBinding.mpz_com_test;
const
  OP_1 = 2;
var rop, op1: mpz_t;
begin
  mpz_init(rop);
  mpz_init_set_si(op1, OP_1);
  mpz_com(rop, op1);
  assert(mpz_get_si(rop) = not OP_1 );
  mpz_clear(rop);
  mpz_clear(op1);
end;

procedure TTestGmpBinding.mpz_popcount_test;
const
  OP_ = $55AA;
  N = 8;
var op: mpz_t;
begin
  mpz_init_set_si(op, OP_);
  assert(mpz_popcount(op) = N);
  mpz_clear(op);
end;

procedure TTestGmpBinding.mpz_hamdist_test;
const
  OP_1 = $55;
  OP_2 = $7A;
  N = 5;
var op1, op2: mpz_t;
begin
  mpz_init_set_si(op1, OP_1);
  mpz_init_set_si(op2, OP_2);
  assert(mpz_hamdist(op1, op2) = N);
  mpz_clear(op1);
  mpz_clear(op2);
end;

procedure TTestGmpBinding.mpz_scan0_test;
const
  OP_ = $F75;
  STARTING_BIT = 4;
  N = 7;
var op: mpz_t;
begin
  mpz_init_set_si(op, OP_);
  assert(mpz_scan0(op, STARTING_BIT) = N);
  mpz_clear(op);
end;

procedure TTestGmpBinding.mpz_scan1_test;
const
  OP_ = $85;
  STARTING_BIT = 4;
  N = 7;
var op: mpz_t;
begin
  mpz_init_set_si(op, OP_);
  assert(mpz_scan1(op, STARTING_BIT) = N);
  mpz_clear(op);
end;

procedure TTestGmpBinding.mpz_setbit_test;
const
  ROP_ = $F75;
  BIT_INDEX = 7;
var rop: mpz_t;
begin
  mpz_init_set_ui(rop, ROP_);
  mpz_setbit(rop, BIT_INDEX);
  assert(mpz_get_ui(rop) = ROP_ or (1 shl BIT_INDEX));
  mpz_clear(rop);
end;

procedure TTestGmpBinding.mpz_clrbit_test;
const
  ROP_ = $F75;
  BIT_INDEX = 9;
var rop: mpz_t;
begin
  mpz_init_set_ui(rop, ROP_);
  mpz_clrbit(rop, BIT_INDEX);
  assert(mpz_get_ui(rop) = ROP_ and not (1 shl BIT_INDEX));
  mpz_clear(rop);
end;

procedure TTestGmpBinding.mpz_combit_test;
const
  ROP_ = $F75;
  BIT_INDEX = 7;
var rop: mpz_t;
begin
  mpz_init_set_ui(rop, ROP_);
  mpz_combit(rop, BIT_INDEX);
  assert(mpz_get_ui(rop) = ROP_ xor (1 shl BIT_INDEX));
  mpz_clear(rop);
end;

procedure TTestGmpBinding.mpz_tstbit_test;
const
  ROP_ = $F75;
  BIT_INDEX1 = 7;
  BIT_INDEX2 = 8;
var rop: mpz_t;
begin
  mpz_init_set_ui(rop, ROP_);
  assert(longbool(mpz_tstbit(rop, BIT_INDEX1)) = longbool(ROP_ and (1 shl BIT_INDEX1)));
  assert(longbool(mpz_tstbit(rop, BIT_INDEX2)) = longbool(ROP_ and (1 shl BIT_INDEX2)));
  mpz_clear(rop);
end;

procedure TTestGmpBinding.mpz_urandomb_test;
const
  B = 8 * (sizeof(valuint) - 1);
  N = 1 shl B;
var
  rop: mpz_t;
  state: randstate_t;
begin
  mp_randinit_default(state);
  mpz_init(rop);
  mpz_urandomb(rop, state, B);
  assert(mpz_cmp_ui(rop, N) < 0);
  mpz_clear(rop);
  mp_randclear(state);
end;

procedure TTestGmpBinding.mpz_urandomm_test;
const
  N_ = 1000000;
var
  rop, n: mpz_t;
  state: randstate_t;
begin
  mp_randinit_default(state);
  mpz_init(rop);
  mpz_init_set_ui(n, N_);
  mpz_urandomm(rop, state, n);
  assert(mpz_cmp_ui(rop, N_) < 0);
  mpz_clear(rop);
  mpz_clear(n);
  mp_randclear(state);
end;

procedure TTestGmpBinding.mpz_rrandomb_test;
const
  B = 8 * sizeof(valuint) - 2;
  N = 1 shl B;
var
  rop: mpz_t;
  state: randstate_t;
begin
  mp_randinit_default(state);
  mpz_init(rop);
  mpz_rrandomb(rop, state, B);
  assert(mpz_cmp_ui(rop, N) < 0);
  mpz_clear(rop);
  mp_randclear(state);
end;

procedure TTestGmpBinding.mpz_fits_ushort_p_test;
var op: mpz_t;
begin
  mpz_init_set_ui(op, high(word));
  assert(longbool(mpz_fits_ushort_p(op)));
  mpz_add_ui(op, op, 1);
  assert(not longbool(mpz_fits_ushort_p(op)));
  mpz_clear(op);
end;

procedure TTestGmpBinding.mpz_fits_sshort_p_test;
var op: mpz_t;
begin
  mpz_init_set_si(op, high(smallint));
  assert(longbool(mpz_fits_sshort_p(op)));
  mpz_add_ui(op, op, 1);
  assert(not longbool(mpz_fits_sshort_p(op)));
  mpz_clear(op);
end;

procedure TTestGmpBinding.mpz_fits_uint_p_test;
var op: mpz_t;
begin
  mpz_init_set_ui(op, high(cardinal));
  assert(longbool(mpz_fits_uint_p(op)));
  mpz_add_ui(op, op, 1);
  assert(not longbool(mpz_fits_uint_p(op)));
  mpz_clear(op);
end;

procedure TTestGmpBinding.mpz_fits_sint_p_test;
var op: mpz_t;
begin
  mpz_init_set_si(op, high(integer));
  assert(longbool(mpz_fits_sint_p(op)));
  mpz_add_ui(op, op, 1);
  assert(not longbool(mpz_fits_sint_p(op)));
  mpz_clear(op);
end;

procedure TTestGmpBinding.mpz_fits_ulong_p_test;
var op: mpz_t;
begin
  mpz_init_set_ui(op, high(valuint));
  assert(longbool(mpz_fits_ulong_p(op)));
  mpz_add_ui(op, op, 1);
  assert(not longbool(mpz_fits_ulong_p(op)));
  mpz_clear(op);
end;

procedure TTestGmpBinding.mpz_fits_slong_p_test;
var op: mpz_t;
begin
  mpz_init_set_si(op, high(valsint));
  assert(longbool(mpz_fits_slong_p(op)));
  mpz_add_ui(op, op, 1);
  assert(not longbool(mpz_fits_slong_p(op)));
  mpz_clear(op);
end;

procedure TTestGmpBinding.mpz_sizeinbase_test;
const
  OP_ = 1000000;
var rop: mpz_t;
begin
  mpz_init_set_ui(rop, OP_);
  assert(mpz_sizeinbase(rop, BASE10) - length(inttostr(OP_)) = 0);
  mpz_clear(rop);
end;

procedure TTestGmpBinding.mpz_size_test;
var rop: mpz_t;
begin
  mpz_init(rop);
  assert(mpz_size(rop) = 0);
  mpz_add_ui(rop, rop, 1);
  assert(mpz_size(rop) = 1);
  mpz_clear(rop);
end;

procedure TTestGmpBinding.mpq_init_test;
var rational: mpq_t;
begin
  mpq_init(rational);
  assert(mpq_cmp_ui(rational, 0, 1) = 0);
  mpq_clear(rational);
end;

procedure TTestGmpBinding.mpq_clear_test;
var rational: mpq_t;
begin
  mpq_init(rational);
  mpq_clear(rational);
end;

procedure TTestGmpBinding.mpq_set_ui_test;
const
  OP_1 = 13;
  OP_2 = 17;
var rational: mpq_t;
begin
  mpq_init(rational);
  mpq_set_ui(rational, OP_1, OP_2);
  assert(mpq_cmp_ui(rational, OP_1, OP_2) = 0);
  mpq_clear(rational);
end;

procedure TTestGmpBinding.mpq_set_si_test;
const
  OP_1 = -13;
  OP_2 = 17;
var rational: mpq_t;
begin
  mpq_init(rational);
  mpq_set_si(rational, OP_1, OP_2);
  assert(mpq_cmp_si(rational, OP_1, OP_2) = 0);
  mpq_clear(rational);
end;

procedure TTestGmpBinding.mpq_canonicalize_test;
const
  OP_1 = -13;
  OP_2 = 17;
  CF = 11;
var rational: mpq_t;
begin
  mpq_init(rational);
  mpq_set_si(rational, CF * OP_1, CF * OP_2);
  mpq_canonicalize(rational);
  assert(mpq_cmp_si(rational, OP_1, OP_2) = 0);
  mpq_clear(rational);
end;

procedure TTestGmpBinding.mpq_set_test;
const
  OP_1 = -13;
  OP_2 = 17;
var rational1, rational2: mpq_t;
begin
  mpq_init(rational1);
  mpq_init(rational2);
  mpq_set_si(rational2, OP_1, OP_2);
  mpq_set(rational1, rational2);
  assert(mpq_cmp_si(rational1, OP_1, OP_2) = 0);
  mpq_clear(rational1);
  mpq_clear(rational2);
end;

procedure TTestGmpBinding.mpq_setmpz_test;
const
  OP_1 = -13;
var
  rational: mpq_t;
  op: mpz_t;
begin
  mpq_init(rational);
  mpz_init_set_si(op, OP_1);
  mpq_set_z(rational, op);
  assert(mpq_cmp_si(rational, OP_1, 1) = 0);
  mpq_clear(rational);
  mpz_clear(op);
end;

procedure TTestGmpBinding.mpq_set_str_test;
const
  OP_1 = -13;
  OP_2 = 17;
var rational: mpq_t;
begin
  mpq_init(rational);
  assert(mpq_set_str(rational, pchar(inttostr(OP_1) + '/' + inttostr(OP_2)), BASE10) = 0);
  assert(mpq_cmp_si(rational, OP_1, OP_2) = 0);
  assert(mpq_set_str(rational, pchar(inttostr(OP_1) + '*' + inttostr(OP_2)), BASE10) <> 0);
  mpq_clear(rational);
end;

procedure TTestGmpBinding.mpq_swap_test;
const
  ROP_1N = -13;
  ROP_1D = 17;
  ROP_2N = -23;
  ROP_2D = 31;
var rop1, rop2: mpq_t;
begin
  mpq_init(rop1);
  mpq_init(rop2);
  mpq_set_si(rop1, ROP_1N, ROP_1D);
  mpq_set_si(rop2, ROP_2N, ROP_2D);
  mpq_swap(rop1, rop2);
  assert(mpq_cmp_si(rop1, ROP_2N, ROP_2D) = 0);
  assert(mpq_cmp_si(rop2, ROP_1N, ROP_1D) = 0);
  mpq_clear(rop1);
  mpq_clear(rop2);
end;

procedure TTestGmpBinding.mpq_get_d_test;
const
  OP_1 = -1;
  OP_2 = 2;
var op: mpq_t;
begin
  mpq_init(op);
  mpq_set_si(op, OP_1, OP_2);
  assert(mpq_get_d(op) = OP_1 / OP_2);
  mpq_clear(op);
end;

procedure TTestGmpBinding.mpq_set_d_test;
const
  OP_1 = -1;
  OP_2 = 2;
var rop: mpq_t;
begin
  mpq_init(rop);
  mpq_set_d(rop, OP_1 / OP_2);
  assert(mpq_cmp_si(rop, OP_1, OP_2) = 0);
  mpq_clear(rop);
end;

procedure TTestGmpBinding.mpq_set_f_test;
const OP_ = -10;
var
  op: mpf_t;
  rop: mpq_t;
begin
  mpf_init_set_si(op, OP_);
  mpq_init(rop);
  mpq_set_f(rop, op);
  assert(mpq_get_d(rop) = OP_);
  mpq_clear(rop);
  mpf_clear(op);
end;

procedure TTestGmpBinding.mpq_get_str_test;
const
  OP_1 = -1;
  OP_2 = 2;
var
  op: mpq_t;
  p: pchar;
begin
  mpq_init(op);
  mpq_set_si(op, OP_1, OP_2);
  p := mpq_get_str(nil, BASE10, op);
  assert(p = inttostr(OP_1) + '/' + inttostr(OP_2));
  freemem(p);
  mpq_clear(op);
end;

procedure TTestGmpBinding.mpq_add_test;
const
  N1 = 5;
  D1 = 2;
  N2 = -7;
  D2 = 3;
var sum, addend1, addend2: mpq_t;
begin
  mpq_init(sum);
  mpq_init(addend1);
  mpq_init(addend2);
  mpq_set_si(addend1, N1, D1);
  mpq_set_si(addend2, N2, D2);
  mpq_add(sum, addend1, addend2);
  assert(mpq_cmp_si(sum, N1 * D2 + N2 * D1, D1 * D2) = 0);
  mpq_clear(sum);
  mpq_clear(addend1);
  mpq_clear(addend2);
end;

procedure TTestGmpBinding.mpq_sub_test;
const
  N1 = 5;
  D1 = 2;
  N2 = -7;
  D2 = 3;
var difference, minuend, subtrahend: mpq_t;
begin
  mpq_init(difference);
  mpq_init(minuend);
  mpq_init(subtrahend);
  mpq_set_si(minuend, N1, D1);
  mpq_set_si(subtrahend, N2, D2);
  mpq_sub(difference, minuend, subtrahend);
  assert(mpq_cmp_si(difference, N1 * D2 - N2 * D1, D1 * D2) = 0);
  mpq_clear(difference);
  mpq_clear(minuend);
  mpq_clear(subtrahend);
end;

procedure TTestGmpBinding.mpq_mul_test;
const
  N1 = 5;
  D1 = 2;
  N2 = -7;
  D2 = 3;
var product, multiplier, multiplicant: mpq_t;
begin
  mpq_init(product);
  mpq_init(multiplier);
  mpq_init(multiplicant);
  mpq_set_si(multiplier, N1, D1);
  mpq_set_si(multiplicant, N2, D2);
  mpq_mul(product, multiplier, multiplicant);
  assert(mpq_cmp_si(product, N1 * N2, D1 * D2) = 0);
  mpq_clear(product);
  mpq_clear(multiplier);
  mpq_clear(multiplicant);
end;

procedure TTestGmpBinding.mpq_mul_2exp_test;
const
  N1 = 5;
  D1 = 3;
  OP_2 = 2;
var rop, op1: mpq_t;
begin
  mpq_init(rop);
  mpq_init(op1);
  mpq_set_si(op1, N1, D1);
  mpq_mul_2exp(rop, op1, OP_2);
  assert(mpq_cmp_si(rop, N1 * (1 shl OP_2), D1) = 0);
  mpq_clear(rop);
  mpq_clear(op1);
end;

procedure TTestGmpBinding.mpq_div_test;
const
  N1 = 5;
  D1 = 2;
  N2 = -7;
  D2 = 3;
var quotient, dividend, divisor: mpq_t;
begin
  mpq_init(quotient);
  mpq_init(dividend);
  mpq_init(divisor);
  mpq_set_si(dividend, N1, D1);
  mpq_set_si(divisor, N2, D2);
  mpq_div(quotient, dividend, divisor);
  assert(mpq_cmp_si(quotient, -N1 * D2, -D1 * N2) = 0);
  mpq_clear(quotient);
  mpq_clear(dividend);
  mpq_clear(divisor);
end;

procedure TTestGmpBinding.mpq_div_2exp_test;
const
  N1 = 5;
  D1 = 3;
  OP_2 = 2;
var rop, op1: mpq_t;
begin
  mpq_init(rop);
  mpq_init(op1);
  mpq_set_si(op1, N1, D1);
  mpq_div_2exp(rop, op1, OP_2);
  assert(mpq_cmp_si(rop, N1, D1 * (1 shl OP_2)) = 0);
  mpq_clear(rop);
  mpq_clear(op1);
end;

procedure TTestGmpBinding.mpq_neg_test;
const
  N1 = -5;
  D1 = 3;
var negated_operand, operand: mpq_t;
begin
  mpq_init(negated_operand);
  mpq_init(operand);
  mpq_set_si(operand, N1, D1);
  mpq_neg(negated_operand, operand);
  assert(mpq_cmp_si(negated_operand, -N1, D1) = 0);
  mpq_clear(negated_operand);
  mpq_clear(operand);
end;

procedure TTestGmpBinding.mpq_abs_test;
const
  N1 = 5;
  D1 = 3;
var rop, op: mpq_t;
begin
  mpq_init(rop);
  mpq_init(op);
  mpq_set_si(op, -N1, D1);
  mpq_abs(rop, op);
  assert(mpq_cmp_si(rop, N1, D1) = 0);
  mpq_clear(rop);
  mpq_clear(op);
end;

procedure TTestGmpBinding.mpq_inv_test;
const
  N1 = -5;
  D1 = 3;
var inverted_number, number: mpq_t;
begin
  mpq_init(inverted_number);
  mpq_init(number);
  mpq_set_si(number, N1, D1);
  mpq_inv(inverted_number, number);
  assert(mpq_cmp_si(inverted_number, -D1, -N1) = 0);
  mpq_clear(inverted_number);
  mpq_clear(number);
end;

procedure TTestGmpBinding.mpq_cmp_test;
const
  N1 = -7;
  D1 = 5;
var op1, op2: mpq_t;
begin
  mpq_init(op1);
  mpq_init(op2);
  mpq_set_si(op1, N1, D1);
  mpq_set_si(op2, N1, D1);
  assert(mpq_cmp(op1, op2) = 0);
  mpq_set_si(op1, N1 + 1, D1);
  assert(mpq_cmp(op1, op2) > 0);
  mpq_set_si(op1, N1 - 1, D1);
  assert(mpq_cmp(op1, op2) < 0);
  mpq_clear(op1);
  mpq_clear(op2);
end;

procedure TTestGmpBinding.mpq_cmp_si_test;
const
  N1 = -7;
  D1 = 5;
var op1: mpq_t;
begin
  mpq_init(op1);
  mpq_set_si(op1, N1, D1);
  assert(mpq_cmp_si(op1, N1, D1) = 0);
  assert(mpq_cmp_si(op1, N1 - 1, D1) > 0);
  assert(mpq_cmp_si(op1, N1 + 1, D1) < 0);
  mpq_clear(op1);
end;

procedure TTestGmpBinding.mpq_cmp_ui_test;
const
  N1 = 7;
  D1 = 5;
var op1: mpq_t;
begin
  mpq_init(op1);
  mpq_set_si(op1, N1, D1);
  assert(mpq_cmp_ui(op1, N1, D1) = 0);
  assert(mpq_cmp_ui(op1, N1 - 1, D1) > 0);
  assert(mpq_cmp_ui(op1, N1 + 1, D1) < 0);
  mpq_clear(op1);
end;

procedure TTestGmpBinding.mpq_equal_test;
const
  N1 = -7;
  D1 = 5;
var op1, op2: mpq_t;
begin
  mpq_init(op1);
  mpq_init(op2);
  mpq_set_si(op1, N1, D1);
  mpq_set_si(op2, N1, D1);
  assert(longbool(mpq_equal(op1, op2)));
  mpq_set_si(op1, N1 + 1, D1);
  assert(not longbool(mpq_equal(op1, op2)));
  mpq_set_si(op1, N1 - 1, D1);
  assert(not longbool(mpq_equal(op1, op2)));
  mpq_clear(op1);
  mpq_clear(op2);
end;

procedure TTestGmpBinding.mpq_get_num_test;
const
  N1 = -7;
  D1 = 5;
var
  rational: mpq_t;
  numerator: mpz_t;
begin
  mpq_init(rational);
  mpz_init(numerator);
  mpq_set_si(rational, N1, D1);
  mpq_get_num(numerator, rational);
  assert(mpz_get_si(numerator) = N1);
  mpq_clear(rational);
  mpz_clear(numerator);
end;

procedure TTestGmpBinding.mpq_get_den_test;
const
  N1 = -7;
  D1 = 5;
var
  rational: mpq_t;
  denominator: mpz_t;
begin
  mpq_init(rational);
  mpz_init(denominator);
  mpq_set_si(rational, N1, D1);
  mpq_get_den(denominator, rational);
  assert(mpz_get_si(denominator) = D1);
  mpq_clear(rational);
  mpz_clear(denominator);
end;

procedure TTestGmpBinding.mpq_set_num_test;
const
  N1 = -7;
  D1 = 5;
  N2 = 3;
var
  rational: mpq_t;
  numerator: mpz_t;
begin
  mpq_init(rational);
  mpz_init_set_si(numerator, N2);
  mpq_set_si(rational, N1, D1);
  mpq_set_num(rational, numerator);
  assert(mpq_cmp_si(rational, N2, D1) = 0);
  mpq_clear(rational);
  mpz_clear(numerator);
end;

procedure TTestGmpBinding.mpq_set_den_test;
const
  N1 = -7;
  D1 = 5;
  D2 = 3;
var
  rational: mpq_t;
  denominator: mpz_t;
begin
  mpq_init(rational);
  mpz_init_set_si(denominator, D2);
  mpq_set_si(rational, N1, D1);
  mpq_set_den(rational, denominator);
  assert(mpq_cmp_si(rational, N1, D2) = 0);
  mpq_clear(rational);
  mpz_clear(denominator);
end;

procedure TTestGmpBinding.mpf_get_default_prec_test;
begin
  assert(mpf_get_default_prec > 0);
end;

procedure TTestGmpBinding.mpf_set_default_prec_test;
var prec: valuint;
begin
  prec := mpf_get_default_prec;
  mpf_set_default_prec(2 * prec);
  assert(mpf_get_default_prec  = 2 * prec);
  mpf_set_default_prec(prec);
end;

procedure TTestGmpBinding.mpf_init_test;
var x: mpf_t;
begin
  mpf_init(x);
  assert(mpf_get_prec(x) = mpf_get_default_prec);
  mpf_clear(x);
end;

procedure TTestGmpBinding.mpf_init2_test;
var x: mpf_t;
begin
  mpf_init2(x, 2 * mpf_get_default_prec);
  assert(mpf_get_prec(x) = 2 * mpf_get_default_prec);
  mpf_clear(x);
end;

procedure TTestGmpBinding.mpf_clear_test;
var x: mpf_t;
begin
  mpf_init(x);
  mpf_clear(x);
end;

procedure TTestGmpBinding.mpf_get_prec_test;
var x: mpf_t;
begin
  mpf_init(x);
  mpf_set_prec(x, 2 * mpf_get_default_prec);
  assert(mpf_get_prec(x) = 2 * mpf_get_default_prec);
  mpf_clear(x);
end;

procedure TTestGmpBinding.mpf_set_prec_test;
var x: mpf_t;
begin
  mpf_init(x);
  mpf_set_prec(x, 2 * mpf_get_default_prec);
  assert(mpf_get_prec(x) = 2 * mpf_get_default_prec);
  mpf_clear(x);
end;

procedure TTestGmpBinding.mpf_set_prec_raw_test;
const N = 10;
var x: mpf_t;
begin
  mpf_init2(x, N * mpf_get_default_prec);
  mpf_set_prec_raw(x, mpf_get_default_prec);
  mpf_set_prec_raw(x, N * mpf_get_default_prec);
  mpf_clear(x);
end;

procedure TTestGmpBinding.mpf_set_q_test;
const OP_ = -10;
var
  op: mpq_t;
  rop: mpf_t;
begin
  mpq_init(op);
  mpf_init(rop);
  mpq_set_si(op, OP_, 1);
  mpf_set_q(rop, op);
  assert(mpf_get_si(rop) = OP_);
  mpf_clear(rop);
  mpq_clear(op);
end;

procedure TTestGmpBinding.mpf_set_ui_test;
const OP = 1000000;
var rop: mpf_t;
begin
  mpf_init(rop);
  mpf_set_ui(rop, OP);
  assert(mpf_get_ui(rop) = OP);
  mpf_clear(rop);
end;

procedure TTestGmpBinding.mpf_get_ui_test;
const OP = 1000000;
var rop: mpf_t;
begin
  mpf_init(rop);
  mpf_set_ui(rop, OP);
  assert(mpf_get_ui(rop) = OP);
  mpf_clear(rop);
end;

procedure TTestGmpBinding.mpf_set_si_test;
const OP = -1000000;
var rop: mpf_t;
begin
  mpf_init(rop);
  mpf_set_si(rop, OP);
  assert(mpf_get_si(rop) = OP);
  mpf_clear(rop);
end;

procedure TTestGmpBinding.mpf_get_si_test;
const OP = -1000000;
var rop: mpf_t;
begin
  mpf_init(rop);
  mpf_set_si(rop, OP);
  assert(mpf_get_si(rop) = OP);
  mpf_clear(rop);
end;

procedure TTestGmpBinding.mpf_set_test;
const N = -1000000;
var rop, op: mpf_t;
begin
  mpf_init(rop);
  mpf_init_set_si(op, N);
  mpf_set(rop, op);
  assert(mpf_get_si(rop) = N);
  mpf_clear(rop);
  mpf_clear(op);
end;

procedure TTestGmpBinding.mpf_set_str_test;
const N = -123;
var rop: mpf_t;
begin
  mpf_init(rop);
  assert(mpf_set_str(rop, pchar(inttostr(N)), BASE10) = 0);
  assert(mpf_get_si(rop) = N);
  mpf_clear(rop);
end;

procedure TTestGmpBinding.mpf_swap_test;
const
  ROP_1 = -13;
  ROP_2 = -23;
var rop1, rop2: mpf_t;
begin
  mpf_init_set_si(rop1, ROP_1);
  mpf_init_set_si(rop2, ROP_2);
  mpf_swap(rop1, rop2);
  assert(mpf_get_si(rop1) =  ROP_2);
  assert(mpf_get_si(rop2) = ROP_1);
  mpf_clear(rop1);
  mpf_clear(rop2);
end;

procedure TTestGmpBinding.mpf_get_d_test;
const N = -1.5;
var rop: mpf_t;
begin
  mpf_init_set_d(rop, N);
  assert(mpf_get_d(rop) = N);
  mpf_clear(rop);
end;

procedure TTestGmpBinding.mpf_set_d_test;
const N = -1.5;
var rop: mpf_t;
begin
  mpf_init(rop);
  mpf_set_d(rop, N);
  assert(mpf_get_d(rop) = N);
  mpf_clear(rop);
end;

procedure TTestGmpBinding.mpf_setmpz_test;
const
  OP_1 = -13;
var
  rop: mpf_t;
  op: mpz_t;
begin
  mpf_init(rop);
  mpz_init_set_si(op, OP_1);
  mpf_set_z(rop, op);
  assert(mpf_get_si(rop) = OP_1);
  mpf_clear(rop);
  mpz_clear(op);
end;

procedure TTestGmpBinding.mpf_get_d_2exp_test;
const
  M = 0.5;
  E = 4;
  N = (1 shl E) div 2;
var
  rop: mpf_t;
  exp: valsint;
begin
  mpf_init_set_d(rop, N);
  exp := -1;
  assert(mpf_get_d_2exp(exp, rop) = M);
  assert(exp = E);
  mpf_clear(rop);
end;

procedure TTestGmpBinding.mpf_get_str_test;
const
  N = 3.1416;
  S = '31416';
  E = 1;
var
  op: mpf_t;
  p: pchar;
  exp: mp_exp_t;
begin
  mpf_init_set_d(op, N);
  p := mpf_get_str(nil, exp, BASE10, length(S), op);
  assert(p = S);
  assert(exp = E);
  freemem(p);
  mpf_clear(op);
end;

procedure TTestGmpBinding.mpf_add_test;
const
  OP1_ = 1;
  OP2_ = 2;
var rop, op1, op2: mpf_t;
begin
  mpf_init(rop);
  mpf_init_set_d(op1, OP1_);
  mpf_init_set_d(op2, OP2_);
  mpf_add(rop, op1, op2);
  assert(mpf_get_d(rop) = OP1_ + OP2_);
  mpf_clear(rop);
  mpf_clear(op1);
  mpf_clear(op2);
end;

procedure TTestGmpBinding.mpf_add_ui_test;
const
  OP1_ = 1;
  OP2_ = 2;
var rop, op1: mpf_t;
begin
  mpf_init(rop);
  mpf_init_set_d(op1, OP1_);
  mpf_add_ui(rop, op1, OP2_);
  assert(mpf_get_d(rop) = OP1_ + OP2_);
  mpf_clear(rop);
  mpf_clear(op1);
end;

procedure TTestGmpBinding.mpf_sub_test;
const
  OP1_ = 3;
  OP2_ = 1;
var rop, op1, op2: mpf_t;
begin
  mpf_init(rop);
  mpf_init_set_d(op1, OP1_);
  mpf_init_set_d(op2, OP2_);
  mpf_sub(rop, op1, op2);
  assert(mpf_get_d(rop) = OP1_ - OP2_);
  mpf_clear(rop);
  mpf_clear(op1);
  mpf_clear(op2);
end;

procedure TTestGmpBinding.mpf_sub_ui_test;
const
  OP1_ = 3;
  OP2_ = 1;
var rop, op1: mpf_t;
begin
  mpf_init(rop);
  mpf_init_set_d(op1, OP1_);
  mpf_sub_ui(rop, op1, OP2_);
  assert(mpf_get_d(rop) = OP1_ - OP2_);
  mpf_clear(rop);
  mpf_clear(op1);
end;

procedure TTestGmpBinding.mpf_ui_sub_test;
const
  OP1_ = 3;
  OP2_ = 1;
var rop, op2: mpf_t;
begin
  mpf_init(rop);
  mpf_init_set_d(op2, OP2_);
  mpf_ui_sub(rop, OP1_, op2);
  assert(mpf_get_d(rop) = OP1_ - OP2_);
  mpf_clear(rop);
  mpf_clear(op2);
end;

procedure TTestGmpBinding.mpf_mul_test;
const
  OP1_ = 2;
  OP2_ = 3;
var rop, op1, op2: mpf_t;
begin
  mpf_init(rop);
  mpf_init_set_d(op1, OP1_);
  mpf_init_set_d(op2, OP2_);
  mpf_mul(rop, op1, op2);
  assert(mpf_get_d(rop) = OP1_ * OP2_);
  mpf_clear(rop);
  mpf_clear(op1);
  mpf_clear(op2);
end;

procedure TTestGmpBinding.mpf_mul_ui_test;
const
  OP1_ = 2;
  OP2_ = 3;
var rop, op1: mpf_t;
begin
  mpf_init(rop);
  mpf_init_set_d(op1, OP1_);
  mpf_mul_ui(rop, op1, OP2_);
  assert(mpf_get_d(rop) = OP1_ * OP2_);
  mpf_clear(rop);
  mpf_clear(op1);
end;

procedure TTestGmpBinding.mpf_mul_2exp_test;
const
  OP1_ = 3;
  OP2_ = 4;
var rop, op1: mpf_t;
begin
  mpf_init(rop);
  mpf_init_set_d(op1, OP1_);
  mpf_mul_2exp(rop, op1, OP2_);
  assert(mpf_get_d(rop) = OP1_ * (1 shl OP2_));
  mpf_clear(rop);
  mpf_clear(op1);
end;

procedure TTestGmpBinding.mpf_div_test;
const
  OP1_ = 6;
  OP2_ = 2;
var rop, op1, op2: mpf_t;
begin
  mpf_init(rop);
  mpf_init_set_d(op1, OP1_);
  mpf_init_set_d(op2, OP2_);
  mpf_div(rop, op1, op2);
  assert(mpf_get_d(rop) = OP1_ / OP2_);
  mpf_clear(rop);
  mpf_clear(op1);
  mpf_clear(op2);
end;

procedure TTestGmpBinding.mpf_div_ui_test;
const
  OP1_ = 6;
  OP2_ = 2;
var rop, op1: mpf_t;
begin
  mpf_init(rop);
  mpf_init_set_d(op1, OP1_);
  mpf_div_ui(rop, op1, OP2_);
  assert(mpf_get_d(rop) = OP1_ / OP2_);
  mpf_clear(rop);
  mpf_clear(op1);
end;

procedure TTestGmpBinding.mpf_ui_div_test;
const
  OP1_ = 6;
  OP2_ = 2;
var rop, op2: mpf_t;
begin
  mpf_init(rop);
  mpf_init_set_si(op2, OP2_);
  mpf_ui_div(rop, OP1_, op2);
  assert(mpf_get_d(rop) = OP1_ / OP2_);
  mpf_clear(rop);
  mpf_clear(op2);
end;

procedure TTestGmpBinding.mpf_div_2exp_test;
const
  OP1_ = 16;
  OP2_ = 2;
var rop, op1: mpf_t;
begin
  mpf_init(rop);
  mpf_init_set_d(op1, OP1_);
  mpf_div_2exp(rop, op1, OP2_);
  assert(mpf_get_d(rop) = OP1_ / (1 shl OP2_));
  mpf_clear(rop);
  mpf_clear(op1);
end;

procedure TTestGmpBinding.mpf_sqrt_test;
const
  OP_ = 4;
var rop, op: mpf_t;
begin
  mpf_init(rop);
  mpf_init_set_d(op, OP_);
  mpf_sqrt(rop, op);
  assert(mpf_get_d(rop) = sqrt(OP_));
  mpf_clear(rop);
  mpf_clear(op);
end;

procedure TTestGmpBinding.mpf_sqrt_ui_test;
const
  OP_ = 4;
var rop: mpf_t;
begin
  mpf_init(rop);
  mpf_sqrt_ui(rop, OP_);
  assert(mpf_get_d(rop) = sqrt(OP_));
  mpf_clear(rop);
end;

procedure TTestGmpBinding.mpf_pow_ui_test;
const
  OP1_ = 3;
  OP2_ = 2;
var rop, op1: mpf_t;
begin
  mpf_init(rop);
  mpf_init_set_d(op1, OP1_);
  mpf_pow_ui(rop, op1, OP2_);
  assert(mpf_get_d(rop) = OP1_ ** OP2_);
  mpf_clear(rop);
  mpf_clear(op1);
end;

procedure TTestGmpBinding.mpf_neg_test;
const
  OP_ = 4;
var rop, op: mpf_t;
begin
  mpf_init(rop);
  mpf_init_set_d(op, OP_);
  mpf_neg(rop, op);
  assert(mpf_get_d(rop) = -OP_);
  mpf_clear(rop);
  mpf_clear(op);
end;

procedure TTestGmpBinding.mpf_abs_test;
const OP_ = -4;
var rop, op: mpf_t;
begin
  mpf_init(rop);
  mpf_init_set_d(op, OP_);
  mpf_abs(rop, op);
  assert(mpf_get_d(rop) = -OP_);
  mpf_clear(rop);
  mpf_clear(op);
end;

procedure TTestGmpBinding.mpf_cmp_test;
const OP_1 = -10;
var op1, op2: mpf_t;
begin
  mpf_init_set_si(op1, OP_1);
  mpf_init_set_si(op2, OP_1);
  assert(mpf_cmp(op1, op2) = 0);
  mpf_set_si(op2, OP_1 + 1);
  assert(mpf_cmp(op1, op2) < 0);
  mpf_set_si(op2, OP_1 - 1);
  assert(mpf_cmp(op1, op2) > 0);
  mpf_clear(op1);
  mpf_clear(op2);
end;

procedure TTestGmpBinding.mpf_cmp_d_test;
const OP_ = -10;
var op: mpf_t;
begin
  mpf_init_set_si(op, OP_);
  assert(mpf_cmp_d(op, OP_) = 0);
  assert(mpf_cmp_d(op, OP_ + 1) < 0);
  assert(mpf_cmp_d(op, OP_ - 1) > 0);
  mpf_clear(op);
end;

procedure TTestGmpBinding.mpf_cmp_si_test;
const OP_ = -10;
var op: mpf_t;
begin
  mpf_init_set_si(op, OP_);
  assert(mpf_cmp_si(op, OP_) = 0);
  assert(mpf_cmp_si(op, OP_ + 1) < 0);
  assert(mpf_cmp_si(op, OP_ - 1) > 0);
  mpf_clear(op);
end;

procedure TTestGmpBinding.mpf_cmp_ui_test;
const OP_ = 10;
var op: mpf_t;
begin
  mpf_init_set_si(op, OP_);
  assert(mpf_cmp_ui(op, OP_) = 0);
  assert(mpf_cmp_ui(op, OP_ + 1) < 0);
  assert(mpf_cmp_ui(op, OP_ - 1) > 0);
  mpf_clear(op);
end;

procedure TTestGmpBinding.mpf_eq_test;
const
  OP_1 = 10;
  OP_3 = 8;
var op1, op2: mpf_t;
begin
  mpf_init_set_si(op1, OP_1);
  mpf_init_set_si(op2, OP_1);
  assert(mpf_eq(op1, op2, OP_3) <> 0);
  mpf_clear(op1);
  mpf_clear(op2);
end;

procedure TTestGmpBinding.mpf_reldiff_test;
const
  OP1_ = 4;
  OP2_ = 2;
var rop, op1, op2: mpf_t;
begin
  mpf_init(rop);
  mpf_init_set_d(op1, OP1_);
  mpf_init_set_d(op2, OP2_);
  mpf_reldiff(rop, op1, op2);
  assert(mpf_get_d(rop) = abs(OP1_ - OP2_) / OP1_);
  mpf_clear(rop);
  mpf_clear(op1);
  mpf_clear(op2);
end;

procedure TTestGmpBinding.mpf_ceil_test;
const N = 3.1;
var rop, op: mpf_t;
begin
  mpf_init(rop);
  mpf_init_set_d(op, N);
  mpf_ceil(rop, op);
  assert(mpf_get_d(rop) = ceil(N));
  mpf_clear(rop);
  mpf_clear(op);
end;

procedure TTestGmpBinding.mpf_floor_test;
const N = 3.9;
var rop, op: mpf_t;
begin
  mpf_init(rop);
  mpf_init_set_d(op, N);
  mpf_floor(rop, op);
  assert(mpf_get_d(rop) = floor(N));
  mpf_clear(rop);
  mpf_clear(op);
end;

procedure TTestGmpBinding.mpf_trunc_test;
const N = -3.9;
var rop, op: mpf_t;
begin
  mpf_init(rop);
  mpf_init_set_d(op, N);
  mpf_trunc(rop, op);
  assert(mpf_get_d(rop) = trunc(N));
  mpf_clear(rop);
  mpf_clear(op);
end;

procedure TTestGmpBinding.mpf_integer_p_test;
const
  N1 = 10;
  N2 = 10.1;
var op: mpf_t;
begin
  mpf_init_set_si(op, N1);
  assert(longbool(mpf_integer_p(op)));
  mpf_set_d(op, N2);
  assert(not longbool(mpf_integer_p(op)));
  mpf_clear(op);
end;

procedure TTestGmpBinding.mpf_fits_ushort_p_test;
var op: mpf_t;
begin
  mpf_init_set_ui(op, high(word));
  assert(longbool(mpf_fits_ushort_p(op)));
  mpf_add_ui(op, op, 1);
  assert(not longbool(mpf_fits_ushort_p(op)));
  mpf_clear(op);
end;

procedure TTestGmpBinding.mpf_fits_sshort_p_test;
var op: mpf_t;
begin
  mpf_init_set_ui(op, high(smallint));
  assert(longbool(mpf_fits_sshort_p(op)));
  mpf_add_ui(op, op, 1);
  assert(not longbool(mpf_fits_sshort_p(op)));
  mpf_clear(op);
end;

procedure TTestGmpBinding.mpf_fits_uint_p_test;
var op: mpf_t;
begin
  mpf_init_set_ui(op, high(cardinal));
  assert(longbool(mpf_fits_uint_p(op)));
  mpf_add_ui(op, op, 1);
  assert(not longbool(mpf_fits_uint_p(op)));
  mpf_clear(op);
end;

procedure TTestGmpBinding.mpf_fits_sint_p_test;
var op: mpf_t;
begin
  mpf_init_set_ui(op, high(integer));
  assert(longbool(mpf_fits_sint_p(op)));
  mpf_add_ui(op, op, 1);
  assert(not longbool(mpf_fits_sint_p(op)));
  mpf_clear(op);
end;

procedure TTestGmpBinding.mpf_fits_ulong_p_test;
var op: mpf_t;
begin
  mpf_init_set_ui(op, high(valuint));
  assert(longbool(mpf_fits_ulong_p(op)));
  mpf_add_ui(op, op, 1);
  assert(not longbool(mpf_fits_ulong_p(op)));
  mpf_clear(op);
end;

procedure TTestGmpBinding.mpf_fits_slong_p_test;
var op: mpf_t;
begin
  mpf_init_set_ui(op, high(valsint));
  assert(longbool(mpf_fits_slong_p(op)));
  mpf_add_ui(op, op, 1);
  assert(not longbool(mpf_fits_slong_p(op)));
  mpf_clear(op);
end;

procedure TTestGmpBinding.mpf_urandomb_test;
const NBITS = 100;
var
  rop: mpf_t;
  state: randstate_t;
  cmp: longint;
begin
  mp_randinit_default(state);
  mpf_init(rop);
  mpf_urandomb(rop, state, NBITS);
  cmp := mpf_cmp_ui(rop, 0);
  assert(cmp >= 0);
  assert(mpf_cmp_ui(rop, 1) < 0);
  mpf_clear(rop);
  mp_randclear(state);
end;

procedure TTestGmpBinding.mpf_random2_test;
var
  rop: mpf_t;
begin
  mpf_init(rop);
  mpf_random2(rop, 1, 1);
  mpf_clear(rop);
end;

procedure TTestGmpBinding.mpf_init_set_test;
const N = -1000000;
var rop, op: mpf_t;
begin
  mpf_init_set_si(op, N);
  mpf_init_set(rop, op);
  assert(mpf_get_si(rop) = N);
  mpf_clear(rop);
  mpf_clear(op);
end;

procedure TTestGmpBinding.mpf_init_set_d_test;
const N = -1.5;
var rop: mpf_t;
begin
  mpf_init_set_d(rop, N);
  assert(mpf_get_d(rop) = N);
  mpf_clear(rop);
end;

procedure TTestGmpBinding.mpf_init_set_si_test;
const N = -5;
var rop: mpf_t;
begin
  mpf_init_set_si(rop, N);
  assert(mpf_get_si(rop) = N);
  mpf_clear(rop);
end;

procedure TTestGmpBinding.mpf_init_set_str_test;
const N = -123;
var rop: mpf_t;
begin
  mpf_init_set_str(rop, pchar(inttostr(N)), BASE10);
  assert(mpf_get_si(rop) = N);
  mpf_clear(rop);
end;

procedure TTestGmpBinding.mpf_init_set_ui_test;
const N = 5;
var rop: mpf_t;
begin
  mpf_init_set_ui(rop, N);
  assert(mpf_get_ui(rop) = N);
  mpf_clear(rop);
end;

procedure TTestGmpBinding.mpn_add_n_test;
var
  rop, s1, s2: mp_limb_t;
begin
  s1 := high(mp_limb_t);
  s2 := s1;
  rop := 2;
  assert(mpn_add_n(@rop, @s1, @s2, 1) = 1);
  assert(rop = high(mp_limb_t) - 1);
end;

procedure TTestGmpBinding.mpn_add_1_test;
var
  rop, s1: mp_limb_t;
begin
  s1 := high(mp_limb_t);
  rop := 2;
  assert(mpn_add_1(@rop, @s1, 1, high(mp_limb_t)) = 1);
  assert(rop = high(mp_limb_t) - 1);
end;

procedure TTestGmpBinding.mpn_add_test;
var
  rop, s1, s2: mp_limb_t;
begin
  s1 := high(mp_limb_t);
  s2 := s1;
  rop := 2;
  assert(mpn_add(@rop, @s1, 1, @s2, 1) = 1);
  assert(rop = high(mp_limb_t) - 1);
end;

procedure TTestGmpBinding.mpn_sub_n_test;
var
  rop, s1, s2: mp_limb_t;
begin
  s1 := 0;
  s2 := high(mp_limb_t);
  rop := 2;
  assert(mpn_sub_n(@rop, @s1, @s2, 1) = 1);
  assert(rop = 1);
end;

procedure TTestGmpBinding.mpn_sub_1_test;
var
  rop, s1: mp_limb_t;
begin
  s1 := 0;
  rop := 2;
  assert(mpn_sub_1(@rop, @s1, 1, high(mp_limb_t)) = 1);
  assert(rop = 1);
end;

procedure TTestGmpBinding.mpn_sub_test;
var
  rop, s1, s2: mp_limb_t;
begin
  s1 := 0;
  s2 := high(mp_limb_t);
  rop := 2;
  assert(mpn_sub(@rop, @s1, 1, @s2, 1) = 1);
  assert(rop = 1);
end;

procedure TTestGmpBinding.mpn_mul_n_test;
var
  rop: array[0..1] of mp_limb_t;
  s1, s2: mp_limb_t;
begin
  s1 := high(mp_limb_t);
  s2 := s1;
  mpn_mul_n(@rop, @s1, @s2, 1);
  assert(rop[0] = 1);
  assert(rop[1] = high(mp_limb_t) - 1);
end;

procedure TTestGmpBinding.mpn_mul_1_test;
var
  rop: array[0..1] of mp_limb_t;
  s1: mp_limb_t;
begin
  s1 := high(mp_limb_t);
  assert(mpn_mul_1(@rop, @s1, 1, high(mp_limb_t)) = high(mp_limb_t) - 1);
  assert(rop[0] = 1);
end;

procedure TTestGmpBinding.mpn_mul_test;
var
  rop: array[0..1] of mp_limb_t;
  s1, s2: mp_limb_t;
begin
  s1 := high(mp_limb_t);
  s2 := s1;
  assert(mpn_mul(@rop, @s1, 1, @s2, 1) = high(mp_limb_t) - 1);
  assert(rop[0] = 1);
  assert(rop[1] = high(mp_limb_t) - 1);
end;

procedure TTestGmpBinding.mpn_addmul_1_test;
var
  rop: array[0..1] of mp_limb_t;
  s1: mp_limb_t;
begin
  rop[0] := 1;
  rop[1] := 0;
  s1 := high(mp_limb_t);
  assert(mpn_addmul_1(@rop, @s1, 1, high(mp_limb_t)) = high(mp_limb_t) - 1);
  assert(rop[0] = 2);
end;

procedure TTestGmpBinding.mpn_submul_1_test;
var
  rop: array[0..1] of mp_limb_t;
  s1: mp_limb_t;
begin
  rop[0] := 1;
  rop[1] := 0;
  s1 := high(mp_limb_t);
  assert(mpn_submul_1(@rop, @s1, 1, high(mp_limb_t)) = high(mp_limb_t) - 1);
  assert(rop[0] = 0);
end;

procedure TTestGmpBinding.mpn_tdiv_qr_test;
const
  N_ = 19;
  D_ = 4;
var
  q, r: array[0..1] of mp_limb_t;
  n, d: mp_limb_t;
begin
  n := N_;
  d := D_;
  q[0] := 0;
  q[1] := 0;
  r[0] := 0;
  r[1] := 0;
  mpn_tdiv_qr(@q, @r, 0, @n, 1, @d, 1);
  assert(q[0] = N_ div D_);
  assert(r[0] = N_ mod D_);
end;

procedure TTestGmpBinding.mpn_divrem_1_test;
const
  N_ = 19;
  D_ = 4;
var r1, s2: mp_limb_t;
begin
  r1 := 0;
  s2 := N_;
  assert(mpn_divrem_1(@r1, 0, @s2, 1, D_) = N_ mod D_);
  assert(r1 = N_ div D_);
end;

procedure TTestGmpBinding.mpn_divexact_by3c_test;
const
  K = 3;
  N_ = 17;
var r, s: mp_limb_t;
begin
  r := 0;
  s := K * N_;
  assert(mpn_divexact_by3c(@r, @s, 1, 0) = 0);
  assert(r = N_);
  s := K * N_ + 1;
  assert(mpn_divexact_by3c(@r, @s, 1, 0) <> 0);
end;

procedure TTestGmpBinding.mpn_mod_1_test;
const
  N_ = 19;
  D_ = 4;
var s1: mp_limb_t;
begin
  s1 := N_;
  assert(mpn_mod_1(@s1, 1, D_) = N_ mod D_);
end;

procedure TTestGmpBinding.mpn_lshift_test;
var r, s: mp_limb_t;
begin
  r := 0;
  s := high(mp_limb_t);
  assert(mpn_lshift(@r, @s, 1, 1) = 1);
  assert(r = high(mp_limb_t) - 1);
end;

procedure TTestGmpBinding.mpn_rshift_test;
var r, s: mp_limb_t;
begin
  r := 0;
  s := high(mp_limb_t);
  assert(mpn_rshift(@r, @s, 1, 1) = valuint(1) shl (bits_per_limb - 1));
  assert(r = high(mp_limb_t) shr 1);
end;

procedure TTestGmpBinding.mpn_cmp_test;
const S1_ = 10;
var s1, s2: mp_limb_t;
begin
  s1 := S1_;
  S2 := S1_;
  assert(mpn_cmp(@s1, @s2, 1) = 0);
  S2 := S1_ - 1;
  assert(mpn_cmp(@s1, @s2, 1) > 0);
  S2 := S1_ + 1;
  assert(mpn_cmp(@s1, @s2, 1) < 0);
end;

procedure TTestGmpBinding.mpn_gcd_test;
const
  S1_ = 30;
  S2_ = 105;
  RP_ = 15;
var r, s1, s2: mp_limb_t;
begin
  s1 := S1_;
  s2 := S2_;
  r := 0;
  assert(mpn_gcd(@r, @s1, 1, @s2, 1) = 1);
  assert(r = RP_);
end;

procedure TTestGmpBinding.mpn_gcd_1_test;
const
  S1_ = 30;
  S2_ = 105;
  R_ = 15;
var s1: mp_limb_t;
begin
  s1 := S1_;
  assert(mpn_gcd_1(@s1, 1, S2_) = R_);
end;

procedure TTestGmpBinding.mpn_gcdext_test;
const
  S1_ = 3 * 5;
  S2_ = 2 * 3;
  R1_ = 3;
var
  r1, r2, s1, s2: array[0..1] of mp_limb_t;
  r2n: mp_size_t;
  k: valsint;
begin
  s1[0] := S1_;
  s1[1] := 0;
  s2[0] := S2_;
  s2[1] := 0;
  assert(mpn_gcdext(@r1, @r2, r2n, @s1, 1, @s2, 1) = 1);
  assert(r2n = 1);
  assert(r1[0] = R1_);
  k := (valsint(r1[0]) - valsint(r2[0]) * valsint(s1[0])) div valsint(s2[0]);
  assert(valsint(r1[0]) = valsint(r2[0]) * valsint(s1[0]) + k * valsint(s2[0]));
end;

procedure TTestGmpBinding.mpn_sqrtrem_test;
const
  ROOT = 3;
var r1, r2, s: mp_limb_t;
begin
  s := ROOT ** 2 + ROOT - 1;
  assert(mpn_sqrtrem(@r1, @r2, @s, 1) = 1);
  assert(r1 = ROOT);
  assert(r2 = ROOT - 1);
end;

procedure TTestGmpBinding.mpn_get_str_test;
const N = uint64($FEDCBA9876543210);
var
  str: array[0..2 * sizeof(mp_limb_t)] of byte;
  s1: mp_limb_t;
  i: integer;
begin
  s1 := N and (high(mp_limb_t) - 1);
  assert(mpn_get_str(@str, BASE16, @s1, 1) = high(str));
  for i := 0 to high(str) - 1 do
    assert(str[i] = high(str) - i - 1);
end;

procedure TTestGmpBinding.mpn_set_str_test;
const N = uint64($FEDCBA9876543210);
var
  r: mp_limb_t;
  str: array[0..2 * sizeof(mp_limb_t)] of byte;
  i: integer;
begin
  for i := 0 to high(str) - 1 do
    str[i] := high(str) - i - 1;
  assert(mpn_set_str(@r, @str, high(str), BASE16) = 1);
  assert(r = N and (high(mp_limb_t) - 1));
end;

procedure TTestGmpBinding.mpn_scan0_test;
const
  S1_ = $F75;
  STARTING_BIT = 4;
  N = 7;
var s1: mp_limb_t;
begin
  s1 := S1_;
  assert(mpn_scan0(@s1, STARTING_BIT) = N);
end;

procedure TTestGmpBinding.mpn_scan1_test;
const
  S1_ = $85;
  STARTING_BIT = 4;
  N = 7;
var s1: mp_limb_t;
begin
  s1 := S1_;
  assert(mpn_scan1(@s1, STARTING_BIT) = N);
end;

procedure TTestGmpBinding.mpn_popcount_test;
const
  S1_ = $55AA;
  N = 8;
var s1: mp_limb_t;
begin
  s1 := S1_;
  assert(mpn_popcount(@s1, 1) = N);
end;

procedure TTestGmpBinding.mpn_hamdist_test;
const
  S1_ = $55;
  S2_ = $7A;
  N = 5;
var s1, s2: mp_limb_t;
begin
  s1 := S1_;
  s2 := S2_;
  assert(mpn_hamdist(@s1, @s2, 1) = N);
end;

procedure TTestGmpBinding.mpn_perfect_square_p_test;
const ROOT = 3;
var s1: mp_limb_t;
begin
  s1 := ROOT ** 2;
  assert(mpn_perfect_square_p(@s1, 1) <> 0);
  s1 := s1 or 2;
  assert(mpn_perfect_square_p(@s1, 1) = 0);
end;

procedure TTestGmpBinding.mpn_random_test;
var r1: array[0..2] of mp_limb_t;
begin
  mpn_random(@r1, high(r1) + 1);
  assert((r1[2] <> 0) and ((r1[0] <> r1[1]) or (r1[0] <> r1[2]) or (r1[1] <> r1[2])));
end;

procedure TTestGmpBinding.mpn_random2_test;
var r1: array[0..2] of mp_limb_t;
begin
  mpn_random2(@r1, high(r1) + 1);
  assert((r1[2] <> 0) and ((r1[0] <> r1[1]) or (r1[0] <> r1[2]) or (r1[1] <> r1[2])));
end;

procedure TTestGmpBinding.bits_per_limb_test;
begin
  assert(bits_per_limb = 8 * sizeof(mp_limb_t));
end;

procedure TTestGmpBinding.version_test;

  function isvernum(s: string; first, last: integer): boolean;
  var i: integer;
  begin
    i := strtointdef(copy(s, first, last - first + 1), -1);
    result := i >= 0;
  end;

const DOT = '.';
var
  v: string;
  dot1, dot2: integer;
begin
  v := version;
  dot1 := pos(DOT, v);
  assert(dot1 <> 0);
  dot2 := posex(DOT, v, dot1 + 1);
  assert(dot2 <> 0);
  assert(isvernum(v, 1, dot1 - 1));
  assert(isvernum(v, dot1 + 1, dot2 - 1));
  assert(isvernum(v, dot2 + 1, length(v)));
end;

{ TTestGmpExtensions }

procedure TTestGmpExtensions.randinit_default_test;
var state: MPRandState;
begin
  randinit_default(state);
  assert(rnd_test(state.Ptr^));
end;

procedure TTestGmpExtensions.randinit_mt_test;
var state: MPRandState;
begin
  randinit_mt(state);
  assert(rnd_test(state.Ptr^));
end;

procedure TTestGmpExtensions.randinit_lc_2exp_test;
const
  A_ =  3;
  C = 5;
  M2EXP = 8;
var
  state: MPRandState;
  a: MPInteger;
begin
  z_init_set_ui(a, A_);
  randinit_lc_2exp(state, a, C, M2EXP);
  assert(rnd_test(state.Ptr^));
end;

procedure TTestGmpExtensions.randinit_lc_2exp_size_test;
var state: MPRandState;
begin
  assert(randinit_lc_2exp_size(state, 0));
  assert(rnd_test(state.Ptr^));
end;

procedure TTestGmpExtensions.randinit_set_test;
var rop, op: MPRandState;
begin
  randinit_default(op);
  randinit_set(rop, op);
  assert(rnd_test(rop.Ptr^));
end;

procedure TTestGmpExtensions.randclear_test;
var state: MPRandState;
begin
  randinit_default(state);
  randclear(state);
end;

procedure TTestGmpExtensions.randseed_test;
var
  state: MPRandState;
  seed: MPInteger;
begin
  randinit_default(state);
  z_init_set_ui(seed, 0);
  randseed(state, seed);
end;

procedure TTestGmpExtensions.randseed_ui_test;
var state: MPRandState;
begin
  randinit_default(state);
  randseed_ui(state, 0);
end;

procedure TTestGmpExtensions.urandomb_ui_test;
const
  B = 8 * (sizeof(valuint) - 1);
  N = 1 shl B;
var
  state: MPRandState;
  r1, r2, r3: valuint;
begin
  randinit_default(state);
  r1 := urandomb_ui(state, B);
  r2 := urandomb_ui(state, B);
  r3 := urandomb_ui(state, B);
  assert((r1 < N) and (r2 < N) and (r3 < N) and ((r1 <> r2) or (r1 <> r3) or (r2 <> r3)));
end;

procedure TTestGmpExtensions.urandomm_ui_test;
const  N = high(valuint);
var
  state: MPRandState;
  r1, r2, r3: valuint;
begin
  randinit_default(state);
  r1 := urandomm_ui(state, N);
  r2 := urandomm_ui(state, N);
  r3 := urandomm_ui(state, N);
  assert((r1 < N) and (r2 < N) and (r3 < N) and ((r1 <> r2) or (r1 <> r3) or (r2 <> r3)));
end;

//procedure TTestGmpExtensions.asprintf_test;
//const
//  N = 123456;
//  S = '123456';
//var
//  z: MPInteger;
//  p: pchar;
//begin
//  z_init_set_ui(z, N);
//  assert(asprintf(p, Z_FMT, [@z]) = length(S));
//  assert(p = S);
//  freemem(p);
//  z_clear(z);
//end;
//
//procedure TTestGmpExtensions.asprintf_test2;
//const
//  N = 123456;
//  S = '123456';
//var
//  z: MPInteger;
//  p: pchar;
//begin
//  z_init_set_ui(z, N);
//  assert(asprintf(p, Z_FMT, @z) = length(S));
//  assert(p = S);
//  freemem(p);
//  z_clear(z);
//end;
//
//procedure TTestGmpExtensions.snprintf_test;
//const
//  N = 123456;
//  S = '123456';
//var
//  z: MPInteger;
//  buf: pchar;
//begin
//  z_init_set_ui(z, N);
//  getmem(buf, length(S) + 1);
//  assert(snprintf(buf, length(S) + 1, Z_FMT, [@z]) = length(S));
//  assert(buf = S);
//  freemem(buf);
//  z_clear(z);
//end;
//
//procedure TTestGmpExtensions.snprintf_test2;
//const
//  N = 123456;
//  S = '123456';
//var
//  z: MPInteger;
//  buf: pchar;
//begin
//  z_init_set_ui(z, N);
//  getmem(buf, length(S) + 1);
//  assert(snprintf(buf, length(S) + 1, Z_FMT, @z) = length(S));
//  assert(buf = S);
//  freemem(buf);
//  z_clear(z);
//end;
//
//procedure TTestGmpExtensions.sprintf_test;
//const
//  N = 123456;
//  S = '123456';
//var
//  z: MPInteger;
//  buf: pchar;
//begin
//  z_init_set_ui(z, N);
//  getmem(buf, length(S) + 1);
//  assert(sprintf(buf, Z_FMT, [@z]) = length(S));
//  assert(buf = S);
//  freemem(buf);
//  z_clear(z);
//end;
//
//procedure TTestGmpExtensions.sprintf_test2;
//const
//  N = 123456;
//  S = '123456';
//var
//  z: MPInteger;
//  buf: pchar;
//begin
//  z_init_set_ui(z, N);
//  getmem(buf, length(S) + 1);
//  assert(sprintf(buf, Z_FMT, @z) = length(S));
//  assert(buf = S);
//  freemem(buf);
//  z_clear(z);
//end;
//
//procedure TTestGmpExtensions.sscanf_test;
//const
//  N = 3;
//  D = 2;
//var q: mpq_t;
//begin
//  q_init(q);
//  assert(sscanf(pchar(inttostr(N) + '/' + inttostr(D)), Q_FMT, [@q]) = 1);
//  assert(q_cmp_ui(q, N, D) = 0);
//  q_clear(q);
//end;
//
//procedure TTestGmpExtensions.sscanf_test2;
//const
//  N = 3;
//  D = 2;
//var q: mpq_t;
//begin
//  q_init(q);
//  assert(sscanf(pchar(inttostr(N) + '/' + inttostr(D)), Q_FMT, @q) = 1);
//  assert(q_cmp_ui(q, N, D) = 0);
//  q_clear(q);
//end;

procedure TTestGmpExtensions.z_init_test;
var integer_: MPInteger;
begin
  z_init(integer_);
  assert(z_get_si(integer_) = 0);
end;

procedure TTestGmpExtensions.z_clear_test;
const N = 1000000;
var integer_: MPInteger;
begin
  z_init2(integer_, N);
  z_clear(integer_);
  assert(z_get_si(integer_) = 0);
end;

procedure TTestGmpExtensions.z_init2_test;
const N = 1000000;
var integer_: MPInteger;
begin
  z_init2(integer_, N);
  assert(z_get_si(integer_) = 0);
end;

procedure TTestGmpExtensions.z_init_set_si_test;
const N = -1000000;
var rop: MPInteger;
begin
  z_init_set_si(rop, N);
  assert(z_get_si(rop) = N);
end;

procedure TTestGmpExtensions.z_init_set_ui_test;
const N = 1000000;
var rop: MPInteger;
begin
  z_init_set_ui(rop, N);
  assert(z_get_ui(rop) = N);
end;

procedure TTestGmpExtensions.z_init_set_d_test;
const N = -1000000;
var rop: MPInteger;
begin
  z_init_set_d(rop, N);
  assert(z_get_si(rop) = N);
end;

procedure TTestGmpExtensions.z_init_set_test;
const N = -1000000;
var rop, op: MPInteger;
begin
  z_init_set_si(op, N);
  z_init_set(rop, op);
  assert(z_get_si(rop) = N);
end;

procedure TTestGmpExtensions.z_init_set_str_test;
const N = 1000000;
var rop: MPInteger;
begin
  assert(z_init_set_str(rop, IntToStr(N), BASE10));
  assert(z_get_si(rop) = N);
end;

procedure TTestGmpExtensions.z_realloc_test;
const N = 1000;
var integer_: MPInteger;
begin
  z_init(integer_);
  z_realloc(integer_, N);
end;

procedure TTestGmpExtensions.z_realloc2_test;
const N = 1000;
var integer_: MPInteger;
begin
  z_init(integer_);
  z_realloc2(integer_, N);
end;

procedure TTestGmpExtensions.z_getlimbn_test;
const N = 1234;
var op: MPInteger;
begin
  z_init_set_ui(op, N);
  assert(z_getlimbn(op, 0) = N);
end;

procedure TTestGmpExtensions.z_export_test;
type t = word;
const
  N = 1000000;
  BUF_HIGH = sizeof(N) div sizeof(t);
var
  export_op, import_op: MPInteger;
  buf: array[0..BUF_HIGH] of t;
  count: sizeuint;
begin
  z_init_set_si(export_op, N);
  z_init(import_op);
  z_export(buf, count, -1, sizeof(t), 0, 0, export_op);
  z_import(import_op, count, -1, sizeof(t), 0, 0, buf);
  assert(z_get_si(export_op) = z_get_si(import_op));
end;

procedure TTestGmpExtensions.z_import_test;
type t = word;
const
  N = 1000000;
  BUF_HIGH = sizeof(N) div sizeof(t);
var
  export_op, import_op: MPInteger;
  buf: array[0..BUF_HIGH] of t;
  count: sizeuint;
begin
  z_init_set_si(export_op, N);
  z_init(import_op);
  z_export(buf, count, -1, sizeof(t), 0, 0, export_op);
  z_import(import_op, count, -1, sizeof(t), 0, 0, buf);
  assert(z_get_si(export_op) = z_get_si(import_op));
end;

procedure TTestGmpExtensions.z_swap_test;
const
  ROP_1 = -1000;
  ROP_2 = 1000000;
var rop1, rop2: MPInteger;
begin
  z_init_set_si(rop1, ROP_1);
  z_init_set_si(rop2, ROP_2);
  z_swap(rop1, rop2);
  assert(z_get_si(rop1) = ROP_2);
  assert(z_get_si(rop2) = ROP_1);
end;

procedure TTestGmpExtensions.z_set_test;
const OP_ = 1000000;
var rop, op: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op, OP_);
  z_set(rop, op);
  assert(z_get_si(rop) = OP_);
end;

procedure TTestGmpExtensions.z_set_d_test;
const OP_ = 1000000;
var rop: MPInteger;
begin
  z_init(rop);
  z_set_d(rop, OP_);
  assert(z_get_si(rop) = OP_);
end;

procedure TTestGmpExtensions.z_set_f_test;
const OP_ = -10;
var
  op: MPFloat;
  rop: MPInteger;
begin
  f_init_set_si(op, OP_);
  z_init(rop);
  z_set_f(rop, op);
  assert(z_get_si(rop) = OP_);
end;

procedure TTestGmpExtensions.z_set_q_test;
const OP_ = -10;
var
  op: MPRational;
  rop: MPInteger;
begin
  q_init(op);
  z_init(rop);
  q_set_si(op, OP_, 1);
  z_set_q(rop, op);
  assert(z_get_si(rop) = OP_);
end;

procedure TTestGmpExtensions.z_set_si_test;
const OP_ = -1000000;
var rop: MPInteger;
begin
  z_init(rop);
  z_set_si(rop, OP_);
  assert(z_get_si(rop) = OP_);
end;

procedure TTestGmpExtensions.z_set_str_test;
const OP_ = -1000000;
var rop: MPInteger;
begin
  z_init(rop);
  assert(z_set_str(rop, IntToStr(OP_), BASE10));
  assert(z_get_si(rop) = OP_);
end;

procedure TTestGmpExtensions.z_set_ui_test;
const OP_ = 1000000;
var rop: MPInteger;
begin
  z_init(rop);
  z_set_ui(rop, OP_);
  assert(z_get_si(rop) = OP_);
end;

procedure TTestGmpExtensions.z_get_d_test;
const N = -1000;
var op: MPInteger;
begin
  z_init_set_si(op, N);
  assert(z_get_d(op) = N);
end;

procedure TTestGmpExtensions.z_get_d_2exp_test;
const
  N_LG2 = 4;
  N = 1 shl N_LG2;
var
  op: MPInteger;
  exp: mp_exp_t;
begin
  z_init_set_si(op, N);
  assert(z_get_d_2exp(exp, op) = 1 / 2);
  assert(exp = N_LG2 + 1);
end;

procedure TTestGmpExtensions.z_get_si_test;
const N = -1000000;
var op: MPInteger;
begin
  z_init_set_si(op, N);
  assert(z_get_si(op) = N);
end;

procedure TTestGmpExtensions.z_get_str_test;
const N = -1000000;
var
  op: MPInteger;
  p: pchar;
  s, s0, s1: String;
begin
  z_init_set_si(op, N);
  s0 := inttostr(N);
  s := z_get_str(BASE10, op);
  assert(s = s0);
  p := z_get_str(nil, BASE10, op);
  assert(p = s0);
  freemem(p);
  setlength(s1, length(s0));
  assert(pointer(z_get_str(pchar(s1), BASE10, op)) = pointer(s1));
  assert(s1 = s0);
end;

procedure TTestGmpExtensions.z_get_ui_test;
const N = 1000000;
var op: MPInteger;
begin
  z_init_set_ui(op, N);
  assert(z_get_ui(op) = N);
end;

procedure TTestGmpExtensions.z_add_test;
const
  OP_1 = -1000;
  OP_2 = 1000000;
var rop, op1, op2, sum: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op1, OP_1);
  z_init_set_si(op2, OP_2);
  z_add(rop, op1, op2);
  assert(z_get_si(rop) = OP_1 + OP_2);
  sum := z_add(op1, op2);
  assert(z_get_si(sum) = OP_1 + OP_2);
end;

procedure TTestGmpExtensions.z_add_ui_test;
const
  OP_1 = -1000;
  OP_2 = 1000000;
var rop, op1, sum: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op1, OP_1);
  z_add_ui(rop, op1, OP_2);
  assert(z_get_si(rop) = OP_1 + OP_2);
  sum := z_add_ui(op1, OP_2);
  assert(z_get_si(sum) = OP_1 + OP_2);
end;

procedure TTestGmpExtensions.z_sub_test;
const
  OP_1 = -1000;
  OP_2 = 1000000;
var rop, op1, op2, diff: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op1, OP_1);
  z_init_set_si(op2, OP_2);
  z_sub(rop, op1, op2);
  assert(z_get_si(rop) = OP_1 - OP_2);
  diff := z_sub(op1, op2);
  assert(z_get_si(diff) = OP_1 - OP_2);
end;

procedure TTestGmpExtensions.z_sub_ui_test;
const
  OP_1 = -1000;
  OP_2 = 1000000;
var rop, op1, diff: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op1, OP_1);
  z_sub_ui(rop, op1, OP_2);
  assert(z_get_si(rop) = OP_1 - OP_2);
  diff := z_sub_ui(op1, OP_2);
  assert(z_get_si(diff) = OP_1 - OP_2);
end;

procedure TTestGmpExtensions.z_ui_sub_test;
const
  OP_1 = 1000;
  OP_2 = 1000000;
var rop, op2, diff: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op2, OP_2);
  z_ui_sub(rop, OP_1, op2);
  assert(z_get_si(rop) = OP_1 - OP_2);
  diff := z_ui_sub(OP_1, op2);
  assert(z_get_si(diff) = OP_1 - OP_2);
end;

procedure TTestGmpExtensions.z_mul_test;
const
  OP_1 = -10;
  OP_2 = 1000;
var rop, op1, op2, prod: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op1, OP_1);
  z_init_set_si(op2, OP_2);
  z_mul(rop, op1, op2);
  assert(z_get_si(rop) = OP_1 * OP_2);
  prod := z_mul(op1, op2);
  assert(z_get_si(prod) = OP_1 * OP_2);
end;

procedure TTestGmpExtensions.z_mul_si_test;
const
  OP_1 = 10;
  OP_2 = -1000;
var rop, op1, prod: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op1, OP_1);
  z_mul_si(rop, op1, OP_2);
  assert(z_get_si(rop) = OP_1 * OP_2);
  prod := z_mul_si(op1, OP_2);
  assert(z_get_si(prod) = OP_1 * OP_2);
end;

procedure TTestGmpExtensions.z_mul_ui_test;
const
  OP_1 = -10;
  OP_2 = 1000;
var rop, op1, prod: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op1, OP_1);
  z_mul_ui(rop, op1, OP_2);
  assert(z_get_si(rop) = OP_1 * OP_2);
  prod := z_mul_ui(op1, OP_2);
  assert(z_get_si(prod) = OP_1 * OP_2);
end;

procedure TTestGmpExtensions.z_addmul_test;
const
  ROP_ = 10;
  OP_1 = -100;
  OP_2 = 1000;
var rop, op1, op2: MPInteger;
begin
  z_init_set_si(rop, ROP_);
  z_init_set_si(op1, OP_1);
  z_init_set_si(op2, OP_2);
  z_addmul(rop, op1, op2);
  assert(z_get_si(rop) = ROP_ + OP_1 * OP_2);
end;

procedure TTestGmpExtensions.z_addmul_ui_test;
const
  ROP_ = 10;
  OP_1 = -100;
  OP_2 = 1000;
var rop, op1: MPInteger;
begin
  z_init_set_si(rop, ROP_);
  z_init_set_si(op1, OP_1);
  z_addmul_ui(rop, op1, OP_2);
  assert(z_get_si(rop) = ROP_ + OP_1 * OP_2);
end;

procedure TTestGmpExtensions.z_submul_test;
const
  ROP_ = 10;
  OP_1 = -100;
  OP_2 = 1000;
var rop, op1, op2: MPInteger;
begin
  z_init_set_si(rop, ROP_);
  z_init_set_si(op1, OP_1);
  z_init_set_si(op2, OP_2);
  z_submul(rop, op1, op2);
  assert(z_get_si(rop) = ROP_ - OP_1 * OP_2);
end;

procedure TTestGmpExtensions.z_submul_ui_test;
const
  ROP_ = 10;
  OP_1 = -100;
  OP_2 = 1000;
var rop, op1: MPInteger;
begin
  z_init_set_si(rop, ROP_);
  z_init_set_si(op1, OP_1);
  z_submul_ui(rop, op1, OP_2);
  assert(z_get_si(rop) = ROP_ - OP_1 * OP_2);
end;

procedure TTestGmpExtensions.z_mul_2exp_test;
const
  OP_1 = 10;
  OP_2 = 10;
var rop, op1, x: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op1, OP_1);
  z_mul_2exp(rop, op1, OP_2);
  assert(z_get_si(rop) = OP_1 * (1 shl OP_2));
  x := z_mul_2exp(op1, OP_2);
  assert(z_get_si(x) = OP_1 * (1 shl OP_2));
end;

procedure TTestGmpExtensions.z_neg_test;
const OP_ = 1000;
var rop, op, x: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op, OP_);
  z_neg(rop, op);
  assert(z_get_si(rop) = -OP_);
  x := z_neg(op);
  assert(z_get_si(x) = -OP_);
end;

procedure TTestGmpExtensions.z_abs_test;
const OP_ = -1000;
var rop, op, x: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op, OP_);
  z_abs(rop, op);
  assert(z_get_si(rop) = abs(OP_));
  x := z_abs(op);
  assert(z_get_si(x) = abs(OP_));
end;

procedure TTestGmpExtensions.z_cdiv_q_test;
const
  N_ = -17;
  D_ = 4;
var q, n, d, q2: MPInteger;
begin
  z_init(q);
  z_init_set_si(n, N_);
  z_init_set_si(d, D_);
  z_cdiv_q(q, n, d);
  assert(z_get_si(q) = ceil(N_ / D_));
  q2 := z_cdiv_q(n, d);
  assert(z_get_si(q2) = ceil(N_ / D_));
end;

procedure TTestGmpExtensions.z_fdiv_q_test;
const
  N_ = -17;
  D_ = 4;
var q, n, d, q2: MPInteger;
begin
  z_init(q);
  z_init_set_si(n, N_);
  z_init_set_si(d, D_);
  z_fdiv_q(q, n, d);
  assert(z_get_si(q) = floor(N_ / D_));
  q2 := z_fdiv_q(n, d);
  assert(z_get_si(q2) = floor(N_ / D_));
end;

procedure TTestGmpExtensions.z_tdiv_q_test;
const
  N_ = -17;
  D_ = 4;
var q, n, d, q2: MPInteger;
begin
  z_init(q);
  z_init_set_si(n, N_);
  z_init_set_si(d, D_);
  z_tdiv_q(q, n, d);
  assert(z_get_si(q) = trunc(N_ / D_));
  q2 := z_tdiv_q(n, d);
  assert(z_get_si(q2) = trunc(N_ / D_));
end;

procedure TTestGmpExtensions.z_cdiv_q_2exp_test;
const
  N_ = -17;
  B_ = 2;
  D_ = 1 shl B_;
var q, n, q2: MPInteger;
begin
  z_init(q);
  z_init_set_si(n, N_);
  z_cdiv_q_2exp(q, n, B_);
  assert(z_get_si(q) = ceil(N_ / D_));
  q2 := z_cdiv_q_2exp(n, B_);
  assert(z_get_si(q2) = ceil(N_ / D_));
end;

procedure TTestGmpExtensions.z_fdiv_q_2exp_test;
const
  N_ = -17;
  B_ = 2;
  D_ = 1 shl B_;
var q, n, q2: MPInteger;
begin
  z_init(q);
  z_init_set_si(n, N_);
  z_fdiv_q_2exp(q, n, B_);
  assert(z_get_si(q) = floor(N_ / D_));
  q2 := z_fdiv_q_2exp(n, B_);
  assert(z_get_si(q2) = floor(N_ / D_));
end;

procedure TTestGmpExtensions.z_tdiv_q_2exp_test;
const
  N_ = -17;
  B_ = 2;
  D_ = 1 shl B_;
var q, n, q2: MPInteger;
begin
  z_init(q);
  z_init_set_si(n, N_);
  z_tdiv_q_2exp(q, n, B_);
  assert(z_get_si(q) = trunc(N_ / D_));
  q2 := z_tdiv_q_2exp(n, B_);
  assert(z_get_si(q2) = trunc(N_ / D_));
end;

procedure TTestGmpExtensions.z_cdiv_q_ui_test;
const
  N_ = -17;
  D_ = 4;
var
  q, n: MPInteger;
  ur: valuint;
begin
  z_init(q);
  z_init_set_si(n, N_);
  ur := z_cdiv_q_ui(q, n, D_);
  assert(z_get_si(q) = ceil(N_ / D_));
  assert(ur - abs(N_ - z_get_si(q) * D_) = 0);
end;

procedure TTestGmpExtensions.z_fdiv_q_ui_test;
const
  N_ = -17;
  D_ = 4;
var
  q, n: MPInteger;
  ur: valuint;
begin
  z_init(q);
  z_init_set_si(n, N_);
  ur := z_fdiv_q_ui(q, n, D_);
  assert(z_get_si(q) = floor(N_ / D_));
  assert(ur - abs(N_ - z_get_si(q) * D_) = 0);
end;

procedure TTestGmpExtensions.z_tdiv_q_ui_test;
const
  N_ = -17;
  D_ = 4;
var
  q, n: MPInteger;
  ur: valuint;
begin
  z_init(q);
  z_init_set_si(n, N_);
  ur := z_tdiv_q_ui(q, n, D_);
  assert(z_get_si(q) = trunc(N_ / D_));
  assert(ur - abs(N_ - z_get_si(q) * D_) = 0);
end;

procedure TTestGmpExtensions.z_cdiv_qr_test;
const
  N_ = -17;
  D_ = 4;
var q, r, n, d: MPInteger;
begin
  z_init(q);
  z_init(r);
  z_init_set_si(n, N_);
  z_init_set_si(d, D_);
  z_cdiv_qr(q, r, n, d);
  assert(z_get_si(q) = ceil(N_ / D_));
  assert(z_get_si(q) * D_ + z_get_si(r) = N_);
end;

procedure TTestGmpExtensions.z_fdiv_qr_test;
const
  N_ = -17;
  D_ = 4;
var q, r, n, d: MPInteger;
begin
  z_init(q);
  z_init(r);
  z_init_set_si(n, N_);
  z_init_set_si(d, D_);
  z_fdiv_qr(q, r, n, d);
  assert(z_get_si(q) = floor(N_ / D_));
  assert(z_get_si(q) * D_ + z_get_si(r) = N_);
end;

procedure TTestGmpExtensions.z_tdiv_qr_test;
const
  N_ = -17;
  D_ = 4;
var q, r, n, d: MPInteger;
begin
  z_init(q);
  z_init(r);
  z_init_set_si(n, N_);
  z_init_set_si(d, D_);
  z_tdiv_qr(q, r, n, d);
  assert(z_get_si(q) = trunc(N_ / D_));
  assert(z_get_si(q) * D_ + z_get_si(r) = N_);
end;

procedure TTestGmpExtensions.z_cdiv_qr_ui_test;
const
  N_ = -17;
  D_ = 4;
var
  q, r, n: MPInteger;
  ur: valuint;
begin
  z_init(q);
  z_init(r);
  z_init_set_si(n, N_);
  ur := z_cdiv_qr_ui(q, r, n, D_);
  assert(z_get_si(q) = ceil(N_ / D_));
  assert(z_get_si(q) * D_ + z_get_si(r) = N_);
  assert(ur - abs(z_get_si(r)) = 0);
end;

procedure TTestGmpExtensions.z_fdiv_qr_ui_test;
const
  N_ = -17;
  D_ = 4;
var
  q, r, n: MPInteger;
  ur: valuint;
begin
  z_init(q);
  z_init(r);
  z_init_set_si(n, N_);
  ur := z_fdiv_qr_ui(q, r, n, D_);
  assert(z_get_si(q) = floor(N_ / D_));
  assert(z_get_si(q) * D_ + z_get_si(r) = N_);
  assert(ur - abs(z_get_si(r)) = 0);
end;

procedure TTestGmpExtensions.z_tdiv_qr_ui_test;
const
  N_ = -17;
  D_ = 4;
var
  q, r, n: MPInteger;
  ur: valuint;
begin
  z_init(q);
  z_init(r);
  z_init_set_si(n, N_);
  ur := z_tdiv_qr_ui(q, r, n, D_);
  assert(z_get_si(q) = trunc(N_ / D_));
  assert(z_get_si(q) * D_ + z_get_si(r) = N_);
  assert(ur - abs(z_get_si(r)) = 0);
end;

procedure TTestGmpExtensions.z_cdiv_r_test;
const
  N_ = -17;
  D_ = 4;
var r, n, d, r2: MPInteger;
begin
  z_init(r);
  z_init_set_si(n, N_);
  z_init_set_si(d, D_);
  z_cdiv_r(r, n, d);
  assert(ceil(N_ / D_) * D_ + z_get_si(r) = N_);
  r2 := z_cdiv_r(n, d);
  assert(ceil(N_ / D_) * D_ + z_get_si(r2) = N_);
end;

procedure TTestGmpExtensions.z_fdiv_r_test;
const
  N_ = -17;
  D_ = 4;
var r, n, d, r2: MPInteger;
begin
  z_init(r);
  z_init_set_si(n, N_);
  z_init_set_si(d, D_);
  z_fdiv_r(r, n, d);
  assert(floor(N_ / D_) * D_ + z_get_si(r) = N_);
  r2 := z_fdiv_r(n, d);
  assert(floor(N_ / D_) * D_ + z_get_si(r2) = N_);
end;

procedure TTestGmpExtensions.z_tdiv_r_test;
const
  N_ = -17;
  D_ = 4;
var r, n, d, r2: MPInteger;
begin
  z_init(r);
  z_init_set_si(n, N_);
  z_init_set_si(d, D_);
  z_tdiv_r(r, n, d);
  assert(trunc(N_ / D_) * D_ + z_get_si(r) = N_);
  r2 := z_tdiv_r(n, d);
  assert(trunc(N_ / D_) * D_ + z_get_si(r2) = N_);
end;

procedure TTestGmpExtensions.z_cdiv_r_2exp_test;
const
  N_ = -17;
  B_ = 2;
  D_ = 1 shl B_;
var r, n, r2: MPInteger;
begin
  z_init(r);
  z_init_set_si(n, N_);
  z_cdiv_r_2exp(r, n, B_);
  assert(ceil(N_ / D_) * D_ + z_get_si(r) = N_);
  r2 := z_cdiv_r_2exp(n, B_);
  assert(ceil(N_ / D_) * D_ + z_get_si(r2) = N_);
end;

procedure TTestGmpExtensions.z_fdiv_r_2exp_test;
const
  N_ = -17;
  B_ = 2;
  D_ = 1 shl B_;
var r, n, r2: MPInteger;
begin
  z_init(r);
  z_init_set_si(n, N_);
  z_fdiv_r_2exp(r, n, B_);
  assert(floor(N_ / D_) * D_ + z_get_si(r) = N_);
  r2 := z_fdiv_r_2exp(n, B_);
  assert(floor(N_ / D_) * D_ + z_get_si(r2) = N_);
end;

procedure TTestGmpExtensions.z_tdiv_r_2exp_test;
const
  N_ = -17;
  B_ = 2;
  D_ = 1 shl B_;
var r, n, r2: MPInteger;
begin
  z_init(r);
  z_init_set_si(n, N_);
  z_tdiv_r_2exp(r, n, B_);
  assert(trunc(N_ / D_) * D_ + z_get_si(r) = N_);
  r2 := z_tdiv_r_2exp(n, B_);
  assert(trunc(N_ / D_) * D_ + z_get_si(r2) = N_);
end;

procedure TTestGmpExtensions.z_cdiv_r_ui_test;
const
  N_ = -17;
  D_ = 4;
var
  r, n: MPInteger;
  ur: valuint;
begin
  z_init(r);
  z_init_set_si(n, N_);
  ur := z_cdiv_r_ui(r, n, D_);
  assert(ceil(N_ / D_) * D_ + z_get_si(r) = N_);
  assert(ur - abs(z_get_si(r)) = 0);
end;

procedure TTestGmpExtensions.z_fdiv_r_ui_test;
const
  N_ = -17;
  D_ = 4;
var
  r, n: MPInteger;
  ur: valuint;
begin
  z_init(r);
  z_init_set_si(n, N_);
  ur := z_fdiv_r_ui(r, n, D_);
  assert(floor(N_ / D_) * D_ + z_get_si(r) = N_);
  assert(ur - abs(z_get_si(r)) = 0);
end;

procedure TTestGmpExtensions.z_tdiv_r_ui_test;
const
  N_ = -17;
  D_ = 4;
var
  r, n: MPInteger;
  ur: valuint;
begin
  z_init(r);
  z_init_set_si(n, N_);
  ur := z_tdiv_r_ui(r, n, D_);
  assert(trunc(N_ / D_) * D_ + z_get_si(r) = N_);
  assert(ur - abs(z_get_si(r)) = 0);
end;

procedure TTestGmpExtensions.z_cdiv_ui_test;
const
  N_ = -17;
  D_ = 4;
var n: MPInteger;
begin
  z_init_set_si(n, N_);
  assert(z_cdiv_ui(n, D_) - abs(N_ - ceil(N_ / D_) * D_) = 0);
end;

procedure TTestGmpExtensions.z_fdiv_ui_test;
const
  N_ = -17;
  D_ = 4;
var n: MPInteger;
begin
  z_init_set_si(n, N_);
  assert(z_fdiv_ui(n, D_) - abs(N_ - floor(N_ / D_) * D_) = 0);
end;

procedure TTestGmpExtensions.z_tdiv_ui_test;
const
  N_ = -17;
  D_ = 4;
var n: MPInteger;
begin
  z_init_set_si(n, N_);
  assert(z_tdiv_ui(n, D_) = abs(N_ - trunc(N_ / D_) * D_));
end;

procedure TTestGmpExtensions.z_mod_test;
const
  N_ = 17;
  D_ = -4;
var r, n, d, r2: MPInteger;
begin
  z_init(r);
  z_init_set_si(n, N_);
  z_init_set_si(d, D_);
  z_mod(r, n, d);
  assert(z_get_si(r) = N_ mod abs(D_));
  r2 := z_mod(n, d);
  assert(z_get_si(r2) = N_ mod abs(D_));
end;

procedure TTestGmpExtensions.z_divexact_test;
const
  N_ = -16;
  D_ = 4;
var q, n, d, q2: MPInteger;
begin
  z_init(q);
  z_init_set_si(n, N_);
  z_init_set_si(d, D_);
  z_divexact(q, n, d);
  assert(z_get_si(q) * D_ = N_);
  q2 := z_divexact(n, d);
  assert(z_get_si(q2) * D_ = N_);
end;

procedure TTestGmpExtensions.z_divexact_ui_test;
const
  N_ = -16;
  D_ = 4;
var q, n, q2: MPInteger;
begin
  z_init(q);
  z_init_set_si(n, N_);
  z_divexact_ui(q, n, D_);
  assert(z_get_si(q) * D_ = N_);
  q2 := z_divexact_ui(n, D_);
  assert(z_get_si(q2) * D_ = N_);
end;

procedure TTestGmpExtensions.z_divisible_p_test;
const
  N_ = -16;
  D_ = 4;
var n, d: MPInteger;
begin
  z_init_set_si(n, N_);
  z_init_set_si(d, D_);
  assert(z_divisible_p(n, d));
  z_set_si(n, z_get_si(n) + 1);
  assert(not z_divisible_p(n, d));
end;

procedure TTestGmpExtensions.z_divisible_ui_p_test;
const
  N_ = -16;
  D_ = 4;
var n: MPInteger;
begin
  z_init_set_si(n, N_);
  assert(z_divisible_ui_p(n, D_));
  z_set_si(n, z_get_si(n) + 1);
  assert(not z_divisible_ui_p(n, D_));
end;

procedure TTestGmpExtensions.z_divisible_2exp_p_test;
const
  N_ = -16;
  B_ = 2;
var n: MPInteger;
begin
  z_init_set_si(n, N_);
  assert(z_divisible_2exp_p(n, B_));
  z_set_si(n, z_get_si(n) + 1);
  assert(not z_divisible_ui_p(n, B_));
end;

procedure TTestGmpExtensions.z_congruent_p_test;
const
  N_ = 23;
  C_ = 6;
  D_ = 17;
var n, c, d: MPInteger;
begin
  z_init_set_si(n, N_);
  z_init_set_si(c, C_);
  z_init_set_si(d, D_);
  assert(z_congruent_p(n, c, d));
  z_set_si(n, z_get_si(n) + 1);
  assert(not z_congruent_p(n, c, d));
end;

procedure TTestGmpExtensions.z_congruent_2exp_p_test;
const
  N_ = 23;
  C_ = 7;
  B_ = 4;
var n, c: MPInteger;
begin
  z_init_set_si(n, N_);
  z_init_set_si(c, C_);
  assert(z_congruent_2exp_p(n, c, B_));
  z_set_si(n, z_get_si(n) + 1);
  assert(not z_congruent_2exp_p(n, c, B_));
end;

procedure TTestGmpExtensions.z_congruent_ui_p_test;
const
  N_ = 23;
  C_ = 6;
  D_ = 17;
var n: MPInteger;
begin
  z_init_set_si(n, N_);
  assert(z_congruent_ui_p(n, C_, D_));
  z_set_si(n, z_get_si(n) + 1);
  assert(not z_congruent_ui_p(n, C_, D_));
end;

procedure TTestGmpExtensions.z_powm_test;
const
  N_EXP = 3;
  N_MOD = 16;
  ROP_ = 8;
var rop, base, exp, mod_, r2: MPInteger;
begin
  z_init(rop);
  z_init_set_si(base, BASE10);
  z_init_set_si(exp, N_EXP);
  z_init_set_si(mod_, N_MOD);
  z_powm(rop, base, exp, mod_);
  assert(z_get_si(rop) = ROP_);
  r2 := z_powm(base, exp, mod_);
  assert(z_get_si(r2) = ROP_);
end;

procedure TTestGmpExtensions.z_powm_ui_test;
const
  N_EXP = 3;
  N_MOD = 16;
  ROP_ = 8;
var rop, base, mod_, r2: MPInteger;
begin
  z_init(rop);
  z_init_set_si(base, BASE10);
  z_init_set_si(mod_, N_MOD);
  z_powm_ui(rop, base, N_EXP, mod_);
  assert(z_get_si(rop) = ROP_);
  r2 := z_powm_ui(base, N_EXP, mod_);
  assert(z_get_si(r2) = ROP_);
end;

procedure TTestGmpExtensions.z_pow_ui_test;
const
  BASE_ = 10;
  EXP = 3;
var rop, base, r2: MPInteger;
begin
  z_init(rop);
  z_init_set_si(base, BASE_);
  z_pow_ui(rop, base, EXP);
  assert(z_get_si(rop) = BASE_ ** EXP);
  r2 := z_pow_ui(base, EXP);
  assert(z_get_si(r2) = BASE_ ** EXP);
end;

procedure TTestGmpExtensions.z_ui_pow_ui_test;
const
  BASE_ = 10;
  EXP = 3;
var rop, r2: MPInteger;
begin
  z_init(rop);
  z_ui_pow_ui(rop, BASE_, EXP);
  assert(z_get_si(rop) = BASE_ ** EXP);
  r2 := z_ui_pow_ui(BASE_, EXP);
  assert(z_get_si(r2) = BASE_ ** EXP);
end;

procedure TTestGmpExtensions.z_root_test;
const
  ROOT = 5;
  N_ = 3;
var rop, op: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op, ROOT ** N_);
  assert(z_root(rop, op, N_));
  assert(z_get_si(rop) = ROOT);
  z_set_si(op, ROOT ** N_ + 1);
  assert(not z_root(rop, op, N_));
  assert(z_get_si(rop) = ROOT);
end;

procedure TTestGmpExtensions.z_rootrem_test;
const
  ROOT_ = 5;
  N_ = 3;
  REM_ = 1;
var root, rem, u: MPInteger;
begin
  assert(REM_ < ROOT_);
  z_init(root);
  z_init(rem);
  z_init_set_si(u, ROOT_ ** N_);
  z_rootrem(root, rem, u, N_);
  assert(z_get_si(root) = ROOT_);
  assert(z_get_si(rem) = 0);
  z_set_si(u, ROOT_ ** N_ + REM_);
  z_rootrem(root, rem, u, N_);
  assert(z_get_si(root) = ROOT_);
  assert(z_get_si(rem) = REM_);
end;

procedure TTestGmpExtensions.z_sqrt_test;
const ROOT = 5;
var rop, op, r2: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op, ROOT ** 2);
  z_sqrt(rop, op);
  assert(z_get_si(rop) = ROOT);
  r2 := z_sqrt(op);
  assert(z_get_si(r2) = ROOT);
end;

procedure TTestGmpExtensions.z_sqrtrem_test;
const
  ROOT = 5;
  REM = 1;
var rop1, rop2, op: MPInteger;
begin
  z_init(rop1);
  z_init(rop2);
  z_init_set_si(op, ROOT ** 2 + REM);
  z_sqrtrem(rop1, rop2, op);
  assert(z_get_si(rop1) = ROOT);
  assert(z_get_si(rop2) = REM);
end;

procedure TTestGmpExtensions.z_perfect_power_p_test;
const
  ROOT = 3;
  EXP = 3;
var op: MPInteger;
begin
  z_init_set_si(op, ROOT ** EXP);
  assert(z_perfect_power_p(op));
  z_set_si(op, ROOT ** EXP + 1);
  assert(not z_perfect_power_p(op));
end;

procedure TTestGmpExtensions.z_perfect_square_p_test;
const ROOT = 3;
var op: MPInteger;
begin
  z_init_set_si(op, ROOT ** 2);
  assert(z_perfect_square_p(op));
  z_set_si(op, ROOT ** 2 or 2);
  assert(not z_perfect_square_p(op));
end;

procedure TTestGmpExtensions.z_probab_prime_p_test;
const
  N_ = 127;
  REPS = 5;
var n: MPInteger;
begin
  z_init_set_si(n, N_);
  assert(z_probab_prime_p(n, REPS) = 2);
  z_set_si(n, N_ + 1);
  assert(z_probab_prime_p(n, REPS) = 0);
end;

procedure TTestGmpExtensions.z_nextprime_test;
const
  PRIME3 = 5;
  PRIME4 = 7;
var rop, op, r2: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op, PRIME3);
  z_nextprime(rop, op);
  assert(z_get_si(rop) = PRIME4);
  r2 := z_nextprime(op);
  assert(z_get_si(r2) = PRIME4);
end;

procedure TTestGmpExtensions.z_gcd_test;
const
  OP_1 = 42;
  OP_2 = 56;
  ROP_ = 14;
var rop, op1, op2, r2: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op1, OP_1);
  z_init_set_si(op2, OP_2);
  z_gcd(rop, op1, op2);
  assert(z_get_si(rop) = ROP_);
  r2 := z_gcd(op1, op2);
  assert(z_get_si(r2) = ROP_);
end;

procedure TTestGmpExtensions.z_gcd_ui_test;
const
  OP_1 = 42;
  OP_2 = 56;
  ROP_ = 14;
var rop, op1: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op1, OP_1);
  assert(z_gcd_ui(rop, op1, OP_2) = ROP_);
  assert(z_get_si(rop) = ROP_);
end;

procedure TTestGmpExtensions.z_gcdext_test;
const
  A_ = 42;
  B_ = 56;
  G_ = 14;
var g, s, t, a, b: MPInteger;
begin
  z_init(g);
  z_init(s);
  z_init(t);
  z_init_set_si(a, A_);
  z_init_set_si(b, B_);
  z_gcdext(g, s, t, a, b);
  assert(z_get_si(g) = G_);
  assert(A_ * z_get_si(s) + B_ * z_get_si(t) = G_);
end;

procedure TTestGmpExtensions.z_lcm_test;
const
  OP_1 = 21;
  OP_2 = 6;
  ROP_ = 42;
var rop, op1, op2, r2: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op1, OP_1);
  z_init_set_si(op2, OP_2);
  z_lcm(rop, op1, op2);
  assert(z_get_si(rop) = ROP_);
  r2 := z_lcm(op1, op2);
  assert(z_get_si(r2) = ROP_);
end;

procedure TTestGmpExtensions.z_lcm_ui_test;
const
  OP_1 = 21;
  OP_2 = 6;
  ROP_ = 42;
var rop, op1, r2: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op1, OP_1);
  z_lcm_ui(rop, op1, OP_2);
  assert(z_get_si(rop) = ROP_);
  r2 := z_lcm_ui(op1, OP_2);
  assert(z_get_si(r2) = ROP_);
end;

procedure TTestGmpExtensions.z_invert_test;
const
  OP_1 = 256;
  OP_2 = 337;
  ROP_ = 104;
var rop, op1, op2: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op1, OP_1);
  z_init_set_si(op2, OP_2);
  assert(z_invert(rop, op1, op2) <> 0);
  assert(z_get_si(rop) = ROP_);
end;

procedure TTestGmpExtensions.z_jacobi_test;
const
  B0 = 11;
  A0 = 13 * B0;
  AM1 = 1001;
  BM1 = 9907;
  A1 = 4;
  B1 = 7;
var a, b: MPInteger;
begin
  z_init_set_si(a, A0);
  z_init_set_si(b, B0);
  assert(z_jacobi(a, b) = 0);
  z_set_si(a, AM1);
  z_set_si(b, BM1);
  assert(z_jacobi(a, b) = -1);
  z_set_si(a, A1);
  z_set_si(b, B1);
  assert(z_jacobi(a, b) = 1);
end;

procedure TTestGmpExtensions.z_kronecker_si_test;
const
  A_ = 8;
  B0 = 6;
  BM1 = 5;
  B1 = 7;
var a: MPInteger;
begin
  z_init_set_si(a, A_);
  assert(z_kronecker_si(a, B1) = 1);
  assert(z_kronecker_si(a, B0) = 0);
  assert(z_kronecker_si(a, BM1) = -1);
end;

procedure TTestGmpExtensions.z_kronecker_ui_test;
const
  A_ = 8;
  B0 = 6;
  BM1 = 5;
  B1 = 7;
var a: MPInteger;
begin
  z_init_set_si(a, A_);
  assert(z_kronecker_ui(a, B1) = 1);
  assert(z_kronecker_ui(a, B0) = 0);
  assert(z_kronecker_ui(a, BM1) = -1);
end;

procedure TTestGmpExtensions.z_si_kronecker_test;
const
  A_ = 8;
  B0 = 6;
  BM1 = 5;
  B1 = 7;
var b: MPInteger;
begin
  z_init_set_si(b, B1);
  assert(z_si_kronecker(A_, b) = 1);
  z_set_si(b, B0);
  assert(z_si_kronecker(A_, b) = 0);
  z_set_si(b, BM1);
  assert(z_si_kronecker(A_, b) = -1);
end;

procedure TTestGmpExtensions.z_ui_kronecker_test;
const
  A_ = 8;
  B0 = 6;
  BM1 = 5;
  B1 = 7;
var b: MPInteger;
begin
  z_init_set_si(b, B1);
  assert(z_ui_kronecker(A_, b) = 1);
  z_set_si(b, B0);
  assert(z_ui_kronecker(A_, b) = 0);
  z_set_si(b, BM1);
  assert(z_ui_kronecker(A_, b) = -1);
end;

procedure TTestGmpExtensions.z_remove_test;
const
  F_ = 5;
  OP_ = 3 * F_ * 7;
  ROP_ = OP_ div F_;
var rop, op, f: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op, OP_);
  z_init_set_si(f, F_);
  assert(z_remove(rop, op, f) = 1);
  assert(z_get_si(rop) = ROP_);
end;

procedure TTestGmpExtensions.z_fac_ui_test;
const
  OP = 4;
  ROP_ = 2 * 3 * 4;
var rop, r2: MPInteger;
begin
  z_init(rop);
  z_fac_ui(rop, OP);
  assert(z_get_si(rop) = ROP_);
  r2 := z_fac_ui(OP);
  assert(z_get_si(r2) = ROP_);
end;

procedure TTestGmpExtensions.z_bin_ui_test;
const
  N_ = 7;
  K = 3;
  ROP_ = 35;
var rop, n, r2: MPInteger;
begin
  z_init(rop);
  z_init_set_si(n, N_);
  z_bin_ui(rop, n, K);
  assert(z_get_si(rop) = ROP_);
  r2 := z_bin_ui(n, K);
  assert(z_get_si(r2) = ROP_);
end;

procedure TTestGmpExtensions.z_bin_uiui_test;
const
  N = 7;
  K = 3;
  ROP_ = 35;
var rop, r2: MPInteger;
begin
  z_init(rop);
  z_bin_uiui(rop, N, K);
  assert(z_get_si(rop) = ROP_);
  r2 := z_bin_uiui(N, K);
  assert(z_get_si(r2) = ROP_);
end;

procedure TTestGmpExtensions.z_fib_ui_test;
const
  N = 10;
  FN_ = 55;
var fn, ff: MPInteger;
begin
  z_init(fn);
  z_fib_ui(fn, N);
  assert(z_get_si(fn) = FN_);
  ff := z_fib_ui(N);
  assert(z_get_si(ff) = FN_);
end;

procedure TTestGmpExtensions.z_fib2_ui_test;
const
  N = 10;
  FN_ = 55;
  FNSUB1_ = 34;
var fn, fnsub1, ff, ff1: MPInteger;
begin
  z_init(fn);
  z_init(fnsub1);
  z_fib2_ui(fn, fnsub1, N);
  assert(z_get_si(fn) = FN_);
  assert(z_get_si(fnsub1) = FNSUB1_);
  z_init(ff1);
  ff := z_fib2_ui(ff1, N);
  assert(z_get_si(ff) = FN_);
  assert(z_get_si(ff1) = FNSUB1_);
end;

procedure TTestGmpExtensions.z_lucnum_ui_test;
const
  N = 10;
  LN_ = 123;
var ln, l2: MPInteger;
begin
  z_init(ln);
  z_lucnum_ui(ln, N);
  assert(z_get_si(ln) = LN_);
  l2 := z_lucnum_ui(N);
  assert(z_get_si(l2) = LN_);
end;

procedure TTestGmpExtensions.z_lucnum2_ui_test;
const
  N = 10;
  LN_ = 123;
  LNSUB1_ = 76;
var ln, lnsub1, l2, l2s1: MPInteger;
begin
  z_init(ln);
  z_init(lnsub1);
  z_lucnum2_ui(ln, lnsub1, N);
  assert(z_get_si(ln) = LN_);
  assert(z_get_si(lnsub1) = LNSUB1_);
  z_init(l2s1);
  l2 := z_lucnum2_ui(l2s1, N);
  assert(z_get_si(l2) = LN_);
  assert(z_get_si(l2s1) = LNSUB1_);
end;

procedure TTestGmpExtensions.z_cmp_test;
const
  OP_1 = -10;
  OP_2 = OP_1 - 1;
var op1, op2: MPInteger;
begin
  z_init_set_si(op1, OP_1);
  z_init_set_si(op2, OP_2);
  assert(z_cmp(op1, op2) > 0);
  z_set_si(op2, OP_1);
  assert(z_cmp(op1, op2) = 0);
  z_set_si(op2, OP_1 + 1);
  assert(z_cmp(op1, op2) < 0);
end;

procedure TTestGmpExtensions.z_cmp_d_test;
const
  OP_1 = -10;
var op1: MPInteger;
begin
  z_init_set_si(op1, OP_1);
  assert(z_cmp_d(op1, OP_1 - 1) > 0);
  assert(z_cmp_d(op1, OP_1) = 0);
  assert(z_cmp_d(op1, OP_1 + 1) < 0);
end;

procedure TTestGmpExtensions.z_cmp_si_test;
const
  OP_1 = -10;
var op1: MPInteger;
begin
  z_init_set_si(op1, OP_1);
  assert(z_cmp_si(op1, OP_1 - 1) > 0);
  assert(z_cmp_si(op1, OP_1) = 0);
  assert(z_cmp_si(op1, OP_1 + 1) < 0);
end;

procedure TTestGmpExtensions.z_cmp_ui_test;
const
  OP_1 = 10;
var op1: MPInteger;
begin
  z_init_set_si(op1, OP_1);
  assert(z_cmp_ui(op1, OP_1 - 1) > 0);
  assert(z_cmp_ui(op1, OP_1) = 0);
  assert(z_cmp_ui(op1, OP_1 + 1) < 0);
end;

procedure TTestGmpExtensions.z_cmpabs_test;
const
  OP_1 = -10;
  OP_2 = -OP_1 - 1;
var op1, op2: MPInteger;
begin
  z_init_set_si(op1, OP_1);
  z_init_set_si(op2, -OP_2);
  assert(z_cmpabs(op1, op2) > 0);
  z_set_si(op2, -OP_1);
  assert(z_cmpabs(op1, op2) = 0);
  z_set_si(op2, -OP_1 + 1);
  assert(z_cmpabs(op1, op2) < 0);
end;

procedure TTestGmpExtensions.z_cmpabs_d_test;
const OP_1 = -10;
var op1: MPInteger;
begin
  z_init_set_si(op1, OP_1);
  assert(z_cmpabs_d(op1, -OP_1 - 1) > 0);
  assert(z_cmpabs_d(op1, -OP_1) = 0);
  assert(z_cmpabs_d(op1, -OP_1 + 1) < 0);
end;

procedure TTestGmpExtensions.z_cmpabs_ui_test;
const OP_1 = -10;
var op1: MPInteger;
begin
  z_init_set_si(op1, OP_1);
  assert(z_cmpabs_ui(op1, -OP_1 - 1) > 0);
  assert(z_cmpabs_ui(op1, -OP_1) = 0);
  assert(z_cmpabs_ui(op1, -OP_1 + 1) < 0);
end;

procedure TTestGmpExtensions.z_and_test;
const
  OP_1 = $c;
  OP_2 = $a;
var rop, op1, op2, r2: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op1, OP_1);
  z_init_set_si(op2, OP_2);
  z_and(rop, op1, op2);
  assert(z_get_si(rop) = OP_1 and OP_2);
  r2 := z_and(op1, op2);
  assert(z_get_si(r2) = OP_1 and OP_2);
end;

procedure TTestGmpExtensions.z_ior_test;
const
  OP_1 = $c;
  OP_2 = $a;
var rop, op1, op2, r2: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op1, OP_1);
  z_init_set_si(op2, OP_2);
  z_ior(rop, op1, op2);
  assert(z_get_si(rop) = OP_1 or OP_2);
  r2 := z_ior(op1, op2);
  assert(z_get_si(r2) = OP_1 or OP_2);
end;

procedure TTestGmpExtensions.z_xor_test;
const
  OP_1 = $c;
  OP_2 = $a;
var rop, op1, op2, r2: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op1, OP_1);
  z_init_set_si(op2, OP_2);
  z_xor(rop, op1, op2);
  assert(z_get_si(rop) = OP_1 xor OP_2);
  r2 := z_xor(op1, op2);
  assert(z_get_si(r2) = OP_1 xor OP_2);
end;

procedure TTestGmpExtensions.z_com_test;
const
  OP_1 = 2;
var rop, op1, r2: MPInteger;
begin
  z_init(rop);
  z_init_set_si(op1, OP_1);
  z_com(rop, op1);
  assert(z_get_si(rop) = not OP_1 );
  r2 := z_com(op1);
  assert(z_get_si(r2) = not OP_1 );
end;

procedure TTestGmpExtensions.z_popcount_test;
const
  OP_ = $55AA;
  N = 8;
var op: MPInteger;
begin
  z_init_set_si(op, OP_);
  assert(z_popcount(op) = N);
end;

procedure TTestGmpExtensions.z_hamdist_test;
const
  OP_1 = $55;
  OP_2 = $7A;
  N = 5;
var op1, op2: MPInteger;
begin
  z_init_set_si(op1, OP_1);
  z_init_set_si(op2, OP_2);
  assert(z_hamdist(op1, op2) = N);
end;

procedure TTestGmpExtensions.z_scan0_test;
const
  OP_ = $F75;
  STARTING_BIT = 4;
  N = 7;
var op: MPInteger;
begin
  z_init_set_si(op, OP_);
  assert(z_scan0(op, STARTING_BIT) = N);
end;

procedure TTestGmpExtensions.z_scan1_test;
const
  OP_ = $85;
  STARTING_BIT = 4;
  N = 7;
var op: MPInteger;
begin
  z_init_set_si(op, OP_);
  assert(z_scan1(op, STARTING_BIT) = N);
end;

procedure TTestGmpExtensions.z_setbit_test;
const
  ROP_ = $F75;
  BIT_INDEX = 7;
var rop: MPInteger;
begin
  z_init_set_ui(rop, ROP_);
  z_setbit(rop, BIT_INDEX);
  assert(z_get_ui(rop) = ROP_ or (1 shl BIT_INDEX));
end;

procedure TTestGmpExtensions.z_clrbit_test;
const
  ROP_ = $F75;
  BIT_INDEX = 9;
var rop: MPInteger;
begin
  z_init_set_ui(rop, ROP_);
  z_clrbit(rop, BIT_INDEX);
  assert(z_get_ui(rop) = ROP_ and not (1 shl BIT_INDEX));
end;

procedure TTestGmpExtensions.z_combit_test;
const
  ROP_ = $F75;
  BIT_INDEX = 7;
var rop: MPInteger;
begin
  z_init_set_ui(rop, ROP_);
  z_combit(rop, BIT_INDEX);
  assert(z_get_ui(rop) = ROP_ xor (1 shl BIT_INDEX));
end;

procedure TTestGmpExtensions.z_tstbit_test;
const
  ROP_ = $F75;
  BIT_INDEX1 = 7;
  BIT_INDEX2 = 8;
var rop: MPInteger;
begin
  z_init_set_ui(rop, ROP_);
  assert(z_tstbit(rop, BIT_INDEX1) = longbool(ROP_ and (1 shl BIT_INDEX1)));
  assert(z_tstbit(rop, BIT_INDEX2) = longbool(ROP_ and (1 shl BIT_INDEX2)));
end;

procedure TTestGmpExtensions.z_urandomb_test;
const
  B = 8 * (sizeof(valuint) - 1);
  N = 1 shl B;
var
  rop, r2: MPInteger;
  state: MPRandState;
begin
  randinit_default(state);
  z_init(rop);
  z_urandomb(rop, state, B);
  assert(z_cmp_ui(rop, N) < 0);
  r2 := z_urandomb(state, B);
  assert(z_cmp_ui(r2, N) < 0);
end;

procedure TTestGmpExtensions.z_urandomm_test;
const
  N_ = 1000000;
var
  rop, n, r2: MPInteger;
  state: MPRandState;
begin
  randinit_default(state);
  z_init(rop);
  z_init_set_ui(n, N_);
  z_urandomm(rop, state, n);
  assert(z_cmp_ui(rop, N_) < 0);
  r2 := z_urandomm(state, n);
  assert(z_cmp_ui(r2, N_) < 0);
end;

procedure TTestGmpExtensions.z_rrandomb_test;
const
  B = 8 * sizeof(valuint) - 2;
  N = 1 shl B;
var
  rop, r2: MPInteger;
  state: MPRandState;
begin
  randinit_default(state);
  z_init(rop);
  z_rrandomb(rop, state, B);
  assert(z_cmp_ui(rop, N) < 0);
  r2 := z_rrandomb(state, B);
  assert(z_cmp_ui(r2, N) < 0);
end;

procedure TTestGmpExtensions.z_fits_ushort_p_test;
var op: MPInteger;
begin
  z_init_set_ui(op, high(word));
  assert(z_fits_ushort_p(op));
  z_add_ui(op, op, 1);
  assert(not z_fits_ushort_p(op));
end;

procedure TTestGmpExtensions.z_fits_sshort_p_test;
var op: MPInteger;
begin
  z_init_set_si(op, high(smallint));
  assert(z_fits_sshort_p(op));
  z_add_ui(op, op, 1);
  assert(not z_fits_sshort_p(op));
end;

procedure TTestGmpExtensions.z_fits_uint_p_test;
var op: MPInteger;
begin
  z_init_set_ui(op, high(cardinal));
  assert(z_fits_uint_p(op));
  z_add_ui(op, op, 1);
  assert(not z_fits_uint_p(op));
end;

procedure TTestGmpExtensions.z_fits_sint_p_test;
var op: MPInteger;
begin
  z_init_set_si(op, high(integer));
  assert(z_fits_sint_p(op));
  z_add_ui(op, op, 1);
  assert(not z_fits_sint_p(op));
end;

procedure TTestGmpExtensions.z_fits_ulong_p_test;
var op: MPInteger;
begin
  z_init_set_ui(op, high(valuint));
  assert(z_fits_ulong_p(op));
  z_add_ui(op, op, 1);
  assert(not z_fits_ulong_p(op));
end;

procedure TTestGmpExtensions.z_fits_slong_p_test;
var op: MPInteger;
begin
  z_init_set_si(op, high(valsint));
  assert(z_fits_slong_p(op));
  z_add_ui(op, op, 1);
  assert(not z_fits_slong_p(op));
end;

procedure TTestGmpExtensions.z_sizeinbase_test;
const
  OP_ = 1000000;
var rop: MPInteger;
begin
  z_init_set_ui(rop, OP_);
  assert(z_sizeinbase(rop, BASE10) - length(inttostr(OP_)) = 0);
end;

procedure TTestGmpExtensions.z_size_test;
var rop: MPInteger;
begin
  z_init(rop);
  assert(z_size(rop) = 0);
  z_add_ui(rop, rop, 1);
  assert(z_size(rop) = 1);
end;

procedure TTestGmpExtensions.q_init_test;
var rational: MPRational;
begin
  q_init(rational);
  assert(q_cmp_ui(rational, 0, 1) = 0);
end;

procedure TTestGmpExtensions.q_clear_test;
var rational: MPRational;
begin
  q_init(rational);
  q_clear(rational);
end;

procedure TTestGmpExtensions.q_set_ui_test;
const
  OP_1 = 13;
  OP_2 = 17;
var rational: MPRational;
begin
  q_init(rational);
  q_set_ui(rational, OP_1, OP_2);
  assert(q_cmp_ui(rational, OP_1, OP_2) = 0);
end;

procedure TTestGmpExtensions.q_set_si_test;
const
  OP_1 = -13;
  OP_2 = 17;
var rational: MPRational;
begin
  q_init(rational);
  q_set_si(rational, OP_1, OP_2);
  assert(q_cmp_si(rational, OP_1, OP_2) = 0);
end;

procedure TTestGmpExtensions.q_canonicalize_test;
const
  OP_1 = -13;
  OP_2 = 17;
  CF = 11;
var rational: MPRational;
begin
  q_init(rational);
  q_set_si(rational, CF * OP_1, CF * OP_2);
  q_canonicalize(rational);
  assert(q_cmp_si(rational, OP_1, OP_2) = 0);
end;

procedure TTestGmpExtensions.q_set_test;
const
  OP_1 = -13;
  OP_2 = 17;
var rational1, rational2: MPRational;
begin
  q_init(rational1);
  q_init(rational2);
  q_set_si(rational2, OP_1, OP_2);
  q_set(rational1, rational2);
  assert(q_cmp_si(rational1, OP_1, OP_2) = 0);
end;

procedure TTestGmpExtensions.q_set_z_test;
const
  OP_1 = -13;
var
  rational: MPRational;
  op: MPInteger;
begin
  q_init(rational);
  z_init_set_si(op, OP_1);
  q_set_z(rational, op);
  assert(q_cmp_si(rational, OP_1, 1) = 0);
end;

procedure TTestGmpExtensions.q_set_str_test;
const
  OP_1 = -13;
  OP_2 = 17;
var rational: MPRational;
begin
  q_init(rational);
  assert(q_set_str(rational, pchar(inttostr(OP_1) + '/' + inttostr(OP_2)), BASE10));
  assert(q_cmp_si(rational, OP_1, OP_2) = 0);
  assert(not q_set_str(rational, pchar(inttostr(OP_1) + '*' + inttostr(OP_2)), BASE10));
end;

procedure TTestGmpExtensions.q_swap_test;
const
  ROP_1N = -13;
  ROP_1D = 17;
  ROP_2N = -23;
  ROP_2D = 31;
var rop1, rop2: MPRational;
begin
  q_init(rop1);
  q_init(rop2);
  q_set_si(rop1, ROP_1N, ROP_1D);
  q_set_si(rop2, ROP_2N, ROP_2D);
  q_swap(rop1, rop2);
  assert(q_cmp_si(rop1, ROP_2N, ROP_2D) = 0);
  assert(q_cmp_si(rop2, ROP_1N, ROP_1D) = 0);
end;

procedure TTestGmpExtensions.q_get_d_test;
const
  OP_1 = -1;
  OP_2 = 2;
var op: MPRational;
begin
  q_init(op);
  q_set_si(op, OP_1, OP_2);
  assert(q_get_d(op) = OP_1 / OP_2);
end;

procedure TTestGmpExtensions.q_set_d_test;
const
  OP_1 = -1;
  OP_2 = 2;
var rop: MPRational;
begin
  q_init(rop);
  q_set_d(rop, OP_1 / OP_2);
  assert(q_cmp_si(rop, OP_1, OP_2) = 0);
end;

procedure TTestGmpExtensions.q_set_f_test;
const OP_ = -10;
var
  op: MPFloat;
  rop: MPRational;
begin
  f_init_set_si(op, OP_);
  q_init(rop);
  q_set_f(rop, op);
  assert(q_get_d(rop) = OP_);
end;

procedure TTestGmpExtensions.q_get_str_test;
const
  OP_1 = -1;
  OP_2 = 2;
var
  op: MPRational;
  p: pchar;
  s, s2: string;
begin
  q_init(op);
  q_set_si(op, OP_1, OP_2);
  s := q_get_str(BASE10, op);
  assert(s = inttostr(OP_1) + '/' + inttostr(OP_2));
  p := q_get_str(nil, BASE10, op);
  assert(p = inttostr(OP_1) + '/' + inttostr(OP_2));
  freemem(p);
  setlength(s2, length(inttostr(OP_1) + '/' + inttostr(OP_2)));
  assert(pointer(q_get_str(pchar(s2), BASE10, op)) = pointer(s2));
  assert(s2 = inttostr(OP_1) + '/' + inttostr(OP_2));
end;

procedure TTestGmpExtensions.q_add_test;
const
  N1 = 5;
  D1 = 2;
  N2 = -7;
  D2 = 3;
var sum, addend1, addend2, s2: MPRational;
begin
  q_init(sum);
  q_init(addend1);
  q_init(addend2);
  q_set_si(addend1, N1, D1);
  q_set_si(addend2, N2, D2);
  q_add(sum, addend1, addend2);
  assert(q_cmp_si(sum, N1 * D2 + N2 * D1, D1 * D2) = 0);
  s2 := q_add(addend1, addend2);
  assert(q_cmp_si(s2, N1 * D2 + N2 * D1, D1 * D2) = 0);
end;

procedure TTestGmpExtensions.q_sub_test;
const
  N1 = 5;
  D1 = 2;
  N2 = -7;
  D2 = 3;
var difference, minuend, subtrahend, diff2: MPRational;
begin
  q_init(difference);
  q_init(minuend);
  q_init(subtrahend);
  q_set_si(minuend, N1, D1);
  q_set_si(subtrahend, N2, D2);
  q_sub(difference, minuend, subtrahend);
  assert(q_cmp_si(difference, N1 * D2 - N2 * D1, D1 * D2) = 0);
  diff2 := q_sub(minuend, subtrahend);
  assert(q_cmp_si(diff2, N1 * D2 - N2 * D1, D1 * D2) = 0);
end;

procedure TTestGmpExtensions.q_mul_test;
const
  N1 = 5;
  D1 = 2;
  N2 = -7;
  D2 = 3;
var product, multiplier, multiplicant, p2: MPRational;
begin
  q_init(product);
  q_init(multiplier);
  q_init(multiplicant);
  q_set_si(multiplier, N1, D1);
  q_set_si(multiplicant, N2, D2);
  q_mul(product, multiplier, multiplicant);
  assert(q_cmp_si(product, N1 * N2, D1 * D2) = 0);
  p2 := q_mul(multiplier, multiplicant);
  assert(q_cmp_si(p2, N1 * N2, D1 * D2) = 0);
end;

procedure TTestGmpExtensions.q_mul_2exp_test;
const
  N1 = 5;
  D1 = 3;
  OP_2 = 2;
var rop, op1, r2: MPRational;
begin
  q_init(rop);
  q_init(op1);
  q_set_si(op1, N1, D1);
  q_mul_2exp(rop, op1, OP_2);
  assert(q_cmp_si(rop, N1 * (1 shl OP_2), D1) = 0);
  r2 := q_mul_2exp(op1, OP_2);
  assert(q_cmp_si(r2, N1 * (1 shl OP_2), D1) = 0);
end;

procedure TTestGmpExtensions.q_div_test;
const
  N1 = 5;
  D1 = 2;
  N2 = -7;
  D2 = 3;
var quotient, dividend, divisor, q2: MPRational;
begin
  q_init(quotient);
  q_init(dividend);
  q_init(divisor);
  q_set_si(dividend, N1, D1);
  q_set_si(divisor, N2, D2);
  q_div(quotient, dividend, divisor);
  assert(q_cmp_si(quotient, -N1 * D2, -D1 * N2) = 0);
  q2 := q_div(dividend, divisor);
  assert(q_cmp_si(q2, -N1 * D2, -D1 * N2) = 0);
end;

procedure TTestGmpExtensions.q_div_2exp_test;
const
  N1 = 5;
  D1 = 3;
  OP_2 = 2;
var rop, op1, r2: MPRational;
begin
  q_init(rop);
  q_init(op1);
  q_set_si(op1, N1, D1);
  q_div_2exp(rop, op1, OP_2);
  assert(q_cmp_si(rop, N1, D1 * (1 shl OP_2)) = 0);
  r2 := q_div_2exp(op1, OP_2);
  assert(q_cmp_si(r2, N1, D1 * (1 shl OP_2)) = 0);
end;

procedure TTestGmpExtensions.q_neg_test;
const
  N1 = -5;
  D1 = 3;
var negated_operand, operand, n2: MPRational;
begin
  q_init(negated_operand);
  q_init(operand);
  q_set_si(operand, N1, D1);
  q_neg(negated_operand, operand);
  assert(q_cmp_si(negated_operand, -N1, D1) = 0);
  n2 := q_neg(operand);
  assert(q_cmp_si(n2, -N1, D1) = 0);
end;

procedure TTestGmpExtensions.q_abs_test;
const
  N1 = 5;
  D1 = 3;
var rop, op, r2: MPRational;
begin
  q_init(rop);
  q_init(op);
  q_set_si(op, -N1, D1);
  q_abs(rop, op);
  assert(q_cmp_si(rop, N1, D1) = 0);
  r2 := q_abs(op);
  assert(q_cmp_si(r2, N1, D1) = 0);
end;

procedure TTestGmpExtensions.q_inv_test;
const
  N1 = -5;
  D1 = 3;
var inverted_number, number, i2: MPRational;
begin
  q_init(inverted_number);
  q_init(number);
  q_set_si(number, N1, D1);
  q_inv(inverted_number, number);
  assert(q_cmp_si(inverted_number, -D1, -N1) = 0);
  i2 := q_inv(number);
  assert(q_cmp_si(i2, -D1, -N1) = 0);
end;

procedure TTestGmpExtensions.q_cmp_test;
const
  N1 = -7;
  D1 = 5;
var op1, op2: MPRational;
begin
  q_init(op1);
  q_init(op2);
  q_set_si(op1, N1, D1);
  q_set_si(op2, N1, D1);
  assert(q_cmp(op1, op2) = 0);
  q_set_si(op1, N1 + 1, D1);
  assert(q_cmp(op1, op2) > 0);
  q_set_si(op1, N1 - 1, D1);
  assert(q_cmp(op1, op2) < 0);
end;

procedure TTestGmpExtensions.q_cmp_si_test;
const
  N1 = -7;
  D1 = 5;
var op1: MPRational;
begin
  q_init(op1);
  q_set_si(op1, N1, D1);
  assert(q_cmp_si(op1, N1, D1) = 0);
  assert(q_cmp_si(op1, N1 - 1, D1) > 0);
  assert(q_cmp_si(op1, N1 + 1, D1) < 0);
end;

procedure TTestGmpExtensions.q_cmp_ui_test;
const
  N1 = 7;
  D1 = 5;
var op1: MPRational;
begin
  q_init(op1);
  q_set_si(op1, N1, D1);
  assert(q_cmp_ui(op1, N1, D1) = 0);
  assert(q_cmp_ui(op1, N1 - 1, D1) > 0);
  assert(q_cmp_ui(op1, N1 + 1, D1) < 0);
end;

procedure TTestGmpExtensions.q_equal_test;
const
  N1 = -7;
  D1 = 5;
var op1, op2: MPRational;
begin
  q_init(op1);
  q_init(op2);
  q_set_si(op1, N1, D1);
  q_set_si(op2, N1, D1);
  assert(q_equal(op1, op2));
  q_set_si(op1, N1 + 1, D1);
  assert(not q_equal(op1, op2));
  q_set_si(op1, N1 - 1, D1);
  assert(not q_equal(op1, op2));
end;

procedure TTestGmpExtensions.q_get_num_test;
const
  N1 = -7;
  D1 = 5;
var
  rational: MPRational;
  numerator, n2: MPInteger;
begin
  q_init(rational);
  z_init(numerator);
  q_set_si(rational, N1, D1);
  q_get_num(numerator, rational);
  assert(z_get_si(numerator) = N1);
  n2 := q_get_num(rational);
  assert(z_get_si(n2) = N1);
end;

procedure TTestGmpExtensions.q_get_den_test;
const
  N1 = -7;
  D1 = 5;
var
  rational: MPRational;
  denominator, d2: MPInteger;
begin
  q_init(rational);
  z_init(denominator);
  q_set_si(rational, N1, D1);
  q_get_den(denominator, rational);
  assert(z_get_si(denominator) = D1);
  d2 := q_get_den(rational);
  assert(z_get_si(d2) = D1);
end;

procedure TTestGmpExtensions.q_set_num_test;
const
  N1 = -7;
  D1 = 5;
  N2 = 3;
var
  rational: MPRational;
  numerator: MPInteger;
begin
  q_init(rational);
  z_init_set_si(numerator, N2);
  q_set_si(rational, N1, D1);
  q_set_num(rational, numerator);
  assert(q_cmp_si(rational, N2, D1) = 0);
end;

procedure TTestGmpExtensions.q_set_den_test;
const
  N1 = -7;
  D1 = 5;
  D2 = 3;
var
  rational: MPRational;
  denominator: MPInteger;
begin
  q_init(rational);
  z_init_set_si(denominator, D2);
  q_set_si(rational, N1, D1);
  q_set_den(rational, denominator);
  assert(q_cmp_si(rational, N1, D2) = 0);
end;

procedure TTestGmpExtensions.f_get_default_prec_test;
begin
  assert(f_get_default_prec > 0);
end;

procedure TTestGmpExtensions.f_set_default_prec_test;
var prec: valuint;
begin
  prec := f_get_default_prec;
  f_set_default_prec(2 * prec);
  assert(f_get_default_prec  = 2 * prec);
  f_set_default_prec(prec);
end;

procedure TTestGmpExtensions.f_init_test;
var x: MPFloat;
begin
  f_init(x);
  assert(f_get_prec(x) = f_get_default_prec);
end;

procedure TTestGmpExtensions.f_init2_test;
var x: MPFloat;
begin
  f_init2(x, 2 * f_get_default_prec);
  assert(f_get_prec(x) = 2 * f_get_default_prec);
end;

procedure TTestGmpExtensions.f_clear_test;
var x: MPFloat;
begin
  f_init(x);
  f_clear(x);
end;

procedure TTestGmpExtensions.f_get_prec_test;
var x: MPFloat;
begin
  f_init(x);
  f_set_prec(x, 2 * f_get_default_prec);
  assert(f_get_prec(x) = 2 * f_get_default_prec);
end;

procedure TTestGmpExtensions.f_set_prec_test;
var x: MPFloat;
begin
  f_init(x);
  f_set_prec(x, 2 * f_get_default_prec);
  assert(f_get_prec(x) = 2 * f_get_default_prec);
end;

procedure TTestGmpExtensions.f_set_prec_raw_test;
const N = 10;
var x: MPFloat;
begin
  f_init2(x, N * f_get_default_prec);
  f_set_prec_raw(x, f_get_default_prec);
  f_set_prec_raw(x, N * f_get_default_prec);
end;

procedure TTestGmpExtensions.f_set_q_test;
const OP_ = -10;
var
  op: MPRational;
  rop: MPFloat;
begin
  q_init(op);
  f_init(rop);
  q_set_si(op, OP_, 1);
  f_set_q(rop, op);
  assert(f_get_si(rop) = OP_);
end;

procedure TTestGmpExtensions.f_set_ui_test;
const OP = 1000000;
var rop: MPFloat;
begin
  f_init(rop);
  f_set_ui(rop, OP);
  assert(f_get_ui(rop) = OP);
end;

procedure TTestGmpExtensions.f_get_ui_test;
const OP = 1000000;
var rop: MPFloat;
begin
  f_init(rop);
  f_set_ui(rop, OP);
  assert(f_get_ui(rop) = OP);
end;

procedure TTestGmpExtensions.f_set_si_test;
const OP = -1000000;
var rop: MPFloat;
begin
  f_init(rop);
  f_set_si(rop, OP);
  assert(f_get_si(rop) = OP);
end;

procedure TTestGmpExtensions.f_get_si_test;
const OP = -1000000;
var rop: MPFloat;
begin
  f_init(rop);
  f_set_si(rop, OP);
  assert(f_get_si(rop) = OP);
end;

procedure TTestGmpExtensions.f_set_test;
const N = -1000000;
var rop, op: MPFloat;
begin
  f_init(rop);
  f_init_set_si(op, N);
  f_set(rop, op);
  assert(f_get_si(rop) = N);
end;

procedure TTestGmpExtensions.f_set_str_test;
const N = -123;
var rop: MPFloat;
begin
  f_init(rop);
  assert(f_set_str(rop, inttostr(N), BASE10));
  assert(f_get_si(rop) = N);
end;

procedure TTestGmpExtensions.f_swap_test;
const
  ROP_1 = -13;
  ROP_2 = -23;
var rop1, rop2: MPFloat;
begin
  f_init_set_si(rop1, ROP_1);
  f_init_set_si(rop2, ROP_2);
  f_swap(rop1, rop2);
  assert(f_get_si(rop1) =  ROP_2);
  assert(f_get_si(rop2) = ROP_1);
end;

procedure TTestGmpExtensions.f_get_d_test;
const N = -1.5;
var rop: MPFloat;
begin
  f_init_set_d(rop, N);
  assert(f_get_d(rop) = N);
end;

procedure TTestGmpExtensions.f_set_d_test;
const N = -1.5;
var rop: MPFloat;
begin
  f_init(rop);
  f_set_d(rop, N);
  assert(f_get_d(rop) = N);
end;

procedure TTestGmpExtensions.f_set_z_test;
const
  OP_1 = -13;
var
  rop: MPFloat;
  op: MPInteger;
begin
  f_init(rop);
  z_init_set_si(op, OP_1);
  f_set_z(rop, op);
  assert(f_get_si(rop) = OP_1);
end;

procedure TTestGmpExtensions.f_get_d_2exp_test;
const
  M = 0.5;
  E = 4;
  N = (1 shl E) div 2;
var
  rop: MPFloat;
  exp: valsint;
begin
  f_init_set_d(rop, N);
  exp := -1;
  assert(f_get_d_2exp(exp, rop) = M);
  assert(exp = E);
end;

procedure TTestGmpExtensions.f_get_str_test;
const
  N = 3.1416;
  S = '31416';
  E = 1;
var
  op: MPFloat;
  p: pchar;
  exp: mp_exp_t;
  s1: string;
begin
  f_init_set_d(op, N);
  p := f_get_str(nil, exp, BASE10, length(S), op);
  assert(p = S);
  assert(exp = E);
  freemem(p);
  setlength(s1, length(S));
  assert(pointer(f_get_str(pchar(s1), exp, BASE10, length(s), op)) = pointer(s1));
  assert(s1 = S);
  s1 := f_get_str(exp, BASE10, length(S), op);
  assert(s1 = S);
end;

procedure TTestGmpExtensions.f_add_test;
const
  OP1_ = 1;
  OP2_ = 2;
var rop, op1, op2, r2: MPFloat;
begin
  f_init(rop);
  f_init_set_d(op1, OP1_);
  f_init_set_d(op2, OP2_);
  f_add(rop, op1, op2);
  assert(f_get_d(rop) = OP1_ + OP2_);
  r2 := f_add(op1, op2);
  assert(f_get_d(r2) = OP1_ + OP2_);
end;

procedure TTestGmpExtensions.f_add_ui_test;
const
  OP1_ = 1;
  OP2_ = 2;
var rop, op1, r2: MPFloat;
begin
  f_init(rop);
  f_init_set_d(op1, OP1_);
  f_add_ui(rop, op1, OP2_);
  assert(f_get_d(rop) = OP1_ + OP2_);
  r2 := f_add_ui(op1, OP2_);
  assert(f_get_d(r2) = OP1_ + OP2_);
end;

procedure TTestGmpExtensions.f_sub_test;
const
  OP1_ = 3;
  OP2_ = 1;
var rop, op1, op2, r2: MPFloat;
begin
  f_init(rop);
  f_init_set_d(op1, OP1_);
  f_init_set_d(op2, OP2_);
  f_sub(rop, op1, op2);
  assert(f_get_d(rop) = OP1_ - OP2_);
  r2 := f_sub(op1, op2);
  assert(f_get_d(r2) = OP1_ - OP2_);
end;

procedure TTestGmpExtensions.f_sub_ui_test;
const
  OP1_ = 3;
  OP2_ = 1;
var rop, op1, r2: MPFloat;
begin
  f_init(rop);
  f_init_set_d(op1, OP1_);
  f_sub_ui(rop, op1, OP2_);
  assert(f_get_d(rop) = OP1_ - OP2_);
  r2 := f_sub_ui(op1, OP2_);
  assert(f_get_d(r2) = OP1_ - OP2_);
end;

procedure TTestGmpExtensions.f_ui_sub_test;
const
  OP1_ = 3;
  OP2_ = 1;
var rop, op2, r2: MPFloat;
begin
  f_init(rop);
  f_init_set_d(op2, OP2_);
  f_ui_sub(rop, OP1_, op2);
  assert(f_get_d(rop) = OP1_ - OP2_);
  r2 := f_ui_sub(OP1_, op2);
  assert(f_get_d(r2) = OP1_ - OP2_);
end;

procedure TTestGmpExtensions.f_mul_test;
const
  OP1_ = 2;
  OP2_ = 3;
var rop, op1, op2, r2: MPFloat;
begin
  f_init(rop);
  f_init_set_d(op1, OP1_);
  f_init_set_d(op2, OP2_);
  f_mul(rop, op1, op2);
  assert(f_get_d(rop) = OP1_ * OP2_);
  r2 := f_mul(op1, op2);
  assert(f_get_d(r2) = OP1_ * OP2_);
end;

procedure TTestGmpExtensions.f_mul_ui_test;
const
  OP1_ = 2;
  OP2_ = 3;
var rop, op1, r2: MPFloat;
begin
  f_init(rop);
  f_init_set_d(op1, OP1_);
  f_mul_ui(rop, op1, OP2_);
  assert(f_get_d(rop) = OP1_ * OP2_);
  r2 := f_mul_ui(op1, OP2_);
  assert(f_get_d(r2) = OP1_ * OP2_);
end;

procedure TTestGmpExtensions.f_mul_2exp_test;
const
  OP1_ = 3;
  OP2_ = 4;
var rop, op1, r2: MPFloat;
begin
  f_init(rop);
  f_init_set_d(op1, OP1_);
  f_mul_2exp(rop, op1, OP2_);
  assert(f_get_d(rop) = OP1_ * (1 shl OP2_));
  r2 := f_mul_2exp(op1, OP2_);
  assert(f_get_d(r2) = OP1_ * (1 shl OP2_));
end;

procedure TTestGmpExtensions.f_div_test;
const
  OP1_ = 6;
  OP2_ = 2;
var rop, op1, op2, r2: MPFloat;
begin
  f_init(rop);
  f_init_set_d(op1, OP1_);
  f_init_set_d(op2, OP2_);
  f_div(rop, op1, op2);
  assert(f_get_d(rop) = OP1_ / OP2_);
  r2 := f_div(op1, op2);
  assert(f_get_d(r2) = OP1_ / OP2_);
end;

procedure TTestGmpExtensions.f_div_ui_test;
const
  OP1_ = 6;
  OP2_ = 2;
var rop, op1, r2: MPFloat;
begin
  f_init(rop);
  f_init_set_d(op1, OP1_);
  f_div_ui(rop, op1, OP2_);
  assert(f_get_d(rop) = OP1_ / OP2_);
  r2 := f_div_ui(op1, OP2_);
  assert(f_get_d(r2) = OP1_ / OP2_);
end;

procedure TTestGmpExtensions.f_ui_div_test;
const
  OP1_ = 6;
  OP2_ = 2;
var rop, op2, r2: MPFloat;
begin
  f_init(rop);
  f_init_set_si(op2, OP2_);
  f_ui_div(rop, OP1_, op2);
  assert(f_get_d(rop) = OP1_ / OP2_);
  r2 := f_ui_div(OP1_, op2);
  assert(f_get_d(r2) = OP1_ / OP2_);
end;

procedure TTestGmpExtensions.f_div_2exp_test;
const
  OP1_ = 16;
  OP2_ = 2;
var rop, op1, r2: MPFloat;
begin
  f_init(rop);
  f_init_set_d(op1, OP1_);
  f_div_2exp(rop, op1, OP2_);
  assert(f_get_d(rop) = OP1_ / (1 shl OP2_));
  r2 := f_div_2exp(op1, OP2_);
  assert(f_get_d(r2) = OP1_ / (1 shl OP2_));
end;

procedure TTestGmpExtensions.f_sqrt_test;
const
  OP_ = 4;
var rop, op, r2: MPFloat;
begin
  f_init(rop);
  f_init_set_d(op, OP_);
  f_sqrt(rop, op);
  assert(f_get_d(rop) = sqrt(OP_));
  r2 := f_sqrt(op);
  assert(f_get_d(r2) = sqrt(OP_));
end;

procedure TTestGmpExtensions.f_sqrt_ui_test;
const
  OP_ = 4;
var rop, r2: MPFloat;
begin
  f_init(rop);
  f_sqrt_ui(rop, OP_);
  assert(f_get_d(rop) = sqrt(OP_));
  r2 := f_sqrt_ui(OP_);
  assert(f_get_d(r2) = sqrt(OP_));
end;

procedure TTestGmpExtensions.f_pow_ui_test;
const
  OP1_ = 3;
  OP2_ = 2;
var rop, op1, r2: MPFloat;
begin
  f_init(rop);
  f_init_set_d(op1, OP1_);
  f_pow_ui(rop, op1, OP2_);
  assert(f_get_d(rop) = OP1_ ** OP2_);
  r2 := f_pow_ui(op1, OP2_);
  assert(f_get_d(r2) = OP1_ ** OP2_);
end;

procedure TTestGmpExtensions.f_neg_test;
const
  OP_ = 4;
var rop, op, r2: MPFloat;
begin
  f_init(rop);
  f_init_set_d(op, OP_);
  f_neg(rop, op);
  assert(f_get_d(rop) = -OP_);
  r2 := f_neg(op);
  assert(f_get_d(r2) = -OP_);
end;

procedure TTestGmpExtensions.f_abs_test;
const OP_ = -4;
var rop, op, r2: MPFloat;
begin
  f_init(rop);
  f_init_set_d(op, OP_);
  f_abs(rop, op);
  assert(f_get_d(rop) = -OP_);
  r2 := f_abs(op);
  assert(f_get_d(r2) = -OP_);
end;

procedure TTestGmpExtensions.f_cmp_test;
const OP_1 = -10;
var op1, op2: MPFloat;
begin
  f_init_set_si(op1, OP_1);
  f_init_set_si(op2, OP_1);
  assert(f_cmp(op1, op2) = 0);
  f_set_si(op2, OP_1 + 1);
  assert(f_cmp(op1, op2) < 0);
  f_set_si(op2, OP_1 - 1);
  assert(f_cmp(op1, op2) > 0);
end;

procedure TTestGmpExtensions.f_cmp_d_test;
const OP_ = -10;
var op: MPFloat;
begin
  f_init_set_si(op, OP_);
  assert(f_cmp_d(op, OP_) = 0);
  assert(f_cmp_d(op, OP_ + 1) < 0);
  assert(f_cmp_d(op, OP_ - 1) > 0);
end;

procedure TTestGmpExtensions.f_cmp_si_test;
const OP_ = -10;
var op: MPFloat;
begin
  f_init_set_si(op, OP_);
  assert(f_cmp_si(op, OP_) = 0);
  assert(f_cmp_si(op, OP_ + 1) < 0);
  assert(f_cmp_si(op, OP_ - 1) > 0);
end;

procedure TTestGmpExtensions.f_cmp_ui_test;
const OP_ = 10;
var op: MPFloat;
begin
  f_init_set_si(op, OP_);
  assert(f_cmp_ui(op, OP_) = 0);
  assert(f_cmp_ui(op, OP_ + 1) < 0);
  assert(f_cmp_ui(op, OP_ - 1) > 0);
end;

procedure TTestGmpExtensions.f_eq_test;
const
  OP_1 = 10;
  OP_3 = 8;
var op1, op2: MPFloat;
begin
  f_init_set_si(op1, OP_1);
  f_init_set_si(op2, OP_1);
  assert(f_eq(op1, op2, OP_3));
end;

procedure TTestGmpExtensions.f_reldiff_test;
const
  OP1_ = 4;
  OP2_ = 2;
var rop, op1, op2, r2: MPFloat;
begin
  f_init(rop);
  f_init_set_d(op1, OP1_);
  f_init_set_d(op2, OP2_);
  f_reldiff(rop, op1, op2);
  assert(f_get_d(rop) = abs(OP1_ - OP2_) / OP1_);
  r2 := f_reldiff(op1, op2);
  assert(f_get_d(r2) = abs(OP1_ - OP2_) / OP1_);
end;

procedure TTestGmpExtensions.f_ceil_test;
const N = 3.1;
var rop, op, r2: MPFloat;
begin
  f_init(rop);
  f_init_set_d(op, N);
  f_ceil(rop, op);
  assert(f_get_d(rop) = ceil(N));
  r2 := f_ceil(op);
  assert(f_get_d(r2) = ceil(N));
end;

procedure TTestGmpExtensions.f_floor_test;
const N = 3.9;
var rop, op, r2: MPFloat;
begin
  f_init(rop);
  f_init_set_d(op, N);
  f_floor(rop, op);
  assert(f_get_d(rop) = floor(N));
  r2 := f_floor(op);
  assert(f_get_d(r2) = floor(N));
end;

procedure TTestGmpExtensions.f_trunc_test;
const N = -3.9;
var rop, op, r2: MPFloat;
begin
  f_init(rop);
  f_init_set_d(op, N);
  f_trunc(rop, op);
  assert(f_get_d(rop) = trunc(N));
  r2 := f_trunc(op);
  assert(f_get_d(r2) = trunc(N));
end;

procedure TTestGmpExtensions.f_integer_p_test;
const
  N1 = 10;
  N2 = 10.1;
var op: MPFloat;
begin
  f_init_set_si(op, N1);
  assert(f_integer_p(op));
  f_set_d(op, N2);
  assert(not f_integer_p(op));
end;

procedure TTestGmpExtensions.f_fits_ushort_p_test;
var op: MPFloat;
begin
  f_init_set_ui(op, high(word));
  assert(f_fits_ushort_p(op));
  f_add_ui(op, op, 1);
  assert(not f_fits_ushort_p(op));
end;

procedure TTestGmpExtensions.f_fits_sshort_p_test;
var op: MPFloat;
begin
  f_init_set_ui(op, high(smallint));
  assert(f_fits_sshort_p(op));
  f_add_ui(op, op, 1);
  assert(not f_fits_sshort_p(op));
  f_clear(op);
end;

procedure TTestGmpExtensions.f_fits_uint_p_test;
var op: MPFloat;
begin
  f_init_set_ui(op, high(cardinal));
  assert(f_fits_uint_p(op));
  f_add_ui(op, op, 1);
  assert(not f_fits_uint_p(op));
  f_clear(op);
end;

procedure TTestGmpExtensions.f_fits_sint_p_test;
var op: MPFloat;
begin
  f_init_set_ui(op, high(integer));
  assert(f_fits_sint_p(op));
  f_add_ui(op, op, 1);
  assert(not f_fits_sint_p(op));
  f_clear(op);
end;

procedure TTestGmpExtensions.f_fits_ulong_p_test;
var op: MPFloat;
begin
  f_init_set_ui(op, high(valuint));
  assert(f_fits_ulong_p(op));
  f_add_ui(op, op, 1);
  assert(not f_fits_ulong_p(op));
  f_clear(op);
end;

procedure TTestGmpExtensions.f_fits_slong_p_test;
var op: MPFloat;
begin
  f_init_set_ui(op, high(valsint));
  assert(f_fits_slong_p(op));
  f_add_ui(op, op, 1);
  assert(not f_fits_slong_p(op));
  f_clear(op);
end;

procedure TTestGmpExtensions.f_urandomb_test;
const NBITS = 100;
var
  rop, r2: MPFloat;
  state: MPRandState;
  cmp: longint;
begin
  randinit_default(state);
  f_init(rop);
  f_urandomb(rop, state, NBITS);
  cmp := f_cmp_ui(rop, 0);
  assert(cmp >= 0);
  assert(f_cmp_ui(rop, 1) < 0);
  r2 := f_urandomb(state, NBITS);
  cmp := f_cmp_ui(r2, 0);
  assert(cmp >= 0);
  assert(f_cmp_ui(r2, 1) < 0);
end;

procedure TTestGmpExtensions.f_random2_test;
var
  rop, r2: MPFloat;
begin
  f_init(rop);
  f_random2(rop, 1, 1);
  r2 := f_random2(1, 1);
  assert(assigned(r2))
end;

procedure TTestGmpExtensions.f_init_set_test;
const N = -1000000;
var rop, op: MPFloat;
begin
  f_init_set_si(op, N);
  f_init_set(rop, op);
  assert(f_get_si(rop) = N);
end;

procedure TTestGmpExtensions.f_init_set_d_test;
const N = -1.5;
var rop: MPFloat;
begin
  f_init_set_d(rop, N);
  assert(f_get_d(rop) = N);
end;

procedure TTestGmpExtensions.f_init_set_si_test;
const N = -5;
var rop: MPFloat;
begin
  f_init_set_si(rop, N);
  assert(f_get_si(rop) = N);
end;

procedure TTestGmpExtensions.f_init_set_str_test;
const N = -123;
var rop: MPFloat;
begin
  f_init_set_str(rop, inttostr(N), BASE10);
  assert(f_get_si(rop) = N);
end;

procedure TTestGmpExtensions.f_init_set_ui_test;
const N = 5;
var rop: MPFloat;
begin
  f_init_set_ui(rop, N);
  assert(f_get_ui(rop) = N);
end;

{ TTestGmpOperators }

// operator * (op1: MPFloat; op2: MPFloat): MPFloat;
procedure TTestGmpOperators.star__MPFloat__MPFloat__MPFloat_;
const
  OP1_ = 2;
  OP2_ = 3;
var rop, op1, op2: MPFloat;
begin
  f_init_set_d(op1, OP1_);
  f_init_set_d(op2, OP2_);
  rop := op1 * op2;
  assert(f_get_d(rop) = OP1_ * OP2_);
end;

// operator * (op1: MPInteger; op2: MPInteger): MPInteger;
procedure TTestGmpOperators.star__MPInteger__MPInteger__MPInteger_;
const
  OP_1 = -10;
  OP_2 = 1000;
var rop, op1, op2: MPInteger;
begin
  z_init_set_si(op1, OP_1);
  z_init_set_si(op2, OP_2);
  rop := op1 * op2;
  assert(z_get_si(rop) = OP_1 * OP_2);
end;

// operator * (op1: MPRational; op2: MPRational): MPRational;
procedure TTestGmpOperators.star__MPRational__MPRational__MPRational_;
const
  N1 = 5;
  D1 = 2;
  N2 = -7;
  D2 = 3;
var product, multiplier, multiplicant: MPRational;
begin
  q_init(multiplier);
  q_init(multiplicant);
  q_set_si(multiplier, N1, D1);
  q_set_si(multiplicant, N2, D2);
  product := multiplier * multiplicant;
  assert(q_cmp_si(product, N1 * N2, D1 * D2) = 0);
end;

// operator ** (op1: MPFloat; op2: valuint): MPFloat;
procedure TTestGmpOperators.pow__MPFloat_valuint_MPFloat_;
const
  OP1_ = 3;
  OP2_ = 2;
var rop, op1: MPFloat;
begin
  f_init_set_d(op1, OP1_);
  rop := op1 ** OP2_;
  assert(f_get_d(rop) = OP1_ ** OP2_);
end;

// operator ** (op1: MPInteger; op2: valuint): MPInteger;
procedure TTestGmpOperators.pow__MPInteger_valuint_MPInteger_;
const
  BASE_ = 10;
  EXP = 3;
var rop, base: MPInteger;
begin
  z_init_set_si(base, BASE_);
  rop := base ** EXP;
  assert(z_get_si(rop) = BASE_ ** EXP);
end;

// operator + (op1: MPFloat; op2: MPFloat): MPFloat;
procedure TTestGmpOperators.plus__MPFloat__MPFloat__MPFloat_;
const
  OP1_ = 1;
  OP2_ = 2;
var rop, op1, op2: MPFloat;
begin
  f_init_set_d(op1, OP1_);
  f_init_set_d(op2, OP2_);
  rop := op1 + op2;
  assert(f_get_d(rop) = OP1_ + OP2_);
end;

// operator + (op1: MPInteger; op2: MPInteger): MPInteger;
procedure TTestGmpOperators.plus__MPInteger__MPInteger__MPInteger_;
const
  OP_1 = -1000;
  OP_2 = 1000000;
var rop, op1, op2: MPInteger;
begin
  z_init_set_si(op1, OP_1);
  z_init_set_si(op2, OP_2);
  rop := op1 + op2;
  assert(z_get_si(rop) = OP_1 + OP_2);
end;

// operator + (op1: MPRational; op2: MPRational): MPRational;
procedure TTestGmpOperators.plus__MPRational__MPRational__MPRational_;
const
  N1 = 5;
  D1 = 2;
  N2 = -7;
  D2 = 3;
var sum, addend1, addend2: MPRational;
begin
  q_init(addend1);
  q_init(addend2);
  q_set_si(addend1, N1, D1);
  q_set_si(addend2, N2, D2);
  sum := addend1 + addend2;
  assert(q_cmp_si(sum, N1 * D2 + N2 * D1, D1 * D2) = 0);
end;

// operator - (op: MPFloat): MPFloat;
procedure TTestGmpOperators.minus__MPFloat__MPFloat_;
const
  OP_ = 4;
var rop, op: MPFloat;
begin
  f_init_set_d(op, OP_);
  rop := -op;
  assert(f_get_d(rop) = -OP_);
end;

// operator - (op: MPInteger): MPInteger;
procedure TTestGmpOperators.minus__MPInteger__MPInteger_;
const OP_ = 1000;
var rop, op: MPInteger;
begin
  z_init_set_si(op, OP_);
  rop := -op;
  assert(z_get_si(rop) = -OP_);
end;

// operator - (op: MPRational): MPRational;
procedure TTestGmpOperators.minus__MPRational__MPRational_;
const
  N1 = -5;
  D1 = 3;
var negated_operand, operand: MPRational;
begin
  q_init(negated_operand);
  q_init(operand);
  q_set_si(operand, N1, D1);
  negated_operand := - operand;
  assert(q_cmp_si(negated_operand, -N1, D1) = 0);
end;

// operator - (op1: MPFloat; op2: MPFloat): MPFloat;
procedure TTestGmpOperators.minus__MPFloat__MPFloat__MPFloat_;
const
  OP1_ = 3;
  OP2_ = 1;
var rop, op1, op2: MPFloat;
begin
  f_init_set_d(op1, OP1_);
  f_init_set_d(op2, OP2_);
  rop := op1 - op2;
  assert(f_get_d(rop) = OP1_ - OP2_);
end;

// operator - (op1: MPInteger; op2: MPInteger): MPInteger;
procedure TTestGmpOperators.minus__MPInteger__MPInteger__MPInteger_;
const
  OP_1 = -1000;
  OP_2 = 1000000;
var rop, op1, op2: MPInteger;
begin
  z_init_set_si(op1, OP_1);
  z_init_set_si(op2, OP_2);
  rop := op1 - op2;
  assert(z_get_si(rop) = OP_1 - OP_2);
end;

// operator - (op1: MPRational; op2: MPRational): MPRational;
procedure TTestGmpOperators.minus__MPRational__MPRational__MPRational_;
const
  N1 = 5;
  D1 = 2;
  N2 = -7;
  D2 = 3;
var difference, minuend, subtrahend: MPRational;
begin
  q_init(minuend);
  q_init(subtrahend);
  q_set_si(minuend, N1, D1);
  q_set_si(subtrahend, N2, D2);
  difference := minuend - subtrahend;
  assert(q_cmp_si(difference, N1 * D2 - N2 * D1, D1 * D2) = 0);
end;

// operator / (op1: MPFloat; op2: MPFloat): MPFloat;
procedure TTestGmpOperators.slash__MPFloat__MPFloat__MPFloat_;
const
  OP1_ = 6;
  OP2_ = 2;
var rop, op1, op2: MPFloat;
begin
  f_init_set_d(op1, OP1_);
  f_init_set_d(op2, OP2_);
  rop := op1 / op2;
  assert(f_get_d(rop) = OP1_ / OP2_);
end;

// operator / (op1: MPInteger; op2: MPInteger): MPInteger;
procedure TTestGmpOperators.slash__MPInteger__MPInteger__MPInteger_;
const
  N_ = -17;
  D_ = 4;
var q, n, d: MPInteger;
begin
  z_init_set_si(n, N_);
  z_init_set_si(d, D_);
  q := n / d;
  assert(z_get_si(q) = trunc(N_ / D_));
end;

// operator / (op1: MPRational; op2: MPRational): MPRational;
procedure TTestGmpOperators.slash__MPRational__MPRational__MPRational_;
const
  N1 = 5;
  D1 = 2;
  N2 = -7;
  D2 = 3;
var quotient, dividend, divisor: MPRational;
begin
  q_init(dividend);
  q_init(divisor);
  q_set_si(dividend, N1, D1);
  q_set_si(divisor, N2, D2);
  quotient := dividend / divisor;
  assert(q_cmp_si(quotient, -N1 * D2, -D1 * N2) = 0);
end;

// operator := (op: double): MPFloat;
procedure TTestGmpOperators.assign_double_MPFloat_;
const N = -1.5;
var rop: MPFloat;
begin
  rop := N;
  assert(f_get_d(rop) = N);
end;

// operator := (op: double): MPInteger;
procedure TTestGmpOperators.assign_double_MPInteger_;
const OP_ = 1000000;
var rop: MPInteger;
begin
  rop := OP_;
  assert(z_get_si(rop) = OP_);
end;

// operator := (op: double): MPRational;
procedure TTestGmpOperators.assign_double_MPRational_;
const
  OP_1 = -1;
  OP_2 = 2;
var rop: MPRational;
begin
  rop := OP_1 / OP_2;
  assert(q_cmp_si(rop, OP_1, OP_2) = 0);
end;

// operator := (op: MPFloat): cardinal;
procedure TTestGmpOperators.assign__MPFloat_Cardinal;
const OP = 1000000;
var rop: MPFloat;
begin
  rop := OP;
  assert(f_get_ui(rop) = OP);
end;

// operator := (op: MPFloat): double;
procedure TTestGmpOperators.assign__MPFloat_double;
const N = -1.5;
var rop: MPFloat;
begin
  rop := N;
  assert(f_get_d(rop) = N);
end;

// operator := (op: MPFloat): integer;
procedure TTestGmpOperators.assign__MPFloat_integer;
const OP = -1000000;
var rop: MPFloat;
begin
  rop := OP;
  assert(f_get_si(rop) = OP);
end;

// operator := (op: MPFloat): mpf_t;
procedure TTestGmpOperators.assign__MPFloat_mpf_t;
const N = -1000000;
var
  rop: mpf_t;
  op: MPFloat;
begin
  f_init_set_si(op, N);
  rop := op;
  assert(mpf_get_si(rop) = N);
  mpf_clear(rop);
end;

// operator := (op: MPFloat): MPInteger;
procedure TTestGmpOperators.assign__MPFloat__MPInteger_;
const OP_ = -10;
var
  op: MPFloat;
  rop: MPInteger;
begin
  f_init_set_si(op, OP_);
  rop := op;
  assert(z_get_si(rop) = OP_);
end;

// operator := (op: MPFloat): MPRational;
procedure TTestGmpOperators.assign__MPFloat__MPRational_;
const OP_ = -10;
var
  op: MPFloat;
  rop: MPRational;
begin
  f_init_set_si(op, OP_);
  rop :=  op;
  assert(q_get_d(rop) = OP_);
end;

// operator := (op: MPFloat): string;
procedure TTestGmpOperators.assign__MPFloat_string;
const
  N = 123456;
  S = '123456';
var
  op: MPFloat;
  r: string;
begin
  f_init_set_d(op, N);
  r := op;
  assert(r = S);
end;

// operator := (op: MPFloat): valsint;
procedure TTestGmpOperators.assign__MPFloat_valsint;
const OP_ = -1000000;
var
  op: MPFloat;
  r: valsint;
begin
  f_init_set_si(op, OP_);
  r := op;
  assert(r = OP_);
end;

// operator := (op: MPFloat): valuint;
procedure TTestGmpOperators.assign__MPFloat_valuint;
const OP_ = 1000000;
var
  op: MPFloat;
  r: valuint;
begin
  f_init_set_si(op, OP_);
  r := op;
  assert(r = OP_);
end;

// operator := (var op: mpf_t): MPFloat;
procedure TTestGmpOperators.assign_mpf_t_MPFloat_;
const N = -1000000;
var
  op: mpf_t;
  rop: MPFloat;
begin
  mpf_init_set_si(op, N);
  rop := op;
  assert(f_get_si(rop) = N);
  mpf_clear(op);
end;

// operator := (op: MPInteger): cardinal;
procedure TTestGmpOperators.assign__MPInteger_cardinal;
const N = 1000000;
var
  op: MPInteger;
  r: cardinal;
begin
  z_init_set_ui(op, N);
  r := op;
  assert(r = N);
end;

// operator := (op: MPInteger): double;
procedure TTestGmpOperators.assign__MPInteger_double;
const N = -1000;
var
  op: MPInteger;
  r: double;
begin
  z_init_set_si(op, N);
  r := op;
  assert(r = N);
end;

// operator := (op: MPInteger): integer;
procedure TTestGmpOperators.assign__MPInteger_integer;
const N = -1000000;
var
  op: MPInteger;
  r: integer;
begin
  z_init_set_si(op, N);
  r := op;
  assert(r = N);
end;

// operator := (op: MPInteger): MPFloat;
procedure TTestGmpOperators.assign__MPInteger__MPFloat_;
const
  OP_1 = -13;
var
  rop: MPFloat;
  op: MPInteger;
begin
  z_init_set_si(op, OP_1);
  rop := op;
  assert(f_get_si(rop) = OP_1);
end;

// operator := (op: MPInteger): MPRational;
procedure TTestGmpOperators.assign__MPInteger__MPRational_;
const
  OP_1 = -13;
var
  rational: MPRational;
  op: MPInteger;
begin
  z_init_set_si(op, OP_1);
  rational := op;
  assert(q_cmp_si(rational, OP_1, 1) = 0);
end;

// operator := (op: MPInteger): mpz_t;
procedure TTestGmpOperators.assign__MPInteger_mpz_t;
const N = -1000000;
var
  rop: mpz_t;
  op: MPInteger;
begin
  z_init_set_si(op, N);
  rop := op;
  assert(mpz_get_si(rop) = N);
  mpz_clear(rop);
end;

// operator := (op: MPInteger): string;
procedure TTestGmpOperators.assign__MPInteger_string;
const
  N = 123456;
  S = '123456';
var
  op: MPInteger;
  r: string;
begin
  z_init_set_ui(op, N);
  r := op;
  assert(r = S);
end;

// operator := (op: MPInteger): valsint;
procedure TTestGmpOperators.assign__MPInteger_valsint;
const N = -1000000;
var
  op: MPInteger;
  r: valsint;
begin
  z_init_set_si(op, N);
  r := op;
  assert(r = N);
end;

// operator := (op: MPInteger): valuint;
procedure TTestGmpOperators.assign__MPInteger_valuint;
const N = 1000000;
var
  op: MPInteger;
  r: valuint;
begin
  z_init_set_ui(op, N);
  r := op;
  assert(r = N);
end;

// operator := (var op: mpq_t): MPRational;
procedure TTestGmpOperators.assign_mpq_t_MPRational_;
const
  N = -17;
  D = 3;
var
  op: mpq_t;
  rop: MPRational;
begin
  mpq_init(op);
  mpq_set_si(op, N, D);
  rop := op;
  assert(q_cmp_si(rop, N, D) = 0);
  mpq_clear(op);
end;

// operator := (op: MPRandState): randstate_t;
procedure TTestGmpOperators.assign__MPRandState_randstate_t;
var
  op: MPRandState;
  rop: randstate_t;
begin
  randinit_default(op);
  rop := op;
  assert(rnd_test(rop));
  mp_randclear(rop);
end;

// operator := (op: MPRational): double;
procedure TTestGmpOperators.assign__MPRational_double;
const
  OP_1 = -1;
  OP_2 = 2;
var
  op: MPRational;
  r: double;
begin
  q_init(op);
  q_set_si(op, OP_1, OP_2);
  r := op;
  assert(r = OP_1 / OP_2);
end;

// operator := (op: MPRational): MPFloat;
procedure TTestGmpOperators.assign__MPRational__MPFloat_;
const OP_ = -10;
var
  op: MPRational;
  rop: MPFloat;
begin
  q_init(op);
  q_set_si(op, OP_, 1);
  rop :=  op;
  assert(f_get_si(rop) = OP_);
end;

// operator := (op: MPRational): MPInteger;
procedure TTestGmpOperators.assign__MPRational__MPInteger_;
const OP_ = -10;
var
  op: MPRational;
  rop: MPInteger;
begin
  q_init(op);
  q_set_si(op, OP_, 1);
  rop := op;
  assert(z_get_si(rop) = OP_);
end;

// operator := (op: MPRational): mpq_t;
procedure TTestGmpOperators.assign__MPRational_mpq_t;
const
  OP_1 = -13;
  OP_2 = 17;
var
  rational: mpq_t;
  op: MPRational;
begin
  q_init(op);
  q_set_si(op, OP_1, OP_2);
  rational := op;
  assert(mpq_cmp_si(rational, OP_1, OP_2) = 0);
  mpq_clear(rational);
end;

// operator := (op: MPRational): string;
procedure TTestGmpOperators.assign__MPRational_string;
const
  N = 5;
  D = 3;
  S = '5/3';
var
  op: MPRational;
  r: string;
begin
  q_init(op);
  q_set_si(op, N, D);
  r := op;
  assert(r = S);
end;

// operator := (var op: mpz_t): MPInteger;
procedure TTestGmpOperators.assign_mpz_t_MPInteger_;
const N = -1000000;
var
  op: mpz_t;
  rop: MPInteger;
begin
  mpz_init_set_si(op, N);
  rop := op;
  assert(z_get_si(rop) = N);
  mpz_clear(op);
end;

// operator := (var op: randstate_t): MPRandState;
procedure TTestGmpOperators.assign_randstate_t_MPRandState_;
var
  op: randstate_t;
  rop: MPRandState;
begin
  mp_randinit_default(op);
  rop := op;
  assert(rnd_test(rop.ptr^));
  mp_randclear(op);
end;

// operator := (op: string): MPFloat;
procedure TTestGmpOperators.assign_string_MPFloat_;
const N = -123;
var rop: MPFloat;
begin
  rop := inttostr(N);
  assert(f_get_si(rop) = N);
end;

// operator := (op: string): MPInteger;
procedure TTestGmpOperators.assign_string_MPInteger_;
const N = -123;
var rop: MPInteger;
begin
  rop := inttostr(N);
  assert(z_get_si(rop) = N);
end;

// operator := (op: string): MPRational;
procedure TTestGmpOperators.assign_string_MPRational_;
const
  N = 5;
  D = 3;
  S = '5/3';
var rop: MPRational;
begin
  rop := S;
  assert(q_cmp_si(rop, N, D) = 0);
end;

// operator := (op: valsint): MPFloat;
procedure TTestGmpOperators.assign_valsint_MPFloat_;
const OP = -1000000;
var rop: MPFloat;
begin
  rop := OP;
  assert(f_get_si(rop) = OP);
end;

// operator := (op: valsint): MPInteger;
procedure TTestGmpOperators.assign_valsint_MPInteger_;
const OP = -1000000;
var rop: MPInteger;
begin
  rop := OP;
  assert(z_get_si(rop) = OP);
end;

// operator := (op: valsint): MPRational;
procedure TTestGmpOperators.assign_valsint_MPRational_;
const OP = -1000000;
var rop: MPRational;
begin
  rop := OP;
  assert(q_cmp_si(rop, OP, 1) = 0);
end;

// operator := (op: valuint): MPFloat;
procedure TTestGmpOperators.assign_valuint_MPFloat_;
const OP = 1000000;
var rop: MPFloat;
begin
  rop := OP;
  assert(f_get_ui(rop) = OP);
end;

// operator := (op: valuint): MPInteger;
procedure TTestGmpOperators.assign_valuint_MPInteger_;
const OP = 1000000;
var rop: MPInteger;
begin
  rop := OP;
  assert(z_get_ui(rop) = OP);
end;

// operator := (op: valuint): MPRational;
procedure TTestGmpOperators.assign_valuint_MPRational_;
const OP = 1000000;
var rop: MPRational;
begin
  rop := OP;
  assert(q_cmp_ui(rop, OP, 1) = 0);
end;

// operator < (op1: MPFloat; op2: MPFloat): boolean;
procedure TTestGmpOperators.lt__MPFloat__MPFloat_boolean;
const
  OP1_ = 1.2;
  OP2_ = OP1_ + 1;
var op1, op2: MPFloat;
begin
  f_init_set_d(op1, OP1_);
  f_init_set_d(op2, OP2_);
  assert(op1 < op2);
end;

// operator < (op1: MPInteger; op2: MPInteger): boolean;
procedure TTestGmpOperators.lt__MPInteger__MPInteger_boolean;
const
  OP1_ = 12;
  OP2_ = OP1_ + 1;
var op1, op2: MPInteger;
begin
  z_init_set_d(op1, OP1_);
  z_init_set_d(op2, OP2_);
  assert(op1 < op2);
end;

// operator < (op1: MPRational; op2: MPRational): boolean;
procedure TTestGmpOperators.lt__MPRational__MPRational_boolean;
const
  OP1_ = 12;
  OP2_ = OP1_ + 1;
var op1, op2: MPRational;
begin
  q_init(op1);
  q_init(op2);
  q_set_si(op1, OP1_, 1);
  q_set_si(op2, OP2_, 1);
  assert(op1 < op2);
end;

// operator <= (op1: MPFloat; op2: MPFloat): boolean;
procedure TTestGmpOperators.le__MPFloat__MPFloat_boolean;
const
  OP1_ = 1.2;
  OP2_ = OP1_ + 1;
var op1, op2: MPFloat;
begin
  f_init_set_d(op1, OP1_);
  f_init_set_d(op2, OP2_);
  assert(op1 <= op2);
  f_set_d(op2, OP1_);
  assert(op1 <= op2);
end;

// operator <= (op1: MPInteger; op2: MPInteger): boolean;
procedure TTestGmpOperators.le__MPInteger__MPInteger_boolean;
const
  OP1_ = 12;
  OP2_ = OP1_ + 1;
var op1, op2: MPInteger;
begin
  z_init_set_ui(op1, OP1_);
  z_init_set_ui(op2, OP2_);
  assert(op1 <= op2);
  z_set_ui(op2, OP1_);
  assert(op1 <= op2);
end;

// operator <= (op1: MPRational; op2: MPRational): boolean;
procedure TTestGmpOperators.le__MPRational__MPRational_boolean;
const
  OP1_ = 12;
  OP2_ = OP1_ + 1;
var op1, op2: MPRational;
begin
  q_init(op1);
  q_init(op2);
  q_set_si(op1, OP1_, 1);
  q_set_si(op2, OP2_, 1);
  assert(op1 <= op2);
  q_set_si(op2, OP1_, 1);
  assert(op1 <= op2);
end;

// operator > (op1: MPFloat; op2: MPFloat): boolean;
procedure TTestGmpOperators.gt__MPFloat__MPFloat_boolean;
const
  OP1_ = 1.2;
  OP2_ = OP1_ - 1;
var op1, op2: MPFloat;
begin
  f_init_set_d(op1, OP1_);
  f_init_set_d(op2, OP2_);
  assert(op1 > op2);
end;

// operator > (op1: MPInteger; op2: MPInteger): boolean;
procedure TTestGmpOperators.gt__MPInteger__MPInteger_boolean;
const
  OP1_ = 12;
  OP2_ = OP1_ - 1;
var op1, op2: MPInteger;
begin
  z_init_set_d(op1, OP1_);
  z_init_set_d(op2, OP2_);
  assert(op1 > op2);
end;

// operator > (op1: MPRational; op2: MPRational): boolean;
procedure TTestGmpOperators.gt__MPRational__MPRational_boolean;
const
  OP1_ = 12;
  OP2_ = OP1_ - 1;
var op1, op2: MPRational;
begin
  q_init(op1);
  q_init(op2);
  q_set_si(op1, OP1_, 1);
  q_set_si(op2, OP2_, 1);
  assert(op1 > op2);
end;

// operator >= (op1: MPFloat; op2: MPFloat): boolean;
procedure TTestGmpOperators.ge__MPFloat__MPFloat_boolean;
const
  OP1_ = 1.2;
  OP2_ = OP1_ - 1;
var op1, op2: MPFloat;
begin
  f_init_set_d(op1, OP1_);
  f_init_set_d(op2, OP2_);
  assert(op1 >= op2);
  f_set_d(op2, OP1_);
  assert(op1 >= op2);
end;

// operator >= (op1: MPInteger; op2: MPInteger): boolean;
procedure TTestGmpOperators.ge__MPInteger__MPInteger_boolean;
const
  OP1_ = 12;
  OP2_ = OP1_ - 1;
var op1, op2: MPInteger;
begin
  z_init_set_ui(op1, OP1_);
  z_init_set_ui(op2, OP2_);
  assert(op1 >= op2);
  z_set_ui(op2, OP1_);
  assert(op1 >= op2);
end;

// operator >= (op1: MPRational; op2: MPRational): boolean;
procedure TTestGmpOperators.ge__MPRational__MPRational_boolean;
const
  OP1_ = 12;
  OP2_ = OP1_ - 1;
var op1, op2: MPRational;
begin
  q_init(op1);
  q_init(op2);
  q_set_si(op1, OP1_, 1);
  q_set_si(op2, OP2_, 1);
  assert(op1 >= op2);
  q_set_si(op2, OP1_, 1);
  assert(op1 >= op2);
end;

