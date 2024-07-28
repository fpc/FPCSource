{ %cpu=wasm32 }

program twasminlineasm1;

const
  eps = 0.000001;

procedure Check(Cond: Boolean);
begin
  if not Cond then
  begin
    Writeln('Error!');
    Halt(1);
  end;
end;

function test_i32_const: longint; assembler; nostackframe;
asm
  i32.const 5
end;

function test_i64_const: int64; assembler; nostackframe;
asm
  i64.const 1234567890123456
end;

function test_f32_const: single; assembler; nostackframe;
asm
  f32.const 3.14159
end;

function test_f64_const: double; assembler; nostackframe;
asm
  f64.const 2.71828
end;

function test_i32_add(a, b: longint): longint; assembler; nostackframe;
asm
  local.get 0
  local.get 1
  i32.add
end;

function test_i64_add(a, b: int64): int64; assembler; nostackframe;
asm
  local.get 0
  local.get 1
  i64.add
end;

function test_f32_add(a, b: single): single; assembler; nostackframe;
asm
  local.get 0
  local.get 1
  f32.add
end;

function test_f64_add(a, b: double): double; assembler; nostackframe;
asm
  local.get 0
  local.get 1
  f64.add
end;

function test_i32_sub(a, b: longint): longint; assembler; nostackframe;
asm
  local.get 0
  local.get 1
  i32.sub
end;

function test_i64_sub(a, b: int64): int64; assembler; nostackframe;
asm
  local.get 0
  local.get 1
  i64.sub
end;

function test_f32_sub(a, b: single): single; assembler; nostackframe;
asm
  local.get 0
  local.get 1
  f32.sub
end;

function test_f64_sub(a, b: double): double; assembler; nostackframe;
asm
  local.get 0
  local.get 1
  f64.sub
end;

function test_i32_mul(a, b: longint): longint; assembler; nostackframe;
asm
  local.get 0
  local.get 1
  i32.mul
end;

function test_i64_mul(a, b: int64): int64; assembler; nostackframe;
asm
  local.get 0
  local.get 1
  i64.mul
end;

function test_f32_mul(a, b: single): single; assembler; nostackframe;
asm
  local.get 0
  local.get 1
  f32.mul
end;

function test_f64_mul(a, b: double): double; assembler; nostackframe;
asm
  local.get 0
  local.get 1
  f64.mul
end;

function test_i32_div_s(a, b: longint): longint; assembler; nostackframe;
asm
  local.get 0
  local.get 1
  i32.div_s
end;

function test_i64_div_s(a, b: int64): longint; assembler; nostackframe;
asm
  local.get 0
  local.get 1
  i64.div_s
end;

function test_i32_div_u(a, b: longword): longword; assembler; nostackframe;
asm
  local.get 0
  local.get 1
  i32.div_u
end;

function test_i64_div_u(a, b: qword): qword; assembler; nostackframe;
asm
  local.get 0
  local.get 1
  i64.div_u
end;

function test_call: longint; assembler; nostackframe;
asm
  i32.const 10
  i32.const 17
  call $test_i32_add
end;

begin
  Check(test_i32_const = 5);
  Check(test_i64_const = 1234567890123456);
  Check(Abs(test_f32_const - 3.14159) < eps);
  Check(Abs(test_f64_const - 2.71828) < eps);
  Check(test_i32_add(2, 3) = 5);
  Check(test_i64_add(12354312234, 654765532432) = 667119844666);
  Check(Abs(test_f32_add(1.23, 4.56) - 5.79) < eps);
  Check(Abs(test_f64_add(1.23456, 4.56789) - 5.80245) < eps);
  Check(test_i32_sub(2, 3) = -1);
  Check(test_i64_sub(12354312234, 654765532432) = -642411220198);
  Check(Abs(test_f32_sub(1.23, 4.56) + 3.33) < eps);
  Check(Abs(test_f64_sub(1.23456, 4.56789) + 3.33333) < eps);
  Check(test_i32_mul(1234, 9583) = 11825422);
  Check(test_i64_mul(1235454, 635456) = 785076657024);
  Check(Abs(test_f32_mul(1.23, 4.56) - 5.6088) < eps);
  Check(Abs(test_f64_mul(1.23456, 4.56789) - 5.639334278) < eps);
  Check(test_i32_div_s(-1023, 4) = -255);
  Check(test_i64_div_s(-4378294334934, 43) = -101820798486);
  Check(test_i32_div_u(3735928559, 5) = 747185711);
  Check(test_i64_div_u(16045690984833335023, 5) = 3209138196966667004);
  Check(test_call = 27);
  Writeln('Ok!');
end.
