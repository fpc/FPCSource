unit uwasmglobal2;

interface

procedure RunTest;

implementation

const
  eps = 0.000001;

var
  global_i32: LongInt; section 'WebAssembly.Global';
  global_i64: Int64; section 'WebAssembly.Global';
  global_f32: Single; section 'WebAssembly.Global';
  global_f64: Double; section 'WebAssembly.Global';

procedure Add(a: Integer);
begin
  Inc(global_i32, a);
  Inc(global_i64, a);
  global_f32 := global_f32 + a;
  global_f64 := global_f64 + a;
end;

procedure Check(Cond: Boolean);
begin
  if not Cond then
  begin
    Writeln('Error!');
    Halt(1);
  end;
end;

procedure RunTest;

begin
  global_i32 := 5;
  global_i64 := 17;
  global_f32 := 3.14;
  global_f64 := -123.456;
  Add(5);
  Check(global_i32 = 10);
  Check(global_i64 = 22);
  Check(Abs(global_f32 - 8.14) < eps);
  Check(Abs(global_f64 - (-118.456)) < eps);
  Writeln('Ok!');
end;

end.
