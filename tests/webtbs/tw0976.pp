{ Source provided for Free Pascal Bug Report 976 }
{ Submitted by    }
{ e-mail:  }
Program Test_Me;

type PDouble = ^Double;
var A, B: PDouble;
    x: Double;

Operator + (x: Double; A: PDouble) B: Double;

  begin
  B := x + A^;
  end;

{ This was wrong because B value is not initialized !!
Operator + (x: Single; A: PDouble) B: PDouble;

  begin
  B^ := x + A^;
  end; }

begin
new (A);
new (B);
x  := 0.5;
A^ := x;

{--- Addition "Double + Double": OK}
B^ := x + A^;
writeln (B^:4:2);
if B^<>1.0 then
  Halt(1);
{---Identical error messages for addition "PDouble + Double" and "Double + PDouble"}
{---in spite of overloaded + operator}
// B := A + x;
B^ := x + A;
writeln (B^:4:2);
if B^<>1.0 then
  Halt(1);
end.
