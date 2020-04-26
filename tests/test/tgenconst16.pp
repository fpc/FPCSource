{ %NORUN }
{$mode objfpc}
{$modeswitch advancedrecords}
{
  various operator tests
}
program tgenconst16;

type
  Day = (mon,tue,wed,thu,fri,sat,sun);  
  Days = set of Day;  
  generic TSet<const I: Days> = record
    const
      d0 = I + I;   // Union
      d1 = I - I;   // Difference
      d2 = I * I;   // Intersection
      d3 = I >< I;  // Symmetric difference
      d4 = I <= I;  // Contains
      d5 = mon in I;
  end;
  generic TArray<const I: integer> = record
    type
      t0 = array[0..I - 1] of integer;
      t1 = array[0..high(I)] of integer;
      t2 = array[0..low(I)] of integer;
      t3 = array[0..sizeof(I)] of integer;
    public
      d0: array[0..I - 1] of integer;
      d1: array[0..high(I)] of integer;
      d2: array[0..low(I)] of integer;
      d3: array[0..sizeof(I)] of integer;
  end;
  generic TUnaryOp<const I: integer> = record
    const
      d0 = -I;
      d1 = +I;
      d2 = not I;
  end;
  generic TBinaryOp<const I: integer> = record
    const
      // Arithmetic operators
      // https://freepascal.org/docs-html/ref/refsu45.html
      d0 = I + I;
      d1 = I - I;
      d2 = I * I; 
      d3 = I / I; 
      d4 = I div I; 
      d5 = I mod I; 
      // Boolean operators
      // https://freepascal.org/docs-html/ref/refsu47.html
      d6 = I and I;
      d7 = I or I;
      d8 = I xor I;
      // Logical operators
      // https://freepascal.org/docs-html/ref/refsu46.html
      d9 = I shl I;
      d10 = I shr I;
      d11 = I << I;
      d12 = I >> I;
      // Relational operators
      // https://freepascal.org/docs-html/ref/refsu50.html#x153-17500012.8.6
      d13 = I <> I;
      d14 = I < I;
      d15 = I > I;
      d16 = I <= I;
      d17 = I >= I;
      d18 = I = I;
  end;
  generic TOther<const I: integer> = record
    procedure DoThis(param: integer = I);
  end;

procedure TOther.DoThis(param: integer = I);
begin
  writeln(param, ' default:', I);
end;

begin
end.
