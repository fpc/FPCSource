{ %VERSION=1.1 }
program testv5;

{$ifopt R+}
  {$define skip_range_error_tests}
{$endif }
uses variants,varutils;

Procedure TestLongInt(I : Longint);

Var
  V : Variant;
  B : Boolean;

begin
  Write('Longint to boolean assignment: ');
  V:=I;
  B:=V;
  Writeln(I,' -> ',B);
end;

Procedure TestSmallInt(I : SmallInt);

Var
  V : Variant;
  B : Boolean;

begin
  Write('Smallint to boolean assignment: ');
  V:=I;
  B:=V;
  Writeln(I,' -> ',B);
end;

Procedure TestShortInt(I : ShortInt);

Var
  V : Variant;
  B : Boolean;

begin
  Write('Shortint to boolean assignment: ');
  V:=I;
  B:=V;
  Writeln(I,' -> ',B);
end;

Procedure TestCardinal(I : Cardinal);

Var
  V : Variant;
  B : Boolean;

begin
  Write('Cardinal to boolean assignment: ');
  V:=I;
  B:=V;
  Writeln(I,' -> ',B);
end;

Procedure TestWord(I : Word);

Var
  V : Variant;
  B : Boolean;

begin
  Write('Word to boolean assignment: ');
  V:=I;
  B:=V;
  Writeln(I,' -> ',B);
end;

Procedure TestByte(I : Byte);

Var
  V : Variant;
  B : Boolean;

begin
  Write('Byte to boolean assignment: ');
  V:=I;
  B:=V;
  Writeln(I,' -> ',B);
end;

// 64 bit values

Procedure TestInt64(I : Int64);

Var
  V : Variant;
  B : Boolean;

begin
  Write('Int64 to boolean assignment: ');
  V:=I;
  B:=V;
  Writeln(I,' -> ',B);
end;


Procedure TestQWord(I : QWord);

Var
  V : Variant;
  B : Boolean;

begin
  Write('QWord to boolean assignment: ');
  V:=I;
  B:=V;
  Writeln(I,' -> ',B);
end;

begin
  TestLongint(0);
  TestSmallInt(0);
  TestShortInt(0);
  TestCardinal(0);
  TestWord(0);
  TestByte(0);
  TestInt64(0);
  TestQWord(0);
  TestLongint(1);
  TestSmallInt(1);
  TestShortInt(1);
  TestCardinal(1);
  TestWord(1);
  TestByte(1);
  TestInt64(1);
  TestQWord(1);
  TestLongint(-1);
  TestSmallInt(-1);
  TestShortInt(-1);
  TestInt64(-1);
{$ifndef skip_range_error_tests}
  TestCardinal(-1);
  TestWord(-1);
  TestByte(-1);
  TestQWord(-1);
{$endif ndef skip_range_error_tests}
end.
