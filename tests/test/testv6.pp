{ %VERSION=1.1 }
program testv6;

uses variants,varutils;

Procedure TestLongInt(B : Boolean);

Var
  V : Variant;
  I : LongInt;

begin
  Write('Longint assignment : ',B,' -> ');
  V:=B;
  I:=V;
  Writeln(I);
end;

Procedure Testsmallint(B : Boolean);

Var
  V : Variant;
  I : smallint;

begin
  Write('smallint assignment : ',B,' -> ');
  V:=B;
  I:=V;
  Writeln(I);
end;


Procedure TestShortInt(B : Boolean);

Var
  V : Variant;
  I : ShortInt;

begin
  Write('ShortInt assignment : ',B,' -> ');
  V:=B;
  I:=V;
  Writeln(I);
end;

Procedure TestCardinal(B : Boolean);

Var
  V : Variant;
  I : Cardinal;

begin
  Write('Cardinal assignment : ',B,' -> ');
  V:=B;
  I:=V;
  Writeln(I);
end;

Procedure TestWord(B : Boolean);

Var
  V : Variant;
  I : Word;

begin
  Write('Word assignment : ',B,' -> ');
  V:=B;
  I:=V;
  Writeln(I);
end;

Procedure TestByte(B : Boolean);

Var
  V : Variant;
  I : Byte;

begin
  Write('Byte assignment : ',B,' -> ');
  V:=B;
  I:=V;
  Writeln(I);
end;


Procedure TestInt64(B : Boolean);

Var
  V : Variant;
  I : Int64;

begin
  Write('Int64 assignment : ',B,' -> ');
  V:=B;
  I:=V;
  Writeln(I);
end;

Procedure TestQWord(B : Boolean);

Var
  V : Variant;
  I : QWord;

begin
  Write('QWord assignment : ',B,' -> ');
  V:=B;
  I:=V;
  Writeln(I);
end;

begin
  TestLongint(True);
  TestSmallInt(True);
  TestShortInt(True);
  TestCardinal(True);
  TestWord(True);
  TestByte(True);
  TestInt64(True);
  TestQWord(True);
  TestLongint(False);
  TestSmallInt(False);
  TestShortInt(False);
  TestCardinal(False);
  TestWord(False);
  TestByte(False);
  TestInt64(False);
  TestQWord(False);
end.
