{ %VERSION=1.1 }
program testv3;

uses variants,varutils;

Procedure TestLongInt;

Var
  V : Variant;
  I,J : LongInt;

begin
  Writeln('Longint assignment');
  I:=1;
  V:=I;
  J:=V;
  Writeln('Result (',I,'): ',J);
end;

Procedure TestSmallInt;

Var
  V : Variant;
  I,J : SmallInt;

begin
  Writeln('Smallint assignment');
  I:=2;
  V:=I;
  J:=V;
  Writeln('Result (',I,'): ',J);
end;

Procedure TestShortInt;

Var
  V : Variant;
  I,J : ShortInt;

begin
  Writeln('ShortInt assignment');
  I:=3;
  V:=I;
  J:=V;
  Writeln('Result (',I,'): ',J);
end;

Procedure TestCardinal;

Var
  V : Variant;
  C,D : Cardinal;

begin
  Writeln('Cardinal assignment');
  C:=4;
  V:=C;
  D:=V;
  Writeln('Result (',C,'): ',D);
end;

Procedure TestWord;


Var
  V : Variant;
  W,X : Word;

begin
  Writeln('Word assignment');
  W:=5;
  V:=W;
  X:=V;
  Writeln('Result (',X,'): ',W);
end;

Procedure TestByte;


Var
  V : Variant;
  B,C : Byte;

begin
  Writeln('Byte assignment');
  B:=6;
  V:=B;
  C:=V;
  Writeln('Result (',B,'): ',C);
end;

// 64 bit values

Procedure TestInt64;

Var
  V : Variant;
  I,J : int64;

begin
  Writeln('Int64 assignment');
  I:=7;
  V:=I;
  J:=V;
  Writeln('Result (',I,'): ',J);
end;


Procedure TestQWord;

Var
  V : Variant;
  Q,R : QWord;

begin
  Writeln('QWord assignment');
  Q:=8;
  V:=Q;
  R:=V;
  Writeln('Result (',Q,'): ',R);
end;

begin
  TestLongint;
  TestSmallInt;
  TestShortInt;
  TestCardinal;
  TestWord;
  TestByte;
  TestInt64;
  TestQWord;
end.
