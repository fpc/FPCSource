{ %version=1.1 }
program testv2;

uses variants,varutils;

Procedure TestLongInt;

Var
  V : Variant;
  I : LongInt;

begin
  Writeln('Longint assignment');
  I:=1;
  V:=I;
  DumpVariant(TVarData(V));
end;

Procedure TestSmallInt;

Var
  V : Variant;
  I : SmallInt;

begin
  Writeln('Smallint assignment');
  I:=2;
  V:=I;
  DumpVariant(TVarData(V));
end;

Procedure TestShortInt;

Var
  V : Variant;
  I : ShortInt;

begin
  Writeln('ShortInt assignment');
  I:=3;
  V:=I;
  DumpVariant(TVarData(V));
end;

Procedure TestCardinal;

Var
  V : Variant;
  C : Cardinal;

begin
  Writeln('Cardinal assignment');
  C:=4;
  V:=C;
  DumpVariant(TVarData(V));
end;

Procedure TestWord;


Var
  V : Variant;
  W : Word;

begin
  Writeln('Word assignment');
  W:=5;
  V:=W;
  DumpVariant(TVarData(V));
end;

Procedure TestByte;


Var
  V : Variant;
  B : Byte;

begin
  Writeln('Byte assignment');
  B:=6;
  V:=B;
  DumpVariant(TVarData(V));
end;

// 64 bit values

Procedure TestInt64;

Var
  V : Variant;
  I : int64;

begin
  Writeln('Int64 assignment');
  I:=7;
  V:=I;
  DumpVariant(TVarData(V));
end;


Procedure TestQWord;

Var
  V : Variant;
  Q : QWord;

begin
  Writeln('QWord assignment');
  Q:=8;
  V:=Q;
  DumpVariant(TVarData(V));
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
