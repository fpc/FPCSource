{ %VERSION=1.1 }
program testv3;

uses variants,varutils;

Procedure TestConvert(Var V : Variant);

Var
  I64 : Int64;
  LI  : Longint;
  SI  : Smallint;
  HI  : Shortint;
  Q : QWord;
  C  : Cardinal;
  W  : Word;
  B  : Byte;

begin
  DumpVariant(TVarData(V));
  I64:=V;
  Writeln('To Int64 : ',I64);
  LI:=V;
  Writeln('To Longint : ',LI);
  SI:=V;
  Writeln('To Smallint : ',SI);
  HI:=V;
  Writeln('To Shortint : ',HI);
  Q:=V;
  Writeln('To QWord : ',Q);
  C:=V;
  Writeln('To Cardinal : ',C);
  W:=V;
  Writeln('To Word : ',W);
  B:=V;
  Writeln('To Byte : ',B);
end;


Procedure TestLongInt;

Var
  V : Variant;
  I : LongInt;

begin
  I:=1;
  V:=I;
  TestConvert(V);
end;

Procedure TestSmallInt;

Var
  V : Variant;
  I : SmallInt;

begin
  I:=2;
  V:=I;
  TestConvert(V);
end;

Procedure TestShortInt;

Var
  V : Variant;
  I : ShortInt;

begin
  I:=3;
  V:=I;
  TestConvert(V);
end;

Procedure TestCardinal;

Var
  V : Variant;
  C : Cardinal;

begin
  C:=4;
  V:=C;
  TestConvert(V);
end;

Procedure TestWord;


Var
  V : Variant;
  W : Word;

begin
  W:=5;
  V:=W;
  TestConvert(V);
end;

Procedure TestByte;


Var
  V : Variant;
  B : Byte;

begin
  B:=6;
  V:=B;
  TestConvert(V);
end;

// 64 bit values

Procedure TestInt64;

Var
  V : Variant;
  I : int64;

begin
  I:=7;
  V:=I;
  TestConvert(V);
end;


Procedure TestQWord;

Var
  V : Variant;
  Q : QWord;

begin
  Q:=8;
  V:=Q;
  TestConvert(V);
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
