{ %VERSION=1.1 }
program testv8;

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
  R  : Real;
  D : Double;
  E : Extended;
  S : Single;
  Bo : Boolean;

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
  R:=V;
  Writeln('To Real : ',R);
  D := v;
  Writeln('To  Double : ',D);
  E := v;
  Writeln('To  Extended : ',E);
  S := v;
  Writeln('To  Single : ',S);
  Bo := v;
  Writeln('To  Boolean : ',Bo);
end;


Procedure TestReal(R : Real);

Var
  V : Variant;

begin
  V:=R;
  TestConvert(V);
  V:=-R;
  TestConvert(V);
end;

Procedure TestDouble(R : Double);

Var
  V : Variant;

begin
  V:=R;
  TestConvert(V);
  V:=-R;
  TestConvert(V);
end;

Procedure TestSingle(R : Single);

Var
  V : Variant;

begin
  V:=R;
  TestConvert(V);
  V:=-R;
  TestConvert(V);
end;

Procedure TestExtended(R : Extended);

Var
  V : Variant;

begin
  V:=R;
  TestConvert(V);
  V:=-R;
  TestConvert(V);
end;

begin
  TestReal(1.0E-1);
  TestDouble(2.0E-2);
  TestSingle(3.0E-3);
  TestExtended(4.0E-4);
  TestReal(1.0E1);
  TestDouble(2.0E2);
  TestSingle(3.0E3);
  TestExtended(4.0E4);
  TestReal(0.0);
  TestDouble(0.0);
  TestSingle(0.0);
  TestExtended(0.0);
  TestReal(1.0E-39);
  TestDouble(2.0E-39);
  TestSingle(3.0E-39);
  TestExtended(4.0E-39);
end.
