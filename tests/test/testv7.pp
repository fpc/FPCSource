{ %VERSION=1.1 }
program testv2;

uses variants,varutils;

Procedure TestReal;

Var
  V : Variant;
  R : Real;

begin
  Writeln('Real assignment');
  R:=1.0E-1;
  V:=R;
  DumpVariant(TVarData(V));
end;

Procedure TestDouble;

Var
  V : Variant;
  R : Double;

begin
  Writeln('Double assignment');
  R:=2.0E-2;
  V:=R;
  DumpVariant(TVarData(V));
end;

Procedure TestExtended;

Var
  V : Variant;
  R : Extended;

begin
  Writeln('Extended assignment');
  R:=3.0E-3;
  V:=R;
  DumpVariant(TVarData(V));
end;

Procedure TestSingle;

Var
  V : Variant;
  R : Single;

begin
  Writeln('Single assignment');
  R:=4.0E-4;
  V:=R;
  DumpVariant(TVarData(V));
end;

begin
  TestReal;
  TestDouble;
  TestExtended;
  TestSingle;
end.
