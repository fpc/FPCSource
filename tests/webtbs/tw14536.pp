program test_varbyref;

{$mode objfpc}{$H+}
{$apptype console}

uses
  Variants;

var
  V: Variant;
  P: Integer;
begin
  P := 1;
  TVarData(V).vtype := varbyref or varinteger;
  TVarData(V).vpointer := @P;
  WriteLn(string(V));
  WriteLn(SmallInt(V));
  WriteLn(Longint(V));
  WriteLn(ShortInt(V));
  WriteLn(Cardinal(V));
  WriteLn(Single(V));
  WriteLn(Double(V));
  WriteLn(TDateTime(V));
  WriteLn(Currency(V));
  WriteLn(Boolean(V));
  WriteLn(Byte(V));
  WriteLn(Int64(V));
  WriteLn(QWord(V));
  WriteLn(WideString(V));
  WriteLn(ShortString(V));
  WriteLn(V);
  TVarData(V).vtype := varEmpty;
  TVarData(V).vpointer := nil;
end.

