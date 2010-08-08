program test_varbyref;

{$mode objfpc}{$H+}
{$apptype console}

uses
  Variants, VarUtils;

var
  V: Variant;
  P: Integer;
begin
  P := 1;
  TVarData(V).vtype := varbyref or varinteger;
  TVarData(V).vpointer := @P;
  WriteLn(VariantToAnsiString(TVarData(V)));
  WriteLn(VariantToSmallInt(TVarData(V)));
  WriteLn(VariantToLongint(TVarData(V)));
  WriteLn(VariantToShortint(TVarData(V)));
  WriteLn(VariantToCardinal(TVarData(V)));
  WriteLn(VariantToSingle(TVarData(V)));
  WriteLn(VariantToDouble(TVarData(V)));
  WriteLn(VariantToDate(TVarData(V)));
  WriteLn(VariantToCurrency(TVarData(V)));
  WriteLn(VariantToBoolean(TVarData(V)));
  WriteLn(VariantToByte(TVarData(V)));
  WriteLn(VariantToInt64(TVarData(V)));
  WriteLn(VariantToQWord(TVarData(V)));
  WriteLn(VariantToWideString(TVarData(V)));
  WriteLn(VariantToAnsiString(TVarData(V)));
  WriteLn(VariantToShortString(TVarData(V)));
  WriteLn(V);
  TVarData(V).vtype := varEmpty;
  TVarData(V).vpointer := nil;
end.

