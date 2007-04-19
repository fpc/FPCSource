program EnumPtrConvTest;

{$APPTYPE CONSOLE}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

type
  TEnum = (a, b);

var
  e: TEnum;
  p: Pointer;

begin
  e := b;
  p := Pointer(e);
  WriteLn(Integer(p)); // produces "1" in Delphi
end.
