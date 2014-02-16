{ this tests that class methods can be used in type helpers }

program tthlp9;

{$mode objfpc}
{$modeswitch typehelpers}
{$apptype console}

type
  TInt32Helper = type helper for Int32
    class function Test: LongInt; static;
  end;

  TShortStringHelper = type helper for ShortString
    class function Test: LongInt; static;
  end;

class function TInt32Helper.Test: LongInt;
begin
  Result := SizeOf(Int32);
end;

class function TShortStringHelper.Test: LongInt;
begin
  Result := SizeOf(AnsiChar);
end;

var
  i: LongInt;
begin
  if LongInt.Test <> 4 then
    Halt(1);
  if i.Test <> 4 then
    Halt(2);
  if ShortString.Test <> 1 then
    Halt(3);
  Writeln('OK');
end.
