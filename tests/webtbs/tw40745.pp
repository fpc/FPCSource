program test;
{$mode objfpc}
type
  TSetOfChar = set of char;
  TMyObject=class
    class function Method(setofchar: TSetOfChar): Boolean; inline;
  end;

class function TMyObject.Method(setofchar: TSetOfChar): Boolean;
var sym: char=#0;
begin
  Result:=sym in setofchar;
end;

begin
  TMyObject.Method([]);
end.