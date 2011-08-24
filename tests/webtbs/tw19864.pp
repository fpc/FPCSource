{ %NORUN }
unit tw19864;

{$mode objfpc}

interface

implementation

procedure testfun;
begin
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN SYMBOL_PLATFORM ON}
  {$PUSH}{$R-,Q-}
  {$POP}
  {$PUSH}{$R-,Q-}
  {$POP}
end;
  {$WARN SYMBOL_PLATFORM OFF}
var
  test1 : longint platform;
  {$WARN SYMBOL_PLATFORM ON}

var
  test2 : longint platform;

begin
  {$WARN SYMBOL_PLATFORM OFF}
  test2:=2;
  {$WARN SYMBOL_PLATFORM ON}
  test1:=1;
end.
