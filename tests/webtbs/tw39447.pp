unit tw39447;
{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface
uses sysutils;

function ptr_twist() : boolean;

implementation

function ptr_twist() : boolean;
var
  x : pchar;
  x2 : pchar;
const
  str : ShortString = '123456789';
begin

  x := @(str[2]);
  x := x + 5;
  x2:= 5 + x;             // FPC fails here

  result := (x = x2);

end;

end.

