{ %FAIL }
{ Old file: tbf0347.pp }
{  }

{$mode delphi}

type x = ^longint;

var y:x;

begin
 y [5]:=5;
end.
