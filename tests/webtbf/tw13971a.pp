{ %fail }

{$ifdef fpc}
{$mode objfpc}
{$endif}

type
  tc = class
    function getx(i: longint): longint;
    property prop[i: longint]: longint read getx;
    default: longint;
  end;

function tc.getx(i: longint): longint;
begin
end;

begin
end.
