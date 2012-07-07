{ %fail }

{$ifdef fpc}
{$mode delphi}
{$endif}

unit tw10425d;

interface

function test(var f: file of byte): longint;

implementation


function test(var f: file of byte): longint;
begin
end;

end.
