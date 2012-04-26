program tbyte;

{$mode delphi}

uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

function test: longint;
var
 a : longword;
begin
 a := 123456789;
 result := JLInteger.Create(Byte(a)).intValue;
end;

begin
  if test<>21 then
    raise JLException.create('boe!');
end.
