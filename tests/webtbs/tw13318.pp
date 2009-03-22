program fpctest4;
{$ifdef fpc}
{$mode delphi}
{$endif fpc}

uses
  Classes;

var 
  f:TStream;
  l: longint;
begin
   l:=1;
   f:=TMemoryStream.Create;
   f.position:=-1;
   if (f.write(l,4)<>0) then
     halt(1);
   f.free;
end.
