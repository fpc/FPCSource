{ %cpu=i386 }

{$mode delphi}

{ Source provided for Free Pascal Bug Report 2703 }
{ Submitted by "Johannes Berg" on  2003-10-01 }
{ e-mail: johannes -at- sipsolutions -dot- de }
function InterlockedIncrement(var I: Integer): Integer;
asm
        MOV     EDX,1
        XCHG    EAX,EDX
  LOCK  XADD    [EDX],EAX
        INC     EAX
end;

var
  i : integer;
begin
  i:=1;
  i:=InterlockedIncrement(i);
  if i<>2 then
   halt(1);
end.
