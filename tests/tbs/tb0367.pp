{ %CPU=i386 }
{ %VERSION=1.1 }

{$ifdef fpc}
  {$mode delphi}
  {$asmmode intel}
{$endif}

function LRot(Value:Byte) : Byte; assembler;
asm
        MOV     CL, Value
        MOV     Result, CL
        MOV     AL, 20
end;


var
  i : Byte;
begin
  i:=LRot(10);
  writeln('LRot(10) = ',i,' (should be 10)');
  if i<>10 then
   begin
     writeln('ERROR!');
     halt(1);
   end;
end.
