{ %cpu=i386 }

{ Source provided for Free Pascal Bug Report 3274 }
{ Submitted by "Frank Kintrup" on  2004-08-27 }
{ e-mail: frank.kintrup@gmx.de }
{$MODE Delphi}
{$ASMMODE Intel}

var
  X : Integer;
  B : byte;
begin
   X:=10;
   asm
        LEA  ESI, X
        MOV  AL, Byte([ESI])
        MOV  B, AL
   end;
   writeln(b);
   if b<>10 then
     halt(1);
end.
