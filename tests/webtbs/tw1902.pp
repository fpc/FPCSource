{ %cpu=i386 }
{ %OPT=-Cg- }

{$ifdef fpc}
{$MODE DELPHI}
{$ASMMODE INTEL}
{$endif}

const myoffset=10;

var
  r : array[0..19] of char;
  c : char;
begin
  r:='01234567890123456789';
  asm
   lea eax,r
   mov al,[eax].myoffset
   mov c,al
  end;
  writeln(c);
  if c<>'0' then
   begin
     writeln('ERROR!');
     halt(1);
   end;
end.
