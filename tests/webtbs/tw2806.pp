{ %cpu=i386 }
{ %OPT=-Cg- }

{$ifdef fpc}{$asmmode intel}{$endif}

type
  tptentry=record
    l1,l2 : longint;
  end;
var
  piecetab : array[0..10] of tptentry;
  p1,p2 : pointer;
begin
  p1:=@piecetab[8];
  asm
    lea ecx,PieceTab+8 * type(tPTEntry)
    mov p2,ecx
  end;
  if p1<>p2 then
    begin
      writeln('Error!');
      halt(1);
    end;
end.
