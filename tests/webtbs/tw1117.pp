{ %CPU=i386 }
{ %OPT=-Cg- }
{$asmmode intel}
var
  l1,l2 : longint;

procedure DrawSprite1( spr : longint ); assembler;
asm
    mov eax,spr
    mov l1, eax
end;

procedure DrawSprite2( spr : longint );
begin
asm
    mov eax,spr
    mov l2,eax
end;
end;

begin
  DrawSprite1(1);
  DrawSprite2(1);
  if l1<>l2 then
   begin
     Writeln('Error!');
     halt(1);
   end;
end.
