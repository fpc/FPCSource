{ %CPU=i386 }
{ %OPT=-Cg- }
{$asmmode intel}
Type TFather = Object A : Integer; end;
     TSon = Object (TFather) B : Integer; end;

Var Son : TSon;

begin
  Asm
    mov  ax, Son.A
    mov  ax, Son.B
  end;
end.
