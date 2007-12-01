{ %CPU=i386 }
{ %OPT=-Cg- }
{ Old file: tbs0276.pp }
{ Asm, intel reference parsing incompatibility         OK 0.99.13 (PFV) }

{$asmmode intel}
type
  trec = record
    ypos,
    xpos : longint;
  end;

  z80cont = record
     dummy : longint;
     page: array [0..11,0..16383] of byte;
  end;

var
  rec : tRec;
  myz80 : z80cont;
  error : boolean;
  test  : byte;
begin
  error:=false;
  test:=23;
  rec.xpos:=1;
  myz80.page[0,5]:=15;
  asm
     lea   edi, Rec
     cmp   byte ptr [edi+tRec.Xpos], 1
     jne   @error
     cmp   byte ptr [edi].trec.Xpos, 1
     jne   @error
     mov   ecx, 5
     mov   dh,byte ptr myz80.page[ecx]
     cmp   dh,15
     jne   @error
     mov   byte ptr myz80.page[ecx],51
     jmp   @noerror
     @error:
     mov   byte ptr error,1
     @noerror:
  end;
  if error or (test<>23) or (myz80.page[0,5]<>51) then
    begin
      Writeln('Error in assembler code generation');
      Halt(1);
    end
  else
    Writeln('Correct assembler generated');
end.
