{ Old file: tbs0174.pp }
{ Asm, offsets of fields are not possible yet           OK 0.99.9 (PFV) }

{$ASMMODE ATT}

type
  tobj=object
   l : longint;
  end;
var
  t : tobj;

procedure kl;assembler;
asm
  movl tobj.l,%eax        // tobj.l should return the offset of l in tobj
end;


begin
end.


