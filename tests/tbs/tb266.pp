{ Old file: tbs0304.pp }
{ Label redefined when inlining assembler              OK 0.99.13 (PFV) }

{$asmmode intel}
{$inline on}

var
  cb : word;

procedure A(B: word); assembler; inline;
asm
   MOV  AX,B
   CMP  AX,[CB]
   JZ   @@10
   MOV  [CB],AX
@@10:
end;

begin
  a(1);
  a(2);
end.