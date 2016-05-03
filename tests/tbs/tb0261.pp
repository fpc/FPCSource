{ %CPU=i386 }
{ %OPT=-Cg- }
{ Old file: tbs0304.pp }
{ Label redefined when inlining assembler              OK 0.99.13 (PFV) }

{$asmmode intel}
{$inline on}

var
  cb : word;

procedure A(B: word); assembler; inline;
{$ifdef CPUI386}
asm
   MOV  AX,B
   CMP  AX,[CB]
   JZ   @@10
   MOV  [CB],AX
@@10:
end;
{$endif CPUI386}
{$ifdef CPU68K}
asm
   move.w  b,d0
   cmp.w   cb,d0
   beq     @L10
   move.w  d0,cb
@L10:
end;
{$endif CPU68K}

begin
  a(1);
  a(2);
end.
