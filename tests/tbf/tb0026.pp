{ %FAIL }
{ Old file: tbf0153.pp }
{ Asm, indexing a local/para var should produce an error like tp7 OK 0.99.9 (PFV) }

{$asmmode att}

procedure asmfunc(p:pointer);assembler;
asm
{
  this is changed into movl %eax,(%ebx+8) which is not correct, and tp7
  also doesn't allow 'mov p[bx],ax' or 'mov p+bx,ax'

  Solution: for parameters and locals the index must be turned off

  Don't forget to check the intel assembler also
}
        movl    %eax,p(%ebx)
end;

begin
end.
