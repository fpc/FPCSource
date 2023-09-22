{ %NORUN }
{ %CPU=i386,x86_64 }

program tw40390;

{$asmmode att}
procedure SetMiddleTo5(p: pointer; n: SizeUint); {$ifdef cpu386}register;{$endif} assembler; nostackframe;
asm
{$ifdef cpu386}
    shrl $1, n     // becomes “shrq $1, %rdx” (Win64) or “shrq $1, %rsi” (System V)
    movb $5, (p,n) // Invalid reference syntax.
    movb $5, (%edx,n)
    movb $5, (p,%edx)
{$elseif defined(cpux86_64)}
    shrq $1, n     // becomes “shrq $1, %rdx” (Win64) or “shrq $1, %rsi” (System V)
    movb $5, (p,n) // Invalid reference syntax.
    movb $5, (%rdx,n)
    movb $5, (p,%rdx)
{$endif}
end;

begin
end.
