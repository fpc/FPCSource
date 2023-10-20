{ %CPU=x86_64,i386 }

program tw40451;

{$mode objfpc}
{$asmmode att}
procedure DoubleUint32ToTheLeft(x: pointer); {$ifdef cpu386}register;{$endif} assembler; nostackframe;
asm
	movl -4(x), %edx // Becomes "movl (x), %edx"
	shl  $1, %edx
	movl %edx, -4(x) // Becomes "movl %edx, (x)"
end;

var
	a: array[0 .. 2] of uint32 = (10, 11, 12);

begin
	DoubleUint32ToTheLeft(@a[1]);
	writeln('Got:      ', a[0], ' ', a[1], ' ', a[2]);
	writeln('Expected: 20 11 12');
        if (a[0] <> 20) or (a[1] <> 11) or (a[2] <> 12) then
          Halt(1);
end.

