
function asmstr:string;assembler;
asm
	movl	__RESULT,%edi
	movl	$0x4101,%al
	stosw
end;

begin
  writeln(asmstr);
end;