function asmstr:string;assembler;
asm
	movl	__RESULT,%edi
	movl	$0x4101,%al
	stosw
end;

procedure aasmstr(l : longint);assembler;
asm
	movl	l,%edi
end;

begin
  writeln(asmstr);
end.