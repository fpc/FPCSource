{ %cpu=x86_64 }
{ %opt=-Sew }
{ %fail }

{$asmmode intel}
begin
	asm
		lea   rax, [rsp-32] // (1)
		lea   rax, @table[rip]
		lea   rsp, [rsp+32]
		jmp   @next
		db    0, 1, 2, 3, 4, 5, 6, 7
	@table:
		db    8, 9, 10
	@next:
	end ['rax', 'xmm0'];
end.
