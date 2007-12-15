{ %cpu=i386 }
{ %OPT=-Cg- }

{$asmmode intel}

procedure SmallForwardMove_3; assembler;
asm
jmp dword ptr [@@FwdJumpTable+ecx*4]
nop {Align Jump Table}
@@Done:
@@FwdJumpTable:
dd @@Done {Removes need to test for zero size move}
end;

begin
end.
