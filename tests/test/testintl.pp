program test_intel_syntax;

{$ifdef CPU86}
var
  sti : longint;

{$asmmode intel}
function get_sti_value : longint;assembler;
asm
  mov eax,dword ptr [sti]
end;

function get_sti_addr : pointer;assembler;
asm
  mov eax,sti
end;


{$endif CPU86}

begin
{$ifdef CPU86}
  sti:=56;
  if get_sti_value<>sti then
    begin
      Writeln(' "mov eax,sti" does not get the address of sti var');
      Halt(1);
    end;
  if get_sti_addr<>@sti then
    begin
      Writeln(' "mov eax,sti" does not get the address of sti var');
      Halt(1);
    end;
{$endif CPU86}
end.
