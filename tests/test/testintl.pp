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


function get_local_value : longint;
var
  x : longint;
begin
  x:=66;
  asm
    mov eax,dword ptr [x]
    mov @result,eax
  end;
end;

function get_local_addr : pointer;
var
  x : longint;
begin
  x:=66;
  asm
    mov eax,x
    mov @result,eax
  end;
end;

const
  program_has_error : boolean = false;
{$endif CPU86}

begin
{$ifdef CPU86}
  sti:=56;
  if get_sti_value<>sti then
    begin
      Writeln(' "mov eax,dword ptr [sti]" does not get the value of static sti var');
      program_has_error:=true;
    end;
  if get_sti_addr<>@sti then
    begin
      Writeln(' "mov eax,sti" does not get the address of sti var');
      program_has_error:=true;
    end;
  if get_local_value<>66 then
    begin
      Writeln(' "mov eax,dword ptr [x]" does not get the value of local x var');
      program_has_error:=true;
    end;
  if longint(get_local_addr)=66 then
    begin
      Writeln(' "mov eax,x"  gets the value of local x var');
      program_has_error:=true;
    end;
  if program_has_error then
    Halt(1);
{$endif CPU86}
end.