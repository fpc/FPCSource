{ %CPU=i386 }
{ Source provided for Free Pascal Bug Report 1023 }
{ Submitted by "Denis Yarkovoy" on  2000-07-03 }
{ e-mail: gunky9@geocities.com }
 {$goto on}
 {$asmmode intel}
 label l1;

var
  pp : pointer;

 procedure p1; assembler; asm
  mov eax, offset l1
  lea edi,pp
  mov dword ptr [edi],eax
 end;

 procedure p; assembler; asm
  l1:
  clc
 end;

begin
 pp:=nil;
 p1;
 if pp=nil then
   halt(1)
 else
   Writeln('Bug 1023 fixed');
end.
