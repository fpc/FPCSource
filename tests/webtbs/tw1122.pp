{ %version=1.1 }
{ %cpu=i386 }
{$mode objfpc}
{ Source provided for Free Pascal Bug Report 1122 }
{ Submitted by "Thomas Schatzl" on  2000-08-31 }
{ e-mail: tom_at_work@yline.com }

uses
   sysutils;

var
   error : boolean;

begin
  error:=true;
  try
     asm
        // invalid opcode, e.g. SSE instruction
        // try several opcode to get the invalid instruction exception
        movaps %xmm6, %xmm7
        .byte 0x0f,0xc7,0xc8
        .byte 1,2,3,4
     end;
  except
     error:=false;
  end;
  if error then
    begin
       writeln('Invalid opcode exception doesn'' work');
       halt(1);
    end;
end.
