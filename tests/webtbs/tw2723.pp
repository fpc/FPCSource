{ %cpu=i386 }

{ Source provided for Free Pascal Bug Report 2723 }
{ Submitted by "marco" on  2003-10-07 }
{ e-mail:  }
{$ifdef fpc}{$mode delphi}{$endif}
function GetClassParent(AClass: TClass): TClass; assembler;
asm
        MOV     EAX, [AClass].vmtParent      // line 1324
        TEST    Result, EAX
        JE      @@Exit
        MOV     EAX, [EAX]
@@Exit:
end;

begin
end.
