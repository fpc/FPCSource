{ %cpu=i386 }

{ Source provided for Free Pascal Bug Report 2666 }
{ Submitted by "marcov" on  2003-09-06 }
{ e-mail: marco@freepascal.org }

{$mode delphi}

function GetDynamicMethod(AClass: TClass; Index: Integer): Pointer; assembler;
asm
        CALL    System.@Paramcount
end;

begin
  GetDynamicMethod(nil,0);
end.
