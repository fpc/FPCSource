{ %cpu=i386 }

{ Source provided for Free Pascal Bug Report 2631 }
{ Submitted by "Arnstein" on  2003-08-12 }
{ e-mail: Arnstein.Prytz@jcu.edu.au }

{$ifdef fpc}{$mode delphi}{$endif}

function d : int64;
  begin
    asm
      xor ecx,ecx
      mov dword ptr Result,$00000000;
      mov dword ptr Result+4,$00100000;
      mov dword ptr Result[4+ECX*2],$00100000;
    end;
  end;

begin
  WRITELN( d );
  if d<>$0010000000000000 then
    halt(1);
end.
