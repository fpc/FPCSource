{ %cpu=i386 }

{ Source provided for Free Pascal Bug Report 3460 }
{ Submitted by "Danny Milosavljevic" on  2004-12-22 }
{ e-mail: danny_milo@yahoo.com }

{$ifdef fpc}{$mode delphi}{$endif}

type
  TGValue=array[0..10] of byte;
  PGValue=^TGValue;

function x: Integer;
asm
  mov edx,0
  add edx, dword(sizeof(Integer))
  mov Result, edx
end;

function SignalHandlerNextParam(param: PGValue): PGValue;
asm
  mov edx, param
  add edx, dword(sizeof(TGValue))
  mov Result, edx
end;

var
  a : array[0..10] of TGValue;
begin
  SignalHandlerNextParam(@a[0]);
  writeln(x);
  if x<>sizeof(integer) then
    halt(1);
end.
