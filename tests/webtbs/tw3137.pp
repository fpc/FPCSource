{ Source provided for Free Pascal Bug Report 3137 }
{ Submitted by "Vincent Snijders" on  2004-06-04 }
{ e-mail: vslist@zonnet.nl }
unit tw3137;

{$ifdef fpc}{$mode delphi}{$endif}

interface

function a: integer; assembler;

implementation

function a: integer;
asm
end;

end.
