{ %KNOWNCOMPILEERROR=Operator precedence is not influenced by types }
{ Source provided for Free Pascal Bug Report 2656 }
{ Submitted by "marco" on  2003-08-27 }
{ e-mail: marco@freepascal.org }
{$ifdef fpc}{$mode Delphi}{$endif}
var p1,p2 : pchar;
    a,b,c : longint;

begin

 c:=a+(p1-p2)+3;   // fpc accepts this. Delphi ?

 // vs

 c:=a+p1-p2+3;     // delphi accepts this, fpc not.

end.
