{ Source provided for Free Pascal Bug Report 2946 }
{ Submitted by "Marco (Gory Bugs Department)" on  2004-02-06 }
{ e-mail:  }

{$ifdef fpc}{$mode Delphi}{$endif}
var p:array of pchar;
    t: ^pchar;
begin
  p:=pointer(t);
end.
