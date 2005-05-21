{ Source provided for Free Pascal Bug Report 2834 }
{ Submitted by "Mattias Gaertner" on  2003-12-06 }
{ e-mail: matias@freepascal.org }

{$ifdef fpc}{$mode objfpc}{$H+}{$endif}

uses
  uw2834;

var
  l1,l2 : pointer;
begin
  writeln('Should be the same:');
  l1:=PrintTypeInfo;
  l2:=pointer(TypeInfo(TMyType));
  writeln(cardinal(l1),' - ',cardinal(l2));
  if l1<>l2 then
    halt(1);
end.
