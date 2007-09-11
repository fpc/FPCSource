program av;
{$ifdef FPC}{$mode objfpc}{$h+}{$INTERFACES CORBA}{$endif}
{$ifdef mswindows}{$apptype console}{$endif}
uses
 {$ifdef FPC}{$ifdef unix}cthreads,cwstring,{$endif}{$endif}sysutils;
type
 testrecty = record
  str: widestring;
  int: integer;
 end;

var
 tr1,tr2: testrecty;
 
begin
 tr1.str:= 'abc';
 tr1.int:= 0;
 tr2:= tr1;
end.
