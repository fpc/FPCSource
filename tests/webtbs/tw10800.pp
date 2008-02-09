program widestringdelete;
{$ifdef FPC}{$mode objfpc}{$h+}{$INTERFACES CORBA}{$endif}
{$ifdef mswindows}{$apptype console}{$endif}
uses
 {$ifdef FPC}{$ifdef unix}cthreads,{$endif}{$endif}
 sysutils;
var
 wstr1: widestring;
begin
 setlength(wstr1,10240);
 delete(wstr1,1,10230);
end.

