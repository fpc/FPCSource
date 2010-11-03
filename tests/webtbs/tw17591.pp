program comparetext;
{$ifdef FPC}{$mode objfpc}{$h+}{$endif}
{$ifdef mswindows}{$apptype console}{$endif}
uses
 sysutils
{$ifdef unix}
 ,cwstring
{$endif}
;
var
 int1: integer;
 ustr1,ustr2: unicodestring;
begin
 ustr1:= 'A';
 ustr2:= 'a';
 if unicodecomparestr(ustr1,ustr2)=0 then
   halt(1);
 if unicodecomparetext(ustr1,ustr2)<>0 then
   halt(2);
end.
