program comparetext;
{$ifdef FPC}{$mode objfpc}{$h+}{$endif}
{$ifdef mswindows}{$apptype console}{$endif}
{$ifdef go32v2}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
{$endif}
uses
{$ifdef USE_FPWIDESTRING_UNIT}
  fpwidestring,
{$endif}
{$ifdef USE_UNICODEDUCET_UNIT}
  unicodeducet,
{$endif}
 sysutils
{$ifdef unix}
 ,{$ifdef darwin}iosxwstr{$else}cwstring{$endif}
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
