program av;
{$ifdef FPC}{$mode objfpc}{$h+}{$INTERFACES CORBA}{$endif}
{$ifdef mswindows}{$apptype console}{$endif}
{$ifdef go32v2}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
{$endif}
uses
{$ifdef FPC}
  {$ifdef unix}
  cthreads,
    {$ifdef darwin}
    iosxwstr
    {$else}
    cwstring
    {$endif}
  ,{$endif}
  {$ifdef USE_FPWIDESTRING_UNIT}
    fpwidestring,
  {$endif}
  {$ifdef USE_UNICODEDUCET_UNIT}
    unicodeducet,
  {$endif}
{$endif}
  sysutils;
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
