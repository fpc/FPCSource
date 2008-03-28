{ %norun }

program rangeerror;
{$ifdef FPC}{$mode objfpc}{$h+}{$endif}
{$ifdef mswindows}{$apptype console}{$endif}
uses
{$ifdef FPC}{$ifdef linux}cthreads,{$endif}{$endif}
sysutils;
type
colorty = type longword;
const
cl_mapped = colorty($90000000);
type
ttestclass = class
 private
  fcolor: colorty;
 published
  property color: colorty read fcolor write fcolor default cl_mapped; //<<--
end; 
begin
end.
