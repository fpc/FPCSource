{ %opt=-gh }

program tw19701;
{$ifdef FPC}{$mode objfpc}{$h+}{$endif}
{$ifdef mswindows}{$apptype console}{$endif}
uses
 {$ifdef FPC}{$ifdef linux}cthreads,cwstring,{$endif}{$endif}
 sysutils,uw19701;
begin
  HaltOnNotReleased:=True;
end.
