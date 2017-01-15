{ %fail }

program main;
{$ifdef FPC}{$mode objfpc}{$h+}{$endif}
uses bugunit;

begin
  proc( 1, 2 ); // no error with -B compiler flag
end.


