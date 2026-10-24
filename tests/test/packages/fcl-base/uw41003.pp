{ Resourcestring checked in the finalization section, for tw41003 }
unit uw41003;

{$mode objfpc}{$h+}

interface

resourcestring
  rsHello = 'Hello';

implementation

finalization
  if rsHello<>'Hello' then
    ExitCode:=1;
end.
