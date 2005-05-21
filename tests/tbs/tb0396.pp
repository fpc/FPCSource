{ %VERSION=1.1 }
{$ifdef fpc}{$mode objfpc}{$endif}
type
   to2 = interface
     function bufwrite(eat : boolean = true) : integer;stdcall;
   end;

begin
end.
