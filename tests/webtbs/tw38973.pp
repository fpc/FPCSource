{%OPT=-O2}
{$mode objfpc}
var
     b : LongBOOL;
     pb : ^LongBOOL;
begin
    b:=true;
    pb:=nil;
   if Assigned(pb) and pb^ then
     Exit;
    b:=false;
end.
