{$ifdef FPC}
{$mode Delphi}
{$endif}
{$APPTYPE CONSOLE}
var
 x:function(x:longint):longint;
 y:pointer absolute x;
 z,w,v:pointer;
begin
 z:=@@x;
 w:=addr(@x);
 v:=@(addr(x));
 writeln(longint(y),' ',longint(z),' ',longint(w),' ',longint(v));
 if (z<>w) or (z<>v) then
  begin
    writeln('Addr Error');
    halt(1);
  end;
 if (y<>nil) then
  begin
    writeln('Absolute Error');
    halt(1);
  end;
end.
