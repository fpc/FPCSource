{$ifdef fpc}
{$mode delphi}
{$endif fpc}

function times2(x : longint) : longint;

begin
  times2:=2*x;
end;

var
 x:function(x:longint):longint;
 y:pointer absolute x;
 z,w,v:pointer;
begin
 x:=times2;
 z:=@x;
 w:=addr(x);
 v:=@times2;
 writeln(longint(y),' ',longint(z),' ',longint(w),' ',longint(v));
 if (z<>w) or (z<>v) or (y<>z) then
  begin
    writeln('Addr Error');
    halt(1);
  end;
 if (y<>@times2) then
  begin
    writeln('Absolute Error');
    halt(1);
  end;

end.
