{$ifdef fpc}
{$mode fpc}
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
 z:=@x;
 w:=addr(x);
 v:=@y;
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
 x:=@times2;
 if (y<>pointer(@times2)) then
  begin
    writeln('Absolute Error');
    halt(1);
  end;

end.
