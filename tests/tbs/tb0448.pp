{$mode delphi}

procedure p1(s:string);overload;
begin
end;

procedure p1(l:longint);overload;
begin
end;

var
  pv : procedure(l:longint);
begin
  pv:=p1;
end.
