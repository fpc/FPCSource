{$mode delphi}

var
  err : boolean;

procedure p1(s:string);overload;
begin
end;

procedure p1(l:longint);overload;
begin
  err:=false;
end;

var
  pv : procedure(l:longint);
begin
  err:=true;
  pv:=p1;
  pv(1);
  if err then
   begin
     writeln('Error!');
     halt(1);
   end;
end.
