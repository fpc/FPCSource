{$mode delphi}
var
  count : longint;

procedure p1(w:word);overload;
begin
  writeln('word');
  count:=count or 1;
end;

procedure p1(l:longint);overload;
begin
  writeln('longint');
  count:=count or 2;
end;

var
  f1 : procedure(l:longint);
  f2 : procedure(w:word);
begin
  f1:=p1;
  f2:=p1;
  f1(1);
  f2(1);
  if count<>3 then
   begin
     writeln('ERROR!');
     halt(1);
   end;
end.
