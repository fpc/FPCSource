{$mode delphi}
type
  tprocedure = procedure;
  pprocedure = ^tprocedure;

var
  l : longint;

function _f1 : plongint;
  begin
    result:=@l;
  end;

var
  f1 : function : plongint;
  f2 : function : pprocedure;

procedure p;
  begin
    l:=2;
  end;

begin
  f1^:=1;
  if l<>1 then
    halt(1);
  f2^:=p;
  f2^;
  if l<>2 then
    halt(1);
  writeln('ok');
end.
