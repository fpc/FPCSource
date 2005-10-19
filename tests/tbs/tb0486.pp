{$ifdef fpc}{$mode delphi}{$endif}
type
  tprocedure = procedure;
  pprocedure = ^tprocedure;

var
  l : longint;
  l2 : tprocedure;

function _f1 : plongint;
  begin
    result:=@l;
  end;

function _f2 : pprocedure;
  begin
    result:=@@l2;
  end;

var
  f1 : function : plongint;
  f2 : function : pprocedure;

procedure p;
  begin
    l:=2;
  end;

begin
  f1:=_f1;
  f2:=_f2;
  f1^:=1;
  if l<>1 then
    halt(1);
  f2^:=p;
  f2^;
  if l<>2 then
    halt(1);
  writeln('ok');
end.
