{$mode objfpc}

uses sysutils;
type
  zz=class(tobject)
    procedure test;virtual;
    procedure test1;virtual;
  end;
procedure zz.test;
begin
  writeln('ok');
end;
procedure zz.test1;
begin
  try
    raise exception.create('zz');
  except
    on e:exception do test;
  end;
end;
var
  z:zz;
begin
  z:=zz.create;
  z.test1;
  z.destroy;
end.
