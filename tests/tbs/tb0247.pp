{ Old file: tbs0288.pp }
{ crash with virtual method in except part             OK 0.99.13 (PFV) }

{$mode objfpc}

uses sysutils;
const
  test_run : boolean = false;

type
  zz=class(tobject)
    procedure test;virtual;
    procedure test1;virtual;
  end;
procedure zz.test;
begin
  writeln('ok');
  test_run:=true;
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
  if not test_run then
    begin
      Writeln('Problem with virtual method in except block');
      Halt(1);
    end;
end.
