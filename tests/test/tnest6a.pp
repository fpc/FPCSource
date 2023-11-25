{ %fail }

{$mode objfpc}

uses
  unest6a;

const
  ok: boolean = false;

procedure test(const i: IInf);

  procedure nest;
    begin
      ok:=true;
    end;

begin
  i.InfMethod(@nest);
end;

var
  i: IInf;
begin
  i:= tobj.create;
  test(i);
  i:=nil;
  halt(ord(not ok));
end.
