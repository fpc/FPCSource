{$mode objfpc}
program change;

type
  aclasstype = class
    prop : integer;
  end;

procedure trychange (const aclass : aclasstype);
begin
  aclass.prop := 5;
end;

var
  aclass : aclasstype;

begin
  aclass := AClassType.Create;
  try
    trychange (aclass)
  finally
    aclass.free;
  end;
end.
