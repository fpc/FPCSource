{ %fail }
{$ifdef fpc}
{$mode delphi}
{$endif}

type
  tr = record
    a: integer;
  end;

  tc = class
    private
      f: tr;
    public
      property prop: tr read f write f;
  end;

var
  c: tc;
begin
  c.prop.a := 5;
end.
