{ %norun }

{$ifdef fpc}
{$mode delphi}
{$endif}

type
  tc = class
    private
      f: integer;
    public
      property prop: integer read f write f;
  end;

var
  c: tc;
  p: pointer;
begin
  p := @c.f;
end.
