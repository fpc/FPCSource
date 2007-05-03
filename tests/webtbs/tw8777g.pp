{ %norun }

{$ifdef fpc}
{$mode delphi}
{$endif}

type
  tc = class
    private
      f: integer;
      procedure setf(const val: integer);
    public
      property prop: integer read f write setf;
  end;

procedure tc.setf(const val: integer);
begin
  f:=val;
end;

var
  c: tc;
  p: pointer;
begin
  p := @c.prop;
end.
