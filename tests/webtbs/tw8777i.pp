{ %norun }
{$ifdef fpc}
{$mode delphi}
{$endif}

type
  tc = class
    private
      f: array[0..5] of integer;
      procedure setf(const val: integer);
    public
      property prop: integer read f[1] write setf;
  end;

procedure tc.setf(const val: integer);
begin
  f[1]:=val;
end;

var
  c: tc;
  p: pointer;
begin
  p := @c.prop;
end.
