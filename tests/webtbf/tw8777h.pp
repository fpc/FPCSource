{ %fail }
{ %norun }

{$ifdef fpc}
{$mode delphi}
{$endif}

type
  tc = class
    private
      f: integer;
      procedure setf(const val: integer);
      function getf: integer;
    public
      property prop: integer read getf write setf;
  end;

procedure tc.setf(const val: integer);
begin
  f:=val;
end;

function tc.getf: integer;
begin
  getf:=f;
end;

var
  c: tc;
  p: pointer;
begin
  p := @c.prop;
end.
