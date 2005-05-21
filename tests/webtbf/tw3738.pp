{ %fail }

program fjf775a;

type
  a = object
    f: Integer
  end;

  b = object (a)
    procedure p;
  end;

procedure b.p;
begin
  WriteLn (inherited f)  { WRONG }
end;

begin
end.
