{ %fail }
procedure p1(const v);

  begin
  end;

procedure p2;

  var
    lb : longbool;

  begin
    p1(pchar(@lb));
  end;

begin
end.
