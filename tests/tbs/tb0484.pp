type
  r1 = record
    p : procedure stdcall;
    i : longint;
  end;

  r2 = record
    p : procedure;
    i : longint;
  end;

  r3 = record
    p : procedure
  end;

  { ugly, but should work (FK) }
  r4 = record
    p : procedure stdcall
  end;

begin
end.
