type
  psearchrec=^longint;

Var Sr : PSearchrec;

begin
  Sr := New(PSearchRec);
  Sr^ := 45;
  if Sr^<>45 then
    Halt(1);
  Dispose(Sr);
end.
