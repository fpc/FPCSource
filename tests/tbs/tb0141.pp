{ Old file: tbs0169.pp }
{ missing new(type) support for not object/class             OK 0.99.9 (PM) }

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
