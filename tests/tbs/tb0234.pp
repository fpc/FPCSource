{ Old file: tbs0274.pp }
{ @(proc) is not allowed                               OK 0.99.13 (PFV) }

type
  proc=procedure(a:longint);

procedure prc(a:longint);
begin
end;

var
  p : proc;
begin
  p:=@prc;
  p:=@(prc);  { should this be allowed ? }
end.
