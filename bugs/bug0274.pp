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