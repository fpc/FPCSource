{ Old file: tbs0271.pp }
{ abstract methods can't be assigned to methodpointers OK 0.99.13 (??) }

{$mode fpc}
  type
    tproc = procedure;

procedure proc1;
begin
end;

var
  _copyscan : tproc;

procedure setproc;
begin
    _copyscan := @proc1;
end;

procedure testproc;
begin
  if not (_copyscan=@proc1) then
    begin
      Writeln(' Problem procvar equality');
      Halt(1);
    end
  else
    Writeln(' No problem with procedure equality');
end;

begin
  setproc;
  testproc;
end.
