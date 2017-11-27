{ %cpu=i8086 }

program tfaradr1;

var
  global_variable: Integer;

procedure Fail(const S: string);
begin
  Writeln('Error in FarAddr(', S, ')');
  Halt(1);
end;

procedure test_local_variable;
var
  local_variable: Integer;
begin
  if FarAddr(local_variable) <> Ptr(Seg(local_variable), Ofs(local_variable)) then
    Fail('local_variable');
end;

procedure proc;
begin
  Writeln('Hi, i''m a proc.');
end;

var
  proc_addr: FarPointer;
begin
  if FarAddr(global_variable) <> Ptr(Seg(global_variable), Ofs(global_variable)) then
    Fail('global_variable');

  test_local_variable;

  proc_addr := FarAddr(proc);
  if proc_addr <> Ptr(Seg(proc), Ofs(proc)) then
    Fail('proc');

  Writeln('Ok!');
end.
