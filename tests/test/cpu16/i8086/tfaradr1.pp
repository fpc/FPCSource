{ %cpu=i8086 }

program tfaradr1;

type
  PFarInteger = ^Integer; far;

var
  global_variable: Integer;

procedure Fail(const S: string);
begin
  Writeln('Error in FarAddr(', S, ')');
  Halt(1);
end;

procedure test_local_variable;
var
  pfv: FarPointer;
  pfi: PFarInteger;
  local_variable: Integer;
begin
  pfv := FarAddr(local_variable);
  if pfv <> Ptr(Seg(local_variable), Ofs(local_variable)) then
    Fail('local_variable');
  pfi := FarAddr(local_variable);
  if pfi <> Ptr(Seg(local_variable), Ofs(local_variable)) then
    Fail('local_variable');
  if FarAddr(local_variable) <> Ptr(Seg(local_variable), Ofs(local_variable)) then
    Fail('local_variable');
end;

procedure proc;
begin
  Writeln('Hi, i''m a proc.');
end;

var
  pfv: FarPointer;
  pfi: PFarInteger;
  proc_addr: FarPointer;
begin
  { test FarAddr(global_variable) }
  pfv := FarAddr(global_variable);
  if pfv <> Ptr(Seg(global_variable), Ofs(global_variable)) then
    Fail('global_variable');
  pfi := FarAddr(global_variable);
  if pfi <> Ptr(Seg(global_variable), Ofs(global_variable)) then
    Fail('global_variable');
  if FarAddr(global_variable) <> Ptr(Seg(global_variable), Ofs(global_variable)) then
    Fail('global_variable');

  { test FarAddr(local_variable) }
  test_local_variable;

  { test FarAddr(proc) }
  proc_addr := FarAddr(proc);
  if proc_addr <> Ptr(Seg(proc), Ofs(proc)) then
    Fail('proc');

  Writeln('Ok!');
end.
