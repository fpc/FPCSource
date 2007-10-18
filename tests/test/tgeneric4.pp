{ %fail }

uses ugeneric4;

procedure LocalFill;
begin
  globaldata:='Program';
end;

{ The next specialization should not find the LocalFill
  defined in the program. It should found the LocalFill
  in ugeneric4, but for the moment that is not allowed since
  the assembler symbol is not global and will therefor
  generate a failure a linking time (PFV) }
type
  TMyStringList = specialize TList<string>;

var
  slist : TMyStringList;
begin
  slist := TMyStringList.Create;
  slist.Fill;
  writeln(slist.data);
  if slist.data<>'Unit' then
    halt(1);
end.
