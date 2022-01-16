var
  i64: Int64;
  qa, qb: DWord;
begin
  qa := 2147483648;
  qb := 536870912;
{$R+}{$Q+}
  i64 := int64(qa) * int64(qb);
  if i64 <> 1152921504606846976 then
  begin
    Writeln('Error!');
    Halt(1);
  end;
end.
