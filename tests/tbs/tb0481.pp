type
  trec = record
    data : longint;
  end;
  prec = ^trec;

begin
  writeln(longint(@prec(0)^.data));
end.
