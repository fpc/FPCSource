type
  tr = bitpacked record
    l: longint;
  end;

begin
  if bitsizeof(tr) <> 32 then
    halt(1);
end.
