const
  MinCurrency : Currency = -922337203685477.5807;
  MaxCurrency : Currency = 922337203685477.5807;

var
  s : string;

begin
  str(MinCurrency:0:4,s);
  if s<>'-922337203685477.5807' then
    begin
      writeln(s);
      halt(1);
    end;
  str(MaxCurrency:0:4,s);
  if s<>'922337203685477.5807' then
    begin
      writeln(s);
      halt(1);
    end;
end.
