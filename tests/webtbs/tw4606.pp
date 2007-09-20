{$packset 1}
type
  tlettersset=set of 'a'..'z';

begin
  if sizeof(tlettersset)<>4 then
    begin
      writeln(sizeof(tlettersset));
      halt(1);
    end;
end.
