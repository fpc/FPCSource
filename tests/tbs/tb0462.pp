{ %version=1.1 }
type
  RR = record
    RA : WideString;
  end;

const
  Z : RR = (RA: 'B');

begin
  if z.ra<>'B' then
    begin
      writeln('error');
      halt(1);
    end;
end.
