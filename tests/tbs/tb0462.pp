{ %version=1.1 }

{$ifdef fpc}
{$ifdef unix}
uses
  cwstring;
{$endif}
{$endif}

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
