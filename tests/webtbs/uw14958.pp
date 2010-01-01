unit uw14958;

interface

function Fun: Boolean; stdcall;

implementation

function Fun: Boolean; stdcall;
begin
  Fun := False;
end;

initialization
  Writeln('  ExLib Init');

finalization
  Writeln('  ExLib Final');

end.
