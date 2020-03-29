unit uw29245;

{$mode delphi}

interface

 type
  TFoo = class
  class var
    F: array of TObject;
  private
    class constructor Create;
    class destructor Destroy;
  end;

implementation

class constructor TFoo.Create;
begin
  writeln('tfoo class constructor');
  SetLength(F, 10);
end;

class destructor TFoo.Destroy;
begin
  writeln('tfoo class destructor');
  if length(TFOO.F)<>10 then
    halt(3);
end;

initialization
  writeln('unit initialization');
  if length(TFOO.F)<>10 then
    halt(1);

finalization
  writeln('unit finalization');
  if length(TFOO.F)<>10 then
    halt(2);

end.
