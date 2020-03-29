{ %opt=-gh }

{$mode delphi}

uses
  uw29245;

 type
  TBar = class
  class var
    F: array of TObject;
  strict private
    class constructor Create;
    class destructor Destroy;
  end;

class constructor TBar.Create;
begin
  writeln('tbar class constructor');
  SetLength(F, 10);
end;

class destructor TBar.Destroy;
begin
  writeln('tbar class destructor');
  if length(Tbar.F)<>10 then
    halt(5);
end;

begin
  HaltOnNotReleased := true;
  writeln('main program');
  if length(TBar.F)<>10 then
    halt(4);
end.

