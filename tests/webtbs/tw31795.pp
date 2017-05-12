unit tw31795;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils;

type
  TCriticalSection = class(TObject)

  end;

implementation

var
  gbbbbb : TCriticalSection;

procedure init;
var
  s: String;
begin
  s := 'Hello World';
end;

procedure Finalize;
var
  s: String;
begin
  s := 'Hello World';
end;

initialization

  gbbbbb := TCriticalSection.Create;

finalization

  gbbbbb.free;

end.


