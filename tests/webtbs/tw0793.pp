{$MODE Delphi}

program bug;
type

TMyObject = class
  public
    constructor Create; virtual;
    constructor Init;
end;

var
  M: TMyObject;


constructor TMyObject.Create;
begin
  Writeln('Now executing  TmyObject.Create');
end;

constructor TMyObject.Init;
begin
  Create;
  Writeln('Now finishing the INIT constructor.');
end;

begin
  M := TMyObject.Init;
end.
