{$ifdef fpc}{$mode objfpc}{$H+}{$endif}

type
  TMyClass = class
  public
    constructor Create;
    procedure Assign(v:pointer);virtual;
  end;

var
  i : longint;

constructor TMyClass.Create;
begin
  writeln('TMyClass.Create');
  inc(i);
end;

procedure TMyClass.Assign(v:pointer);
begin
end;

function CreateMyClass: TMyClass;
begin
  Result:=TMyClass.Create;
end;

var
  Item: TMyClass;
begin
  CreateMyClass.Assign(nil);
  if i<>1 then
    begin
      writeln('Error!');
      halt(1);
    end;
end.
