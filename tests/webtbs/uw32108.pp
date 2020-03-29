unit uw32108;

{$mode delphi}

interface

function getTFooF: Integer;

implementation

type

  { TFoo }

  TFoo = class
    class var f: integer;
    class constructor Create;
  end;

{ TFoo }

class constructor TFoo.Create;
begin
  f := 1;
end;

function getTFooF: Integer;
begin
  Result := TFoo.f;
end;

begin
end.

