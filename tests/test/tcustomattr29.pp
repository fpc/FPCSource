program tcustomattr29;

{ attributes on the parameters of a routine nested in a method, declared
  forward and implemented, are checked and not kept }

{$mode delphi}
{$modeswitch prefixedattributes}

type
  FlagAttribute = class(TCustomAttribute)
  public
    constructor Create;
  end;

  TThing = class
  public
    function Value: Integer;
  end;

constructor FlagAttribute.Create;
begin
  inherited Create;
end;

function TThing.Value: Integer;

  function Inner([Flag] a: Integer): Integer; forward;

  function Inner([Flag] a: Integer): Integer;
  begin
    Result := a + 1;
  end;

begin
  Result := Inner(1);
end;

var
  gThing: TThing;
begin
  gThing := TThing.Create;
  if gThing.Value <> 2 then
    Halt(1);
  gThing.Free;
  Writeln('ok');
end.
