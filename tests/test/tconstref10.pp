{ %FAIL }
{ a RefAttribute declared by the program is not the system unit's: [Ref] on a
  parameter is then an attribute a parameter does not take }
program tconstref10;

{$mode objfpc}
{$modeswitch prefixedattributes}

type
  RefAttribute = class(TCustomAttribute)
  public
    constructor Create;
  end;

  [Ref]
  TMarked = class
  end;

  TSmall = record
    X: Integer;
  end;

constructor RefAttribute.Create;
begin
end;

procedure P(const [Ref] a: TSmall);
begin
end;

begin
end.
