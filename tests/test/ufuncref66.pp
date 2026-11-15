{ classes for tfuncref66: methods converted to function references in another unit }
unit ufuncref66;

{$mode delphi}

interface

type
  TBase = class
    N: LongInt;
    procedure Handler(out aIntf: IInterface; out aText: String);
    function Add(aValue: LongInt): LongInt;
    class function Name: String;
  end;

implementation

procedure TBase.Handler(out aIntf: IInterface; out aText: String);
begin
  aIntf := TInterfacedObject.Create;
  Inc(N);
  aText := 'called';
end;

function TBase.Add(aValue: LongInt): LongInt;
begin
  Inc(N, aValue);
  Result := N;
end;

class function TBase.Name: String;
begin
  Result := ClassName;
end;

end.
