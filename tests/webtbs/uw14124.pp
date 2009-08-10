unit uw14124;

{$mode objfpc}{$H+}
{$static on}

interface

type
  generic TGenericType<TParamType> = class
  var private
    FDefault: TParamType; static;
    F: TParamType;
  public
    procedure P;
  end;

implementation

procedure TGenericType.P;
begin
  F := FDefault; // <====== unit1.pas(21,16) Fatal: Internal error 200108121
end;

end.