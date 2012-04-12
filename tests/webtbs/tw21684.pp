{$mode delphi}{$H+}{$M+}

uses
  Classes, SysUtils,
  typinfo;

type
  { TTest }
  TTest = class
  private
    function GetX: IUnknown ;
  published
    property X: IUnknown read GetX;
  end;

function TTest.GetX: IUnknown;
begin
  Result := TInterfacedPersistent.Create;
end;

var
  V: IUnknown;
  FT: TTest;
begin
  FT := TTest.Create;
  V := GetInterfaceProp(FT, 'X');
end.
