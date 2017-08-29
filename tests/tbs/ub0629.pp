unit ub0629;

{$mode objfpc}
{$modeswitch advancedrecords}

interface

uses
  typinfo;

type
  TTest = record
    generic class function GetTypeInfo<T>: PTypeInfo; static;
  end;

implementation

generic class function TTest.GetTypeInfo<T>: PTypeInfo;
begin
  Result := System.TypeInfo(T);
end;

end.
