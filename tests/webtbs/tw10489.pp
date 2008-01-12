{$mode objfpc}
program test;

uses
  TypInfo;

function GetTypeInfo(const i: Integer): PTypeInfo;
begin
  case i of
    0: Result := TypeInfo(System.Integer);
    1: Result := TypeInfo(System.Int64);
    2: Result := TypeInfo(System.String); //syntax error
    3: Result := TypeInfo(System.WideString);
  else
    Result := nil;
  end;
end;

begin
end.
