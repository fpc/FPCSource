{ %norun }
unit tw25769a;

{$mode objfpc}{$H+}

interface

type
  TDbgSymbolValue = class(TObject)
  protected
    function GetMemberCountEx(AIndex: array of Integer): Integer;
  public
    property MemberCountEx[AIndex: array of Integer]: Integer read GetMemberCountEx;
  end;


implementation

function TDbgSymbolValue.GetMemberCountEx(AIndex: array of Integer): Integer;
begin
  result:=0;
end;

end.
