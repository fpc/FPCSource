unit uw40784a;

{$mode Delphi}

interface

type

  { TGsAbstractObjectList }

  TGsAbstractObjectList<T: TObject> = class
  protected
    function  GetTypeTagFromRow(ARow: TObject): Integer; virtual;
  end;


implementation

{ TGsAbstractObjectList<T> }

function TGsAbstractObjectList<T>.GetTypeTagFromRow(ARow: TObject): Integer;
begin
  Result := 0;
end;

end.