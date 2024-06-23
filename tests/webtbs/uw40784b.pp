unit uw40784b;

{$mode Delphi}

interface

uses
  uw40784a;

type

  { TCatCatalog }

  TCatCatalog<TItem: TObject> = class(TGsAbstractObjectList<TGsAbstractObjectList<TItem>>)
  protected
    function  GetTypeTagFromRow(ARow: TObject): Integer; override;
  end;

  TIntermediateList = class(TGsAbstractObjectList<TObject>)

  end;

  { TCatCatalog2 }

  TCatCatalog2<TItem: TObject> = class(TIntermediateList)
  protected
    function  GetTypeTagFromRow(ARow: TObject): Integer; override;
  end;

implementation

{ TCatCatalog }

function TCatCatalog<TItem>.GetTypeTagFromRow(ARow: TObject): Integer;
begin
  Result := 1;
end;

{ TCatCatalog2 }

function TCatCatalog2<TItem>.GetTypeTagFromRow(ARow: TObject): Integer;
begin
  Result:=1;
end;

end.