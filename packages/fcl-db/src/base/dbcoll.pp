{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Michael van Canneyt and other members of the
    Free Pascal development team

    Dbcoll fieldmap implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit DBColl;

interface

uses db, classes, sysutils, fieldmap;

{ ---------------------------------------------------------------------
  TFieldMap
  ---------------------------------------------------------------------}

type

  EDBCollection = Class(Exception);

  { TDBCollectionItem }

  TDBCollectionItem = Class(TCollectionItem)
  Protected
    Procedure LoadFromMap(F : TFieldMap); virtual;
    Class Function FieldMapClass: TFieldMapClass; virtual; abstract;
  Public
    Procedure LoadFromDataset(ADataset : TDataset);
  end;
  
  { TDBCollection }

  TDBCollection = Class(TCollection)
  Protected
    Function AddDBItem : TDBCollectionItem;
    Procedure DoLoadFromFieldMap(Map : TFieldMap); virtual;
  Public
    Procedure LoadFromDataset(Dataset : TDataset);
  end;

implementation

resourcestring
  SErrNoDatasetForField = '%s: no dataset to search field %s in.';

{ TDBCollection }

function TDBCollection.AddDBItem: TDBCollectionItem;
begin
  Result:=Add as TDBCollectionItem;
end;

procedure TDBCollection.DoLoadFromFieldMap(Map: TFieldMap);

Var
  I : TDBCollectionItem;

begin
  While Not Map.Dataset.EOF do
    begin
    I:=AddDBItem;
    try
      I.LoadFromMap(Map);
    Except
      I.Free;
      Raise;
    end;
    Map.Dataset.Next;
    end;
end;

procedure TDBCollection.LoadFromDataset(Dataset: TDataset);

Var
  M : TFieldMap;

begin
  M:=TDBCollectionItem(ItemClass).FieldMapClass.Create(Dataset);
  Try
    DoLoadFromFieldMap(M);
  finally
    M.Free;
  end;
end;

{ TDBCollectionItem }

procedure TDBCollectionItem.LoadFromMap(F: TFieldMap);
begin
  F.LoadObject(Self);
end;

procedure TDBCollectionItem.LoadFromDataset(ADataset: TDataset);

Var
  M : TFieldMap;

begin
  M:=FieldMapClass.Create(ADataset);
  Try
    LoadFromMap(M);
  Finally
    M.Free;
  end;
end;

end.
 
