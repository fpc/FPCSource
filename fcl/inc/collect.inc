{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{****************************************************************************}
{*                             TCollectionItem                              *}
{****************************************************************************}


function TCollectionItem.GetIndex: Integer;

begin
end;



procedure TCollectionItem.SetCollection(Value: TCollection);

begin
end;



procedure TCollectionItem.Changed(AllItems: Boolean);

begin
end;



function TCollectionItem.GetNamePath: string;

begin
end;


function TCollectionItem.GetOwner: TPersistent;

begin
end;



function TCollectionItem.GetDisplayName: string; 

begin
end;



procedure TCollectionItem.SetIndex(Value: Integer);

begin
end;


procedure TCollectionItem.SetDisplayName(const Value: string);

begin
end;



constructor TCollectionItem.Create(ACollection: TCollection); 

begin
end;



destructor TCollectionItem.Destroy; 

begin
end;

{****************************************************************************}
{*                             TCollection                                  *}
{****************************************************************************}



function TCollection.GetCount: Integer;

begin
end;



function TCollection.GetPropName: string;

begin
end;



procedure TCollection.InsertItem(Item: TCollectionItem);

begin
end;



procedure TCollection.RemoveItem(Item: TCollectionItem);

begin
end;


function TCollection.GetAttrCount: Integer; 

begin
end;


function TCollection.GetAttr(Index: Integer): string; 

begin
end;



function TCollection.GetItemAttr(Index, ItemIndex: Integer): string; 

begin
end;



function TCollection.GetNamePath: string; 

begin
end;



procedure TCollection.Changed;

begin
end;



function TCollection.GetItem(Index: Integer): TCollectionItem;

begin
end;



procedure TCollection.SetItem(Index: Integer; Value: TCollectionItem);

begin
end;



procedure TCollection.SetItemName(Item: TCollectionItem); 

begin
end;



procedure TCollection.Update(Item: TCollectionItem);

begin
end;



constructor TCollection.Create(AItemClass: TCollectionItemClass);

begin
end;



destructor TCollection.Destroy; 

begin
end;



function TCollection.Add: TCollectionItem;

begin
end;



procedure TCollection.Assign(Source: TPersistent);

begin
end;



procedure TCollection.BeginUpdate;

begin
end;



procedure TCollection.Clear;

begin
end;



procedure TCollection.EndUpdate;

begin
end;



function TCollection.FindItemID(ID: Integer): TCollectionItem;

begin
end;
{
  $Log$
  Revision 1.1  1998-05-04 14:30:11  michael
  * Split file according to Class; implemented dummys for all methods, so unit compiles.

}
