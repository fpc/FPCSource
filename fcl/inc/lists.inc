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
{*                             TList                                        *}
{****************************************************************************}

{  TList = class(TObject)
  private
    FList: PPointerList;
    FCount: Integer;
    FCapacity: Integer;
}

function TList.Get(Index: Integer): Pointer;

begin
end;



procedure TList.Grow;

begin
end;



procedure TList.Put(Index: Integer; Item: Pointer);

begin
end;



procedure TList.SetCapacity(NewCapacity: Integer);

begin
end;



procedure TList.SetCount(NewCount: Integer);

begin
end;



destructor TList.Destroy;

begin
  Clear;
  inherited Destroy;
end;


Function TList.Add(Item: Pointer): Integer;

begin
//  Self.Insert (Count,Item);
end;



Procedure TList.Clear;

begin
end;



Procedure TList.Delete(Index: Integer);

begin
end;


class procedure TList.Error(const Msg: string; Data: Integer);

begin
end;

procedure TList.Exchange(Index1, Index2: Integer);


begin
end;



function TList.Expand: TList;


begin
end;


function TList.First: Pointer;

begin
end;



function TList.IndexOf(Item: Pointer): Integer;

begin
end;



procedure TList.Insert(Index: Integer; Item: Pointer);

begin
end;



function TList.Last: Pointer;

begin
end;


procedure TList.Move(CurIndex, NewIndex: Integer);

begin
end;


function TList.Remove(Item: Pointer): Integer;

begin
end;



procedure TList.Pack;
begin
end;



procedure TList.Sort(Compare: TListSortCompare);

begin
end;


{****************************************************************************}
{*                             TThreadList                                  *}
{****************************************************************************}


constructor TThreadList.Create;

begin
end;


    
destructor TThreadList.Destroy; 

begin
end;



procedure TThreadList.Add(Item: Pointer);

begin
end;


procedure TThreadList.Clear;

begin
end;



function TThreadList.LockList: TList;

begin
end;



procedure TThreadList.Remove(Item: Pointer);


begin
end;



procedure TThreadList.UnlockList;

begin
end;
{
  $Log$
  Revision 1.1  1998-05-04 14:30:12  michael
  * Split file according to Class; implemented dummys for all methods, so unit compiles.

}
