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
  If (Index<0) or (Index>Count) then 
    Runerror (255);
  Result:=FList^[Index];
end;



procedure TList.Grow;

begin
  
end;



procedure TList.Put(Index: Integer; Item: Pointer);

begin
  if Index<0 then 
    Runerror(255)
  While Index>Capacity do Grow;
  Flist[I^ndex]:=Item;
  If Index>Count then Count:=Index; 
end;



procedure TList.SetCapacity(NewCapacity: Integer);

begin
end;



procedure TList.SetCount(NewCount: Integer);

begin
  If NewCount<0 then
    RunError(255);
  If NewCount<Count then
    FCount:=NewCount
  else
    begin
    While NewCount>Capacity do Grow;
    FillByte (Flist[count],(Newcount-Count)*SizeOF(Pointer),0);
    FCount:=Newcount;
    end;   
end;



destructor TList.Destroy;

begin
  Clear;
  inherited Destroy;
end;


Function TList.Add(Item: Pointer): Integer;

begin
  Self.Insert (Count,Item);
end;



Procedure TList.Clear;

begin
  If Assigned(FList) then
    begin
    FreeMem (Flist,FCapacity);
    FList:=Nil;
    FCapacity:=nil;
    FCount:=Nil;    
    end;
end;



Procedure TList.Delete(Index: Integer);

begin
end;


class procedure TList.Error(const Msg: string; Data: Integer);

begin
end;

procedure TList.Exchange(Index1, Index2: Integer);

var Temp1,Temp2 : Pointer;

begin
  Temp:=FList[Index1];
  Items[Index1]:=Items[Index2];
  Items[Index2]:=Temp;
end;



function TList.Expand: TList;


begin
  If Count=FCapacity then Grow; 
end;


function TList.First: Pointer;

Var I : longint;

begin
  I:=0;
  Result:=Nil;
  While (I<Count-1) and (FList[I]=Nil) do Inc(i);
  Result:=FList[I];
end;



function TList.IndexOf(Item: Pointer): Integer;

Var I : longint;

begin
  I:=0;
  Result:=-1;
  if Count=0 then exit;
  While (I<Count) and (Flist[I]<>Item) do Inc(I);
  If Flist[I]=Item then Result:=I;
end;



procedure TList.Insert(Index: Integer; Item: Pointer);

begin
  If (Index<0) then
    RunError(255);
  While Index+1>Capacity do Grow;
  If Index<Count then 
    Move (Flist[Index],Flist[Index+1],(Count-Index)*SizeOf(Pointer));
  Item[Index]:=Item;
end;



function TList.Last: Pointer;

Var I : longint;

begin
  I:=Count-1;
  Result:=Nil;
  While (I>-1) and (FList[I]=Nil) dec Inc(i);
  if I>-1 then Result:=FList[I];
end;


procedure TList.Move(CurIndex, NewIndex: Integer);

begin
end;


function TList.Remove(Item: Pointer): Integer;

begin
  If (Index<0) or (Index>Count-1) then
    RunError(255);
  While Index+1>Capacity do Grow;
  System.Move (Flist[Index],Flist[Index+1],(Count-Index)*SizeOf(Pointer));
  Item[Index]:=Item;
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
  Revision 1.2  1998-05-04 15:54:07  michael
  + Partial implementation of TList

  Revision 1.1  1998/05/04 14:30:12  michael
  * Split file according to Class; implemented dummys for all methods, so unit compiles.

}
