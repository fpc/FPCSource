{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

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
Const
  // Ratio of Pointer and Word Size.
  WordRatio = SizeOf(Pointer) Div SizeOf(Word);

function TList.Get(Index: Integer): Pointer;

begin
  If (Index<0) or (Index>FCount) then
    Error(SListIndexError,Index);
  Result:=FList^[Index];
end;



procedure TList.Grow;

begin
  // Only for compatibility with Delphi. Not needed.
end;



procedure TList.Put(Index: Integer; Item: Pointer);

begin
  if (Index<0) or (Index>=FCount) then
    Error(SListIndexError,Index);
  Flist^[Index]:=Item;
end;



procedure TList.SetCapacity(NewCapacity: Integer);

Var NewList,ToFree : PPointerList;

begin
  If (NewCapacity<0) or (NewCapacity>MaxListSize) then
     Error (SListCapacityError,NewCapacity);
  If NewCapacity>FCapacity then
    begin
    GetMem (NewList,NewCapacity*SizeOf(Pointer));
    If NewList=Nil then
      //!! Find another one here !!
      Error (SListCapacityError,NewCapacity);
    If Assigned(FList) then
      begin
      System.Move (FList^,NewList^,FCapacity*Sizeof(Pointer));
      FillWord (NewList^[FCapacity],(NewCapacity-FCapacity)*WordRatio, 0);
      FreeMem (Flist,FCapacity*SizeOf(Pointer));
      end;
    Flist:=NewList;
    FCapacity:=NewCapacity;
    end
  else if NewCapacity<FCapacity then
    begin
    If NewCapacity<0 then
      Error (SListCapacityError,NEwCapacity);
    ToFree:=Flist+NewCapacity*SizeOf(Pointer);
    FreeMem (ToFree, (FCapacity-NewCapacity)*SizeOf(Pointer));
    FCapacity:=NewCapacity;
    end;
end;



procedure TList.SetCount(NewCount: Integer);

begin
  If (NewCount<0) or (NewCount>MaxListSize)then
    Error(SListCountError,NewCount);
  If NewCount<FCount then
    FCount:=NewCount
  else If NewCount>FCount then
    begin
    If NewCount>FCapacity then
      SetCapacity (NewCount);
    If FCount<NewCount then
      FillWord (Flist^[FCount],(NewCount-FCount)* WordRatio ,0);
    FCount:=Newcount;
    end;
end;



destructor TList.Destroy;

begin
  Self.Clear;
  inherited Destroy;
end;


Function TList.Add(Item: Pointer): Integer;

begin
  Self.Insert (Count,Item);
  Result:=Count-1;
end;



Procedure TList.Clear;

begin
  If Assigned(FList) then
    begin
    FreeMem (Flist,FCapacity*SizeOf(Pointer));
    FList:=Nil;
    FCapacity:=0;
    FCount:=0;
    end;
end;



Procedure TList.Delete(Index: Integer);

begin
  If (Index<0) or (Index>=FCount) then
    Error (SListIndexError,Index);
  FCount:=FCount-1;
  System.Move (FList^[Index+1],FList^[Index],(FCount-Index)*SizeOf(Pointer));
end;


class procedure TList.Error(const Msg: string; Data: Integer);

begin
  //!! Find a way to get call  address
  Raise EListError.CreateFmt(Msg,[Data]);
end;

procedure TList.Exchange(Index1, Index2: Integer);

var Temp : Pointer;

begin
  If ((Index1>=FCount) or (Index1<0)) then
    Error(SListIndexError,Index1);
  If ((Index2>=FCount) or (Index2<0)) then
    Error(SListIndexError,Index2);
  Temp:=FList^[Index1];
  FList^[Index1]:=FList^[Index2];
  FList^[Index2]:=Temp;
end;



function TList.Expand: TList;

Var IncSize : Longint;

begin
  if FCount<FCapacity then exit;
  IncSize:=4;
  if FCapacity>3 then IncSize:=IncSize+4;
  if FCapacity>8 then IncSize:=IncSize+8;
  SetCapacity(FCapacity+IncSize);
  Result:=Self;
end;


function TList.First: Pointer;

begin
  If FCount=0 then
    Result:=Nil
  else
    Result:=Items[0];
end;



function TList.IndexOf(Item: Pointer): Integer;

begin
  Result:=0;
  While (Result<FCount) and (Flist^[Result]<>Item) do Result:=Result+1;
  If Result=FCount  then Result:=-1;
end;



procedure TList.Insert(Index: Integer; Item: Pointer);

begin
  If (Index<0) or (Index>FCount )then
    Error(SlistIndexError,Index);
  IF FCount=FCapacity Then Self.Expand;
  If Index<FCount then
    System.Move (Flist^[Index],Flist^[Index+1],(FCount-Index)*SizeOf(Pointer));
  FList^[Index]:=Item;
  FCount:=FCount+1;
end;



function TList.Last: Pointer;

begin
  // Wouldn't it be better to return nil if the count is zero ?
  If FCount=0 then
    Result:=Nil
  else
    Result:=Items[FCount-1];
end;


procedure TList.Move(CurIndex, NewIndex: Integer);

Var Temp : Pointer;

begin
  If ((CurIndex<0) or (CurIndex>Count-1)) then
    Error(SListIndexError,CurIndex);
  If (NewINdex<0) then
    Error(SlistIndexError,NewIndex);
  Temp:=FList^[CurIndex];
  Self.Delete(CurIndex);
  // ?? If NewIndex>CurIndex then NewIndex:=NewIndex-1;
  // Newindex changes when deleting ??
  Self.Insert (NewIndex,Temp);
end;


function TList.Remove(Item: Pointer): Integer;

begin
  Result:=IndexOf(Item);
  If Result<>-1 then
    Self.Delete (Result);
end;



Procedure TList.Pack;

Var  {Last,I,J,}Runner : Longint;

begin
  // Not the fastest; but surely correct
  For Runner:=Fcount-1 downto 0 do
    if Items[Runner]=Nil then Self.Delete(Runner);
{ The following may be faster in case of large and defragmented lists
  If count=0 then exit;
  Runner:=0;I:=0;
  TheLast:=Count;
  while runner<count do
    begin
    // Find first Nil
    While (FList^[Runner]<>Nil) and (Runner<Count) do Runner:=Runner+1;
    if Runner<Count do
      begin
      // Start searching for non-nil from last known nil+1
      if i<Runner then I:=Runner+1;
      While (Flist[I]^=Nil) and (I<Count) do I:=I+1;
      // Start looking for last non-nil of block.
      J:=I+1;
      While (Flist^[J]<>Nil) and (J<Count) do J:=J+1;
      // Move block and zero out
      Move (Flist^[I],Flist^[Runner],J*SizeOf(Pointer));
      FillWord (Flist^[I],(J-I)*WordRatio,0);
      // Update Runner and Last to point behind last block
      TheLast:=Runner+(J-I);
      If J=Count then
         begin
         // Shortcut, when J=Count we checked all pointers
         Runner:=Count
      else
         begin
         Runner:=TheLast;
         I:=j;
      end;
    end;
  Count:=TheLast;
}
end;

// Needed by Sort method.

Procedure QuickSort (Flist : PPointerList; L,R : Longint;
                     Compare : TListSortCompare);

Var I,J : Longint;
    P,Q : Pointer;

begin
 Repeat
   I:=L;
   J:=R;
   P:=FList^[ (L+R) div 2 ];
   repeat
     While Compare(P,FList^[i])>0 Do I:=I+1;
     While Compare(P,FList^[J])<0 Do J:=J-1;
     If I<=J then
       begin
       Q:=Flist^[I];
       Flist^[I]:=FList^[J];
       FList^[J]:=Q;
       I:=I+1;
       J:=j-1;
       end;
   Until I>J;
   If L<J then QuickSort (FList,L,J,Compare);
   L:=I;
 Until I>=R;
end;

procedure TList.Sort(Compare: TListSortCompare);

begin
  If Not Assigned(FList) or (FCount<2) then exit;
  QuickSort (Flist, 0, FCount-1,Compare);
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
  LockList:=nil;
end;



procedure TThreadList.Remove(Item: Pointer);


begin
end;



procedure TThreadList.UnlockList;

begin
end;


{
  $Log$
  Revision 1.10  2000-01-07 01:24:33  peter
    * updated copyright to 2000

  Revision 1.9  2000/01/06 01:20:33  peter
    * moved out of packages/ back to topdir

  Revision 1.1  2000/01/03 19:33:07  peter
    * moved to packages dir

  Revision 1.7  1999/04/13 12:46:16  michael
  + Some bug fixes by Romio

  Revision 1.6  1999/04/08 10:18:52  peter
    * makefile updates

}
