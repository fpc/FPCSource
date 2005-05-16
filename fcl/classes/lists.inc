{
    $Id: lists.inc,v 1.4 2005/05/12 21:47:34 hajny Exp $
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
  If (Index<0) or (Index>=FCount) then
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


function TList.Extract(item: Pointer): Pointer;
var
  i : Integer;
begin
  result:=nil;
  i:=IndexOf(item);
  if i>=0 then
   begin
     Result:=item;
     FList^[i]:=nil;
     Delete(i);
     Notify(Result,lnExtracted);
   end;
end;


procedure TList.Notify(Ptr: Pointer; Action: TListNotification);
begin
end;


procedure TList.SetCapacity(NewCapacity: Integer);

Var NewList,ToFree : PPointerList;

begin
  If (NewCapacity<0) or (NewCapacity>MaxListSize) then
     Error (SListCapacityError,NewCapacity);
  if NewCapacity=FCapacity then
    exit;
  ReallocMem(FList,SizeOf(Pointer)*NewCapacity);
  if NewCapacity > FCapacity then
    FillChar (FList^ [FCapacity],
                              (NewCapacity - FCapacity) * SizeOf (pointer), 0);
  FCapacity:=NewCapacity;
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

Var
  OldPointer :Pointer;

begin
  If (Index<0) or (Index>=FCount) then
    Error (SListIndexError,Index);
  FCount:=FCount-1;
  OldPointer:=Flist^[Index];
  System.Move (FList^[Index+1],FList^[Index],(FCount-Index)*SizeOf(Pointer));
  // Shrink the list if appropiate
  if (FCapacity > 256) and (FCount < FCapacity shr 2) then
  begin
    FCapacity := FCapacity shr 1;
    ReallocMem(FList, SizeOf(Pointer) * FCapacity);
  end;
  If OldPointer<>nil then
    Notify(OldPointer,lnDeleted);
end;


class procedure TList.Error(const Msg: string; Data: Integer);

begin
{$ifdef VER1_0}
  Raise EListError.CreateFmt(Msg,[Data]) at longint(get_caller_addr(get_frame));
{$else VER1_0}
  Raise EListError.CreateFmt(Msg,[Data]) at get_caller_addr(get_frame);
{$endif VER1_0}
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
  if FCapacity>127 then Inc(IncSize, FCapacity shr 2);
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
  If Item<>NIl then
    Notify(Item,lnAdded);
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
  FList^[CurIndex]:=Nil;
  Self.Delete(CurIndex);
  // ?? If NewIndex>CurIndex then NewIndex:=NewIndex-1;
  // Newindex changes when deleting ??
  Self.Insert (NewIndex,Nil);
  FList^[NewIndex]:=Temp;
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

procedure TList.Assign(Obj:TList);
// Principle copied from TCollection

var i : Integer;
begin
  Clear;
  For I:=0 To Obj.Count-1 Do
    Add(Obj[i]);
end;

{****************************************************************************}
{*                             TThreadList                                  *}
{****************************************************************************}


constructor TThreadList.Create;
begin
inherited Create;
//InitializeCriticalSection(FLock);
FList := TList.Create;
end;



destructor TThreadList.Destroy;
begin
  LockList;
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
  end;
end;



procedure TThreadList.Add(Item: Pointer);
begin
  Locklist;
  try
    //make sure it's not already in the list
    if FList.indexof(Item) = -1 then
       FList.Add(Item);
  finally
  UnlockList;
  end;
end;


procedure TThreadList.Clear;
begin
  Locklist;
  try
    FList.Clear;
  finally
    UnLockList;
  end;
end;



function TThreadList.LockList: TList;


begin
  Result := FList;
end;



procedure TThreadList.Remove(Item: Pointer);
begin
  LockList;
  try
    FList.Remove(Item);
  finally
    UnlockList;
  end;
end;



procedure TThreadList.UnlockList;
begin

end;


{
  $Log: lists.inc,v $
  Revision 1.4  2005/05/12 21:47:34  hajny
    * fix for SIGSEGV due to access to uninitialized pointers in TList

  Revision 1.3  2005/02/14 17:13:11  peter
    * truncate log

}
