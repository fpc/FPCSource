{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2002 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ TInerfaceListEnumerator }

    constructor TInterfaceListEnumerator.Create(AList: TInterfaceList);
      begin
        inherited create;
        FList:=AList;
        FPosition:=-1;
      end;


    function TInterfaceListEnumerator.GetCurrent: IUnknown;
      begin
        Result:=FList[FPosition];
      end;


    function TInterfaceListEnumerator.MoveNext: Boolean;
      begin
        Inc(FPosition);
        Result:=FPosition<FList.Count;
      end;

{ TInterfaceList }

    constructor TInterfaceList.Create;
      begin
        inherited create;
        FList:=TThreadList.Create;
      end;


    destructor TInterfaceList.Destroy;
      begin
        Clear;
        FList.Free;
        inherited Destroy;
      end;


    function TInterfaceList.Get(i : Integer) : IUnknown;
      begin
        FList.Locklist;
        try
          if (i<0) or (i>=FList.FList.Count) then
            FList.FList.Error(SListIndexError,i);
          result:=IUnknown(FList.FList.List^[i]);
        finally
          FList.UnlockList;
        end;
      end;


    function TInterfaceList.GetCapacity : Integer;
      begin
        FList.Locklist;
        try
          result:=FList.FList.Capacity;
        finally
          FList.UnlockList;
        end;
      end;


    function TInterfaceList.GetCount : Integer;
      begin
        FList.Locklist;
        try
          result:=FList.FList.Count;
        finally
          FList.UnlockList;
        end;
      end;


    procedure TInterfaceList.Put(i : Integer;item : IUnknown);
      begin
        FList.Locklist;
        try
          if (i<0) or (i>=FList.FList.Count) then
            FList.FList.Error(SListIndexError,i);
          IUnknown(FList.FList.List^[i]):=item;
        finally
          FList.UnlockList;
        end;
      end;


    procedure TInterfaceList.SetCapacity(NewCapacity : Integer);
      begin
        FList.Locklist;
        try
          FList.FList.Capacity:=NewCapacity;
        finally
          FList.UnlockList;
        end;
      end;


    procedure TInterfaceList.SetCount(NewCount : Integer);
      begin
        FList.Locklist;
        try
          FList.FList.Count:=NewCount;
        finally
          FList.UnlockList;
        end;
      end;


    procedure TInterfaceList.Clear;
      var
        i : SizeInt;
      begin
        FList.Locklist;
        try
          for i:=0 to FList.FList.Count-1 do
            IUnknown(FList.FList.List^[i]):=nil;
          FList.Clear;
        finally
          FList.UnlockList;
        end;
      end;


    procedure TInterfaceList.Delete(index : Integer);
      begin
        FList.Locklist;
        try
          if (index<0) or (index>=FList.FList.Count) then
            FList.FList.Error(SListIndexError,index);
          IUnknown(FList.FList.List^[index]):=nil;
          FList.FList.Delete(index);
        finally
          FList.UnlockList;
        end;
      end;


    procedure TInterfaceList.Exchange(index1,index2 : Integer);
      begin
        FList.Locklist;
        try
          FList.FList.Exchange(index1,index2);
        finally
          FList.UnlockList;
        end;
      end;


    function TInterfaceList.First : IUnknown;
      begin
        result:=Get(0);
      end;


    function TInterfaceList.GetEnumerator: TInterfaceListEnumerator;
      begin
        result:=TInterfaceListEnumerator.Create(Self)
      end;


    function TInterfaceList.IndexOf(item : IUnknown) : Integer;
      begin
        FList.Locklist;
        try
          result:=FList.FList.IndexOf(Pointer(Item));
        finally
          FList.UnlockList;
        end;
      end;


    function TInterfaceList.Add(item : IUnknown) : Integer;
      begin
        FList.Locklist;
        try
          result:=FList.FList.Add(nil);
          IUnknown(FList.FList.List^[result]):=item;
        finally
          FList.UnlockList;
        end;
      end;


    procedure TInterfaceList.Insert(i : Integer;item : IUnknown);
      begin
        FList.Locklist;
        try
          FList.FList.Insert(i,nil);
          IUnknown(FList.FList.List^[i]):=item;
        finally
          FList.UnlockList;
        end;
      end;


    function TInterfaceList.Last : IUnknown;
      begin
        result:=Get(Count-1);
      end;


    function TInterfaceList.Remove(item : IUnknown): Integer;
      begin
        FList.Locklist;
        try
          result:=FList.FList.IndexOf(item);
          if result>=0 then
            begin
              IUnknown(FList.FList.List^[result]):=nil;
              FList.FList.Delete(result);
            end;
        finally
          FList.UnlockList;
        end;
      end;


    procedure TInterfaceList.Lock;
      begin
        FList.Locklist;
      end;


    procedure TInterfaceList.Unlock;
      begin
        FList.UnlockList;
      end;


    function TInterfaceList.Expand : TInterfaceList;
      begin
        FList.Locklist;
        try
          FList.FList.Expand;
          result:=self;
        finally
          FList.UnlockList;
        end;
      end;
