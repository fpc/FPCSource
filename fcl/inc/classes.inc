{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by Michael Van Canneyt and Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************}
{*                               TBits                                      *}
{****************************************************************************}

  procedure TBits.Error;

    begin
{$ifdef NoExceptions}
         ;
{$else}
         Raise(EBitsError);
{$endif}
    end;

  procedure TBits.SetSize(Value: Integer);

    var
       hp : pointer;
       cvalue,csize : Integer;

    begin
       { ajust value to n*8 }
       cvalue:=Value;
       if cvalue mod 8<>0 then
         cvalue:=cvalue+(8-(cvalue mod 8));

       { store pointer to release it later }
       hp:=FBits;

       { ajust size to n*8 }
       csize:=FSize;
       if csize mod 8<>0 then
         csize:=csize+(8-(csize mod 8));

       if FSize>0 then
         begin
            { get new memory }
            GetMem(FBits,cvalue div 8);
            { clear the whole array }
            FillChar(FBits^,cvalue div 8,0);
            { copy old data }
            Move(hp^,FBits^,csize div 8);
         end
       else
         FBits:=nil;

       if assigned(hp) then
         FreeMem(hp,csize div 8);

       FSize:=Value;
    end;

  procedure TBits.SetBit(Index: Integer; Value: Boolean);

    type
       pbyte = ^byte;

    begin
       if (Index>=FSize) or (Index<0)  then
         Error
       else
         begin
            if Value then
              pbyte(FBits)[Index div 8]:=pbyte(FBits)[Index div 8] or
                (1 shl (Index mod 8))
            else
              pbyte(FBits)[Index div 8]:=pbyte(FBits)[Index div 8] and
                not(1 shl (Index mod 8));
         end;
    end;

  function TBits.GetBit(Index: Integer): Boolean;

    type
       pbyte = ^byte;

    begin
       if (Index>=FSize) or (Index<0) then
         Error
       else
         GetBit:=(pbyte(FBits)[Index div 8] and (1 shl (Index mod 8)))<>0;
    end;

  destructor TBits.Destroy;

    var
       csize : Integer;

    begin
       { ajust size to n*8 }
       csize:=FSize;
       if csize mod 8<>0 then
         csize:=csize+(8-(csize mod 8));
       if assigned(FBits) then
         FreeMem(FBits,csize);
       inherited Destroy;
    end;

  function TBits.OpenBit: Integer;

    type
       pbyte = ^byte;

    var
       i : Integer;

    begin
       for i:=0 to FSize-1 do
         if (pbyte(FBits)[i div 8] and (1 shl (i mod 8)))=0 then
           begin
              OpenBit:=i;
              exit;
           end;
       SetSize(FSize+1);
       OpenBit:=FSize-1;
    end;

{****************************************************************************}
{*                             TStream                                      *}
{****************************************************************************}

  function TStream.GetPosition: Longint;

    begin
       GetPosition:=Seek(0,soFromCurrent);
    end;

  procedure TStream.SetPosition(Pos: Longint);

    begin
       Seek(soFromBeginning,Pos);
    end;

  function TStream.GetSize: Longint;

    var
       p : longint;

    begin
       p:=GetPosition;
       GetSize:=Seek(soFromEnd,0);
       Seek(soFromBeginning,p);
    end;

  procedure TStream.SetSize(NewSize: Longint);

    begin
       SetPosition(NewSize);
    end;

  procedure TStream.ReadBuffer(var Buffer; Count: Longint);

    begin
       if Read(Buffer,Count)<Count then
{$ifdef NoExceptions}
         ;
{$else}
         Raise(EReadError);
{$endif}
    end;

  procedure TStream.WriteBuffer(const Buffer; Count: Longint);

    begin
       if Write(Buffer,Count)<Count then
{$ifdef NoExceptions}
         ;
{$else}
         Raise(EWriteError);
{$endif}
    end;

  function TStream.CopyFrom(Source: TStream; Count: Longint): Longint;

    var
       i : longint;
       buffer : array[0..1023] of byte;

    begin
       CopyFrom:=0;
       while Count>0 do
         begin
            if (Count>sizeof(buffer)) then
              i:=sizeof(Buffer)
            else
              i:=Count;
            i:=Source.Read(buffer,i);
            i:=Write(buffer,i);
            dec(count,i);
            CopyFrom:=CopyFrom+i;
            if i=0 then
              exit;
         end;
    end;

  function TStream.ReadComponent(Instance: TComponent): TComponent;

    var
       Reader : TReader;

    begin
       Reader.Create(Self,1024);
       if assigned(Instance) then
         ReadComponent:=Reader.ReadRootComponent(Instance)
       else
         begin
            {!!!!!}
         end;
       Reader.Destroy;
    end;

  function TStream.ReadComponentRes(Instance: TComponent): TComponent;

    begin
       {!!!!!}
    end;

  procedure TStream.WriteComponent(Instance: TComponent);

    var
       Writer : TWriter;

    begin
       Writer.Create(Self,1024);
       Writer.WriteRootComponent(Instance);
       Writer.Destroy;
    end;

  procedure TStream.WriteComponentRes(const ResName: string; Instance: TComponent);

    var
       startpos,s : longint;

    begin
(* 
{$ifdef Win16Res}
       { Numeric resource type }
       WriteByte($ff);
       { Application defined data }
       WriteWord($0a);
       { write the name as asciiz }
       WriteData(ResName[1],length(ResName));
       WriteByte(0);
       { Movable, Pure and Discardable }
       WriteWord($1030);
       { size isn't known yet }
       WriteDWord(0);
       startpos:=GetPosition;
       WriteComponent(Instance);
       { calculate size }
       s:=GetPosition-startpos;
       { back patch size }
       SetPosition(startpos-4);
       WriteDWord(s);
{$endif Win16Res}
*)
    end;

  procedure TStream.WriteDescendent(Instance, Ancestor: TComponent);

    begin
       {!!!!!}
    end;

  procedure TStream.WriteDescendentRes(const ResName: string; Instance, Ancestor: TComponent);

    begin
       {!!!!!}
    end;

  procedure TStream.ReadResHeader;

    begin
{$ifdef Win16Res}
       try
         { application specific resource ? }
         if ReadByte<>$ff then
           raise EInvalidImage;
         if ReadWord<>$000a then
           raise EInvalidImage;
         { read name }
         while ReadByte<>0 do
           ;
         { check the access specifier }
         if ReadWord<>$1030 then
           raise EInvalidImage;
         { ignore the size }
         ReadDWord;
       except
{/////
         on EInvalidImage do
           raise;
         else
           raise(EInvalidImage);
}
       end;
{$endif Win16Res}
    end;

  function TStream.ReadByte : Byte;

    var
       b : Byte;

    begin
       ReadBuffer(b,1);
       ReadByte:=b;
    end;

  function TStream.ReadWord : Word;

    var
       w : Word;

    begin
       ReadBuffer(w,2);
       ReadWord:=w;
    end;

  function TStream.ReadDWord : Cardinal;

    var
       d : Cardinal;

    begin
       ReadBuffer(d,4);
       ReadDWord:=d;
    end;

  procedure TStream.WriteByte(b : Byte);

    begin
       WriteBuffer(b,1);
    end;

  procedure TStream.WriteWord(w : Word);

    begin
       WriteBuffer(w,2);
    end;

  procedure TStream.WriteDWord(d : Cardinal);

    begin
       WriteBuffer(d,4);
    end;

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
  Self.Insert (Count,Item);
end;



Procedure TList.Clear;

begin
end;



Procedure TList.Delete(Index: Integer);



begin
end;


class procedure Error(const Msg: string; Data: Integer);

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

{
  $Log$
  Revision 1.1  1998-05-04 12:16:01  florian
    + Initial revisions after making a new directory structure

}