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
{*                             TStream                                      *}
{****************************************************************************}

  function TStream.GetPosition: Longint;

    begin
       Result:=Seek(0,soFromCurrent);
    end;

  procedure TStream.SetPosition(Pos: Longint);

    begin
       Seek(pos,soFromBeginning);
    end;

  function TStream.GetSize: Longint;

    var
       p : longint;

    begin
       p:=GetPosition;
       GetSize:=Seek(0,soFromEnd);
       Seek(p,soFromBeginning);
    end;

  procedure TStream.SetSize(NewSize: Longint);

    begin
    // We do nothing. Pipe streams don't support this
    // As wel as possible read-ony streams !!
    end;

  procedure TStream.ReadBuffer(var Buffer; Count: Longint);

    begin
       if Read(Buffer,Count)<Count then
         Raise EReadError.Create(SReadError);
    end;

  procedure TStream.WriteBuffer(const Buffer; Count: Longint);

    begin
       if Write(Buffer,Count)<Count then
         Raise EWriteError.Create(SWriteError);
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
      Reader: TReader;

    begin

      Reader := TReader.Create(Self, 4096);
      try
        Result := Reader.ReadRootComponent(Instance);
      finally
        Reader.Free;
      end;

    end;

  function TStream.ReadComponentRes(Instance: TComponent): TComponent;

    begin

      ReadResHeader;
      Result := ReadComponent(Instance);

    end;

  procedure TStream.WriteComponent(Instance: TComponent);

    begin

      WriteDescendent(Instance, nil);

    end;

  procedure TStream.WriteComponentRes(const ResName: string; Instance: TComponent);

    begin

      WriteDescendentRes(ResName, Instance, nil);

    end;

  procedure TStream.WriteDescendent(Instance, Ancestor: TComponent);

    var
       Driver : TAbstractObjectWriter;
       Writer : TWriter;

    begin

       Driver := TBinaryObjectWriter.Create(Self, 4096);
       Try
         Writer := TWriter.Create(Driver);
	 Try
           Writer.WriteDescendent(Instance, Ancestor);
	 Finally
	   Writer.Destroy;
	 end;
       Finally
	 Driver.Free;
       end;

    end;

  procedure TStream.WriteDescendentRes(const ResName: string; Instance, Ancestor: TComponent);

    var
      FixupInfo: Integer;

    begin

      { Write a resource header }
      WriteResourceHeader(ResName, FixupInfo);
      { Write the instance itself }
      WriteDescendent(Instance, Ancestor);
      { Insert the correct resource size into the resource header }
      FixupResourceHeader(FixupInfo);

    end;

  procedure TStream.WriteResourceHeader(const ResName: string; {!!!: out} var FixupInfo: Integer);

    begin
       { Numeric resource type }
       WriteByte($ff);
       { Application defined data }
       WriteWord($0a);
       { write the name as asciiz }
       WriteBuffer(ResName[1],length(ResName));
       WriteByte(0);
       { Movable, Pure and Discardable }
       WriteWord($1030);
       { Placeholder for the resource size }
       WriteDWord(0);
       { Return current stream position so that the resource size can be
         inserted later }
       FixupInfo := Position;
    end;

  procedure TStream.FixupResourceHeader(FixupInfo: Integer);

    var
       ResSize : Integer;

    begin

      ResSize := Position - FixupInfo;

      { Insert the correct resource size into the placeholder written by
        WriteResourceHeader }
      Position := FixupInfo - 4;
      WriteDWord(ResSize);
      { Seek back to the end of the resource }
      Position := FixupInfo + ResSize;

    end;

  procedure TStream.ReadResHeader;

    begin
       try
         { application specific resource ? }
         if ReadByte<>$ff then
           raise EInvalidImage.Create(SInvalidImage);
         if ReadWord<>$000a then
           raise EInvalidImage.Create(SInvalidImage);
         { read name }
         while ReadByte<>0 do
           ;
         { check the access specifier }
         if ReadWord<>$1030 then
           raise EInvalidImage.Create(SInvalidImage);
         { ignore the size }
         ReadDWord;
       except
         on EInvalidImage do
           raise;
         else
           raise EInvalidImage.create(SInvalidImage);
       end;
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

  Function TStream.ReadAnsiString : String;
  Type
    PByte = ^Byte;
  Var
    TheSize : Longint;
    P : PByte ;
  begin
    ReadBuffer (TheSize,SizeOf(TheSize));
    SetLength(Result,TheSize);
    // Illegal typecast if no AnsiStrings defined.
    if TheSize>0 then
     begin
       ReadBuffer (Pointer(Result)^,TheSize);
       P:=Pointer(Result)+TheSize;
       p^:=0;
     end;
   end;

  Procedure TStream.WriteAnsiString (S : String);

  Var L : Longint;

  begin
    L:=Length(S);
    WriteBuffer (L,SizeOf(L));
    WriteBuffer (Pointer(S)^,L);
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
{*                             THandleStream                                *}
{****************************************************************************}

Constructor THandleStream.Create(AHandle: Integer);

begin
  FHandle:=AHandle;
end;


function THandleStream.Read(var Buffer; Count: Longint): Longint;

begin
  Result:=FileRead(FHandle,Buffer,Count);
  If Result=-1 then Result:=0;
end;


function THandleStream.Write(const Buffer; Count: Longint): Longint;

begin
  Result:=FileWrite (FHandle,Buffer,Count);
  If Result=-1 then Result:=0;
end;




{****************************************************************************}
{*                             TFileStream                                  *}
{****************************************************************************}

constructor TFileStream.Create(const AFileName: string; Mode: Word);

begin
  FFileName:=AFileName;
  If Mode=fmcreate then
    FHandle:=FileCreate(AFileName)
  else
    FHAndle:=FileOpen(AFileName,Mode);
  If FHandle<0 then
    If Mode=fmcreate then
      raise EFCreateError.createfmt(SFCreateError,[AFileName])
    else
      raise EFOpenError.Createfmt(SFOpenError,[AFilename]);
end;


destructor TFileStream.Destroy;

begin
  FileClose(FHandle);
end;

Procedure TFileStream.SetSize(NewSize: Longint);

begin
  FileTruncate(FHandle,NewSize);
end;


function TFileStream.Seek(Offset: Longint; Origin: Word): Longint;

begin
  Result:=FileSeek(FHandle,Offset,Origin);
end;


{****************************************************************************}
{*                             TCustomMemoryStream                          *}
{****************************************************************************}

procedure TCustomMemoryStream.SetPointer(Ptr: Pointer; ASize: Longint);

begin
  FMemory:=Ptr;
  FSize:=ASize;
end;


function TCustomMemoryStream.Read(var Buffer; Count: Longint): Longint;

begin
  Result:=0;
  If (FSize>0) and (FPosition<Fsize) then
    begin
    Result:=FSize-FPosition;
    If Result>Count then Result:=Count;
    Move ((FMemory+FPosition)^,Buffer,Result);
    FPosition:=Fposition+Result;
    end;
end;


function TCustomMemoryStream.Seek(Offset: Longint; Origin: Word): Longint;

begin
  Case Origin of
    soFromBeginning : FPosition:=Offset;
    soFromEnd       : FPosition:=FSize+Offset;
    soFromCurrent   : FpoSition:=FPosition+Offset;
  end;
  Result:=FPosition;
end;


procedure TCustomMemoryStream.SaveToStream(Stream: TStream);

begin
  if FSize>0 then Stream.WriteBuffer (FMemory^,FSize);
end;


procedure TCustomMemoryStream.SaveToFile(const FileName: string);

Var S : TFileStream;

begin
  Try
    S:=TFileStream.Create (FileName,fmCreate);
    SaveToStream(S);
  finally
    S.free;
  end;
end;


{****************************************************************************}
{*                             TMemoryStream                                *}
{****************************************************************************}


Const TMSGrow = 4096; { Use 4k blocks. }

procedure TMemoryStream.SetCapacity(NewCapacity: Longint);

begin
  SetPointer (Realloc(NewCapacity),Fsize);
  FCapacity:=NewCapacity;
end;


function TMemoryStream.Realloc(var NewCapacity: Longint): Pointer;

Var MoveSize : Longint;

begin
  If NewCapacity>0 Then // round off to block size.
    NewCapacity := (NewCapacity + (TMSGrow-1)) and not (TMSGROW-1);
  // Only now check !
  If NewCapacity=FCapacity then
    Result:=FMemory
  else
    If NewCapacity=0 then
      FreeMem (FMemory,Fcapacity)
    else
      begin
      GetMem (Result,NewCapacity);
      If Result=Nil then
        Raise EStreamError.Create(SMemoryStreamError);
      If FCapacity>0 then
        begin
        MoveSize:=FSize;
        If MoveSize>NewCapacity then MoveSize:=NewCapacity;
        Move (Fmemory^,Result^,MoveSize);
        FreeMem (FMemory,FCapacity);
        end;
      end;
end;


destructor TMemoryStream.Destroy;

begin
  Clear;
  Inherited Destroy;
end;


procedure TMemoryStream.Clear;

begin
  FSize:=0;
  FPosition:=0;
  SetCapacity (0);
end;


procedure TMemoryStream.LoadFromStream(Stream: TStream);

begin
  Stream.Position:=0;
  SetSize(Stream.Size);
  If FSize>0 then Stream.ReadBuffer(FMemory^,FSize);
end;


procedure TMemoryStream.LoadFromFile(const FileName: string);

Var S : TFileStream;

begin
  Try
    S:=TFileStream.Create (FileName,fmOpenRead);
    LoadFromStream(S);
  finally
    S.free;
  end;
end;


procedure TMemoryStream.SetSize(NewSize: Longint);

begin
  SetCapacity (NewSize);
  FSize:=NewSize;
  IF FPosition>FSize then
    FPosition:=FSize;
end;


function TMemoryStream.Write(const Buffer; Count: Longint): Longint;

Var NewPos : Longint;

begin
  If Count=0 then
    exit(0);
  NewPos:=FPosition+Count;
  If NewPos>Fsize then
    begin
    IF NewPos>FCapacity then
      SetCapacity (NewPos);
    FSize:=Newpos;
    end;
  System.Move (Buffer,(FMemory+FPosition)^,Count);
  FPosition:=NewPos;
  Result:=Count;
end;


{****************************************************************************}
{*                             TStringStream                                *}
{****************************************************************************}

procedure TStringStream.SetSize(NewSize: Longint);

begin
 //!! Setlength(FDataString,NewSize);
 If FPosition>NewSize then FPosition:=NewSize;
end;


constructor TStringStream.Create(const AString: string);

begin
  Inherited create;
  FDataString:=AString;
end;


function TStringStream.Read(var Buffer; Count: Longint): Longint;

begin
  Result:=Length(FDataString)-FPosition;
  If Result>Count then Result:=Count;
  // This supposes FDataString to be of type AnsiString !
  //!! Move (Pchar(FDataString)[FPosition],Buffer,Count);
  FPosition:=FPosition+Count;
end;


function TStringStream.ReadString(Count: Longint): string;

Var NewLen : Longint;

begin
  NewLen:=Length(FDataString)-FPosition;
  If NewLen>Count then NewLen:=Count;
  //!! SetLength(Result,NewLen);
  //!! Read (Pointer(Result)^,NewLen);
  ReadString:='';
end;


function TStringStream.Seek(Offset: Longint; Origin: Word): Longint;

begin
  Case Origin of
    soFromBeginning : FPosition:=Offset;
    soFromEnd       : FPosition:=Length(FDataString)+Offset;
    soFromCurrent   : FpoSition:=FPosition+Offset;
  end;
  If FPosition>Length(FDataString) then FPosition:=Length(FDataString);
  If FPosition<0 then FPosition:=0;
  Result:=FPosition;
end;


function TStringStream.Write(const Buffer; Count: Longint): Longint;

begin
  Result:=Count;
  SetSize(FPosition+Count);
  // This supposes that FDataString is of type AnsiString)
  //!! Move (Buffer,PCHar(FDataString)[Fposition],Count);
  FPosition:=FPosition+Count;
end;


procedure TStringStream.WriteString(const AString: string);

begin
  //!! Write (PChar(Astring)[0],Length(AString));
end;



{****************************************************************************}
{*                             TResourceStream                              *}
{****************************************************************************}

procedure TResourceStream.Initialize(Instance: THandle; Name, ResType: PChar);

begin
end;


constructor TResourceStream.Create(Instance: THandle; const ResName: string; ResType: PChar);

begin
end;


constructor TResourceStream.CreateFromID(Instance: THandle; ResID: Integer; ResType: PChar);

begin
end;


destructor TResourceStream.Destroy;

begin
end;


function TResourceStream.Write(const Buffer; Count: Longint): Longint;

begin
  Write:=0;
end;


{
  $Log$
  Revision 1.1  2000-07-13 06:31:31  michael
  + Initial import

  Revision 1.21  2000/06/29 16:29:23  sg
  * Implemented streaming. Note: The writer driver interface is stable, but
    the reader interface is not final yet!

  Revision 1.20  2000/01/07 01:24:33  peter
    * updated copyright to 2000

  Revision 1.19  2000/01/06 01:20:33  peter
    * moved out of packages/ back to topdir

  Revision 1.2  2000/01/04 18:07:16  michael
  + Streaming implemented

  Revision 1.1  2000/01/03 19:33:08  peter
    * moved to packages dir

  Revision 1.17  1999/11/30 15:28:38  michael
  + Added FileNAme property for filestreams

  Revision 1.16  1999/10/03 19:38:06  peter
    * fixed readansistring
    * fixed constants

  Revision 1.15  1999/09/13 08:35:16  fcl
  * Changed some argument names (Root->ARoot etc.) because the new compiler
    now performs more ambiguity checks  (sg)

  Revision 1.14  1999/07/18 20:58:47  michael
  * fixed bug in realloc and setcapacity of tmemorystream

  Revision 1.13  1999/04/08 10:18:55  peter
    * makefile updates

}
