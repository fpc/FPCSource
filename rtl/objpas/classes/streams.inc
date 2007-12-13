{
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

  function TStream.GetPosition: Int64;

    begin
       Result:=Seek(0,soCurrent);
    end;

  procedure TStream.SetPosition(const Pos: Int64);

    begin
       Seek(pos,soBeginning);
    end;

  procedure TStream.SetSize64(const NewSize: Int64);

    begin
      // Required because can't use overloaded functions in properties
      SetSize(NewSize);
    end;

  function TStream.GetSize: Int64;

    var
       p : int64;

    begin
       p:=Seek(0,soCurrent);
       GetSize:=Seek(0,soEnd);
       Seek(p,soBeginning);
    end;

  procedure TStream.SetSize(NewSize: Longint);

    begin
    // We do nothing. Pipe streams don't support this
    // As wel as possible read-ony streams !!
    end;

  procedure TStream.SetSize(const NewSize: Int64);

    begin
      // Backwards compatibility that calls the longint SetSize
      if (NewSize<Low(longint)) or
         (NewSize>High(longint)) then
        raise ERangeError.Create(SRangeError);
      SetSize(longint(NewSize));
    end;

  function TStream.Seek(Offset: Longint; Origin: Word): Longint;

    type
      TSeek64 = function(const offset:Int64;Origin:TSeekorigin):Int64 of object;
    var
      CurrSeek,
      TStreamSeek : TSeek64;
      CurrClass   : TClass;
    begin
      // Redirect calls to 64bit Seek, but we can't call the 64bit Seek
      // from TStream, because then we end up in an infinite loop
      CurrSeek:=nil;
      CurrClass:=Classtype;
      while (CurrClass<>nil) and
            (CurrClass<>TStream) do
       CurrClass:=CurrClass.Classparent;
      if CurrClass<>nil then
       begin
         CurrSeek:=@Self.Seek;
         TStreamSeek:=@TStream(@CurrClass).Seek;
         if TMethod(TStreamSeek).Code=TMethod(CurrSeek).Code then
          CurrSeek:=nil;
       end;
      if CurrSeek<>nil then
       Result:=Seek(Int64(offset),TSeekOrigin(origin))
      else
       raise EStreamError.CreateFmt(SSeekNotImplemented,[ClassName]);
    end;

  function TStream.Seek(const Offset: Int64; Origin: TSeekorigin): Int64;

    begin
      // Backwards compatibility that calls the longint Seek
      if (Offset<Low(longint)) or
         (Offset>High(longint)) then
        raise ERangeError.Create(SRangeError);
      Result:=Seek(longint(Offset),ord(Origin));
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

  function TStream.CopyFrom(Source: TStream; Count: Int64): Int64;

    var
       i : Int64;
       buffer : array[0..1023] of byte;

    begin
       CopyFrom:=0;
       If (Count=0) then
         begin
         // This WILL fail for non-seekable streams...
         Source.Position:=0;
         Count:=Source.Size;
         end;
       while Count>0 do
         begin
         if (Count>sizeof(buffer)) then
           i:=sizeof(Buffer)
         else
           i:=Count;
         i:=Source.Read(buffer,i);
         i:=Write(buffer,i);
         if i=0 then break;
         dec(count,i);
         CopyFrom:=CopyFrom+i;
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
    var
      ResType, Flags : word;
    begin
       ResType:=NtoLE(word($000A));
       Flags:=NtoLE(word($1030));
       { Note: This is a Windows 16 bit resource }
       { Numeric resource type }
       WriteByte($ff);
       { Application defined data }
       WriteWord(ResType);
       { write the name as asciiz }
       WriteBuffer(ResName[1],length(ResName));
       WriteByte(0);
       { Movable, Pure and Discardable }
       WriteWord(Flags);
       { Placeholder for the resource size }
       WriteDWord(0);
       { Return current stream position so that the resource size can be
         inserted later }
       FixupInfo := Position;
    end;

  procedure TStream.FixupResourceHeader(FixupInfo: Integer);

    var
       ResSize,TmpResSize : Integer;

    begin

      ResSize := Position - FixupInfo;
      TmpResSize := NtoLE(longword(ResSize));

      { Insert the correct resource size into the placeholder written by
        WriteResourceHeader }
      Position := FixupInfo - 4;
      WriteDWord(TmpResSize);
      { Seek back to the end of the resource }
      Position := FixupInfo + ResSize;

    end;

  procedure TStream.ReadResHeader;
    var
      ResType, Flags : word;
    begin
       try
         { Note: This is a Windows 16 bit resource }
         { application specific resource ? }
         if ReadByte<>$ff then
           raise EInvalidImage.Create(SInvalidImage);
         ResType:=LEtoN(ReadWord);
         if ResType<>$000a then
           raise EInvalidImage.Create(SInvalidImage);
         { read name }
         while ReadByte<>0 do
           ;
         { check the access specifier }
         Flags:=LEtoN(ReadWord);
         if Flags<>$1030 then
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

Procedure THandleStream.SetSize(NewSize: Longint);

begin
  SetSize(Int64(NewSize));
end;


Procedure THandleStream.SetSize(const NewSize: Int64);

begin
  FileTruncate(FHandle,NewSize);
end;


function THandleStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;

begin
  Result:=FileSeek(FHandle,Offset,ord(Origin));
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

  If (THandle(FHandle)=feInvalidHandle) then
    If Mode=fmcreate then
      raise EFCreateError.createfmt(SFCreateError,[AFileName])
    else
      raise EFOpenError.Createfmt(SFOpenError,[AFilename]);
end;


constructor TFileStream.Create(const AFileName: string; Mode: Word; Rights: Cardinal);

begin
  FFileName:=AFileName;
  If Mode=fmcreate then
    FHandle:=FileCreate(AFileName,Rights)
  else
    FHAndle:=FileOpen(AFileName,Mode);

  If (THandle(FHandle)=feInvalidHandle) then
    If Mode=fmcreate then
      raise EFCreateError.createfmt(SFCreateError,[AFileName])
    else
      raise EFOpenError.Createfmt(SFOpenError,[AFilename]);
end;


destructor TFileStream.Destroy;

begin
  FileClose(FHandle);
end;

{****************************************************************************}
{*                             TCustomMemoryStream                          *}
{****************************************************************************}

procedure TCustomMemoryStream.SetPointer(Ptr: Pointer; ASize: Longint);

begin
  FMemory:=Ptr;
  FSize:=ASize;
end;


function TCustomMemoryStream.GetSize: Int64;

begin
  Result:=FSize;
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
  S:=TFileStream.Create (FileName,fmCreate);
  Try
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

begin

  If NewCapacity<0 Then
    NewCapacity:=0
  else  
    begin
      // if growing, grow at least a quarter
      if (NewCapacity>FCapacity) and (NewCapacity < (5*FCapacity) div 4) then
        NewCapacity := (5*FCapacity) div 4;
      // round off to block size.
      NewCapacity := (NewCapacity + (TMSGrow-1)) and not (TMSGROW-1);
    end;
  // Only now check !
  If NewCapacity=FCapacity then
    Result:=FMemory
  else
    begin
      Result:=Reallocmem(FMemory,Newcapacity);
      If (Result=Nil) and (Newcapacity>0) then
        Raise EStreamError.Create(SMemoryStreamError);
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
  S:=TFileStream.Create (FileName,fmOpenRead);
  Try
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
 Setlength(FDataString,NewSize);
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
  Move (Pchar(FDataString)[FPosition],Buffer,Result);
  FPosition:=FPosition+Result;
end;


function TStringStream.ReadString(Count: Longint): string;

Var NewLen : Longint;

begin
  NewLen:=Length(FDataString)-FPosition;
  If NewLen>Count then NewLen:=Count;
  SetLength(Result,NewLen);
  Read (Pointer(Result)^,NewLen);
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
  Move (Buffer,PChar(FDataString)[Fposition],Count);
  FPosition:=FPosition+Count;
end;


procedure TStringStream.WriteString(const AString: string);

begin
  Write (PChar(Astring)[0],Length(AString));
end;



{****************************************************************************}
{*                             TResourceStream                              *}
{****************************************************************************}

{$ifdef UNICODE}
procedure TResourceStream.Initialize(Instance: THandle; Name, ResType: PWideChar);
  begin
    Res:=FindResource(Instance, Name, ResType);
    if Res=0 then
      raise EResNotFound.CreateFmt(SResNotFound,[Name]);
    Handle:=LoadResource(Instance,Res);
    if Handle=0 then
      raise EResNotFound.CreateFmt(SResNotFound,[Name]);
    SetPointer(LockResource(Handle),SizeOfResource(Instance,Res));
  end;

constructor TResourceStream.Create(Instance: THandle; const ResName: WideString; ResType: PWideChar);
  begin
    inherited create;
    Initialize(Instance,PWideChar(ResName),ResType);
  end;
constructor TResourceStream.CreateFromID(Instance: THandle; ResID: Integer; ResType: PWideChar);
  begin
    inherited create;
    Initialize(Instance,PWideChar(ResID),ResType);
  end;
{$else UNICODE}

procedure TResourceStream.Initialize(Instance: THandle; Name, ResType: PChar);
  begin
    Res:=FindResource(Instance, Name, ResType);
    if Res=0 then
      raise EResNotFound.CreateFmt(SResNotFound,[Name]);
    Handle:=LoadResource(Instance,Res);
    if Handle=0 then
      raise EResNotFound.CreateFmt(SResNotFound,[Name]);
    SetPointer(LockResource(Handle),SizeOfResource(Instance,Res));
  end;

constructor TResourceStream.Create(Instance: THandle; const ResName: string; ResType: PChar);
  begin
    inherited create;
    Initialize(Instance,pchar(ResName),ResType);
  end;
constructor TResourceStream.CreateFromID(Instance: THandle; ResID: Integer; ResType: PChar);
  begin
    inherited create;
    Initialize(Instance,pchar(PtrInt(ResID)),ResType);
  end;
{$endif UNICODE}


destructor TResourceStream.Destroy;
  begin
    UnlockResource(Handle);
    FreeResource(Handle);
    inherited destroy;
  end;

{$warnings off}
function TResourceStream.Write(const Buffer; Count: Longint): Longint;
  begin
    raise EStreamError.Create(SCantWriteResourceStreamError);
  end;
{$warnings on}

{****************************************************************************}
{*                             TOwnerStream                                 *}
{****************************************************************************}

constructor TOwnerStream.Create(ASource: TStream);
begin
  FSource:=ASource;
end;

destructor TOwnerStream.Destroy;
begin
  If FOwner then
    FreeAndNil(FSource);
  inherited Destroy;
end;

{****************************************************************************}
{*                             TStreamAdapter                               *}
{****************************************************************************}
constructor TStreamAdapter.Create(Stream: TStream; Ownership: TStreamOwnership = soReference);
begin
  inherited Create;
  FStream:=Stream;
  FOwnership:=Ownership;
end;


destructor TStreamAdapter.Destroy;
begin
  if StreamOwnership=soOwned then
    FreeAndNil(FStream);
  inherited Destroy;
end;
  
{$warnings off}
function TStreamAdapter.Read(pv: Pointer; cb: DWORD; pcbRead: PDWORD): HResult; stdcall;
begin
  runerror(217);
end;


function TStreamAdapter.Write(pv: Pointer; cb: DWORD; pcbWritten: PDWORD): HResult; stdcall;
begin
  runerror(217);
end;


function TStreamAdapter.Seek(dlibMove: Largeint; dwOrigin: Longint; out libNewPosition: Largeint): HResult; stdcall;
begin
  runerror(217);
end;


function TStreamAdapter.SetSize(libNewSize: Largeint): HResult; stdcall;
begin
  runerror(217);
end;


function TStreamAdapter.CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint; out cbWritten: Largeint): HResult; stdcall;
begin
  runerror(217);
end;


function TStreamAdapter.Commit(grfCommitFlags: Longint): HResult; stdcall;
begin
  runerror(217);
end;


function TStreamAdapter.Revert: HResult; stdcall;
begin
  runerror(217);
end;


function TStreamAdapter.LockRegion(libOffset: Largeint; cb: Largeint; dwLockType: Longint): HResult; stdcall;
begin
  runerror(217);
end;


function TStreamAdapter.UnlockRegion(libOffset: Largeint; cb: Largeint; dwLockType: Longint): HResult; stdcall;
begin
  runerror(217);
end;


function TStreamAdapter.Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult; stdcall;
begin
  runerror(217);
end;


function TStreamAdapter.Clone(out stm: IStream): HResult; stdcall;
begin
  runerror(217);
end;
{$warnings on}
