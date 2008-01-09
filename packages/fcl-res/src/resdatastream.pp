{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Stream classes to provide copy-on-write functionality

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit resdatastream;

{$MODE OBJFPC}

interface

uses Classes, SysUtils, resource;

type

  TCachedDataStream = class (TStream)
  private
  protected
    fStream : TStream;
    fSize : int64;
    fPosition : int64;
    function  GetPosition: Int64; override;
    procedure SetPosition(const Pos: Int64); override;
    function  GetSize: Int64; override;
    procedure SetSize64(const NewSize: Int64); override;
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(aStream : TStream; aResource : TAbstractResource; aSize : int64); virtual;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;


  { TCachedResourceDataStream }

  TCachedResourceDataStream = class (TCachedDataStream)
  private
    fOffset : int64;
  protected
  public
    constructor Create(aStream : TStream; aResource : TAbstractResource; aSize : int64); override;
    function Read(var Buffer; Count: Longint): Longint; override;
  end;
  
  TCachedStreamClass = class of TCachedDataStream;
  

  TUnderlyingStreamType = (usCached, usMemory, usCustom);

  { TResourceDataStream }

  TResourceDataStream = class(TStream)
  private
    fStream : TStream;
    fStreamType : TUnderlyingStreamType;
    fResource : TAbstractResource;
    procedure CheckChangeStream;
    function GetCached : boolean;
    procedure SetCached(aValue : boolean);
  protected
    function  GetPosition: Int64; override;
    procedure SetPosition(const Pos: Int64); override;
    function  GetSize: Int64; override;
    procedure SetSize64(const NewSize: Int64); override;
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(aStream : TStream; aResource : TAbstractResource; aSize : int64; aClass: TCachedStreamClass);
    destructor Destroy; override;
    function Compare(aStream : TStream) : boolean;
    procedure SetCustomStream(aStream : TStream);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property Cached : boolean read GetCached write SetCached;
  end;
  
implementation

{ TCachedDataStream }

function TCachedDataStream.GetPosition: Int64;
begin
  Result:=fPosition;
end;

procedure TCachedDataStream.SetPosition(const Pos: Int64);
begin
  fPosition:=Pos;
end;

function TCachedDataStream.GetSize: Int64;
begin
  Result:=fSize;
end;

procedure TCachedDataStream.SetSize64(const NewSize: Int64);
begin
  raise EInvalidOperation.Create('');
end;

procedure TCachedDataStream.SetSize(NewSize: Longint);
begin
  SetSize64(NewSize);
end;

procedure TCachedDataStream.SetSize(const NewSize: Int64);
begin
 SetSize64(NewSize);
end;

constructor TCachedDataStream.Create(aStream: TStream;  aResource : TAbstractResource; aSize : int64);
begin
  fStream:=aStream;
  fSize:=aSize;
  fPosition:=0;
end;

function TCachedDataStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EInvalidOperation.Create('');
end;

function TCachedDataStream.Seek(Offset: Longint; Origin: Word
  ): Longint;
begin
  Result:=Seek(Offset,TSeekOrigin(Origin));
end;

function TCachedDataStream.Seek(const Offset: Int64; Origin: TSeekOrigin
  ): Int64;
var newpos : int64;
begin
  case Origin of
    soBeginning : newpos:=Offset;
    soCurrent : newpos:=Position+Offset;
    soEnd : newpos:=fSize-Offset;
  end;
  SetPosition(newpos);
  Result:=Position;
end;

{ TCachedResourceDataStream }

constructor TCachedResourceDataStream.Create(aStream: TStream;  aResource : TAbstractResource; aSize : int64);
begin
  inherited Create(aStream,aResource,aSize);
  fOffset:=fStream.Position;
end;

function TCachedResourceDataStream.Read(var Buffer; Count: Longint): Longint;
var oldpos : int64;
begin
  Result:=fSize-Position;
  if Count<Result then Result:=Count;
  if Result<0 then Result:=0;
  if Result>0 then
  begin
    oldpos:=fStream.Position;
    fStream.Position:=Position+fOffset;
    Result:=fStream.Read(Buffer,Result);
    fPosition:=fStream.Position-fOffset;
    fStream.Position:=oldpos;
  end;
end;

{ TResourceDataStream }

procedure TResourceDataStream.CheckChangeStream;
var NewStream : TMemoryStream;
    oldpos : int64;
begin
  if fStreamType = usCached then
  begin
    NewStream:=TMemoryStream.Create;
    try
      oldpos:=fStream.Position;
      fStream.Position:=0;
      NewStream.CopyFrom(fStream,fStream.Size);
      NewStream.Position:=oldpos;
    except
      NewStream.Free;
      raise;
    end;
    fStream.Free;
    fStream:=NewStream;
    fStreamType:=usMemory;
  end;
end;

function TResourceDataStream.GetCached: boolean;
begin
  Result:=fStreamType = usCached;
end;

procedure TResourceDataStream.SetCached(aValue: boolean);
begin
  if aValue=false then CheckChangeStream;
end;

function TResourceDataStream.GetPosition: Int64;
begin
  Result:=fStream.Position;
end;

procedure TResourceDataStream.SetPosition(const Pos: Int64);
begin
  fStream.Position:=Pos;
end;

function TResourceDataStream.GetSize: Int64;
begin
  Result:=fStream.Size;
end;

procedure TResourceDataStream.SetSize64(const NewSize: Int64);
begin
  CheckChangeStream;
  fStream.Size:=NewSize;
end;

procedure TResourceDataStream.SetSize(NewSize: Longint);
begin
  SetSize64(NewSize);
end;

procedure TResourceDataStream.SetSize(const NewSize: Int64);
begin
  SetSize64(NewSize);
end;

constructor TResourceDataStream.Create(aStream: TStream; aResource :
  TAbstractResource; aSize : int64; aClass: TCachedStreamClass);
begin
  if aStream=nil then fStreamType:=usMemory
  else fStreamType:=usCached;
  case fStreamType of
    usMemory : fStream:=TMemoryStream.Create;
    usCached : fStream:=aClass.Create(aStream,aResource,aSize);
  end;
  fResource:=aResource;
end;

destructor TResourceDataStream.Destroy;
begin
  if fStreamType<>usCustom then fStream.Free;
end;

function TResourceDataStream.Compare(aStream : TStream) : boolean;
var tmp1, tmp2 : PtrUint;
    b1,b2 : byte;
    oldpos1,oldpos2 : int64;
    tocompare : longword;
begin
  Result:=aStream=self;
  if Result then exit;
  Result:=aStream<>nil;
  if not Result then exit;
  Result:=Size=aStream.Size;
  if not Result then exit;
  oldpos1:=Position;
  oldpos2:=aStream.Position;
  Position:=0;
  aStream.Position:=0;
  tocompare:=Size;
  while tocompare >= sizeof(PtrUInt) do
  begin
    ReadBuffer(tmp1,sizeof(PtrUInt));
    aStream.ReadBuffer(tmp2,sizeof(PtrUInt));
    Result:=tmp1=tmp2;
    if not result then
    begin
      tocompare:=0;
      break;
    end;
    dec(tocompare,sizeof(PtrUInt));
  end;
  while tocompare > 0 do
  begin
    ReadBuffer(b1,1);
    aStream.ReadBuffer(b2,1);
    Result:=b1=b2;
    if not result then
      break;
    dec(tocompare);
  end;
  Position:=oldpos1;
  aStream.Position:=oldpos2;
end;

procedure TResourceDataStream.SetCustomStream(aStream: TStream);
begin
  if fStreamType<>usCustom then fStream.Free;
  if aStream=nil then
  begin
    fStream:=TMemoryStream.Create;
    fStreamType:=usMemory;
  end
  else
  begin
    fStreamType:=usCustom;
    fStream:=aStream;
  end;
end;

function TResourceDataStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result:=fStream.Read(Buffer,Count);
end;

function TResourceDataStream.Write(const Buffer; Count: Longint): Longint;
begin
  CheckChangeStream;
  Result:=fStream.Write(Buffer,Count);
end;

function TResourceDataStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result:=Seek(Offset,TSeekOrigin(Origin));
end;

function TResourceDataStream.Seek(const Offset: Int64; Origin: TSeekOrigin
  ): Int64;
var newpos : int64;
begin
  case Origin of
    soBeginning : newpos:=Offset;
    soCurrent : newpos:=Position+Offset;
    soEnd : newpos:=Size-Offset;
  end;
  SetPosition(newpos);
  Result:=Position;
end;

end.
