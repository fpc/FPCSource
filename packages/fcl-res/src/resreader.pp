{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Resource reader for .res files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit resreader;

{$MODE OBJFPC} {$H+}

interface

uses
  Classes, SysUtils, resource;

type

  { TResResourceReader }

  TResResourceReader = class (TAbstractResourceReader)
  private
    fExtensions : string;
    fDescription : string;
    dummyType : TResourceDesc;
    dummyName : TResourceDesc;
    procedure AlignDword(aStream : TStream);
    function ReadResourceHeader(aStream : TStream) : TAbstractResource;
    function ReadUnicodeString(aStream : TStream; maxsize : integer; out count : integer) : widestring;
    function ReadNameID(aStream : TStream; aDesc : TResourceDesc ;maxsize : integer) : integer;
  protected
    function GetExtensions : string; override;
    function GetDescription : string; override;
    procedure Load(aResources : TResources; aStream : TStream); override;
    function CheckMagic(aStream : TStream) : boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;


implementation

uses resdatastream, resfactory;

//This exception is raised when end of stream is detected and expected,
//that is, before reading a new resource header. It's not an error.
type
  EResourceReaderEOSReachedException = class (EResourceReaderException);


{ TResResourceReader }

procedure TResResourceReader.AlignDword(aStream: TStream);
var toskip : integer;
begin
  toskip:=4-(aStream.Position mod 4);
  if toskip<>4 then aStream.Seek(toskip,soFromCurrent);
end;

function TResResourceReader.ReadResourceHeader(aStream: TStream
  ): TAbstractResource;
var DataSize : longword;
    HeaderSize : longword;
    toread : longword;
    DataVersion : longword;
    MemoryFlags : word;
    LanguageID : word;
    Version : longword;
    Characteristics : longword;
    DataOffset : longword;
    RawData : TResourceDataStream;
begin
  try
    aStream.ReadBuffer(DataSize,4);
  except
    on e : EReadError do
      raise EResourceReaderEOSReachedException.Create('');
  end;
  try
    aStream.ReadBuffer(HeaderSize,4);
    {$IFDEF ENDIAN_BIG}
    DataSize:=SwapEndian(DataSize);
    HeaderSize:=SwapEndian(HeaderSize);
    {$ENDIF}
    toread:=headersize-8;
    toread:=toread-ReadNameID(aStream,dummyType,toread-18);
    toread:=toread-ReadNameID(aStream,dummyName,toread-16);
    AlignDword(aStream);
    aStream.ReadBuffer(DataVersion,4);
    aStream.ReadBuffer(MemoryFlags,2);
    aStream.ReadBuffer(LanguageID,2);
    aStream.ReadBuffer(Version,4);
    aStream.ReadBuffer(Characteristics,4);
    {$IFDEF ENDIAN_BIG}
    DataVersion:=SwapEndian(DataVersion);
    MemoryFlags:=SwapEndian(MemoryFlags);
    LanguageID:=SwapEndian(LanguageID);
    Version:=SwapEndian(Version);
    Characteristics:=SwapEndian(Characteristics);
    {$ENDIF}
    DataOffset:=aStream.Position;

    Result:=TResourceFactory.CreateResource(dummyType,dummyName);
    SetDataSize(Result,DataSize);
    SetHeaderSize(Result,HeaderSize);
    Result.DataVersion:=DataVersion;
    Result.MemoryFlags:=MemoryFlags;
    Result.LangID:=LanguageID;
    Result.Version:=Version;
    Result.Characteristics:=Characteristics;
    SetDataOffset(Result,DataOffset);
    RawData:=TResourceDataStream.Create(aStream,Result,Result.DataSize,TCachedResourceDataStream);
    SetRawData(Result,RawData);
    if DataSize>0 then
    begin
      aStream.Seek(DataSize,soFromCurrent);
      AlignDword(aStream);
    end;
  except
    on e : EReadError do
      raise EResourceReaderUnexpectedEndOfStreamException.Create('');
  end;
end;

function TResResourceReader.ReadUnicodeString(aStream: TStream; maxsize: integer;
   out count : integer): widestring;
var w : word;
begin
  Result:='';
  w:=0;
  count:=0;
  repeat
    aStream.ReadBuffer(w,2);
    if w<>0 then
    begin
      {$IFDEF ENDIAN_BIG}
      w:=SwapEndian(w);
      {$ENDIF}
      Result:=Result+widechar(w);
    end;
    dec(maxsize);
    inc(count);
  until (w=0) or (maxsize<=0);
  count:=count*2;
end;

function TResResourceReader.ReadNameID(aStream : TStream; aDesc : TResourceDesc;
  maxsize : integer) : integer;
var tmpw : word;
    ws : widestring;
begin
  aStream.ReadBuffer(tmpw,2);
  {$IFDEF ENDIAN_BIG}
  tmpw:=SwapEndian(tmpw);
  {$ENDIF}
  if tmpw = $FFFF then
  begin
    aStream.ReadBuffer(tmpw,2);
    {$IFDEF ENDIAN_BIG}
    tmpw:=SwapEndian(tmpw);
    {$ENDIF}
    aDesc.ID:=tmpw;
    Result:=4;
  end
  else
  begin
    ws:=widechar(tmpw)+ReadUnicodeString(aStream,maxsize,Result);
    aDesc.Name:=ws;
    inc(Result,2);
  end;
end;

function TResResourceReader.GetExtensions: string;
begin
  Result:=fExtensions;
end;

function TResResourceReader.GetDescription: string;
begin
  Result:=fDescription;
end;

procedure TResResourceReader.Load(aResources: TResources; aStream: TStream);
var aRes : TAbstractResource;
begin
  if not CheckMagic(aStream) then
    raise EResourceReaderWrongFormatException.Create('');
  try
    while true do
    begin
      aRes:=ReadResourceHeader(aStream);
      try
        aResources.Add(aRes);
      except
        on e : EResourceDuplicateException do
        begin
          aRes.Free;
          raise;
        end;
      end;
    end;
  except
    on e : EResourceReaderEOSReachedException do ;
  end;
end;

function TResResourceReader.CheckMagic(aStream: TStream): boolean;
var lw : longword;
begin
  Result:=false;
  aStream.ReadBuffer(lw,4); if lw<>0 then exit; //datasize = 0
  aStream.ReadBuffer(lw,4);
  {$IFDEF ENDIAN_BIG}
  lw:=SwapEndian(lw);
  {$ENDIF}
  if lw<>$20 then exit;                         //headersize = $20
  aStream.ReadBuffer(lw,4);
  {$IFDEF ENDIAN_BIG}
  lw:=SwapEndian(lw);
  {$ENDIF}
  if lw<>$FFFF then exit;                       //type = $0000FFFF
  aStream.ReadBuffer(lw,4);
  {$IFDEF ENDIAN_BIG}
  lw:=SwapEndian(lw);
  {$ENDIF}
  if lw<>$FFFF then exit;                       //name = $0000FFFF
  aStream.ReadBuffer(lw,4); if lw<>0 then exit; //dataversion = 0
  aStream.ReadBuffer(lw,4); if lw<>0 then exit; //memflags,langid = 0
  aStream.ReadBuffer(lw,4); if lw<>0 then exit; //version = 0
  aStream.ReadBuffer(lw,4); if lw<>0 then exit; //characteristics = 0
  Result:=true;
end;

constructor TResResourceReader.Create;
begin
  fExtensions:='.res';
  fDescription:='.res resource reader';
  dummyType:=TResourceDesc.Create;
  dummyName:=TResourceDesc.Create;
end;

destructor TResResourceReader.Destroy;
begin
  dummyType.Free;
  dummyName.Free;
end;

initialization
  TResources.RegisterReader('.res',TResResourceReader);

end.
