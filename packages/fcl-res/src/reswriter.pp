{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Resource writer for .res files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit reswriter;

{$MODE OBJFPC} {$H+}

interface

uses
  Classes, SysUtils, resource;
  
type

  { TResResourceWriter }

  TResResourceWriter = class (TAbstractResourceWriter)
  private
    fExtensions : string;
    fDescription : string;
    procedure AlignDword(aStream : TStream);
    procedure WriteFileHeader(aStream : TStream);
    procedure WriteResource(aStream : TStream; aRes : TAbstractResource);
    procedure WriteNameId(aStream : TStream; aDesc : TResourceDesc);
    procedure WriteUnicodeString(aStream : TStream; const aName : string);
  protected
    function GetExtensions : string; override;
    function GetDescription : string; override;
    procedure Write(aResources : TResources; aStream : TStream); override;
  public
    constructor Create; override;
  end;

implementation

{ TResResourceWriter }

procedure TResResourceWriter.AlignDword(aStream: TStream);
var topad : integer;
    lw : longword;
begin
  lw:=0;
  topad:=4-(aStream.Position mod 4);
  if topad<>4 then aStream.WriteBuffer(lw,topad);
end;

procedure TResResourceWriter.WriteFileHeader(aStream: TStream);
var lw : longword;
begin
  lw:=0; aStream.WriteBuffer(lw,4);             //datasize = 0
  lw:=$20;
  {$IFDEF ENDIAN_BIG}
  lw:=SwapEndian(lw);
  {$ENDIF}
  aStream.WriteBuffer(lw,4);                    //headersize = $20
  lw:=$FFFF;
  {$IFDEF ENDIAN_BIG}
  lw:=SwapEndian(lw);
  {$ENDIF}
  aStream.WriteBuffer(lw,4);                    //type = $0000FFFF
  aStream.WriteBuffer(lw,4);                    //name = $0000FFFF
  lw:=0;
  aStream.WriteBuffer(lw,4);                    //dataversion = 0
  aStream.WriteBuffer(lw,4);                    //memflags,langid = 0
  aStream.WriteBuffer(lw,4);                    //version = 0
  aStream.WriteBuffer(lw,4);                    //characteristics = 0
end;

procedure TResResourceWriter.WriteResource(aStream: TStream;
  aRes: TAbstractResource);
var lw : longword;
    DataVersion : longword;
    MemoryFlags : word;
    LanguageID : word;
    Version : longword;
    Characteristics : longword;
    posbefore, posafter : int64;
begin
  AlignDword(aStream);
  posbefore:=aStream.Position;
  lw:=aRes.DataSize;
  {$IFDEF ENDIAN_BIG}
  lw:=SwapEndian(lw);
  {$ENDIF}
  aStream.WriteBuffer(lw,4);                  //datasize
  aStream.WriteBuffer(lw,4);                  //headersize - to be filled later
  WriteNameId(aStream,aRes._Type);            //type
  WriteNameId(aStream,aRes.Name);             //name
  AlignDword(aStream);
  
  DataVersion:=aRes.DataVersion;
  MemoryFlags:=aRes.MemoryFlags;
  LanguageID:=aRes.LangID;
  Version:=aRes.Version;
  Characteristics:=aRes.Characteristics;
  {$IFDEF ENDIAN_BIG}
  DataVersion:=SwapEndian(DataVersion);
  MemoryFlags:=SwapEndian(MemoryFlags);
  LanguageID:=SwapEndian(LanguageID);
  Version:=SwapEndian(Version);
  Characteristics:=SwapEndian(Characteristics);
  {$ENDIF}
  aStream.WriteBuffer(DataVersion,4);         //dataversion
  aStream.WriteBuffer(MemoryFlags,2);         //memoryflags
  aStream.WriteBuffer(LanguageID,2);          //language id
  aStream.WriteBuffer(Version,4);             //version
  aStream.WriteBuffer(Characteristics,4);     //characteristics
  posafter:=aStream.Position;
  lw:=posafter-posbefore;
  {$IFDEF ENDIAN_BIG}
  lw:=SwapEndian(lw);
  {$ENDIF}
  aStream.Position:=posbefore+4;
  aStream.WriteBuffer(lw,4);                  //fix header size
  aStream.Position:=posafter;

  //copy resource data
  aRes.RawData.Position:=0;
  aStream.CopyFrom(aRes.RawData,aRes.DataSize);
end;

procedure TResResourceWriter.WriteNameId(aStream: TStream; aDesc: TResourceDesc
  );
var w1,w2 : word;
begin
  case aDesc.DescType of
    dtID :
      begin
        w1:=$FFFF;
        w2:=aDesc.ID;
        {$IFDEF ENDIAN_BIG}
        w2:=SwapEndian(w2);
        {$ENDIF}
        aStream.WriteBuffer(w1,2);
        aStream.WriteBuffer(w2,2);
      end;
    dtName : WriteUnicodeString(aStream,aDesc.Name);
  end;
end;

procedure TResResourceWriter.WriteUnicodeString(aStream: TStream;
  const aName: string);
var ws : widestring;
    w : word;
    i : integer;
begin
  ws:=aName;
  for i:=1 to length(ws) do
  begin
    w:=word(ws[i]);
    {$IFDEF ENDIAN_BIG}
    w:=SwapEndian(w);
    {$ENDIF}
    aStream.WriteBuffer(w,2);
  end;
  w:=0;
  aStream.WriteBuffer(w,2);
end;

function TResResourceWriter.GetExtensions: string;
begin
  Result:=fExtensions;
end;

function TResResourceWriter.GetDescription: string;
begin
  Result:=fDescription;
end;

procedure TResResourceWriter.Write(aResources: TResources; aStream: TStream);
var i : integer;
begin
  WriteFileHeader(aStream);
  for i:=0 to aResources.Count-1 do
    WriteResource(aStream,aResources[i]);
end;

constructor TResResourceWriter.Create;
begin
  fExtensions:='.res';
  fDescription:='.res resource writer';
end;

initialization
  TResources.RegisterWriter('.res',TResResourceWriter);

end.
