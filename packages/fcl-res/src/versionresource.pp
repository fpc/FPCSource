{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Version information resource type

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit versionresource;

{$MODE OBJFPC}

interface

uses
  SysUtils, Classes, resource, versiontypes;

type
  TVerBlockHeader = packed record
    length    : word;
    vallength : word;
    valtype   : word;
    key       : string;
  end;

type

  { TVersionResource }

  TVersionResource = class(TAbstractResource)
  private
    fType : TResourceDesc;
    fName : TResourceDesc;
    fFixedInfo : TVersionFixedInfo;
    fStringFileInfo : TVersionStringFileInfo;
    fVarFileInfo : TVersionVarFileInfo;
    function SwapVersion(const aData : qword) : qword;
    procedure AlignDWordReading;
    procedure AlignDWordWriting;
    function GetFileInfo : TVersionStringFileInfo;
    function GetFixedInfo : TVersionFixedInfo;
    function GetVarInfo : TVersionVarFileInfo;
    procedure CheckDataLoaded;
    procedure LoadData;
    procedure LoadFixedInfos;
    function ReadBlockHeader(out aBlock : TVerBlockHeader) : integer;
    function ReadStringFileInfo(toread : integer) : integer;
    function ReadVarFileInfo(toread : integer) : integer;
    function ReadStringTable(toread : integer;aName : string) : integer;
    function ReadWideString: string;
    procedure WriteFixedBlockLength(const position : int64);
    procedure WriteData;
    procedure WriteFixedInfos;
    procedure WriteStringFileInfo;
    procedure WriteStringTable(aTable : TVersionStringTable);
    procedure WriteStringEntry(const aKey, aValue : string);
    procedure WriteVarFileInfo;
    procedure WriteVarEntry(aEntry : TVerTranslationInfo);
    procedure WriteWideString(const aString : string);
  protected
    function GetType : TResourceDesc; override;
    function GetName : TResourceDesc; override;
    function ChangeDescTypeAllowed(aDesc : TResourceDesc) : boolean; override;
    function ChangeDescValueAllowed(aDesc : TResourceDesc) : boolean; override;
    procedure NotifyResourcesLoaded; override;
  public
    constructor Create; override;
    constructor Create(aType,aName : TResourceDesc); override;
    destructor Destroy; override;
    procedure UpdateRawData; override;
    property FixedInfo : TVersionFixedInfo read GetFixedInfo;
    property StringFileInfo : TVersionStringFileInfo read GetFileInfo;
    property VarFileInfo : TVersionVarFileInfo read GetVarInfo;
  end;


implementation

uses
  resfactory;
  
type
  TVSFixedFileInfo = packed record
    signature : longword;
    verMinor  : word;
    verMajor  : word;
    FileVersion : qword;
    ProductVersion : qword;
    FileFlagsMask : longword;
    FileFlags     : longword;
    FileOS        : longword;
    FileType      : longword;
    FileSubType   : longword;
    FileDate      : qword;
  end;


{ TVersionResource }

function TVersionResource.SwapVersion(const aData: qword): qword;
begin
  {$IFDEF ENDIAN_BIG}
  Result:=Swap(aData);                      //MSLW is stored first...
  Result:=SwapEndian(Result);
  {$ELSE}
  Result:=Swap(longword(hi(aData)));
  Result:=Result shl 32;
  Result:=Result or Swap(longword(lo(aData)));
  {$ENDIF}
end;

procedure TVersionResource.AlignDWordReading;
var toskip : integer;
begin
  toskip:=4-(RawData.Position mod 4);
  if toskip<>4 then RawData.Seek(toskip,soFromCurrent);
end;

procedure TVersionResource.AlignDWordWriting;
var topad : integer;
    lw : longword;
begin
  lw:=0;
  topad:=4-(RawData.Position mod 4);
  if topad<>4 then RawData.WriteBuffer(lw,topad);
end;

function TVersionResource.GetFileInfo: TVersionStringFileInfo;
begin
  CheckDataLoaded;
  Result:=fStringFileInfo;
end;

function TVersionResource.GetFixedInfo: TVersionFixedInfo;
begin
  CheckDataLoaded;
  Result:=fFixedInfo;
end;

function TVersionResource.GetVarInfo: TVersionVarFileInfo;
begin
  CheckDataLoaded;
  Result:=fVarFileInfo;
end;

procedure TVersionResource.CheckDataLoaded;
begin
  if fFixedInfo<>nil then exit;
  //if we have no data stream, create empty structures
  if RawData.Size=0 then
  begin
    fFixedInfo:=TVersionFixedInfo.Create;
    fStringFileInfo:=TVersionStringFileInfo.Create;
    fVarFileInfo:=TVersionVarFileInfo.Create;
  end
  else LoadData;
end;

procedure TVersionResource.LoadData;
var tmp : integer;
    toread : word;
    block : TVerBlockHeader;
    i : integer;
begin
  RawData.Position:=0;
  tmp:=ReadBlockHeader(block);
                                        //block.key should be 'VS_VERSION_INFO'
  toread:=block.length;
  LoadFixedInfos;
  AlignDWordReading;
  dec(toread,RawData.Position);
  
  fStringFileInfo:=TVersionStringFileInfo.Create;
  fVarFileInfo:=TVersionVarFileInfo.Create;

  for i:=1 to 2 do
  begin
    if toread<=0 then exit;
    tmp:=ReadBlockHeader(block);
    dec(toread,tmp);
    if block.key='StringFileInfo' then tmp:=ReadStringFileInfo(block.length-tmp)
    else if block.key='VarFileInfo' then tmp:=ReadVarFileInfo(block.length-tmp);
    dec(toread,tmp);
  end;

end;

procedure TVersionResource.LoadFixedInfos;
var infodata : TVSFixedFileInfo;
begin
  RawData.ReadBuffer(infodata,sizeof(infodata));
  infodata.FileVersion:=SwapVersion(infodata.FileVersion);
  infodata.ProductVersion:=SwapVersion(infodata.ProductVersion);
  infodata.FileDate:=Swap(infodata.FileDate);             //MSLW is stored first...
  {$IFDEF ENDIAN_BIG}
  infodata.signature:=SwapEndian(infodata.signature);
  infodata.verMinor:=SwapEndian(infodata.verMinor);
  infodata.verMajor:=SwapEndian(infodata.verMajor);
  infodata.FileFlagsMask:=SwapEndian(infodata.FileFlagsMask);
  infodata.FileFlags:=SwapEndian(infodata.FileFlags);
  infodata.FileOS:=SwapEndian(infodata.FileOS);
  infodata.FileType:=SwapEndian(infodata.FileType);
  infodata.FileSubType:=SwapEndian(infodata.FileSubType);
  infodata.FileDate:=SwapEndian(infodata.FileDate);
  {$ENDIF}
  fFixedInfo:=TVersionFixedInfo.Create;
  fFixedInfo.FileVersion:=TFileProductVersion(infodata.FileVersion);
  fFixedInfo.ProductVersion:=TFileProductVersion(infodata.ProductVersion);
  fFixedInfo.FileFlagsMask:=infodata.FileFlagsMask;
  fFixedInfo.FileFlags:=infodata.FileFlags;
  fFixedInfo.FileOS:=infodata.FileOS;
  fFixedInfo.FileType:=infodata.FileType;
  fFixedInfo.FileSubType:=infodata.FileSubType;
  fFixedInfo.FileDate:=infodata.FileDate;
end;

function TVersionResource.ReadBlockHeader(out aBlock: TVerBlockHeader
  ): integer;
var before : int64;
begin
  before:=RawData.Position;
  RawData.ReadBuffer(aBlock,6);
  {$IFDEF ENDIAN_BIG}
  aBlock.length:=SwapEndian(aBlock.length);
  aBlock.vallength:=SwapEndian(aBlock.vallength);
  aBlock.valtype:=SwapEndian(aBlock.valtype);
  {$ENDIF}
  aBlock.key:=ReadWideString;
  AlignDWordReading;
  Result:=RawData.Position-before;
end;

function TVersionResource.ReadStringFileInfo(toread : integer) : integer;
var block : TVerBlockHeader;
    tmp : integer;
begin
  Result:=0;
  while toread>0 do
  begin
    tmp:=ReadBlockHeader(block);
    dec(toread,tmp); inc(Result,tmp);
    tmp:=ReadStringTable(block.length-tmp,block.key);
    dec(toread,tmp); inc(Result,tmp);
  end;
end;

function TVersionResource.ReadVarFileInfo(toread : integer) : integer;
var block : TVerBlockHeader;
    tmp : integer;
    vinfo : TVerTranslationInfo;
    before : int64;
begin
  Result:=0;
  while toread>0 do
  begin
    before:=RawData.Position;
    ReadBlockHeader(block);
    if (block.valtype<>0) or (block.key<>'Translation') then
      RawData.Seek(block.vallength,soFromCurrent)
    else
    begin
      RawData.ReadBuffer(vinfo,sizeof(vinfo));
      {$IFDEF ENDIAN_BIG}
      vinfo.language:=SwapEndian(vinfo.language);
      vinfo.codepage:=SwapEndian(vinfo.codepage);
      {$ENDIF}
      fVarFileInfo.Add(vinfo);
    end;
    AlignDWordReading;
    tmp:=RawData.Position-before;
    dec(toread,tmp); inc(Result,tmp);
  end;
end;

function TVersionResource.ReadStringTable(toread: integer; aName: string
  ): integer;
var strtable : TVersionStringTable;
    tmp : integer;
    block : TVerBlockHeader;
    value : string;
    before : int64;
begin
  Result:=0;
  strtable:=TVersionStringTable.Create(aName);
  fStringFileInfo.Add(strtable);
  while toread>0 do
  begin
    before:=RawData.Position;
    ReadBlockHeader(block);
    value:=ReadWideString;
    AlignDWordReading;
    tmp:=RawData.Position-before;
    dec(toread,tmp); inc(Result,tmp);
    strtable.Add(block.key,value);
  end;
end;

function TVersionResource.ReadWideString: string;
var w : word;
    ws : widestring;
begin
  ws:='';
  w:=0;
  repeat
    RawData.ReadBuffer(w,2);
    if w = 0 then break;
    {$IFDEF ENDIAN_BIG}
    w:=SwapEndian(w);
    {$ENDIF}
    ws:=ws+widechar(w);
  until false;
  Result:=ws;
end;

procedure TVersionResource.WriteFixedBlockLength(const position: int64);
var after : int64;
    len : word;
begin
  after:=RawData.Position;
  len:=after-position;
  {$IFDEF ENDIAN_BIG}
  len:=SwapEndian(len);
  {$ENDIF}
  RawData.Position:=position;
  RawData.WriteBuffer(len,2);
  RawData.Position:=after;
end;

procedure TVersionResource.WriteData;
var block : TVerBlockHeader;
begin
  RawData.Size:=0;
  RawData.Position:=0;

  block.length:=0;
  block.vallength:=$34;
  block.valtype:=0;
  block.key:='VS_VERSION_INFO';
  {$IFDEF ENDIAN_BIG}
  block.vallength:=SwapEndian(block.vallength);
  block.valtype:=SwapEndian(block.valtype);
  {$ENDIF}

  RawData.WriteBuffer(block,6);
  WriteWideString(block.key);
  AlignDWordWriting;

  WriteFixedInfos;
  AlignDWordWriting;

  if fStringFileInfo.Count>0 then WriteStringFileInfo;
  if fVarFileInfo.Count>0 then WriteVarFileInfo;

  WriteFixedBlockLength(0);
end;

procedure TVersionResource.WriteFixedInfos;
var infodata : TVSFixedFileInfo;
begin
  infodata.signature:=$FEEF04BD;
  infodata.verMinor:=0;
  infodata.verMajor:=1;
  infodata.FileVersion:=qword(fFixedInfo.FileVersion);
  infodata.ProductVersion:=qword(fFixedInfo.ProductVersion);
  infodata.FileFlagsMask:=fFixedInfo.FileFlagsMask;
  infodata.FileFlags:=fFixedInfo.FileFlags;
  infodata.FileOS:=fFixedInfo.FileOS;
  infodata.FileType:=fFixedInfo.FileType;
  infodata.FileSubType:=fFixedInfo.FileSubType;
  infodata.FileDate:=fFixedInfo.FileDate;
  {$IFDEF ENDIAN_BIG}
  infodata.signature:=SwapEndian(infodata.signature);
  infodata.verMinor:=SwapEndian(infodata.verMinor);
  infodata.verMajor:=SwapEndian(infodata.verMajor);
  infodata.FileFlagsMask:=SwapEndian(infodata.FileFlagsMask);
  infodata.FileFlags:=SwapEndian(infodata.FileFlags);
  infodata.FileOS:=SwapEndian(infodata.FileOS);
  infodata.FileType:=SwapEndian(infodata.FileType);
  infodata.FileSubType:=SwapEndian(infodata.FileSubType);
  infodata.FileDate:=SwapEndian(infodata.FileDate);
  {$ENDIF}
  infodata.FileVersion:=SwapVersion(infodata.FileVersion);
  infodata.ProductVersion:=SwapVersion(infodata.ProductVersion);
  infodata.FileDate:=Swap(infodata.FileDate);             //MSLW is stored first...
  RawData.WriteBuffer(infodata,sizeof(infodata));
end;

procedure TVersionResource.WriteStringFileInfo;
var block : TVerBlockHeader;
    i : integer;
    before : int64;
begin
  before:=RawData.Position;
  block.length:=0;
  block.vallength:=0;
  block.valtype:=1;
  block.key:='StringFileInfo';
  {$IFDEF ENDIAN_BIG}
  block.vallength:=SwapEndian(block.vallength);
  block.valtype:=SwapEndian(block.valtype);
  {$ENDIF}
  RawData.WriteBuffer(block,6);
  WriteWideString(block.key);
  AlignDWordWriting;
  
  for i:=0 to fStringFileInfo.Count-1 do
    WriteStringTable(fStringFileInfo[i]);
  
  WriteFixedBlockLength(before);
end;

procedure TVersionResource.WriteStringTable(aTable: TVersionStringTable);
var block : TVerBlockHeader;
    i : integer;
    before : int64;
begin
  before:=RawData.Position;
  block.length:=0;
  block.vallength:=0;
  block.valtype:=1;
  block.key:=aTable.Name;
  {$IFDEF ENDIAN_BIG}
  block.vallength:=SwapEndian(block.vallength);
  block.valtype:=SwapEndian(block.valtype);
  {$ENDIF}
  RawData.WriteBuffer(block,6);
  WriteWideString(block.key);
  AlignDWordWriting;

  for i:=0 to aTable.Count-1 do
    WriteStringEntry(aTable.Keys[i],aTable.ValuesByIndex[i]);

  WriteFixedBlockLength(before);
end;

procedure TVersionResource.WriteStringEntry(const aKey, aValue: string);
var block : TVerBlockHeader;
    before: int64;
begin
  before:=RawData.Position;
  block.length:=0;
  block.vallength:=length(aValue)+1;
  block.valtype:=1;
  block.key:=aKey;
  {$IFDEF ENDIAN_BIG}
  block.vallength:=SwapEndian(block.vallength);
  block.valtype:=SwapEndian(block.valtype);
  {$ENDIF}
  RawData.WriteBuffer(block,6);
  WriteWideString(block.key);
  AlignDWordWriting;
  WriteWideString(aValue);
  AlignDWordWriting;

  WriteFixedBlockLength(before);
end;

procedure TVersionResource.WriteVarFileInfo;
var block : TVerBlockHeader;
    i : integer;
    before : int64;
begin
  before:=RawData.Position;
  block.length:=0;
  block.vallength:=0;
  block.valtype:=1;
  block.key:='VarFileInfo';
  {$IFDEF ENDIAN_BIG}
  block.vallength:=SwapEndian(block.vallength);
  block.valtype:=SwapEndian(block.valtype);
  {$ENDIF}
  RawData.WriteBuffer(block,6);
  WriteWideString(block.key);
  AlignDWordWriting;

  for i:=0 to fVarFileInfo.Count-1 do
    WriteVarEntry(fVarFileInfo[i]);

  WriteFixedBlockLength(before);
end;

procedure TVersionResource.WriteVarEntry(aEntry: TVerTranslationInfo);
var block : TVerBlockHeader;
    before: int64;
begin
  before:=RawData.Position;
  block.length:=0;
  block.vallength:=4;
  block.valtype:=0;
  block.key:='Translation';
  {$IFDEF ENDIAN_BIG}
  block.vallength:=SwapEndian(block.vallength);
  block.valtype:=SwapEndian(block.valtype);
  aEntry.language:=SwapEndian(aEntry.language);
  aEntry.codepage:=SwapEndian(aEntry.codepage);
  {$ENDIF}
  RawData.WriteBuffer(block,6);
  WriteWideString(block.key);
  AlignDWordWriting;
  RawData.WriteBuffer(aEntry,sizeof(aEntry));
  WriteFixedBlockLength(before);
end;

procedure TVersionResource.WriteWideString(const aString: string);
var ws : widestring;
    w : word;
    i : integer;
begin
  ws:=aString;
  for i:=1 to length(ws) do
  begin
    w:=word(ws[i]);
    {$IFDEF ENDIAN_BIG}
    w:=SwapEndian(w);
    {$ENDIF}
    RawData.WriteBuffer(w,2);
  end;
  w:=0;
  RawData.WriteBuffer(w,2);
end;

function TVersionResource.GetType: TResourceDesc;
begin
  Result:=fType;
end;

function TVersionResource.GetName: TResourceDesc;
begin
  Result:=fName;
end;

function TVersionResource.ChangeDescTypeAllowed(aDesc: TResourceDesc): boolean;
begin
  Result:=false;
end;

function TVersionResource.ChangeDescValueAllowed(aDesc: TResourceDesc
  ): boolean;
begin
  Result:=false;
end;

procedure TVersionResource.NotifyResourcesLoaded;
begin
end;

constructor TVersionResource.Create;
begin
  inherited Create;
  fType:=TResourceDesc.Create(RT_VERSION);
  fName:=TResourceDesc.Create(1);
  SetDescOwner(fType);
  SetDescOwner(fName);
  fFixedInfo:=nil;
  fStringFileInfo:=nil;
  fVarFileInfo:=nil;
end;

constructor TVersionResource.Create(aType, aName: TResourceDesc);
begin
  Create;
end;

destructor TVersionResource.Destroy;
begin
  fType.Free;
  fName.Free;
  if fFixedInfo<>nil then fFixedInfo.Free;
  if fStringFileInfo<>nil then fStringFileInfo.Free;
  if fVarFileInfo<>nil then fVarFileInfo.Free;
  inherited Destroy;
end;

procedure TVersionResource.UpdateRawData;
begin
  if fFixedInfo=nil then exit;
  WriteData;
  FreeAndNil(fFixedInfo);
  FreeAndNil(fStringFileInfo);
  FreeAndNil(fVarFileInfo);
end;

initialization
  TResourceFactory.RegisterResourceClass(RT_VERSION,TVersionResource);

end.
