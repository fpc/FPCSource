{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Bitmap resource type

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit bitmapresource;

{$MODE OBJFPC}

interface

uses
  Classes, SysUtils, resource;

type

  { TBitmapResource }

  TBitmapResource = class(TAbstractResource)
  private
    fType : TResourceDesc;
    fName : TResourceDesc;
    fBitmapData : TStream;
    function GetBitmapData : TStream;
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
    procedure SetCustomBitmapDataStream(aStream : TStream);
    property BitmapData : TStream read GetBitmapData;
  end;


implementation

uses
  resfactory, resdatastream;

type
  TBitmapFileHeader = packed record
    Magic : word;
    FileSize : longword;
    Reserved : longword;
    DataOffset : longword;
  end;
  
const
  hdrsize = sizeof(TBitmapFileHeader);

type

  { TBitmapCachedDataStream }

  TBitmapCachedDataStream = class(TCachedDataStream)
  private
    fHeader : TMemoryStream;
    procedure InitHeader(aStream : TStream);
    function CalcDataOffset(aStream : TStream) : longword;
  protected
    function ReadFromSubStream(aStream : TStream; var Buffer; aPosition : int64; aCount : longint) : longint;
  public
    constructor Create(aStream : TStream;  aResource : TAbstractResource; aSize : int64); override;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
  end;

procedure TBitmapCachedDataStream.InitHeader(aStream: TStream);
var bmphdr : TBitmapFileHeader;
begin
  bmphdr.Magic:=$4d42;
  bmphdr.FileSize:=aStream.Size+hdrsize;
  bmphdr.Reserved:=0;
  bmphdr.DataOffset:=CalcDataOffset(aStream);
  {$IFDEF ENDIAN_BIG}
  bmphdr.Magic:=SwapEndian(bmphdr.Magic);
  bmphdr.FileSize:=SwapEndian(bmphdr.FileSize);
  bmphdr.Reserved:=SwapEndian(bmphdr.Reserved);
  bmphdr.DataOffset:=SwapEndian(bmphdr.DataOffset);
  {$ENDIF}
  fHeader.Position:=0;
  fHeader.WriteBuffer(bmphdr,hdrsize);
end;

function TBitmapCachedDataStream.CalcDataOffset(aStream: TStream): longword;
const
  BI_BITFIELDS = 3;
var oldpos : int64;
    compression, clrused : longword;
    bitcount,palentries : word;
    infohdrsize : longword;
begin
//  Data offset: bmp file header + bmp info header + [mask] + [palette]
  Result:=0;
  if aStream.Size<$28 then exit;
  oldpos:=aStream.Position;
  aStream.Position:=0;
  aStream.ReadBuffer(infohdrsize,4);
  aStream.Position:=14;
  aStream.ReadBuffer(bitcount,2);
  aStream.ReadBuffer(compression,4);
  aStream.Seek(12,soFromCurrent);
  aStream.ReadBuffer(clrused,4);
  {$IFDEF ENDIAN_BIG}
  infohdrsize:=SwapEndian(infohdrsize);
  bitcount:=SwapEndian(bitcount);
  compression:=SwapEndian(compression);
  clrused:=SwapEndian(clrused);
  {$ENDIF}
  aStream.Position:=oldpos;
  palentries:=0;
  if ((compression=BI_BITFIELDS) and (bitcount in [16,32])) then
    inc(Result,12)  //arbitrary mask
  else if clrused>0 then palentries:=clrused
  else if bitcount<=8 then palentries:=1 shl bitcount;
  inc(Result,palentries*4);
  inc(Result,hdrsize+infohdrsize);
end;

function TBitmapCachedDataStream.ReadFromSubStream(aStream: TStream;
  var Buffer; aPosition: int64; aCount: longint): longint;
var oldpos : int64;
begin
  Result:=aStream.Size-aPosition;
  if aCount<Result then Result:=aCount;
  if Result<0 then Result:=0;
  oldpos:=aStream.Position;
  aStream.Position:=aPosition;
  Result:=aStream.Read(Buffer,Result);
  aStream.Position:=oldpos;
end;

constructor TBitmapCachedDataStream.Create(aStream: TStream;  aResource : TAbstractResource; aSize: int64);
begin
  inherited Create(aStream,aResource,aSize);
  fHeader:=TMemoryStream.Create;
  inc(fSize,hdrsize);
  InitHeader(aStream);
end;

destructor TBitmapCachedDataStream.Destroy;
begin
  fHeader.Free;
end;

function TBitmapCachedDataStream.Read(var Buffer; Count: Longint): Longint;
var toread,read_in : longint;
    b : pbyte;
begin
  Result:=0;
  toread:=fSize-Position;
  if Count<toread then toread:=Count;
  if toread<0 then toread:=0;
  b:=@buffer;

  read_in:=ReadFromSubStream(fHeader,b^,fPosition,toread);
  inc(fPosition,read_in);
  inc(b,read_in);
  inc(Result,read_in);
  dec(toread,read_in);

  read_in:=ReadFromSubStream(fStream,b^,fPosition-hdrsize,toread);
  inc(fPosition,read_in);
  inc(Result,read_in);
end;


{ TBitmapResource }

function TBitmapResource.GetBitmapData: TStream;
begin
  if fBitmapData = nil then
    fBitmapData:=TResourceDataStream.Create(RawData,self,DataSize,TBitmapCachedDataStream);
  Result:=fBitmapData;
end;

procedure TBitmapResource.UpdateRawData;
begin
  if TResourceDataStream(BitmapData).Cached then exit; //no need to update rawdata
  //rawdata = bitmapdata without bitmap file header
  BitmapData.Position:=hdrsize;
  RawData.Size:=0;
  RawData.Position:=0;
  RawData.CopyFrom(BitmapData,BitmapData.Size-hdrsize);
  FreeAndNil(fBitmapData);
end;

function TBitmapResource.GetType: TResourceDesc;
begin
  Result:=fType;
end;

function TBitmapResource.GetName: TResourceDesc;
begin
  Result:=fName;
end;

function TBitmapResource.ChangeDescTypeAllowed(aDesc: TResourceDesc): boolean;
begin
  Result:=aDesc=fName;
end;

function TBitmapResource.ChangeDescValueAllowed(aDesc: TResourceDesc): boolean;
begin
  Result:=aDesc=fName;
end;

procedure TBitmapResource.NotifyResourcesLoaded;
begin
end;

constructor TBitmapResource.Create;
begin
  inherited Create;
  fType:=TResourceDesc.Create(RT_BITMAP);
  fName:=TResourceDesc.Create(1);
  SetDescOwner(fType);
  SetDescOwner(fName);
  fBitmapData:=nil;
end;

constructor TBitmapResource.Create(aType, aName: TResourceDesc);
begin
  Create;
  fName.Assign(aName);
end;

destructor TBitmapResource.Destroy;
begin
  if fBitmapData<>nil then fBitmapData.Free;
  fType.Free;
  fName.Free;
  inherited Destroy;
end;

procedure TBitmapResource.SetCustomBitmapDataStream(aStream: TStream);
begin
  TResourceDataStream(BitmapData).SetCustomStream(aStream);
end;

initialization
  TResourceFactory.RegisterResourceClass(RT_BITMAP,TBitmapResource);

end.

