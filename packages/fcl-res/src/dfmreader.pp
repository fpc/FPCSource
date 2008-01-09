{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Resource reader for DFM files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit dfmreader;

{$MODE OBJFPC} {$H+}

interface

uses
  Classes, SysUtils, resource;

type

  { TDfmResourceReader }

  TDfmResourceReader = class (TAbstractResourceReader)
  private
    fExtensions : string;
    fDescription : string;
    fLine : string;
    fLinePos : integer;
    fObjectName : string;
    dummyType : TResourceDesc;
    dummyName : TResourceDesc;
    fIsBinary : boolean;
    function IsAlpha : boolean;
    function IsNum : boolean;
    function IsAlphaNum : boolean;
    function IsSpace : boolean;
    procedure SkipSpaces;
    function GetIdent : string;
    procedure ReadLine(aStream : TStream);
    
    function CheckTextDfm(aStream : TStream) : boolean;
    function CheckBinDfm(aStream : TStream) : boolean;
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

uses
  resdatastream, resfactory;

type
  TSignature = array[0..3] of char;

const
  FilerSignature = 'TPF0';

{ TDfmResourceReader }

function TDfmResourceReader.IsAlpha: boolean;
begin
  Result:=pchar(fLine)[fLinePos] in ['_','A'..'Z','a'..'z'];
end;

function TDfmResourceReader.IsNum: boolean;
begin
  Result:=pchar(fLine)[fLinePos] in ['0'..'9'];
end;

function TDfmResourceReader.IsAlphaNum: boolean;
begin
  Result:=IsAlpha or IsNum;
end;

function TDfmResourceReader.IsSpace: boolean;
const TAB = #9;
begin
  Result:=pchar(fLine)[fLinePos] in [' ',TAB];
end;

procedure TDfmResourceReader.SkipSpaces;
begin
  while IsSpace do inc(fLinePos);
end;

function TDfmResourceReader.GetIdent: string;
begin
  Result:='';
  SkipSpaces;
  if not IsAlpha then exit;
  while IsAlphaNum do
  begin
    Result:=Result+pchar(fLine)[fLinePos];
    inc(fLinePos);
  end;
end;

procedure TDfmResourceReader.ReadLine(aStream : TStream);
const CR = #13;
      LF = #10;
var c : char;
begin
  fLine:='';
  
  repeat
    aStream.ReadBuffer(c,1);
    if not (c in [CR,LF,#0]) then
      fLine:=fLine+c;
  until c in [CR,LF,#0];
  fLinePos:=0;
end;

(*should be:  object Name: Type  or inherited Name: Type*)
function TDfmResourceReader.CheckTextDfm(aStream: TStream): boolean;
var tmp : string;
begin
  Result:=false;
  fLine:='';
  while fLine='' do
    ReadLine(aStream);
  tmp:=lowercase(GetIdent);
  if (tmp <> 'object') and (tmp<>'inherited') then exit;
  if GetIdent='' then exit;
  SkipSpaces;
  if pchar(fLine)[fLinePos]<>':' then exit;
  inc(fLinePos);
  SkipSpaces;
  fObjectName:=UpperCase(GetIdent);
  if fObjectName='' then exit;
  Result:=true;
  fIsBinary:=false;
end;

function TDfmResourceReader.CheckBinDfm(aStream: TStream): boolean;
var s : shortstring;
    b : byte;
begin
  aStream.ReadBuffer(b,1);
  s[0]:=Chr(b);
  aStream.ReadBuffer(s[1],b);
  fObjectName:=UpperCase(s);
  Result:=fObjectName<>'';
  fIsBinary:=true;
end;

function TDfmResourceReader.GetExtensions: string;
begin
  Result:=fExtensions;
end;

function TDfmResourceReader.GetDescription: string;
begin
  Result:=fDescription;
end;

procedure TDfmResourceReader.Load(aResources: TResources; aStream: TStream);
var aRes : TAbstractResource;
    RawData : TResourceDataStream;
begin
  if not CheckMagic(aStream) then
    raise EResourceReaderWrongFormatException.Create('');

  dummyName.Name:=fObjectName;
  aRes:=TResourceFactory.CreateResource(dummyType,dummyName);
  if fIsBinary then
  begin
    SetDataSize(aRes,aStream.Size-aStream.Position);
    SetDataOffset(aRes,aStream.Position);
    RawData:=TResourceDataStream.Create(aStream,aRes,aRes.DataSize,TCachedResourceDataStream);
    SetRawData(aRes,RawData);
  end
  else
    ObjectTextToBinary(aStream,aRes.RawData);
    
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

function TDfmResourceReader.CheckMagic(aStream: TStream): boolean;
var sig : TSignature;
    orig : int64;
begin
  orig:=aStream.Position;
  aStream.ReadBuffer(sig,4);
  if sig=FilerSignature then Result:=CheckBinDfm(aStream)
  else
  begin
    aStream.Seek(-4,soFromCurrent);
    Result:=CheckTextDfm(aStream);
  end;
  aStream.Position:=orig;
end;

constructor TDfmResourceReader.Create;
begin
  fExtensions:='.dfm .xfm .lfm';
  fDescription:='DFM resource reader';
  fLine:='';
  fLinePos:=0;
  fObjectName:='';
  fIsBinary:=false;
  dummyType:=TResourceDesc.Create;
  dummyType.ID:=RT_RCDATA;
  dummyName:=TResourceDesc.Create;
end;

destructor TDfmResourceReader.Destroy;
begin
  dummyType.Free;
  dummyName.Free;
end;

initialization
  TResources.RegisterReader('.dfm',TDfmResourceReader);
  TResources.RegisterReader('.xfm',TDfmResourceReader);
  TResources.RegisterReader('.lfm',TDfmResourceReader);

end.
