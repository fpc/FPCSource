{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Resource reader for COFF files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit coffreader;

{$MODE OBJFPC} {$H+}

interface

uses
  Classes, SysUtils, resource, resourcetree, cofftypes;

type

  { TCoffResourceReader }

  TCoffResourceReader = class (TAbstractResourceReader)
  private
    fDescription: string;
    fExtensions: string;
    fCoffHeader : TCoffHeader;
    fResSectHeader : TCoffSectionHeader;
    fResSectStart : int64;
    fRoot : TRootResTreeNode;
    fMachineType : TCoffMachineType;
    function CheckRsrcName(aName : TSectionName) : boolean;
    function ReadCoffHeader(aStream : TStream) : boolean;
    function FindResSectionHeader(aStream : TStream) : boolean;
    procedure ReadResourceTree(aStream : TStream; aResources: TResources);
    procedure ReadNodeTable(aStream : TStream; aNode : TResourceTreeNode; aResources: TResources);
    procedure ReadNodeDirEntry(aStream : TStream; aParent : TResourceTreeNode; aResources: TResources);
    function ReadResString(aStream : TStream; aRVA : longword) : string;
    procedure ReadResDataEntry(aStream : TStream; aNode : TResourceTreeNode; aResources: TResources);
  protected
    function GetExtensions : string; override;
    function GetDescription : string; override;
    procedure Load(aResources : TResources; aStream : TStream); override;
    function CheckMagic(aStream : TStream) : boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property MachineType : TCoffMachineType read fMachineType;
  end;


implementation

uses coffconsts, resdatastream;

{ TCoffResourceReader }

function TCoffResourceReader.CheckRsrcName(aName: TSectionName): boolean;
var s : string;
begin
  Result:=aName=RSRCSectName;
  if not result then
  begin
    s:=aName;
    Result:=copy(s,1,6)='.rsrc$';
  end;
end;

function TCoffResourceReader.ReadCoffHeader(aStream: TStream): boolean;
var hdr : TCoffHeader;
begin
  Result:=false;

  try
    aStream.ReadBuffer(hdr,sizeof(hdr));
  except
    on e : EReadError do exit;
  end;
  
  {$IFDEF ENDIAN_BIG}
  hdr.machine:=SwapEndian(hdr.machine);
  hdr.numsects:=SwapEndian(hdr.numsects);
  hdr.timestamp:=SwapEndian(hdr.timestamp);
  hdr.symtableptr:=SwapEndian(hdr.symtableptr);
  hdr.symnum:=SwapEndian(hdr.symnum);
  hdr.opthdrsize:=SwapEndian(hdr.opthdrsize);
  hdr.characteristics:=SwapEndian(hdr.characteristics);
  {$ENDIF}

  case hdr.machine of
    IMAGE_FILE_MACHINE_I386  : fMachineType:=cmti386;
    IMAGE_FILE_MACHINE_ARM   : fMachineType:=cmtarm;
    IMAGE_FILE_MACHINE_AMD64 : fMachineType:=cmtx8664
    else exit;
  end;

  if hdr.opthdrsize>0 then
    aStream.Seek(hdr.opthdrsize,soFromCurrent);
  Result:=true;
  fCoffHeader:=hdr;
end;

function TCoffResourceReader.FindResSectionHeader(aStream: TStream) : boolean;
var hdr : TCoffSectionHeader;
    i : integer;
begin
  Result:=true;
  for i:=1 to fCoffHeader.NumSects do
  begin
    aStream.ReadBuffer(hdr,sizeof(hdr));

    {$IFDEF ENDIAN_BIG}
    hdr.VirtualSize:=SwapEndian(hdr.VirtualSize);
    hdr.VirtualAddress:=SwapEndian(hdr.VirtualAddress);
    hdr.SizeOfRawData:=SwapEndian(hdr.SizeOfRawData);
    hdr.PointerToRawData:=SwapEndian(hdr.PointerToRawData);
    hdr.PointerToRelocations:=SwapEndian(hdr.PointerToRelocations);
    hdr.PointerToLineNumbers:=SwapEndian(hdr.PointerToLineNumbers);
    hdr.NumberOfRelocations:=SwapEndian(hdr.NumberOfRelocations);
    hdr.NumberOfLineNumbers:=SwapEndian(hdr.NumberOfLineNumbers);
    hdr.Characteristics:=SwapEndian(hdr.Characteristics);
    {$ENDIF}

    if CheckRsrcName(hdr.Name) then
    begin
      fResSectHeader:=hdr;
      fResSectStart:=hdr.PointerToRawData;
      aStream.Position:=fResSectStart;
      exit;
    end;
  end;
  Result:=false;
end;

procedure TCoffResourceReader.ReadResourceTree(aStream: TStream; aResources: TResources);
begin
  fRoot:=TRootResTreeNode(GetTree(aResources));
  ReadNodeTable(aStream,fRoot,aResources);
end;

procedure TCoffResourceReader.ReadNodeTable(aStream: TStream;
  aNode: TResourceTreeNode; aResources: TResources);
var table : TResDirTable;
    i : integer;
begin
  aStream.ReadBuffer(table,sizeof(table));
  {$IFDEF ENDIAN_BIG}
  table.Characteristics:=SwapEndian(table.Characteristics);
  table.TimeStamp:=SwapEndian(table.TimeStamp);
  table.VerMajor:=SwapEndian(table.VerMajor);
  table.VerMinor:=SwapEndian(table.VerMinor);
  table.NamedEntriesCount:=SwapEndian(table.NamedEntriesCount);
  table.IDEntriesCount:=SwapEndian(table.IDEntriesCount);
  {$ENDIF}

  for i:=1 to table.NamedEntriesCount do
    ReadNodeDirEntry(aStream,aNode,aResources);
  for i:=1 to table.IDEntriesCount do
    ReadNodeDirEntry(aStream,aNode,aResources);
end;

procedure TCoffResourceReader.ReadNodeDirEntry(aStream: TStream;
  aParent : TResourceTreeNode; aResources: TResources);
var entry : TResDirEntry;
    desc : TResourceDesc;
    node : TResourceTreeNode;
    oldpos : int64;
begin
  aStream.ReadBuffer(entry,sizeof(entry));
  oldpos:=aStream.Position;
  {$IFDEF ENDIAN_BIG}
  entry.NameID:=SwapEndian(entry.NameID);
  entry.DataSubDirRVA:=SwapEndian(entry.DataSubDirRVA);
  {$ENDIF}

  desc:=TResourceDesc.Create;
  try
    if (entry.NameID and $80000000) = $80000000 then
      desc.Name:=ReadResString(aStream,(entry.NameID and $7FFFFFFF))
    else desc.ID:=entry.NameID;
    node:=aParent.CreateSubNode(desc);

    if (entry.DataSubDirRVA and $80000000) = $80000000 then
    begin
      aStream.Position:=fResSectStart+(entry.DataSubDirRVA and $7FFFFFFF);
      ReadNodeTable(aStream,node,aResources);
    end
    else
    begin
      aStream.Position:=fResSectStart+entry.DataSubDirRVA;
      ReadResDataEntry(aStream,node,aResources);
    end;
    aStream.Position:=oldpos;
  finally
    desc.Free;
  end;
end;

function TCoffResourceReader.ReadResString(aStream: TStream; aRVA: longword
  ): string;
var oldpos : int64;
    ws : widestring;
    w : word;
    i : integer;
begin
  oldpos:=aStream.Position;
  aStream.Position:=fResSectStart+aRVA;

  aStream.ReadBuffer(w,2);
  {$IFDEF ENDIAN_BIG}
  w:=SwapEndian(w);
  {$ENDIF}
  setlength(ws,w);

  for i:=1 to length(ws) do
  begin
    aStream.ReadBuffer(w,2);
    {$IFDEF ENDIAN_BIG}
    w:=SwapEndian(w);
    {$ENDIF}
    ws[i]:=widechar(w);
  end;
  aStream.Position:=oldpos;
  Result:=ws;
end;

procedure TCoffResourceReader.ReadResDataEntry(aStream: TStream;
  aNode: TResourceTreeNode; aResources: TResources);
var entry : TResDataEntry;
    res : TAbstractResource;
    RawData : TResourceDataStream;
begin
  aStream.ReadBuffer(entry,sizeof(entry));
  {$IFDEF ENDIAN_BIG}
  entry.DataRVA:=SwapEndian(entry.DataRVA);
  entry.Size:=SwapEndian(entry.Size);
  entry.Codepage:=SwapEndian(entry.Codepage);
  entry.Reserved:=SwapEndian(entry.Reserved);
  {$ENDIF}

  res:=aNode.CreateResource;
  if res=nil then
    raise EResourceDuplicateException.CreateFmt(SResDuplicate,[
      aNode.Data._Type.Name,aNode.Data.Name.Name,aNode.Data.LangID]);
  res.CodePage:=entry.Codepage;
  SetDataSize(res,entry.Size);
  aStream.Position:=fResSectStart+entry.DataRVA-fResSectHeader.VirtualAddress;
  SetDataOffset(res,aStream.Position);
  RawData:=TResourceDataStream.Create(aStream,res,res.DataSize,TCachedResourceDataStream);
  SetRawData(res,RawData);
  AddNoTree(aResources,res);
end;

function TCoffResourceReader.GetExtensions: string;
begin
  Result:=fExtensions;
end;

function TCoffResourceReader.GetDescription: string;
begin
  Result:=fDescription;
end;

procedure TCoffResourceReader.Load(aResources: TResources; aStream: TStream);
begin
  if not ReadCoffHeader(aStream) then
    raise EResourceReaderWrongFormatException.Create('');
  try
    if not FindResSectionHeader(aStream) then exit; //no resources in this file.
    ReadResourceTree(aStream,aResources);
  except
    on e : EReadError do
      raise EResourceReaderUnexpectedEndOfStreamException.Create('');
  end;
end;

function TCoffResourceReader.CheckMagic(aStream: TStream): boolean;
begin
  Result:=ReadCoffHeader(aStream);
end;

constructor TCoffResourceReader.Create;
begin
  fExtensions:='.o .obj';
  fDescription:='COFF resource reader';
  fResSectStart:=0;
  FillByte(fCoffHeader,sizeof(fCoffHeader),0);
  FillByte(fResSectHeader,sizeof(fResSectHeader),0);
  fRoot:=nil;
  fMachineType:=cmti386;
end;

destructor TCoffResourceReader.Destroy;
begin

end;

initialization
  TResources.RegisterReader('.o',TCoffResourceReader);
  TResources.RegisterReader('.obj',TCoffResourceReader);

end.
