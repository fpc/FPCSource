{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Resource writer for COFF files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit coffwriter;

{$MODE OBJFPC} {$H+}

interface

uses
  Classes, SysUtils, resource, resourcetree, cofftypes;

type

  { TCoffStringTable }

  TCoffStringTable = class(TStringList)
   private
    fSize: ptruint;
   public
    constructor Create;
    function Add(const S: string): Integer; override;
    procedure Delete(Index: Integer); override;
    property Size: ptruint read fSize;
  end;

  { TResourceStringTable }

  TResourceStringTable = class
  private
    fList : TStringList;
    fStartRVA : longword;
    fCurrRVA : longword;
    fEndRVA : longword;
    function GetString(index : integer) : string;
    function GetCount : integer;
    procedure SetStartRVA(aValue : longword);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(s : string);
    procedure Clear;
    property Count : integer read GetCount;
    property Items[index : integer] : string read GetString; default;
    property StartRVA : longword read fStartRVA write SetStartRVA;
    property CurrRVA : longword read fCurrRVA;
    property EndRVA : longword read fEndRVA;
  end;

  TCoffRelocation = packed record
    VirtualAddress : longword;
    SymTableIndex : longword;
    _type : word;
  end;
  PCoffRelocation = ^TCoffRelocation;
  
  { TCoffRelocations }

  TCoffRelocations = class
  private
    fMachineType: TCoffMachineType;
    fList : TFPList;
    fStartAddress : longword;
  protected
    function GetCount : integer;
    function GetRelocation(index : integer) : PCoffRelocation;
  public
    constructor Create(aMachineType: TCoffMachineType);
    destructor Destroy; override;
    procedure Add(aAddress : longword; aType : word; aSymTableIndex: longword);
    procedure AddRelativeToSection(aAddress : longword; aSectSymTableIndex: longword);
    procedure Clear;
    property Count : integer read GetCount;
    property Items[index : integer] : PCoffRelocation read GetRelocation; default;
    property StartAddress : longword read fStartAddress write fStartAddress;
    property MachineType : TCoffMachineType read fMachineType write fMachineType;
  end;

  { TCoffResourceWriter }

  TCoffResourceWriter = class (TAbstractResourceWriter)
  private
    fExtensions : string;
    fDescription : string;
    fResStringTable : TResourceStringTable;
    fMachineType : TCoffMachineType;
    procedure SetDefaultTarget;
    procedure AlignDword(aStream : TStream);
    function NextAlignedDword(aValue : longword) : longword;
    procedure SetNodeStringRVA(aNode : TResourceTreeNode);
    procedure WriteResDirTables(aStream : TStream);
    procedure WriteNodeTables(aStream : TStream; aNode : TResourceTreeNode);
    procedure WriteNodeDirEntry(aStream : TStream; aNode : TResourceTreeNode);
    procedure WriteResString(aStream : TStream; TheString : string);
    procedure WriteResDataEntries(aStream : TStream);
    procedure WriteResDataEntry(aStream : TStream; aNode : TResourceTreeNode);
    procedure WriteNodeRawData(aStream : TStream; aNode : TResourceTreeNode);
    procedure WriteRelocation(aStream : TStream; aRelocation : PCoffRelocation);
    procedure SetMachineType(AValue: TCoffMachineType);
  protected
    fRoot : TRootResTreeNode;
    fRelocations : TCoffRelocations;
    fResDataSectionSymIdx,
    fResHandlesSectionSymIdx : word;
    fResDataEntryCurrentRVA : longword;
    fSymTablePtr : longword;
    fStringTable: TCoffStringTable;
    fNumSymtableEntries: longword;
    fSymStorageClass: byte;
    fOppositeEndianess : boolean;
    procedure WriteEmptyCoffHeader(aStream : TStream);
    procedure WriteEmptySectionHeader(aStream : TStream); virtual;
    procedure WriteResStringTable(aStream : TStream); virtual;
    procedure WriteRawData(aStream : TStream);
    procedure WriteRelocations(aStream : TStream);
    procedure WriteCoffStringTable(aStream : TStream);
    function GetFixedCoffHeader:TCoffHeader; virtual;
    procedure FixCoffHeader(aStream : TStream);
    procedure FixSectionHeader(aStream : TStream; aResources : TResources); virtual;
    function GetExtensions : string; override;
    function GetDescription : string; override;
    function PrescanNode(aNode : TResourceTreeNode; aNodeSize : longword) : longword; virtual;
    procedure PrescanResourceTree; virtual;
    procedure Write(aResources : TResources; aStream : TStream); override;
    procedure WriteSymbolTable(aStream : TStream; aResources : TResources); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    property MachineType : TCoffMachineType read fMachineType write SetMachineType;
    property OppositeEndianess : boolean read fOppositeEndianess write fOppositeEndianess;
  end;

implementation

uses coffconsts;

{ TCoffStringTable }

constructor TCoffStringTable.Create;
  begin
    inherited create;
    fSize:=4;
    Duplicates:=dupIgnore;
  end;

function TCoffStringTable.Add(const S: string): Integer;
  begin
    Result:=inherited Add(S);
    inc(fSize,length(S)+1);
  end;

procedure TCoffStringTable.Delete(Index: Integer);
begin
  dec(fSize,length(Get(Index))+1);
  inherited Delete(Index);
end;

(* Utility function to calculate the timestamp *)
(*
function DateTimeToTimeT(aDate : TDateTime) : longint;
var days : integer;
    seconds : integer;
begin
  days:=trunc(aDate);
  seconds:=trunc(SecsPerDay*frac(aDate));
  dec(days,UnixDateDelta);
  Result:=SecsPerDay*days+seconds;
end;
*)

{ TCoffResourceWriter }

procedure TCoffResourceWriter.SetDefaultTarget;
begin
  fMachineType:=cmti386; //default
  fSymStorageClass:=IMAGE_SYM_CLASS_STATIC;
  {$IFDEF CPUX86_64}
  fMachineType:=cmtx8664;
  fSymStorageClass:=IMAGE_SYM_CLASS_STATIC;
  {$ENDIF}
  {$IFDEF CPUARM}
  fMachineType:=cmtarm;
  fSymStorageClass:=IMAGE_SYM_CLASS_STATIC;
  {$ENDIF}
  {$IFDEF CPUPOWERPC32}
  fMachineType:=cmtppc32aix;
  fSymStorageClass:=IMAGE_SYM_CLASS_HIDEXT;
  {$ENDIF}
  {$IFDEF CPUPOWERPC64}
  fMachineType:=cmtppc64aix;
  fSymStorageClass:=IMAGE_SYM_CLASS_HIDEXT;
  {$ENDIF}

  fOppositeEndianess:=false;
end;

procedure TCoffResourceWriter.AlignDword(aStream: TStream);
var topad : integer;
    lw : longword;
begin
  lw:=0;
  topad:=4-(aStream.Position mod 4);
  if topad<>4 then aStream.WriteBuffer(lw,topad);
end;

procedure TCoffResourceWriter.WriteEmptyCoffHeader(aStream: TStream);
var hdr : TCoffHeader;
begin
  FillByte(hdr,sizeof(hdr),0);
  aStream.WriteBuffer(hdr,sizeof(hdr));
end;

procedure TCoffResourceWriter.WriteEmptySectionHeader(aStream: TStream);
var hdr : TCoffSectionHeader;
begin
  FillByte(hdr,sizeof(hdr),0);
  aStream.WriteBuffer(hdr,sizeof(hdr));
end;

procedure TCoffResourceWriter.WriteResDirTables(aStream: TStream);
begin
  fResDataEntryCurrentRVA:=fResStringTable.EndRVA;
  fRelocations.Clear;
  WriteNodeTables(aStream,fRoot);
end;

procedure TCoffResourceWriter.WriteNodeTables(aStream : TStream; aNode: TResourceTreeNode);
var table : TResDirTable;
    i : integer;
begin
  if aNode.IsLeaf then exit;
  table.Characteristics:=0;
  table.TimeStamp:=0; //DateTimeToTimeT(now);   //we need a crossplatform way to have it UTC
  table.VerMajor:=0;
  table.VerMinor:=0;
  table.NamedEntriesCount:=aNode.NamedCount;
  table.IDEntriesCount:=aNode.IDCount;
  if OppositeEndianess then
    begin
      table.Characteristics:=SwapEndian(table.Characteristics);
      table.TimeStamp:=SwapEndian(table.TimeStamp);
      table.VerMajor:=SwapEndian(table.VerMajor);
      table.VerMinor:=SwapEndian(table.VerMinor);
      table.NamedEntriesCount:=SwapEndian(table.NamedEntriesCount);
      table.IDEntriesCount:=SwapEndian(table.IDEntriesCount);
    end;
  aStream.WriteBuffer(table,sizeof(Table));

  for i:=0 to aNode.NamedCount-1 do
    WriteNodeDirEntry(aStream,aNode.NamedEntries[i]);
  for i:=0 to aNode.IDCount-1 do
    WriteNodeDirEntry(aStream,aNode.IDEntries[i]);

  for i:=0 to aNode.NamedCount-1 do
    WriteNodeTables(aStream,aNode.NamedEntries[i]);
  for i:=0 to aNode.IDCount-1 do
    WriteNodeTables(aStream,aNode.IDEntries[i]);
end;

procedure TCoffResourceWriter.WriteNodeDirEntry(aStream: TStream;
  aNode: TResourceTreeNode);
var entry : TResDirEntry;
    reloctype : longword;
begin
  case aNode.Desc.DescType of
    dtID : entry.NameID:=aNode.Desc.ID;
    dtName : entry.NameID:=(aNode.NameRva+fResStringTable.StartRVA) or $80000000;
  end;
  if aNode.IsLeaf then
  begin
    entry.DataSubDirRVA:=fResDataEntryCurrentRVA;
    inc(fResDataEntryCurrentRVA,sizeof(TResDataEntry));
    fRelocations.AddRelativeToSection(entry.DataSubDirRVA,fResDataSectionSymIdx);
  end
  else entry.DataSubDirRVA:=aNode.SubDirRVA or $80000000;

  if OppositeEndianess then
    begin
      entry.NameID:=SwapEndian(entry.NameID);
      entry.DataSubDirRVA:=SwapEndian(entry.DataSubDirRVA);
    end;
  aStream.WriteBuffer(entry,sizeof(entry));
end;

procedure TCoffResourceWriter.WriteResStringTable(aStream: TStream);
var i : integer;
    lw : longword;
begin
  for i:=0 to fResStringTable.Count-1 do
    WriteResString(aStream,fResStringTable[i]);

  //align on dword boundaries
  i:=fResStringTable.EndRVA-fResStringTable.CurrRVA;
  if i>0 then
  begin
    lw:=0;
    aStream.WriteBuffer(lw,i);
  end;
end;

procedure TCoffResourceWriter.WriteResString(aStream: TStream; TheString: string
  );
var ws : widestring;
    w : word;
    i : integer;
begin
  w:=length(thestring);
  if OppositeEndianess then
    w:=SwapEndian(w);
  aStream.WriteBuffer(w,2);
  ws:=TheString;
  for i:=1 to length(ws) do
  begin
    w:=word(ws[i]);
    if OppositeEndianess then
      w:=SwapEndian(w);
    aStream.WriteBuffer(w,2);
  end;
end;

procedure TCoffResourceWriter.WriteResDataEntries(aStream: TStream);
begin
  WriteResDataEntry(aStream,fRoot);
end;

procedure TCoffResourceWriter.WriteResDataEntry(aStream: TStream;
  aNode: TResourceTreeNode);
var entry : TResDataEntry;
    i : integer;
    res : TAbstractResource;
begin
  if aNode.IsLeaf then
  begin
    res:=aNode.Data;
    aNode.DataRVA:=fResDataEntryCurrentRVA;
    entry.DataRVA:=aNode.DataRVA;
    entry.Size:=res.DataSize;
    entry.Codepage:=res.CodePage;
    entry.Reserved:=0;
    inc(fResDataEntryCurrentRVA,entry.Size);
    fResDataEntryCurrentRVA:=NextAlignedDword(fResDataEntryCurrentRVA);
    if OppositeEndianess then
      begin
        entry.DataRVA:=SwapEndian(entry.DataRVA);
        entry.Size:=SwapEndian(entry.Size);
        entry.Codepage:=SwapEndian(entry.Codepage);
        entry.Reserved:=SwapEndian(entry.Reserved);
      end;
    aStream.WriteBuffer(entry,sizeof(entry));
  end;
  for i:=0 to aNode.NamedCount-1 do
    WriteResDataEntry(aStream,aNode.NamedEntries[i]);
  for i:=0 to aNode.IDCount-1 do
    WriteResDataEntry(aStream,aNode.IDEntries[i]);
end;

procedure TCoffResourceWriter.WriteRawData(aStream: TStream);
begin
  WriteNodeRawData(aStream,fRoot);
end;

procedure TCoffResourceWriter.WriteNodeRawData(aStream: TStream;
  aNode: TResourceTreeNode);
var i : integer;
    res : TAbstractResource;
begin
  if aNode.IsLeaf then
  begin
    res:=aNode.Data;
    res.RawData.Position:=0;
    aStream.CopyFrom(res.RawData,res.DataSize);
    AlignDword(aStream);
  end;
  for i:=0 to aNode.NamedCount-1 do
    WriteNodeRawData(aStream,aNode.NamedEntries[i]);
  for i:=0 to aNode.IDCount-1 do
    WriteNodeRawData(aStream,aNode.IDEntries[i]);
end;

procedure TCoffResourceWriter.WriteRelocations(aStream: TStream);
var i : integer;
begin
  fRelocations.StartAddress:=aStream.Position;
  for i:=0 to fRelocations.Count-1 do
    WriteRelocation(aStream,fRelocations[i]);
end;

procedure TCoffResourceWriter.WriteRelocation(aStream: TStream;
  aRelocation: PCoffRelocation);
var r : TCoffRelocation;
begin
  r:=aRelocation^;
  if OppositeEndianess then
    begin
      r.VirtualAddress:=SwapEndian(r.VirtualAddress);
      r.SymTableIndex:=SwapEndian(r.SymTableIndex);
      r._type:=SwapEndian(r._type);
    end;
  aStream.WriteBuffer(r,sizeof(r));
end;

procedure TCoffResourceWriter.WriteSymbolTable(aStream: TStream; aResources : TResources);
var
  st : TCoffSectionTable;
  aux : TXCoffAuxSymbol32;
  offs : dword;
begin
  fSymTablePtr:=aStream.Position;
  st.Name:=RSRCSectName;
  st.Value:=0;
  st.SectionNumber:=1;
  st._type:=0;
  st.StorageClass:=fSymStorageClass;
  if OppositeEndianess then
    begin
      st.Value:=SwapEndian(st.Value);
      st.SectionNumber:=SwapEndian(st.SectionNumber);
      st._type:=SwapEndian(st._type);
    end;
  aStream.WriteBuffer(st,sizeof(st));
  inc(fNumSymtableEntries);
end;

procedure TCoffResourceWriter.WriteCoffStringTable(aStream : TStream);
var
  lw : longword;
  i : longint;
begin
  lw:=fStringTable.Size;
  if OppositeEndianess then
    lw:=SwapEndian(lw);
  aStream.WriteBuffer(lw,4);
  for i:=0 to fStringTable.Count-1 do
    aStream.WriteBuffer(fStringTable[i][1],length(fStringTable[i])+1);
end;

function TCoffResourceWriter.GetFixedCoffHeader: TCoffHeader;
begin
  case fMachineType of
    cmti386     : Result.machine:=IMAGE_FILE_MACHINE_I386;
    cmtarm      : Result.machine:=IMAGE_FILE_MACHINE_ARM;
    cmtx8664    : Result.machine:=IMAGE_FILE_MACHINE_AMD64;
    cmtaarch64  : Result.machine:=IMAGE_FILE_MACHINE_ARM64;
    cmtppc32aix : Result.machine:=IMAGE_FILE_MACHINE_POWERPC32_AIX;
    cmtppc64aix : Result.machine:=IMAGE_FILE_MACHINE_POWERPC64_AIX;
  end;
  Result.numsects:=1;
  Result.timestamp:=0; //DateTimeToTimeT(now);   //we need a crossplatform way to have it UTC
  Result.symtableptr:=fSymTablePtr;
  Result.symnum:=fNumSymtableEntries;
  Result.opthdrsize:=0;
  Result.characteristics:=IMAGE_FILE_32BIT_MACHINE or IMAGE_FILE_LINE_NUMS_STRIPPED;
end;

procedure TCoffResourceWriter.FixCoffHeader(aStream: TStream);
var hdr : TCoffHeader;
    oldpos : int64;
begin

  oldpos:=aStream.Position;
  aStream.Position:=0;

  hdr:=GetFixedCoffHeader;
  if OppositeEndianess then
    begin
      hdr.machine:=SwapEndian(hdr.machine);
      hdr.numsects:=SwapEndian(hdr.numsects);
      hdr.timestamp:=SwapEndian(hdr.timestamp);
      hdr.symtableptr:=SwapEndian(hdr.symtableptr);
      hdr.symnum:=SwapEndian(hdr.symnum);
      hdr.opthdrsize:=SwapEndian(hdr.opthdrsize);
      hdr.characteristics:=SwapEndian(hdr.characteristics);
    end;
  aStream.WriteBuffer(hdr,sizeof(hdr));
  
  aStream.Position:=oldpos;

end;

procedure TCoffResourceWriter.FixSectionHeader(aStream : TStream; aResources: TResources);
var hdr : TCoffSectionHeader;
    oldpos : int64;
begin
  oldpos:=aStream.Position;
  aStream.Position:=sizeof(TCoffHeader);

  hdr.Name:=RSRCSectName;
  hdr.VirtualSize:=0;
  hdr.VirtualAddress:=0;
  hdr.SizeOfRawData:=fResDataEntryCurrentRVA;
  hdr.PointerToRawData:=sizeof(TCoffHeader)+sizeof(TCoffSectionHeader);
  hdr.PointerToRelocations:=fRelocations.StartAddress;
  hdr.PointerToLineNumbers:=0;
  hdr.NumberOfRelocations:=fRelocations.Count;
  hdr.NumberOfLineNumbers:=0;
  hdr.Characteristics:=IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ or
                       IMAGE_SCN_MEM_WRITE;
  if OppositeEndianess then
    begin
      hdr.VirtualSize:=SwapEndian(hdr.VirtualSize);
      hdr.VirtualAddress:=SwapEndian(hdr.VirtualAddress);
      hdr.SizeOfRawData:=SwapEndian(hdr.SizeOfRawData);
      hdr.PointerToRawData:=SwapEndian(hdr.PointerToRawData);
      hdr.PointerToRelocations:=SwapEndian(hdr.PointerToRelocations);
      hdr.PointerToLineNumbers:=SwapEndian(hdr.PointerToLineNumbers);
      hdr.NumberOfRelocations:=SwapEndian(hdr.NumberOfRelocations);
      hdr.NumberOfLineNumbers:=SwapEndian(hdr.NumberOfLineNumbers);
      hdr.Characteristics:=SwapEndian(hdr.Characteristics);
    end;
  aStream.WriteBuffer(hdr,sizeof(hdr));

  aStream.Position:=oldpos;
end;

procedure TCoffResourceWriter.SetMachineType(AValue: TCoffMachineType);
begin
  fMachineType:=AValue;
{$IFDEF ENDIAN_BIG}
  if fMachineType in [cmti386,cmtx8664,cmtarm,cmtaarch64] then
    fOppositeEndianess:=true;
{$ELSE}
  if fMachineType in [cmtppc32aix,cmtppc64aix] then
    fOppositeEndianess:=true;
{$ENDIF}
  case fMachineType of
    cmti386,
    cmtx8664,
    cmtarm,
    cmtaarch64:
      fSymStorageClass:=IMAGE_SYM_CLASS_STATIC;
    cmtppc32aix,
    cmtppc64aix:
      fSymStorageClass:=IMAGE_SYM_CLASS_HIDEXT;
  end;
  fRelocations.MachineType:=fMachineType;
end;

function TCoffResourceWriter.GetExtensions: string;
begin
  Result:=fExtensions;
end;

function TCoffResourceWriter.GetDescription: string;
begin
  Result:=fDescription;
end;

function TCoffResourceWriter.NextAlignedDword(aValue: longword): longword;
var topad : longword;
begin
  Result:=aValue;
  topad:=4-(aValue mod 4);
  if topad<>4 then inc(Result,topad);
end;

procedure TCoffResourceWriter.SetNodeStringRVA(aNode: TResourceTreeNode);
begin
  if aNode.Desc.DescType=dtID then exit;
  aNode.NameRVA:=fResStringTable.CurrRVA;
  fResStringTable.Add(aNode.Desc.Name);
end;

function TCoffResourceWriter.PrescanNode(aNode: TResourceTreeNode; aNodeSize : longword): longword;
var i : integer;
    currva : longword;
    subnode : TResourceTreeNode;
begin
  currva:=aNode.SubDirRVA;
  SetNodeStringRVA(aNode);
  if aNode.IsLeaf then
  begin
    Result:=currva;
    exit;
  end;
  inc(currva,sizeof(TResDirTable));
  inc(currva,(aNode.NamedCount+aNode.IDCount)*sizeof(TResDirEntry));
  for i:=0 to aNode.NamedCount-1 do
  begin
    subnode:=aNode.NamedEntries[i];
    subnode.SubDirRVA:=currva;
    currva:=PrescanNode(subnode,0);
  end;
  for i:=0 to aNode.IDCount-1 do
  begin
    subnode:=aNode.IDEntries[i];
    subnode.SubDirRVA:=currva;
    currva:=PrescanNode(subnode,0);
  end;
  Result:=currva;
end;

procedure TCoffResourceWriter.PrescanResourceTree;
begin
  fRoot.SubDirRVA:=0;
  fResStringTable.Clear;
  fResStringTable.StartRVA:=PrescanNode(fRoot,0);
end;

procedure TCoffResourceWriter.Write(aResources: TResources; aStream: TStream);
begin
  WriteEmptyCoffHeader(aStream);
  WriteEmptySectionHeader(aStream);
  fRoot:=TRootResTreeNode(GetTree(aResources));
  PrescanResourceTree;
  WriteResDirTables(aStream);
  WriteResStringTable(aStream);
  WriteResDataEntries(aStream);
  WriteRawData(aStream);
  WriteRelocations(aStream);
  WriteSymbolTable(aStream,aResources);
  WriteCoffStringTable(aStream);
  FixCoffHeader(aStream);
  FixSectionHeader(aStream,aResources);
end;

constructor TCoffResourceWriter.Create;
begin
  fExtensions:='.o .obj';
  fDescription:='COFF resource writer';
  SetDefaultTarget;
  fRoot:=nil;
  fResStringTable:=TResourceStringTable.Create;
  fResDataEntryCurrentRVA:=0;
  fSymTablePtr:=0;
  fRelocations:=TCoffRelocations.Create(fMachineType);
  fStringTable:=TCoffStringTable.Create;
  fResDataSectionSymIdx:=0;
  // unused for win32
  fResHandlesSectionSymIdx:=word(low(smallint));
end;

destructor TCoffResourceWriter.Destroy;
begin
  fResStringTable.Free;
  fRelocations.Free;
  fStringTable.Free;
end;

{ TResourceStringTable }

function TResourceStringTable.GetString(index: integer): string;
begin
  Result:=fList[index];
end;

function TResourceStringTable.GetCount: integer;
begin
  Result:=fList.Count;
end;

procedure TResourceStringTable.SetStartRVA(aValue: longword);
var topad : longword;
begin
  fStartRVA:=aValue;
  inc(fCurrRva,fStartRVA);
  fEndRVA:=fCurrRVA;
  topad:=4-(fEndRVA mod 4);
  if topad<>4 then inc(fEndRVA,topad);
end;

constructor TResourceStringTable.Create;
begin
  fList:=TStringList.Create;
  fCurrRVA:=0;
  fStartRVA:=0;
  fEndRVA:=0;
end;

destructor TResourceStringTable.Destroy;
begin
  fList.Free;
end;

procedure TResourceStringTable.Add(s: string);
begin
  fList.Add(s);
  inc(fCurrRVA,(length(s)+1)*2);
end;

procedure TResourceStringTable.Clear;
begin
  fList.Clear;
  fCurrRVA:=0;
  fStartRVA:=0;
  fEndRVA:=0;
end;

{ TCoffRelocations }

function TCoffRelocations.GetCount: integer;
begin
  Result:=fList.Count;
end;

function TCoffRelocations.GetRelocation(index: integer): PCoffRelocation;
begin
  Result:=PCoffRelocation(fList[index]);
end;

constructor TCoffRelocations.Create(aMachineType: TCoffMachineType);
begin
  fList:=TFPList.Create;
  fMachineType:=aMachineType;
  fStartAddress:=0;
end;

destructor TCoffRelocations.Destroy;
begin
  Clear;
  fList.Free;
end;

procedure TCoffRelocations.Add(aAddress : longword; aType : word; aSymTableIndex: longword);
var p : PCoffRelocation;
begin
  p:=GetMem(sizeof(TCoffRelocation));
  p^.VirtualAddress:=aAddress;
  p^.SymTableIndex:=aSymTableIndex;
  p^._type:=aType;
  fList.Add(p);
end;

procedure TCoffRelocations.AddRelativeToSection(aAddress : longword; aSectSymTableIndex: longword);
var reloctype: word;
begin
  case fMachineType of
    cmti386     : reloctype:=IMAGE_REL_I386_DIR32NB;
    cmtarm      : reloctype:=IMAGE_REL_ARM_ADDR32NB;
    cmtx8664    : reloctype:=IMAGE_REL_AMD64_ADDR32NB;
    cmtaarch64  : reloctype:=IMAGE_REL_ARM64_ADDR32NB;
    cmtppc32aix : reloctype:=IMAGE_REL_PPC_POS;
    cmtppc64aix : reloctype:=IMAGE_REL_PPC_POS;
  end;
  Add(aAddress,reloctype,aSectSymTableIndex);
end;

procedure TCoffRelocations.Clear;
var i : integer;
begin
  for i:=0 to fList.Count-1 do
    Freemem(PCoffRelocation(fList[i]));
  fList.Clear;
end;

initialization
  TResources.RegisterWriter('.o',TCoffResourceWriter);
  TResources.RegisterWriter('.obj',TCoffResourceWriter);

end.
