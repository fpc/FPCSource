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
    fList : TFPList;
    fStartAddress : longword;
  protected
    function GetCount : integer;
    function GetRelocation(index : integer) : PCoffRelocation;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(aAddress : longword; aType : word);
    procedure Clear;
    property Count : integer read GetCount;
    property Items[index : integer] : PCoffRelocation read GetRelocation; default;
    property StartAddress : longword read fStartAddress write fStartAddress;
  end;

  { TCoffResourceWriter }

  TCoffResourceWriter = class (TAbstractResourceWriter)
  private
    fExtensions : string;
    fDescription : string;
    fRoot : TRootResTreeNode;
    fResStringTable : TResourceStringTable;
    fResDataEntryCurrentRVA : longword;
    fRelocations : TCoffRelocations;
    fSymTablePtr : longword;
    fMachineType : TCoffMachineType;
    procedure AlignDword(aStream : TStream);
    function NextAlignedDword(aValue : longword) : longword;
    procedure SetNodeStringRVA(aNode : TResourceTreeNode);
    function PrescanNode(aNode : TResourceTreeNode) : longword;
    procedure PrescanResourceTree;
    procedure WriteEmptyCoffHeader(aStream : TStream);
    procedure WriteEmptySectionHeader(aStream : TStream);
    procedure WriteResDirTables(aStream : TStream);
    procedure WriteNodeTables(aStream : TStream; aNode : TResourceTreeNode);
    procedure WriteNodeDirEntry(aStream : TStream; aNode : TResourceTreeNode);
    procedure WriteResStringTable(aStream : TStream);
    procedure WriteResString(aStream : TStream; TheString : string);
    procedure WriteResDataEntries(aStream : TStream);
    procedure WriteResDataEntry(aStream : TStream; aNode : TResourceTreeNode);
    procedure WriteRawData(aStream : TStream);
    procedure WriteNodeRawData(aStream : TStream; aNode : TResourceTreeNode);
    procedure WriteRelocations(aStream : TStream);
    procedure WriteRelocation(aStream : TStream; aRelocation : PCoffRelocation);
    procedure WriteSymbolTable(aStream : TStream);
    procedure WriteEmptyCoffStringTable(aStream : TStream);
    procedure FixCoffHeader(aStream : TStream);
    procedure FixSectionHeader(aStream : TStream);
  protected
    function GetExtensions : string; override;
    function GetDescription : string; override;
    procedure Write(aResources : TResources; aStream : TStream); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property MachineType : TCoffMachineType read fMachineType write fMachineType;
  end;

implementation

uses coffconsts;

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
  {$IFDEF ENDIAN_BIG}
  table.Characteristics:=SwapEndian(table.Characteristics);
  table.TimeStamp:=SwapEndian(table.TimeStamp);
  table.VerMajor:=SwapEndian(table.VerMajor);
  table.VerMinor:=SwapEndian(table.VerMinor);
  table.NamedEntriesCount:=SwapEndian(table.NamedEntriesCount);
  table.IDEntriesCount:=SwapEndian(table.IDEntriesCount);
  {$ENDIF}
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
    case fMachineType of
      cmti386  : reloctype:=IMAGE_REL_I386_DIR32NB;
      cmtarm   : reloctype:=IMAGE_REL_ARM_ADDR32NB;
      cmtx8664 : reloctype:=IMAGE_REL_AMD64_ADDR32NB;
    end;
    fRelocations.Add(entry.DataSubDirRVA,reloctype);
  end
  else entry.DataSubDirRVA:=aNode.SubDirRVA or $80000000;

  {$IFDEF ENDIAN_BIG}
  entry.NameID:=SwapEndian(entry.NameID);
  entry.DataSubDirRVA:=SwapEndian(entry.DataSubDirRVA);
  {$ENDIF}
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
  {$IFDEF ENDIAN_BIG}
  w:=SwapEndian(w);
  {$ENDIF}
  aStream.WriteBuffer(w,2);
  ws:=TheString;
  for i:=1 to length(ws) do
  begin
    w:=word(ws[i]);
    {$IFDEF ENDIAN_BIG}
    w:=SwapEndian(w);
    {$ENDIF}
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
    {$IFDEF ENDIAN_BIG}
    entry.DataRVA:=SwapEndian(entry.DataRVA);
    entry.Size:=SwapEndian(entry.Size);
    entry.Codepage:=SwapEndian(entry.Codepage);
    entry.Reserved:=SwapEndian(entry.Reserved);
    {$ENDIF}
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
  {$IFDEF ENDIAN_BIG}
  r.VirtualAddress:=SwapEndian(r.VirtualAddress);
  r.SymTableIndex:=SwapEndian(r.SymTableIndex);
  r._type:=SwapEndian(r._type);;
  {$ENDIF}
  aStream.WriteBuffer(r,sizeof(r));
end;

procedure TCoffResourceWriter.WriteSymbolTable(aStream: TStream);
var st : TCoffSectionTable;
begin
  fSymTablePtr:=aStream.Position;
  st.Name:=RSRCSectName;
  st.Value:=0;
  st.SectionNumber:=1;
  st._type:=0;
  st.StorageClass:=IMAGE_SYM_CLASS_STATIC;
  st.NumAuxSymbol:=0;
  {$IFDEF ENDIAN_BIG}
  st.Value:=SwapEndian(st.Value);
  st.SectionNumber:=SwapEndian(st.SectionNumber);
  st._type:=SwapEndian(st._type);
  st.StorageClass:=SwapEndian(st.StorageClass);
  st.NumAuxSymbol:=SwapEndian(st.NumAuxSymbol);
  {$ENDIF}
  aStream.WriteBuffer(st,sizeof(st));
end;

procedure TCoffResourceWriter.WriteEmptyCoffStringTable(aStream : TStream);
var lw : longword;
begin
  lw:=4;
  {$IFDEF ENDIAN_BIG}
  lw:=SwapEndian(lw);
  {$ENDIF}
  aStream.WriteBuffer(lw,4);
end;

procedure TCoffResourceWriter.FixCoffHeader(aStream: TStream);
var hdr : TCoffHeader;
    oldpos : int64;
begin

  oldpos:=aStream.Position;
  aStream.Position:=0;

  case fMachineType of
    cmti386  : hdr.machine:=IMAGE_FILE_MACHINE_I386;
    cmtarm   : hdr.machine:=IMAGE_FILE_MACHINE_ARM;
    cmtx8664 : hdr.machine:=IMAGE_FILE_MACHINE_AMD64;
  end;
  hdr.numsects:=1;
  hdr.timestamp:=0; //DateTimeToTimeT(now);   //we need a crossplatform way to have it UTC
  hdr.symtableptr:=fSymTablePtr;
  hdr.symnum:=1;
  hdr.opthdrsize:=0;
  hdr.characteristics:=IMAGE_FILE_32BIT_MACHINE or IMAGE_FILE_LINE_NUMS_STRIPPED;
  {$IFDEF ENDIAN_BIG}
  hdr.machine:=SwapEndian(hdr.machine);
  hdr.numsects:=SwapEndian(hdr.numsects);
  hdr.timestamp:=SwapEndian(hdr.timestamp);
  hdr.symtableptr:=SwapEndian(hdr.symtableptr);
  hdr.symnum:=SwapEndian(hdr.symnum);
  hdr.opthdrsize:=SwapEndian(hdr.opthdrsize);
  hdr.characteristics:=SwapEndian(hdr.characteristics);
  {$ENDIF}
  aStream.WriteBuffer(hdr,sizeof(hdr));
  
  aStream.Position:=oldpos;

end;

procedure TCoffResourceWriter.FixSectionHeader(aStream : TStream);
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
  aStream.WriteBuffer(hdr,sizeof(hdr));

  aStream.Position:=oldpos;
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

function TCoffResourceWriter.PrescanNode(aNode: TResourceTreeNode
  ): longword;
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
    currva:=PrescanNode(subnode);
  end;
  for i:=0 to aNode.IDCount-1 do
  begin
    subnode:=aNode.IDEntries[i];
    subnode.SubDirRVA:=currva;
    currva:=PrescanNode(subnode);
  end;
  Result:=currva;
end;

procedure TCoffResourceWriter.PrescanResourceTree;
begin
  fRoot.SubDirRVA:=0;
  fResStringTable.Clear;
  fResStringTable.StartRVA:=PrescanNode(fRoot);
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
  WriteSymbolTable(aStream);
  WriteEmptyCoffStringTable(aStream);
  FixCoffHeader(aStream);
  FixSectionHeader(aStream);
end;

constructor TCoffResourceWriter.Create;
begin
  fExtensions:='.o .obj';
  fDescription:='COFF resource writer';
  fMachineType:=cmti386;
  fRoot:=nil;
  fResStringTable:=TResourceStringTable.Create;
  fResDataEntryCurrentRVA:=0;
  fSymTablePtr:=0;
  fRelocations:=TCoffRelocations.Create;
end;

destructor TCoffResourceWriter.Destroy;
begin
  fResStringTable.Free;
  fRelocations.Free;
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

constructor TCoffRelocations.Create;
begin
  fList:=TFPList.Create;
  fStartAddress:=0;
end;

destructor TCoffRelocations.Destroy;
begin
  Clear;
  fList.Free;
end;

procedure TCoffRelocations.Add(aAddress: longword; aType: word);
var p : PCoffRelocation;
begin
  p:=GetMem(sizeof(TCoffRelocation));
  p^.VirtualAddress:=aAddress;
  p^.SymTableIndex:=0;
  p^._type:=aType;
  fList.Add(p);
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
