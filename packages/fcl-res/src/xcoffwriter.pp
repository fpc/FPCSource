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

unit xcoffwriter;

{$MODE OBJFPC} {$H+}

interface

uses
  Classes, SysUtils, strtable, resource, resourcetree, cofftypes, coffwriter;

type
  { TXCoffResourceWriter }

  TXCoffResourceWriter = class (TCoffResourceWriter)
  protected
    fResStrTable : TResStringTable;
    fCurOfs : longword;
    fDataAlignment : integer;
    function NextAligned(aBound, aValue : longword) : longword;
    procedure Align(aBound : integer; aStream : TStream);
    function GetFixedCoffHeader: TCoffHeader; override;
    procedure WriteEmptySectionHeader(aStream: TStream); override;
    procedure FixSectionHeader(aStream : TStream; aResources : TResources); override;
    function PrescanNode(aNode: TResourceTreeNode; aNodeSize : longword): longword; override;
    procedure PrescanResourceTree; override;
    procedure WriteNodeInfos(aStream: TStream);
    procedure WriteNodeInfo(aStream: TStream; aNode: TResourceTreeNode);
    procedure WriteSubNodes(aStream : TStream; aNode : TResourceTreeNode);
    procedure WriteResStringTable(aStream: TStream); override;
    procedure WriteDataSymbol(aStream: TStream; const name: String; aStorageClass, aAuxStorageType: byte; aSecNum, aSecOffset, aSize: qword);
    procedure WriteSymbolTable(aStream : TStream; aResources : TResources); override;
    procedure WriteResHeader(aStream : TStream; aResources : TResources);
    procedure Write(aResources : TResources; aStream : TStream); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses coffconsts,fpcrestypes;

type
  // Todo: 64 bit
  _ptrtype_ = longword;
  _TResHdr_ = TResHdr32;
  _TResInfoNode_ = TResInfoNode32;


{ much of the code below is (almost) identical to the code in the Elf and
  Mach-O writers (they already duplicated lots of stuff). Todo: consolidate.
}

{ TXCoffResourceWriter }

function TXCoffResourceWriter.NextAligned(aBound, aValue : longword) : longword;
var topad : longword;
begin
  Result:=aValue;
  topad:=aBound-(aValue mod aBound);
  if topad<>aBound then inc(Result,topad);
end;

procedure TXCoffResourceWriter.Align(aBound: integer; aStream: TStream);
var topad,tmp : integer;
    qw : qword;
begin
  qw:=0;
  topad:=aBound-(aStream.Position mod aBound);
  if topad<>aBound then
    while topad>0 do
    begin
      if topad>8 then tmp:=8 else tmp:=topad;
      aStream.WriteBuffer(qw,tmp);
      dec(topad,tmp);
    end;
end;

function TXCoffResourceWriter.GetFixedCoffHeader: TCoffHeader;
begin
  Result:=inherited GetFixedCoffHeader;
  { we also have a BSS section }
  Result.NumSects:=2;
end;

procedure TXCoffResourceWriter.WriteEmptySectionHeader(aStream: TStream);
// Todo: 64 bit
var hdr : TXCoff32SectionHeader;
begin
  FillByte(hdr,sizeof(hdr),0);
  { .data }
  aStream.WriteBuffer(hdr,sizeof(hdr));
  { .bss }
  aStream.WriteBuffer(hdr,sizeof(hdr));
end;

procedure TXCoffResourceWriter.FixSectionHeader(aStream: TStream; aResources: TResources);
// Todo: 64 bit
var hdr : TXCoff32SectionHeader;
    oldpos : int64;
begin
  oldpos:=aStream.Position;
  aStream.Position:=sizeof(TCoffHeader);

  { initialized data }
  hdr.s_name:='.data'#0#0#0;
  hdr.s_paddr:=0;
  hdr.s_vaddr:=0;
  hdr.s_size:=fResDataEntryCurrentRVA;
  hdr.s_scnptr:=sizeof(TCoffHeader)+2*sizeof(TCoffSectionHeader);
  hdr.s_relptr:=fRelocations.StartAddress;
  hdr.s_lnnoptr:=0;
  hdr.s_nreloc:=fRelocations.Count;
  hdr.s_nlnno:=0;
  hdr.s_flags:=STYP_DATA;
  if OppositeEndianess then
    begin
      hdr.s_paddr:=SwapEndian(hdr.s_paddr);
      hdr.s_vaddr:=SwapEndian(hdr.s_vaddr);
      hdr.s_size:=SwapEndian(hdr.s_size);
      hdr.s_scnptr:=SwapEndian(hdr.s_scnptr);
      hdr.s_relptr:=SwapEndian(hdr.s_relptr);
      hdr.s_lnnoptr:=SwapEndian(hdr.s_lnnoptr);
      hdr.s_nreloc:=SwapEndian(hdr.s_nreloc);
      hdr.s_nlnno:=SwapEndian(hdr.s_nlnno);
      hdr.s_flags:=SwapEndian(hdr.s_flags);
    end;
  aStream.WriteBuffer(hdr,sizeof(hdr));

  { uninitialized data }
  hdr.s_name:='.bss'#0#0#0#0;
  hdr.s_paddr:=0;
  hdr.s_vaddr:=0;
  hdr.s_size:=aResources.Count*sizeof(_ptrtype_);
  hdr.s_scnptr:=0;
  hdr.s_relptr:=0;
  hdr.s_lnnoptr:=0;
  hdr.s_nreloc:=0;
  hdr.s_nlnno:=0;
  hdr.s_flags:=STYP_BSS;
  if OppositeEndianess then
    begin
      hdr.s_paddr:=SwapEndian(hdr.s_paddr);
      hdr.s_vaddr:=SwapEndian(hdr.s_vaddr);
      hdr.s_size:=SwapEndian(hdr.s_size);
      hdr.s_scnptr:=SwapEndian(hdr.s_scnptr);
      hdr.s_relptr:=SwapEndian(hdr.s_relptr);
      hdr.s_lnnoptr:=SwapEndian(hdr.s_lnnoptr);
      hdr.s_nreloc:=SwapEndian(hdr.s_nreloc);
      hdr.s_nlnno:=SwapEndian(hdr.s_nlnno);
      hdr.s_flags:=SwapEndian(hdr.s_flags);
    end;
  aStream.WriteBuffer(hdr,sizeof(hdr));

  aStream.Position:=oldpos;
end;

function TXCoffResourceWriter.PrescanNode(aNode: TResourceTreeNode; aNodeSize : longword): longword;
var curofs : longword;
    i : integer;
    subnode : TResourceTreeNode;
begin
  if aNode.IsLeaf then
  begin
    Result:=aNode.SubDirRVA;
    exit;
  end;

  if aNode.Desc.DescType=dtName then
    aNode.NameRVA:=fResStrTable.Add(aNode.Desc.Name);

  //first node subnodes begin at curofs (after all node headers)
  curofs:=aNode.SubDirRva+(aNode.NamedCount+aNode.IDCount)*aNodeSize;
  for i:=0 to aNode.NamedCount-1 do
  begin
    subnode:=aNode.NamedEntries[i];
    subnode.SubDirRVA:=curofs;
    curofs:=PrescanNode(subnode,aNodeSize);
  end;
  for i:=0 to aNode.IDCount-1 do
  begin
    subnode:=aNode.IDEntries[i];
    subnode.SubDirRVA:=curofs;
    curofs:=PrescanNode(subnode,aNodeSize);
  end;
  Result:=curofs;
end;

procedure TXCoffResourceWriter.PrescanResourceTree;
begin
  fResStrTable.Clear;
  fRoot.SubDirRVA:=sizeof(_TResHdr_)+sizeof(_TResInfoNode_);
  fResStrTable.StartOfs:=PrescanNode(fRoot,sizeof(_TResInfoNode_));
  if fResStrTable.Used then
    fResDataEntryCurrentRVA:=NextAligned(fDataAlignment,fResStrTable.StartOfs+fResStrTable.Size)
  else
    fResDataEntryCurrentRVA:=fResStrTable.StartOfs;
end;

procedure TXCoffResourceWriter.WriteNodeInfos(aStream: TStream);
begin
  fCurOfs:=sizeof(_TResHdr_);
  WriteNodeInfo(aStream,fRoot);
  WriteSubNodes(aStream,fRoot);
end;

procedure TXCoffResourceWriter.WriteNodeInfo(aStream: TStream; aNode: TResourceTreeNode);
var infonode : _TResInfoNode_;
begin
  if aNode.Desc.DescType=dtID then
    infonode.nameid:=aNode.Desc.ID
  else
  begin
    infonode.nameid:=fResStrTable.StartOfs+aNode.NameRVA;
    fRelocations.AddRelativeToSection(fCurOfs,fResDataSectionSymIdx);
  end;
  infonode.ncount:=aNode.NamedCount;
  if aNode.IsLeaf then
  begin
    infonode.idcountsize:=aNode.Data.RawData.Size;
    infonode.subptr:=fResDataEntryCurrentRVA;
    fResDataEntryCurrentRVA:=NextAligned(fDataAlignment,fResDataEntryCurrentRVA+infonode.idcountsize);
  end
  else
  begin
    infonode.idcountsize:=aNode.IDCount;
    infonode.subptr:=aNode.SubDirRVA;
  end;
  fRelocations.AddRelativeToSection(
    fCurOfs+sizeof(infonode.nameid)+sizeof(infonode.ncount)+
    sizeof(infonode.idcountsize),fResDataSectionSymIdx);
  if fOppositeEndianess then
  begin
    infonode.nameid:=SwapEndian(infonode.nameid);
    infonode.ncount:=SwapEndian(infonode.ncount);
    infonode.idcountsize:=SwapEndian(infonode.idcountsize);
    infonode.subptr:=SwapEndian(infonode.subptr);
  end;
  aStream.WriteBuffer(infonode,sizeof(infonode));
  inc(fCurOfs,sizeof(infonode));
end;

procedure TXCoffResourceWriter.WriteSubNodes(aStream: TStream; aNode: TResourceTreeNode);
var i : integer;
begin
  for i:=0 to aNode.NamedCount-1 do
    WriteNodeInfo(aStream,aNode.NamedEntries[i]);
  for i:=0 to aNode.IDCount-1 do
    WriteNodeInfo(aStream,aNode.IDEntries[i]);

  for i:=0 to aNode.NamedCount-1 do
    WriteSubNodes(aStream,aNode.NamedEntries[i]);
  for i:=0 to aNode.IDCount-1 do
    WriteSubNodes(aStream,aNode.IDEntries[i]);
end;

procedure TXCoffResourceWriter.WriteResStringTable(aStream: TStream);
begin
  if fResStrTable.Used then
    fResStrTable.WriteToStream(aStream);
  Align(fDataAlignment,aStream);
end;

procedure TXCoffResourceWriter.WriteDataSymbol(aStream: TStream; const name: String; aStorageClass, aAuxStorageType: byte; aSecNum, aSecOffset, aSize: qword);
var
  st : TCoffSymtableEntry;
  aux : TXCoffAuxSymbol32;
  offs : dword;
begin
  { top 4 bytes 0, lower 4 bytes = offset in string table }
  st.n_name:=#0#0#0#0;
  offs:=fStringTable.Size;
  if OppositeEndianess then
    offs:=SwapEndian(offs);
  PDWord(@st.n_name[4])^:=offs;
  fStringTable.Add(name);

  st.n_value:=aSecOffset;
  st.n_scnum:=aSecNum;
  st.n_type:=0;
  st.n_sclass:=aStorageClass;
  st.n_numaux:=1;
  if OppositeEndianess then
    begin
      st.n_value:=SwapEndian(st.n_value);
      st.n_scnum:=SwapEndian(st.n_scnum);
      st.n_type:=SwapEndian(st.n_type);
    end;
  aStream.WriteBuffer(st,sizeof(st));
  inc(fNumSymtableEntries);

  aux.x_scnlen:=aSize;
  aux.x_parmhash:=0;
  aux.x_snhash:=0;
  aux.x_smtyp:=aAuxStorageType;
  aux.x_smclas:=XMC_RW;
  aux.x_stab:=0;
  aux.x_snstab:=0;
  if OppositeEndianess then
    begin
      aux.x_scnlen:=SwapEndian(aux.x_scnlen);
      aux.x_parmhash:=SwapEndian(aux.x_parmhash);
      aux.x_snhash:=SwapEndian(aux.x_snhash);
      aux.x_stab:=SwapEndian(aux.x_stab);
      aux.x_snstab:=SwapEndian(aux.x_snstab);
    end;
  aStream.WriteBuffer(aux,sizeof(aux));
  inc(fNumSymtableEntries);
end;

procedure TXCoffResourceWriter.WriteSymbolTable(aStream: TStream; aResources : TResources);
const
  SECTION_DATA_ALIGNMENT = 8;
begin
  fSymTablePtr:=aStream.Position;
  { if order is changed, also adapt fResDataSectionSymIdx and fResHandlesSectionSymIdx }

  { initialized data }
  WriteDataSymbol(aStream,XCoffRsrcSectName,IMAGE_SYM_CLASS_HIDEXT,SECTION_DATA_ALIGNMENT or XTY_SD,1,0,fResDataEntryCurrentRVA);
  { create global FPC_RESSYMBOL symbol at the start of XCoffRsrcSectName
    (for XTY_LD: "size" = symbol index of the containing csect) }
  WriteDataSymbol(aStream,'FPC_RESSYMBOL',IMAGE_SYM_CLASS_EXT,XTY_LD,1,0,0);
  { uninitialized data }
  WriteDataSymbol(aStream,XCoffHandlesSectName,IMAGE_SYM_CLASS_HIDEXT,SECTION_DATA_ALIGNMENT or XTY_CM,2,0,aResources.Count*sizeof(_ptrtype_));
end;

procedure TXCoffResourceWriter.WriteResHeader(aStream: TStream; aResources: TResources);
var hdr : _TResHdr_;
begin
  hdr.rootptr:=sizeof(hdr);
  hdr.count:=aResources.Count;
  hdr.usedhandles:=0;
  hdr.handles:=0;
  fRelocations.AddRelativeToSection(0,fResDataSectionSymIdx);
  fRelocations.AddRelativeToSection(sizeof(hdr.rootptr)+sizeof(hdr.count)+sizeof(hdr.usedhandles),fResHandlesSectionSymIdx);
  if fOppositeEndianess then
  begin
    hdr.rootptr:=SwapEndian(hdr.rootptr);
    hdr.count:=SwapEndian(hdr.count);
    // only used at run time, always 0 in object file
//    hdr.usedhandles:=SwapEndian(hdr.usedhandles);
    // pointer to first byte of fpc.reshandles; since it's in a separate
    // section, this is at address 0 so we don't have to change this anymore
    // later either (hdr.handles is not yet known at this point)
//    hdr.handles:=SwapEndian(hdr.handles);
  end;
  aStream.WriteBuffer(hdr,sizeof(hdr));
end;

procedure TXCoffResourceWriter.Write(aResources: TResources; aStream: TStream);
begin
  WriteEmptyCoffHeader(aStream);
  WriteEmptySectionHeader(aStream);
  fRoot:=TRootResTreeNode(GetTree(aResources));
  PrescanResourceTree;
  WriteResHeader(aStream,aResources);
  WriteNodeInfos(aStream);
  WriteResStringTable(aStream);
  WriteRawData(aStream);
  WriteRelocations(aStream);
  WriteSymbolTable(aStream,aResources);
  WriteCoffStringTable(aStream);
  FixCoffHeader(aStream);
  FixSectionHeader(aStream,aResources);
end;

constructor TXCoffResourceWriter.Create;
begin
  inherited;
  fResStrTable:=TResStringTable.Create;
  fResDataSectionSymIdx:=0;
  { skip auxilary symtable entry part of data section symbol and FPC_RESSYMBOL }
  fResHandlesSectionSymIdx:=4;
  // TODO: 64 bit
  fDataAlignment:=4;
end;

destructor TXCoffResourceWriter.Destroy;
begin
  fResStrTable.Free;
  inherited Destroy;
end;

initialization
  TResources.RegisterWriter('.o',TXCoffResourceWriter);

end.
