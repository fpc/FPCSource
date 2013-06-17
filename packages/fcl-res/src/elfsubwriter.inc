{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Resource writer for ELF files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type

(*
  generic TElfRelocTable<_TPElfRela_,_TElfRela_,_TPElfRel_,_TElfRel_,_Tword_> = class
  ...
  TElf32RelocTable = specialize TElfRelocTable<PElf32Rela,TElf32Rela,
                                               PElf32Rel,TElf32Rel,longword>;
  TElf64RelocTable = specialize TElfRelocTable<PElf64Rela,TElf64Rela,
                                               PElf64Rel,TElf64Rel,qword>;
*)

  _TElfRelocTable_= class
  private
    fList : TFPList;
    fRelocType : byte;
    fEntrySize : integer;
    fSectionType : integer;
    fSectionName : string;
    function GetCount : integer;
    function GetItem(index : integer) : _TPElfRela_;
  protected
  public
    constructor Create(const fRelocInfo : TElfRelocInfo);
    destructor Destroy; override;
    procedure Add(const aOffset,aValue : _Tword_; aSymIdx : longword);
    procedure Clear;
    property Count : integer read GetCount;
    property Items[index : integer] : _TPElfRela_ read GetItem; default;
    property EntrySize : integer read fEntrySize;
    property SectionType : integer read fSectionType;
    property SectionName : string read fSectionName;
  end;
  
  { TElfSubWriter }

(*
  generic TElfSubWriter<_TElfRelocTable_,_TElfHdr_,_TElfSectHdr_,_TElfSymbol_,
    _TPElfRela_,_TElfRela_,_TResHdr_,_TResInfoNode_> = class
  ...
  TElf32SubWriter = specialize TElfSubWriter<TElf32RelocTable,_TElf32Hdr_,
    TElf32SectHdr,TElf32Symbol,PElf32Rela,TElf32Rela,TResHdr32,TResInfoNode32>;

  TElf64SubWriter = specialize TElfSubWriter<TElf64RelocTable,_TElf64Hdr_,
    TElf64SectHdr,TElf64Symbol,PElf64Rela,TElf64Rela,TResHdr64,TResInfoNode64>;
*)

  _TElfSubWriter_ = class(TAbstractElfSubWriter)
  private
    fRelocInfo :   TElfRelocInfo;
    fRelocTable : _TElfRelocTable_;
    procedure WriteEmptyElfHeader(aStream : TStream);
    procedure WriteResHeader(aStream : TStream; aResources : TResources);
    procedure WriteNodeInfos(aStream : TStream);
    procedure WriteSectHeaders(aStream : TStream);
    procedure FixElfHeader(aStream : TStream);
    procedure WriteSymbols(aStream : TStream);
    procedure WriteRelocations(aStream : TStream);
  protected
    procedure PrescanResourceTree; override;
    procedure Write(aResources : TResources; aStream : TStream); override;
    procedure WriteNodeInfo(aStream : TStream; aNode : TResourceTreeNode); override;
  public
    constructor Create(aParent : TElfResourceWriter; const aMachineType
      : integer; const aOppositeEndianess : boolean); override;
    destructor Destroy; override;
  end;

{ TElfRelocTable }

function _TElfRelocTable_.GetCount: integer;
begin
  Result:=fList.Count;
end;

function _TElfRelocTable_.GetItem(index: integer): _TPElfRela_;
begin
  Result:=_TPElfRela_(fList[index]);
end;

constructor _TElfRelocTable_.Create(const fRelocInfo : TElfRelocInfo);
begin
  fList:=TFPList.Create;
  fRelocType:=fRelocInfo.RelocType;
  fSectionType:=fRelocInfo.SectionType;
  case fSectionType of
    SHT_REL  : begin
                 fEntrySize:= sizeof(_TElfRel_);
                 fSectionName:='.rel'+RsrcSectName;
               end;
    SHT_RELA : begin
                 fEntrySize:= sizeof(_TElfRela_);
                 fSectionName:='.rela'+RsrcSectName;
               end;
  end;
end;

destructor _TElfRelocTable_.Destroy;
begin
  Clear;
  fList.Free;
end;

procedure _TElfRelocTable_.Add(const aOffset,aValue : _Tword_; aSymIdx : longword);
var p : _TPElfRela_;
begin
  p:=GetMem(sizeof(_TElfRela_));
  p^.Offset:=aOffset;
  P^.Info:=aSymIdx;
  {$IF _TElfRelocTable_=TElf64RelocTable}
  P^.Info:=P^.Info shl 32;
  P^.Info:=P^.Info or fRelocType;
  {$ELSE}
  P^.Info:=P^.Info shl 8;
  P^.Info:=P^.Info or (fRelocType and $FF);
  {$ENDIF}
  p^.addend:=aValue;
  fList.Add(p);
end;

procedure _TElfRelocTable_.Clear;
var i : integer;
    p : _TPElfRela_;
begin
  for i:=0 to fList.Count-1 do
  begin
    p:=_TPElfRela_(fList[i]);
    FreeMem(p);
  end;
  fList.Clear;
end;


{ TElfSubWriter }

procedure _TElfSubWriter_.PrescanResourceTree;
begin
  fResStringTable.Clear;
  fRoot.SubDirRVA:=sizeof(_TResHdr_)+sizeof(_TResInfoNode_);
  fResStringTable.StartOfs:=PrescanNode(fRoot,sizeof(_TResInfoNode_));
  if fResStringTable.Used then
    fDataCurOfs:=NextAligned(fDataAlignment,fResStringTable.StartOfs+fResStringTable.Size)
  else
    fDataCurOfs:=fResStringTable.StartOfs;
end;

procedure _TElfSubWriter_.WriteEmptyElfHeader(aStream: TStream);
var hdr : _TElfHdr_;
begin
  FillByte(hdr,sizeof(hdr),0);
  aStream.WriteBuffer(hdr,sizeof(hdr));
end;

procedure _TElfSubWriter_.WriteResHeader(aStream: TStream;
  aResources: TResources);
var hdr : _TResHdr_;
begin
  hdr.count:=aResources.Count;
  hdr.usedhandles:=0;
  hdr.handles:=0;
  
  fSymbolTable.AddSection(RSRCSECT_IDX);
  fSymbolTable.AddSection(HANDLESECT_IDX);
  case fRelocInfo.SectionType of
    SHT_REL  : hdr.rootptr:=sizeof(hdr);
    SHT_RELA : hdr.rootptr:=0;
  end;

  fRelocTable.Add(0,sizeof(hdr),RSRCSECT_IDX);
  fRelocTable.Add(sizeof(hdr.rootptr)+sizeof(hdr.count)+sizeof(hdr.usedhandles),0,HANDLESECT_IDX);
  if fOppositeEndianess then
  begin
    hdr.rootptr:=SwapEndian(hdr.rootptr);
    hdr.count:=SwapEndian(hdr.count);
    //handles must be fixed later
//    hdr.usedhandles:=SwapEndian(hdr.usedhandles);
//    hdr.handles:=SwapEndian(hdr.handles);
  end;
  aStream.WriteBuffer(hdr,sizeof(hdr));
end;

procedure _TElfSubWriter_.WriteNodeInfos(aStream: TStream);
begin
  fCurOfs:=sizeof(_TResHdr_);
  WriteNodeInfo(aStream,fRoot);
  WriteSubNodes(aStream,fRoot);
end;

procedure _TElfSubWriter_.WriteNodeInfo(aStream: TStream;
  aNode: TResourceTreeNode);
var infonode : _TResInfoNode_;
begin
  if aNode.Desc.DescType=dtID then
    infonode.nameid:=aNode.Desc.ID
  else
  begin
    infonode.nameid:=fResStringTable.StartOfs+aNode.NameRVA;
    fRelocTable.Add(fCurOfs,infonode.nameid,RSRCSECT_IDX);
    if fRelocInfo.SectionType=SHT_RELA then infonode.nameid:=0;
  end;
  infonode.ncount:=aNode.NamedCount;
  if aNode.IsLeaf then
  begin
    infonode.idcountsize:=aNode.Data.RawData.Size;
    infonode.subptr:=fDataCurOfs;
    fDataCurOfs:=NextAligned(fDataAlignment,fDataCurOfs+infonode.idcountsize);
  end
  else
  begin
    infonode.idcountsize:=aNode.IDCount;
    infonode.subptr:=aNode.SubDirRVA;
  end;
  fRelocTable.Add(
    fCurOfs+sizeof(infonode.nameid)+sizeof(infonode.ncount)+
    sizeof(infonode.idcountsize),infonode.subptr,RSRCSECT_IDX);
  if fRelocInfo.SectionType=SHT_RELA then infonode.subptr:=0;
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

procedure _TElfSubWriter_.WriteSectHeaders(aStream: TStream);
var i : integer;
    orig : PElf64SectHdr;
    hdr : _TElfSectHdr_;
begin
  Align(fDataAlignment,aStream);
  fSectHdrOffset:=aStream.Position;
  for i:=0 to fSections.Count-1 do
  begin
    orig:=fSections[i];
    {$IF _TElfSubWriter_=TElf64SubWriter}
    hdr:=orig^;
    {$ELSE}
    hdr.NameIdx:=orig^.NameIdx;
    hdr._Type:=orig^._Type;
    hdr.Flags:=orig^.Flags;
    hdr.Address:=orig^.Address;
    hdr.Offset:=orig^.Offset;
    hdr.Size:=orig^.Size;
    hdr.Link:=orig^.Link;
    hdr.Info:=orig^.Info;
    hdr.AddrAlign:=orig^.AddrAlign;
    hdr.EntSize:=orig^.EntSize;
    {$ENDIF}

    if fOppositeEndianess then
    begin
      hdr.NameIdx:=SwapEndian(hdr.NameIdx);
      hdr._Type:=SwapEndian(hdr._Type);
      hdr.Flags:=SwapEndian(hdr.Flags);
      hdr.Address:=SwapEndian(hdr.Address);
      hdr.Offset:=SwapEndian(hdr.Offset);
      hdr.Size:=SwapEndian(hdr.Size);
      hdr.Link:=SwapEndian(hdr.Link);
      hdr.Info:=SwapEndian(hdr.Info);
      hdr.AddrAlign:=SwapEndian(hdr.AddrAlign);
      hdr.EntSize:=SwapEndian(hdr.EntSize);
    end;

    aStream.WriteBuffer(hdr,sizeof(hdr));
  end;
end;

procedure _TElfSubWriter_.FixElfHeader(aStream: TStream);
var hdr : _TElfHdr_;
begin
  hdr._Type:=ET_REL;
  hdr.Machine:=fMachineType;
  hdr.Version:=EV_CURRENT;
  hdr.Entry:=0;
  hdr.ProgHdrOffset:=0;
  hdr.SectHdrOffset:=fSectHdrOffset;
  hdr.Flags:=fMachineFlags;
  hdr.HdrSize:=sizeof(_TElfHdr_)+sizeof(TElfIdent);
  hdr.ProgHdrEntrySize:=0;
  hdr.ProgHdrNum:=0;
  hdr.SectHdrEntrySize:=sizeof(_TElfSectHdr_);
  hdr.SectHdrNum:=fSections.Count;
  hdr.NameTableIndex:=fShStrTabIdx;

  if fOppositeEndianess then
  begin
    hdr._Type:=SwapEndian(hdr._Type);
    hdr.Machine:=SwapEndian(hdr.Machine);
    hdr.Version:=SwapEndian(hdr.Version);
    hdr.Entry:=SwapEndian(hdr.Entry);
    hdr.ProgHdrOffset:=SwapEndian(hdr.ProgHdrOffset);
    hdr.SectHdrOffset:=SwapEndian(hdr.SectHdrOffset);
    hdr.Flags:=SwapEndian(hdr.Flags);
    hdr.HdrSize:=SwapEndian(hdr.HdrSize);
    hdr.ProgHdrEntrySize:=SwapEndian(hdr.ProgHdrEntrySize);
    hdr.ProgHdrNum:=SwapEndian(hdr.ProgHdrNum);
    hdr.SectHdrEntrySize:=SwapEndian(hdr.SectHdrEntrySize);
    hdr.SectHdrNum:=SwapEndian(hdr.SectHdrNum);
    hdr.NameTableIndex:=SwapEndian(hdr.NameTableIndex);
  end;

  aStream.Position:=sizeof(TElfIdent);
  aStream.WriteBuffer(hdr,sizeof(hdr));
end;

procedure _TElfSubWriter_.WriteSymbols(aStream: TStream);
var i : integer;
    orig : PElf64Symbol;
    sym : _TElfSymbol_;
    startpos : int64;
begin
  Align(fDataAlignment,aStream);
  startpos:=aStream.Position;
  for i:=0 to fSymbolTable.Count-1 do
  begin
    orig:=fSymbolTable[i];
    {$IF _TElfSubWriter_=TElf64SubWriter}
    sym:=orig^;
    {$ELSE}
    sym.Name:=orig^.Name;
    sym.Value:=orig^.Value;
    sym.Size:=orig^.Size;
    sym.Info:=orig^.Info;
    sym.Other:=orig^.Other;
    sym.SectIdx:=orig^.SectIdx;
    {$ENDIF}

    if fOppositeEndianess then
    begin
      sym.Name:=SwapEndian(sym.Name);
      sym.Value:=SwapEndian(sym.Value);
      sym.Size:=SwapEndian(sym.Size);
      sym.SectIdx:=SwapEndian(sym.SectIdx);
    end;

    aStream.WriteBuffer(sym,sizeof(sym));
  end;

  fSymTabIdx:=fSections.Add('.symtab',SHT_SYMTAB,0,startpos,
    fSymbolTable.Count*sizeof(_TElfSymbol_),sizeof(_TElfSymbol_),fSymStrTabIdx,
    fSymbolTable.FirstGlobal,fDataAlignment);
end;

procedure _TElfSubWriter_.WriteRelocations(aStream: TStream);
var orig : _TPElfRela_;
    rel : _TElfRela_;
    startpos : int64;
    i : integer;
begin
  Align(fDataAlignment,aStream);
  startpos:=aStream.Position;
  for i:=0 to fRelocTable.Count-1 do
  begin
    orig:=fRelocTable[i];
    rel:=orig^;

    if fOppositeEndianess then
    begin
      rel.Offset:=SwapEndian(rel.Offset);
      rel.Info:=SwapEndian(rel.Info);
      rel.Addend:=SwapEndian(rel.Addend);
    end;

    aStream.WriteBuffer(rel,fRelocTable.EntrySize);
  end;

  fSections.Add(fRelocTable.SectionName,fRelocTable.SectionType,0,startpos,
    fRelocTable.Count*fRelocTable.EntrySize,fRelocTable.EntrySize,
    fSymTabIdx,1,fDataAlignment);
end;

procedure _TElfSubWriter_.Write(aResources: TResources; aStream: TStream);
begin
  fRoot:=TRootResTreeNode(fParent.GetTree(aResources));
  WriteEmptyElfHeader(aStream);
  fSectionStart:=aStream.Position;
  PrescanResourceTree;
  WriteResHeader(aStream,aResources);
  WriteNodeInfos(aStream);
  WriteResStringTable(aStream);
  WriteRawData(aStream);
  fSections.Add(RsrcSectName, SHT_PROGBITS,SHF_ALLOC or SHF_WRITE,fSectionStart,
    fDataCurOfs,fDataAlignment);
  AddEmptySections(aResources,aStream);
  fSymbolTable.AddGlobal('FPC_RESSYMBOL',0,0,STT_OBJECT,RSRCSECT_IDX);
  WriteStrTab(aStream);
  WriteSymbols(aStream);
  WriteRelocations(aStream);
  WriteShStrTab(aStream);
  WriteSectHeaders(aStream);
  FixElfHeader(aStream);
end;

constructor _TElfSubWriter_.Create(aParent : TElfResourceWriter; const
  aMachineType: integer; const aOppositeEndianess: boolean);
begin
  inherited Create(aParent, aMachineType, aOppositeEndianess);

  with fRelocInfo do
    case aMachineType of
      EM_386    : begin RelocType:=R_386_32;        SectionType:=SHT_REL;  end;
      EM_PPC    : begin RelocType:=R_PPC_ADDR32;    SectionType:=SHT_RELA; end;
      EM_ARM    : begin RelocType:=R_ARM_ABS32;     SectionType:=SHT_REL;  end;
      EM_68K    : begin RelocType:=R_68K_32;        SectionType:=SHT_RELA; end;
      EM_SPARC  : begin RelocType:=R_SPARC_32;      SectionType:=SHT_RELA; end;
      EM_X86_64 : begin RelocType:=R_x86_64_64;     SectionType:=SHT_RELA; end;
      EM_PPC64  : begin RelocType:=R_PPC64_ADDR64;  SectionType:=SHT_RELA; end;
      EM_ALPHA  : begin RelocType:=R_ALPHA_REFQUAD; SectionType:=SHT_RELA; end;
      EM_IA_64  : begin RelocType:=R_IA64_DIR64LSB; SectionType:=SHT_RELA; end;
      EM_MIPS   : begin RelocType:=R_MIPS_32;      SectionType:=SHT_RELA; end;
      else
        raise EElfResourceWriterUnknownMachineException.Create('');
    end;

  fRelocTable:=_TElfRelocTable_.Create(fRelocInfo);
  {$IF _TElfSubWriter_=TElf64SubWriter}
  fDataAlignment:=8;
  {$ELSE}
  fDataAlignment:=4;
  {$ENDIF}
  if aMachineType=EM_IA_64 then fMachineFlags:=EF_IA_64_ABI64
  else fMachineFlags:=0;
end;

destructor _TElfSubWriter_.Destroy;
begin
  fRelocTable.Free;
  inherited Destroy;
end;

