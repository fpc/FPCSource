{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Resource writer for Mach-O files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
  _TMachOSymbolTable_ = class(TMachOSymbolTable)
  protected
    function AddSymbol(aName : string; sect : byte; addr : longword;
      glob, undef : boolean) : integer; override;
  protected
  public
    procedure WriteToStream(aStream : TStream); override;
    procedure SetSymbolOffset(symbolnum : integer; offset: longword); override;
  end;

  _TMachOSubWriter_ = class(TAbstractMachOSubWriter)
  private
    procedure SwapSection(var aSection: _TSection_);
  protected
    procedure PrescanResourceTree; override;
    procedure WriteResHeader(aStream : TStream; aResources : TResources); override;
    procedure WriteNodeInfos(aStream : TStream); override;
    procedure WriteNodeInfo(aStream : TStream; aNode : TResourceTreeNode); override;
    procedure AllocateSpaceForLoadCommands(aStream : TStream); override;

    procedure FixLoadCommands(aStream : TStream; aResources : TResources); override;
  public
    constructor Create(aParent : TMachOResourceWriter; const aMachineType
      : TMachOMachineType; const aSubMachineType: TMachoSubMachineType;
      const aOppositeEndianess : boolean); override;
  end;

{ _TMachOSymbolTable_ }

function _TMachOSymbolTable_.AddSymbol(aName: string; sect: byte; addr: longword;
  glob, undef: boolean): integer;
var p : _PNlist_;
begin
  p:=GetMem(sizeof(_TNlist_));
  p^.strx:=fStringTable.Add(aName);
  if not undef then
    p^._type:=N_SECT
  else
    p^._type:=N_UNDF;
  if glob then
    p^._type:=p^._type or N_EXT;
  p^.desc:=0;
  p^.sect:=sect;
  p^.value:=addr;
  Result:=fList.Count;
  fList.Add(p);
end;

procedure _TMachOSymbolTable_.WriteToStream(aStream: TStream);
var nlist : _TNlist_;
    i : integer;
    sawglobal: boolean;
begin
  { first write local symbols, then global ones, as ilocalsym is hardcoded to
    be index 0. Can't reorder here because we may already have used symbol
    numbers to generate relocations -> give an error if a global symbol
    comes before any local symbols }
  sawglobal:=false;
  for i:=0 to fList.Count-1 do
  begin
    nlist:=_PNlist_(fList[i])^;
    if (nlist._type and N_EXT)<>0 then
      sawglobal:=true
    else if sawglobal then
      raise EMachOResourceWriterSymbolTableWrongOrderException.Create('');
    if fOppositeEndianess then
    begin
      nlist.strx:=SwapEndian(nlist.strx);
      nlist.desc:=SwapEndian(nlist.desc);
      nlist.value:=SwapEndian(nlist.value);
    end;
    aStream.WriteBuffer(nlist,sizeof(nlist));
  end;
end;

procedure _TMachOSymbolTable_.SetSymbolOffset(symbolnum: integer; offset: longword);
var
  p : _PNlist_;
begin
  p:=_PNlist_(flist[symbolnum]);
  p^.value:=offset;
end;

{ _TMachOSubWriter_ }

procedure _TMachOSubWriter_.SwapSection(var aSection: _TSection_);
begin
  aSection.addr:=SwapEndian(aSection.addr);
  aSection.size:=SwapEndian(aSection.size);
  aSection.offset:=SwapEndian(aSection.offset);
  aSection.align:=SwapEndian(aSection.align);
  aSection.reloff:=SwapEndian(aSection.reloff);
  aSection.nreloc:=SwapEndian(aSection.nreloc);
  aSection.flags:=SwapEndian(aSection.flags);
  aSection.reserved1:=SwapEndian(aSection.reserved1);
  aSection.reserved2:=SwapEndian(aSection.reserved2);
end;

procedure _TMachOSubWriter_.PrescanResourceTree;
begin
  fResStrTable.Clear;
  fRoot.SubDirRVA:=sizeof(_TResHdr_)+sizeof(_TResInfoNode_);
  fResStrTable.StartOfs:=PrescanNode(fRoot,sizeof(_TResInfoNode_));
  if fResStrTable.Used then
    fDataCurOfs:=NextAligned(fDataAlignment,fResStrTable.StartOfs+fResStrTable.Size)
  else
    fDataCurOfs:=fResStrTable.StartOfs;
end;

procedure _TMachOSubWriter_.WriteResHeader(aStream: TStream;
  aResources: TResources);
var hdr : _TResHdr_;
begin
  hdr.rootptr:=sizeof(hdr);
  hdr.count:=aResources.Count;
  hdr.usedhandles:=0;
  hdr.handles:=0;
  { the first pointer (rootptr at offset 0) goes to the root node, which comes
    right after the header (the addend has been set to sizeof(hdr) -> add the
    address of the fpc.resources section to it via a relocations}
  fRelocations.Add(0,ffpcresourcessym);
  { the last pointer (handles at offset sizeof(fields before it)) goes to the
    fpc.reshandles section }
  fRelocations.Add(sizeof(hdr.rootptr)+sizeof(hdr.count)+sizeof(hdr.usedhandles),ffpcreshandlessym);
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

procedure _TMachOSubWriter_.WriteNodeInfos(aStream: TStream);
begin
  { offset inside the object }
  fCurOfs:=sizeof(_TResHdr_);
  WriteNodeInfo(aStream,fRoot);
  WriteSubNodes(aStream,fRoot);
end;

procedure _TMachOSubWriter_.WriteNodeInfo(aStream: TStream;
  aNode: TResourceTreeNode);
var infonode : _TResInfoNode_;
begin
  if aNode.Desc.DescType=dtID then
    infonode.nameid:=aNode.Desc.ID
  else
  begin
    infonode.nameid:=fResStrTable.StartOfs+aNode.NameRVA;
    fRelocations.Add(fCurOfs,ffpcresourcessym);
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
  fRelocations.Add(
    fCurOfs+sizeof(infonode.nameid)+sizeof(infonode.ncount)+
    sizeof(infonode.idcountsize),ffpcresourcessym);
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

procedure _TMachOSubWriter_.AllocateSpaceForLoadCommands(aStream: TStream);
var buf : pbyte;
begin
  fHeader.sizeofcmds:=
    //segment+res section+bss section
    sizeof(_TSegmentCommand_)+sizeof(_TSection_)*3+
    //symbol table and dynamic symbol table commands
    sizeof(TSymtabCommand)+sizeof(TDySymtabCommand)+
    //common header of the three commands
    sizeof(TLoadCommand)*3;
  buf:=GetMem(fHeader.sizeofcmds);
  FillByte(buf^,fHeader.sizeofcmds,0);
  try
    aStream.WriteBuffer(buf^,fHeader.sizeofcmds);
  finally
    FreeMem(buf);
  end;
end;

procedure _TMachOSubWriter_.FixLoadCommands(aStream: TStream; aResources : TResources);
var ldcommand : TLoadCommand;
    segcommand : _TSegmentCommand_;
    symcommand : TSymtabCommand;
    dysymcommand : TDySymtabCommand;
    ressection,bsssection,textsection : _TSection_;
begin
  ldcommand.cmd:=fSegType;
  ldcommand.cmdsize:=sizeof(TLoadCommand)+sizeof(segcommand)+sizeof(ressection)*3;

  FillByte(segcommand.name[0],16,0);
  segcommand.vmaddr:=0;
  segcommand.vmsize:=fDataCurOfs+sizeof(_ptrtype_)*aResources.Count;
  segcommand.fileoff:=fSectionStart;
  segcommand.filesize:=fDataCurOfs;
  segcommand.maxprot:=VM_PROT_READ or VM_PROT_WRITE or VM_PROT_EXECUTE;
  segcommand.initprot:=VM_PROT_READ or VM_PROT_WRITE or VM_PROT_EXECUTE;
  segcommand.nsects:=3;
  segcommand.flags:=0;

  fillbyte(textsection,sizeof(textsection),0);
  textsection.sectname:='__text';
  textsection.segname:='__TEXT';
  textsection.addr:=0;
  textsection.size:=0;
  textsection.offset:=segcommand.fileoff;
  textsection.align:=0;
  textsection.reloff:=0;
  textsection.nreloc:=0;
  textsection.flags:=S_ATTR_PURE_INSTRUCTIONS;
  textsection.reserved1:=0;
  textsection.reserved2:=0;

  fillbyte(ressection,sizeof(ressection),0);
  ressection.sectname:=RsrcSectName;
  ressection.segname:=DataSegName;
  ressection.addr:=0;
  ressection.size:=segcommand.filesize;
  ressection.offset:=segcommand.fileoff;
  ressection.align:=fSectAlignment;
  ressection.reloff:=fRelocations.StartOfs;
  ressection.nreloc:=fRelocations.Count;
  ressection.flags:=S_ATTR_LOC_RELOC;
  ressection.reserved1:=0;
  ressection.reserved2:=0;

  fillbyte(bsssection,sizeof(bsssection),0);
  bsssection.sectname:=HandlesSectName;
  bsssection.segname:=DataSegName;
  bsssection.addr:=fDataCurOfs;
  bsssection.size:=sizeof(_ptrtype_)*aResources.Count;
  bsssection.offset:=0;
  bsssection.align:=fSectAlignment;
  bsssection.reloff:=0;
  bsssection.nreloc:=0;
  bsssection.flags:=S_ZEROFILL;
  bsssection.reserved1:=0;
  bsssection.reserved2:=0;

  if fOppositeEndianess then
  begin
    ldcommand.cmd:=SwapEndian(ldcommand.cmd);
    ldcommand.cmdsize:=SwapEndian(ldcommand.cmdsize);

    segcommand.vmaddr:=SwapEndian(segcommand.vmaddr);
    segcommand.vmsize:=SwapEndian(segcommand.vmsize);
    segcommand.fileoff:=SwapEndian(segcommand.fileoff);
    segcommand.filesize:=SwapEndian(segcommand.filesize);
    segcommand.maxprot:=SwapEndian(segcommand.maxprot);
    segcommand.initprot:=SwapEndian(segcommand.initprot);
    segcommand.nsects:=SwapEndian(segcommand.nsects);
    segcommand.flags:=SwapEndian(segcommand.flags);

    SwapSection(textsection);
    SwapSection(ressection);
    SwapSection(bsssection);
  end;

  aStream.WriteBuffer(ldcommand,sizeof(ldcommand));
  aStream.WriteBuffer(segcommand,sizeof(segcommand));
  aStream.WriteBuffer(textsection,sizeof(textsection));
  aStream.WriteBuffer(ressection,sizeof(ressection));
  aStream.WriteBuffer(bsssection,sizeof(bsssection));

  ldcommand.cmd:=LC_SYMTAB;
  ldcommand.cmdsize:=sizeof(TLoadCommand)+sizeof(symcommand);

  symcommand.symoff:=fSymbolTable.StartOfs;
  symcommand.nsyms:=fSymbolTable.Count;
  symcommand.stroff:=fMachOStringTable.StartOfs;
  symcommand.strsize:=NextAligned(4,fMachOStringTable.Size);

  if fOppositeEndianess then
  begin
    ldcommand.cmd:=SwapEndian(ldcommand.cmd);
    ldcommand.cmdsize:=SwapEndian(ldcommand.cmdsize);

    symcommand.symoff:=SwapEndian(symcommand.symoff);
    symcommand.nsyms:=SwapEndian(symcommand.nsyms);
    symcommand.stroff:=SwapEndian(symcommand.stroff);
    symcommand.strsize:=SwapEndian(symcommand.strsize);
  end;

  aStream.WriteBuffer(ldcommand,sizeof(ldcommand));
  aStream.WriteBuffer(symcommand,sizeof(symcommand));

  ldcommand.cmd:=LC_DYSYMTAB;
  ldcommand.cmdsize:=sizeof(TLoadCommand)+sizeof(dysymcommand);

  dysymcommand.ilocalsym:=0;
  dysymcommand.nlocalsym:=fSymbolTable.LocalCount;
  dysymcommand.iextdefsym:=dysymcommand.ilocalsym+dysymcommand.nlocalsym;
  dysymcommand.nextdefsym:=fSymbolTable.GlobalCount;
  dysymcommand.iundefsym:=dysymcommand.iextdefsym+dysymcommand.nextdefsym;
  dysymcommand.nundefsym:=0;
  dysymcommand.tocoff:=0;
  dysymcommand.ntoc:=0;
  dysymcommand.modtaboff:=0;
  dysymcommand.nmodtab:=0;
  dysymcommand.extrefsymoff:=0;
  dysymcommand.nextrefsyms:=0;
  dysymcommand.indirectsymoff:=0;
  dysymcommand.nindirectsyms:=0;
  dysymcommand.extreloff:=0;
  dysymcommand.nextrel:=0;
  dysymcommand.locreloff:=0;
  dysymcommand.nlocrel:=0;

  if fOppositeEndianess then
  begin
    ldcommand.cmd:=SwapEndian(ldcommand.cmd);
    ldcommand.cmdsize:=SwapEndian(ldcommand.cmdsize);

    dysymcommand.ilocalsym:=SwapEndian(dysymcommand.ilocalsym);
    dysymcommand.nlocalsym:=SwapEndian(dysymcommand.nlocalsym);
    dysymcommand.iextdefsym:=SwapEndian(dysymcommand.iextdefsym);
    dysymcommand.nextdefsym:=SwapEndian(dysymcommand.nextdefsym);
    dysymcommand.iundefsym:=SwapEndian(dysymcommand.iundefsym);
    dysymcommand.nundefsym:=SwapEndian(dysymcommand.nundefsym);
    dysymcommand.tocoff:=SwapEndian(dysymcommand.tocoff);
    dysymcommand.ntoc:=SwapEndian(dysymcommand.ntoc);
    dysymcommand.modtaboff:=SwapEndian(dysymcommand.modtaboff);
    dysymcommand.nmodtab:=SwapEndian(dysymcommand.nmodtab);
    dysymcommand.extrefsymoff:=SwapEndian(dysymcommand.extrefsymoff);
    dysymcommand.nextrefsyms:=SwapEndian(dysymcommand.nextrefsyms);
    dysymcommand.indirectsymoff:=SwapEndian(dysymcommand.indirectsymoff);
    dysymcommand.nindirectsyms:=SwapEndian(dysymcommand.nindirectsyms);
    dysymcommand.extreloff:=SwapEndian(dysymcommand.extreloff);
    dysymcommand.nextrel:=SwapEndian(dysymcommand.nextrel);
    dysymcommand.locreloff:=SwapEndian(dysymcommand.locreloff);
    dysymcommand.nlocrel:=SwapEndian(dysymcommand.nlocrel);
  end;

  aStream.WriteBuffer(ldcommand,sizeof(ldcommand));
  aStream.WriteBuffer(dysymcommand,sizeof(dysymcommand));
end;

constructor _TMachOSubWriter_.Create(aParent : TMachOResourceWriter;
  const aMachineType : TMachOMachineType; const aSubMachineType: TMachoSubMachineType; const aOppositeEndianess : boolean);
begin
  inherited Create(aParent,aMachineType,aSubMachineType,aOppositeEndianess);
  fSymbolTable:=_TMachOSymbolTable_.Create(fMachOStringTable);
  fSymbolTable.OppositeEndianess:=fOppositeEndianess;
  {$IF _TMachOSubWriter_=TMachO32SubWriter}
  fDataAlignment:=4;
  fSectAlignment:=2; //2^2
  fSegType:=LC_SEGMENT;
  {$ELSE}
  fDataAlignment:=8;
  fSectAlignment:=3; //2^3
  fSegType:=LC_SEGMENT_64;
  {$ENDIF}
end;

