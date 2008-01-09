{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Resource reader for ELF files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type

(*
  generic TElfRelocTable<_TPElfRela_,_TElfRela_,_TElfSectHdr_,_Tword_> = class
  ...
  TElf32RelocTable = specialize TElfRelocTable<PElf32Rela,TElf32Rela,
                                               TElf32SectHdr,longword>;
  TElf64RelocTable = specialize TElfRelocTable<PElf64Rela,TElf64Rela,
                                               TElf64SectHdr,qword>;
*)

  _TElfRelocTable_= class
  private
    fList : _TPElfRela_;
    fCount : integer;
  protected
  public
    constructor Create(aStream : TStream; const aSectHdr : _TElfSectHdr_; const aOppositeEndianess : boolean);
    destructor Destroy; override;
    function Get(aOffset : _Tword_) : _Tword_;
  end;

(*
  generic TElfSubReader<_TElfHdr_,_TElfSectHdr_,_TResHdr_,_TResInfoNode_> = class
  ...
  TElf32SubReader = specialize TElfSubReader<TElf32Hdr,TElf32SectHdr,TResHdr32,
    TResInfoNode32>;
  TElf64SubReader = specialize TElfSubReader<TElf64Hdr,TElf64SectHdr,TResHdr64,
    TResInfoNode64>;
*)

  { _TElfSubReader_ }

  _TElfSubReader_ = class(TAbstractElfSubReader)
  private
    fElfHdr : _TElfHdr_;
    fSectHeaders : array of _TElfSectHdr_;
    fResHdr : _TResHdr_;
    fRelocations : _TElfRelocTable_;
    procedure ReadElfHeader(aStream : TStream);
    procedure ReadSectionHeaders(aStream : TStream);
    procedure ReadStringTable(aStream : TStream);
    function FindSection(const aName : string) : integer; override;
    procedure ReadRelocations(aStream : TStream);
    procedure ReadResHeader(aStream : TStream);
    procedure ReadNode(aStream : TStream; aParent : TResourceTreeNode;
      aResources : TResources; named : boolean); override;
  protected
    procedure Load(aResources : TResources; aStream : TStream); override;
  public
  end;


constructor _TElfRelocTable_.Create(aStream : TStream;
  const aSectHdr : _TElfSectHdr_; const aOppositeEndianess : boolean);
var i : integer;
begin
  aStream.Position:=aSectHdr.offset;
  fList:=GetMem(aSectHdr.size);
  fCount:=aSectHdr.size div sizeof(_TElfRela_);
  aStream.ReadBuffer(fList[0],aSectHdr.size);
  if aOppositeEndianess then
    for i:=0 to fCount-1 do
    begin
      fList[i].Offset:=SwapEndian(fList[i].Offset);
      fList[i].Info:=SwapEndian(fList[i].Info);
      fList[i].addend:=SwapEndian(fList[i].addend);
    end;
end;

destructor _TElfRelocTable_.Destroy;
begin
  FreeMem(fList);
end;

function _TElfRelocTable_.Get(aOffset : _Tword_) : _Tword_;
var l,r,pivot : integer;
begin
  Result:=0;
  if fCount=0 then exit;
  l:=0; r:=fCount-1;
  while l<=r do
  begin
    pivot:=(l+r) div 2;
    if fList[pivot].Offset<aOffset then l:=pivot+1
    else if fList[pivot].Offset>aOffset then r:=pivot-1
    else
    begin
      Result:=fList[pivot].Addend;
      exit;
    end;
  end;
end;


procedure _TElfSubReader_.ReadElfHeader(aStream: TStream);
begin
  aStream.ReadBuffer(fElfHdr,sizeof(fElfHdr));
  if fOppositeEndianess then
  begin
    fElfHdr._Type:=SwapEndian(fElfHdr._Type);
    fElfHdr.Machine:=SwapEndian(fElfHdr.Machine);
    fElfHdr.Version:=SwapEndian(fElfHdr.Version);
    fElfHdr.Entry:=SwapEndian(fElfHdr.Entry);
    fElfHdr.ProgHdrOffset:=SwapEndian(fElfHdr.ProgHdrOffset);
    fElfHdr.SectHdrOffset:=SwapEndian(fElfHdr.SectHdrOffset);
    fElfHdr.Flags:=SwapEndian(fElfHdr.Flags);
    fElfHdr.HdrSize:=SwapEndian(fElfHdr.HdrSize);
    fElfHdr.ProgHdrEntrySize:=SwapEndian(fElfHdr.ProgHdrEntrySize);
    fElfHdr.ProgHdrNum:=SwapEndian(fElfHdr.ProgHdrNum);
    fElfHdr.SectHdrEntrySize:=SwapEndian(fElfHdr.SectHdrEntrySize);
    fElfHdr.SectHdrNum:=SwapEndian(fElfHdr.SectHdrNum);
    fElfHdr.NameTableIndex:=SwapEndian(fElfHdr.NameTableIndex);
  end;
  fMachineType:=fElfHdr.Machine;
  if fElfHdr.Version<>EV_CURRENT then
    raise EElfResourceReaderUnknownVersionException.Create('');
  if fElfHdr.SectHdrNum=0 then
    raise EElfResourceReaderNoSectionsException.Create('');
  fNeedsReloc:=(fElfHdr._Type = ET_REL);
  if fNeedsReloc then
    case fElfHdr.Machine of
      EM_386    : fNeedsReloc:=false;
      EM_PPC    : ;
      EM_ARM    : fNeedsReloc:=false;
      EM_68K    : ;
      EM_SPARC  : ;
      EM_X86_64 : ;
      EM_PPC64  : ;
      EM_ALPHA  : ;
      EM_IA_64  : ;
    end;
end;

procedure _TElfSubReader_.ReadSectionHeaders(aStream: TStream);
var i : integer;
    delta : integer;
begin
  setlength(fSectHeaders,fElfHdr.SectHdrNum);
  delta:=fElfHdr.SectHdrEntrySize-sizeof(_TElfSectHdr_);
  aStream.Position:=fElfHdr.SectHdrOffset;
  for i:=0 to fElfHdr.SectHdrNum-1 do
  begin
    aStream.ReadBuffer(fSectHeaders[i],sizeof(fSectHeaders[i]));
    if delta>0 then aStream.Seek(delta,soFromCurrent);
    if fOppositeEndianess then
    begin
      fSectHeaders[i].NameIdx:=SwapEndian(fSectHeaders[i].NameIdx);
      fSectHeaders[i]._Type:=SwapEndian(fSectHeaders[i]._Type);
      fSectHeaders[i].Flags:=SwapEndian(fSectHeaders[i].Flags);
      fSectHeaders[i].Address:=SwapEndian(fSectHeaders[i].Address);
      fSectHeaders[i].Offset:=SwapEndian(fSectHeaders[i].Offset);
      fSectHeaders[i].Size:=SwapEndian(fSectHeaders[i].Size);
      fSectHeaders[i].Link:=SwapEndian(fSectHeaders[i].Link);
      fSectHeaders[i].Info:=SwapEndian(fSectHeaders[i].Info);
      fSectHeaders[i].AddrAlign:=SwapEndian(fSectHeaders[i].AddrAlign);
      fSectHeaders[i].EntSize:=SwapEndian(fSectHeaders[i].EntSize);
    end;
  end;
end;

procedure _TElfSubReader_.ReadStringTable(aStream: TStream);
begin
  if ((fElfHdr.NameTableIndex<=0) or
      (fElfHdr.NameTableIndex>=fElfHdr.SectHdrNum)) then
    raise EElfResourceReaderNoStringTableException.Create('');
  aStream.Position:=fSectHeaders[fElfHdr.NameTableIndex].Offset;
  fStringTable:=TObjectStringTable.Create(aStream,fSectHeaders[fElfHdr.NameTableIndex].Size);
end;

function _TElfSubReader_.FindSection(const aName : string) : integer;
var i : integer;
    sectname : string;
begin
  for i:=1 to fElfHdr.SectHdrNum-1 do
  begin
    sectname:=fStringTable.Get(fSectHeaders[i].NameIdx);
    if sectname=aName then
    begin
      Result:=i;
      exit;
    end;
  end;
  Result:=-1;
end;

procedure _TElfSubReader_.ReadRelocations(aStream : TStream);
begin
  fRelocations:=_TElfRelocTable_.Create(aStream,fSectHeaders[fRelSectIdx],fOppositeEndianess);
end;

procedure _TElfSubReader_.ReadResHeader(aStream: TStream);
begin
  aStream.Position:=fSectHeaders[fSectIdx].offset;
  aStream.ReadBuffer(fResHdr,sizeof(fResHdr));
  if fOppositeEndianess then
  begin
    fResHdr.rootptr:=SwapEndian(fResHdr.rootptr);
    fResHdr.count:=SwapEndian(fResHdr.count);
  end;
  if fNeedsReloc then
    fResHdr.rootptr:=fRelocations.Get(0);
  aStream.Position:=fSectHeaders[fSectIdx].offset+fResHdr.rootptr-fSectHeaders[fSectIdx].address;
end;

procedure _TElfSubReader_.ReadNode(aStream : TStream; aParent : TResourceTreeNode;
      aResources : TResources; named : boolean);
var infonode : _TResInfoNode_;
    aNode : TResourceTreeNode;
    i : integer;
    oldpos : int64;
    baseofs : _Tword_;
begin
  baseofs:=aStream.Position-fSectHeaders[fSectIdx].offset;
  aStream.ReadBuffer(infonode,sizeof(infonode));
  oldpos:=aStream.Position;
  if fOppositeEndianess then
  begin
    infonode.nameid:=SwapEndian(infonode.nameid);
    infonode.ncount:=SwapEndian(infonode.ncount);
    infonode.idcountsize:=SwapEndian(infonode.idcountsize);
    infonode.subptr:=SwapEndian(infonode.subptr);
  end;
  if fNeedsReloc then
  begin
    if named then infonode.nameid:=fRelocations.Get(baseofs);
    infonode.subptr:=fRelocations.Get(baseofs+(sizeof(_Tword_)+sizeof(longword)*2));
  end;
  if aParent=nil then aNode:=fRoot
  else
  begin
    if named then dummyDesc.Name:=ReadString(aStream,
      fSectHeaders[fSectIdx].offset+infonode.nameid-fSectHeaders[fSectIdx].address)
    else dummyDesc.ID:=infonode.nameid;
    aNode:=aParent.CreateSubNode(dummyDesc);
  end;
  aStream.Position:=fSectHeaders[fSectIdx].offset+infonode.subptr-fSectHeaders[fSectIdx].address;
  if aNode.IsLeaf then
    ReadResData(aStream,aNode,aResources,infonode.idcountsize)
  else
  begin
    for i:=1 to infonode.ncount do
      ReadNode(aStream,aNode,aResources,true);
    for i:=1 to infonode.idcountsize do
      ReadNode(aStream,aNode,aResources,false);
  end;
  aStream.Position:=oldpos;
end;

procedure _TElfSubReader_.Load(aResources: TResources; aStream: TStream);
begin
  ReadElfHeader(aStream);
  ReadSectionHeaders(aStream);
  ReadStringTable(aStream);
  if not FindResSection then exit;
  if fNeedsReloc then
    ReadRelocations(aStream);
  try
    ReadResHeader(aStream);
    if fResHdr.count=0 then exit; //no resources in this file
    LoadResources(aResources,aStream);
  finally
    if fNeedsReloc then
      fRelocations.Free;
  end;
end;

