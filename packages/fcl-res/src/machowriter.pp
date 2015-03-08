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

unit machowriter;

{$MODE OBJFPC} {$H+}

interface

uses
  Classes, SysUtils, resource, machotypes;

type
  EMachOResourceWriterException = class(EResourceWriterException);
  EMachOResourceWriterUnknownBitSizeException = class(EMachOResourceWriterException);
  EMachOResourceWriterSymbolTableWrongOrderException = class(EMachOResourceWriterException);

type

  { TMachOResourceWriter }

  TMachoSubMachineType = record
    case TMachOMachineType of
      msmppc_all: (fPpcSubType: TMachOSubMachineTypePowerPC);
      msmppc64_all: (fPpc64SubType: TMachOSubMachineTypePowerPC64);
      msm386_all: (f386SubType: TMachOSubMachineType386);
      msmx64_all: (fX64SubType: TMachOSubMachineTypex64);
      mmtarm: (fArmSubType: TMachOSubMachineTypeArm);
      mmtarm64: (fArm64SubType: TMachOSubMachineTypeAarch64);
  end;

  TMachOResourceWriter = class(TAbstractResourceWriter)
  private
    fExtensions : string;
    fDescription : string;
    fNativeEndianess : integer;
    fEndianess : integer;
    fOppositeEndianess : boolean;
    fMachineType : TMachOMachineType;
    fSubMachineType : TMachoSubMachineType;
    fBits : integer;

    procedure SetDefaultTarget;
    procedure SetMachineType(const aMachineType : TMachOMachineType);
    procedure SetSubMachineType(const aSubMachineType: TMachoSubMachineType);
  protected
    function GetExtensions : string; override;
    function GetDescription : string; override;
    procedure Write(aResources : TResources; aStream : TStream); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property MachineType : TMachOMachineType read fMachineType write SetMachineType;
    property SubMachineType : TMachOSubMachineType read fSubMachineType write SetSubMachineType;
  end;

implementation

uses resourcetree, machoconsts, strtable, fpcrestypes;

type

  { TMachORelocations }

  TMachORelocations = class
  private
    fList : TFPList;
    fStartOfs : longword;
    fOppositeEndianess : boolean;
    fEndianess : integer;
    fRelocType : longword;
    fRelocSize : longword;
    function GetCount : integer;
  protected
  public
    constructor Create(aMachineType : TMachOMachineType; aOppositeEndianess : boolean);
    destructor Destroy; override;
    procedure Add(addr: longword; symnum: longword);
    procedure Clear;
    procedure WriteToStream(aStream : TStream);
    property Count : integer read GetCount;
    property StartOfs : longword read fStartOfs write fStartOfs;
  end;

  { TMachOSymbolTable }

  TMachOSymbolTable = class
  protected
    fStringTable : TObjectStringTable;
    fList : TFPList;
    fStartOfs : longword;
    fLocalCount : integer;
    fGlobalCount : integer;
    fOppositeEndianess : boolean;
    function GetCount : integer;
    function AddSymbol(aName : string; sect : byte; addr : longword;
      glob, undef : boolean) : integer; virtual; abstract;
  protected
  public
    constructor Create(aStringTable : TObjectStringTable);
    destructor Destroy; override;
    function AddLocal(const aName : string; sect : byte; addr : longword) : integer;
    function AddGlobal(const aName : string; sect : byte; addr : longword) : integer;
    function AddExternal(const aName : string) : integer;
    procedure Clear;
    procedure WriteToStream(aStream : TStream); virtual; abstract;
    procedure SetSymbolOffset(symbolnum : integer; offset: longword); virtual; abstract;
    property Count : integer read GetCount;
    property LocalCount : integer read fLocalCount;
    property GlobalCount : integer read fGlobalCount;
    property StartOfs : longword read fStartOfs write fStartOfs;
    property OppositeEndianess : boolean read fOppositeEndianess write fOppositeEndianess;
  end;

  { TAbstractMachOSubWriter }

  TAbstractMachOSubWriter = class
  protected
    fParent : TMachOResourceWriter;
    fOppositeEndianess : boolean;
    fMachineType : TMachOMachineType;
    fSubMachineType: TMachoSubMachineType;
    fDataAlignment : integer;
    fSectAlignment : integer;
    fSegType : longword;
    fHeader : TMachHdr;
    fMachOStringTable : TObjectStringTable;
    fSymbolTable : TMachOSymbolTable;
    fRelocations : TMachORelocations;
    fRoot : TRootResTreeNode;
    fResStrTable : TResStringTable;
    fCurOfs : longword;
    fDataCurOfs : longword;
    fSectionStart : longword;
    ffpcresourcessym,
    ffpcreshandlessym : integer;

    function NextAligned(aBound, aValue : longword) : longword;
    procedure Align(aBound : integer; aStream : TStream);

    procedure PrescanResourceTree; virtual; abstract;
    function PrescanNode(aNode : TResourceTreeNode; aNodeSize : longword) : longword;

    procedure WriteEmptyMachOHeader(aStream : TStream);
    procedure WriteResHeader(aStream : TStream; aResources : TResources); virtual; abstract;
    procedure WriteNodeInfos(aStream : TStream); virtual; abstract;
    procedure WriteNodeInfo(aStream : TStream; aNode : TResourceTreeNode); virtual; abstract;
    procedure WriteSubNodes(aStream : TStream; aNode : TResourceTreeNode);
    procedure WriteResStringTable(aStream : TStream);
    procedure WriteRawData(aStream : TStream);
    procedure WriteResData(aStream : TStream; aNode : TResourceTreeNode);
    procedure WriteRelocations(aStream : TStream);
    procedure WriteSymbolTable(aStream : TStream);
    procedure WriteMachOStringTable(aStream : TStream);
    procedure AllocateSpaceForLoadCommands(aStream : TStream); virtual; abstract;

    procedure FixHeader(aStream : TStream);
    procedure FixLoadCommands(aStream : TStream; aResources : TResources); virtual; abstract;
    procedure FixResHeader(aStream : TStream); virtual; abstract;

    procedure Write(aResources: TResources; aStream: TStream);
  public
    constructor Create(aParent : TMachOResourceWriter; const aMachineType
      : TMachOMachineType; const aSubMachineType: TMachoSubMachineType;
      const aOppositeEndianess : boolean); virtual;
    destructor Destroy; override;
  end;

(*
Almost all differences in 32 and 64 bit mach-o files lie in record sizes.
Generics don't work with record types, so use macros to do this task
(uglier, but should be the same)
*)

{$MACRO ON}

//Define TMachO32SymbolTable and TMachO32SubWriter

{$DEFINE _TMachOSymbolTable_:=TMachO32SymbolTable}
{$DEFINE _TMachOSubWriter_:=TMachO32SubWriter}
{$DEFINE _TNlist_:=TNlist32}
{$DEFINE _PNList_:=PNList32}
{$DEFINE _TResHdr_:=TResHdr32}
{$DEFINE _TResInfoNode_:=TResInfoNode32}
{$DEFINE _TSegmentCommand_:=TSegmentCommand32}
{$DEFINE _TSection_:=TSection32}
{$DEFINE _ptrtype_:=longword}
{$INCLUDE machosubwriter.inc}

//Define TMachO64SymbolTable and TMachO64SubWriter

{$DEFINE _TMachOSymbolTable_:=TMachO64SymbolTable}
{$DEFINE _TMachOSubWriter_:=TMachO64SubWriter}
{$DEFINE _TNlist_:=TNlist64}
{$DEFINE _PNList_:=PNList64}
{$DEFINE _TResHdr_:=TResHdr64}
{$DEFINE _TResInfoNode_:=TResInfoNode64}
{$DEFINE _TSegmentCommand_:=TSegmentCommand64}
{$DEFINE _TSection_:=TSection64}
{$DEFINE _ptrtype_:=qword}
{$INCLUDE machosubwriter.inc}

//Clean all this stuff...

{$UNDEF _TMachOSymbolTable_}
{$UNDEF _TMachOSubWriter_}
{$UNDEF _TNlist_}
{$UNDEF _PNList_}
{$UNDEF _TResHdr_}
{$UNDEF _TResInfoNode_}
{$UNDEF _TSegmentCommand_}
{$UNDEF _TSection_}

{ TMachOSymbolTable }

function TMachOSymbolTable.GetCount: integer;
begin
  Result:=fList.Count;
end;

constructor TMachOSymbolTable.Create(aStringTable: TObjectStringTable);
begin
  fStringTable:=aStringTable;
  fList:=TFPList.Create;
  fStartOfs:=0;
  fLocalCount:=0;
  fGlobalCount:=0;
  fOppositeEndianess:=false;
end;

destructor TMachOSymbolTable.Destroy;
begin
  Clear;
  fList.Free;
end;

function TMachOSymbolTable.AddLocal(const aName: string; sect: byte; addr: longword
  ): integer;
begin
  Result:=AddSymbol(aName,sect,addr,false,false);
  inc(fLocalCount);
end;

function TMachOSymbolTable.AddGlobal(const aName: string; sect: byte; addr: longword
  ): integer;
begin
  Result:=AddSymbol(aName,sect,addr,true,false);
  inc(fGlobalCount);
end;

function TMachOSymbolTable.AddExternal(const aName: string): integer;
begin
  Result:=AddSymbol(aName,NO_SECT,0,false,true);
  inc(fGlobalCount);
end;

procedure TMachOSymbolTable.Clear;
var i : integer;
begin
  for i:=0 to fList.Count-1 do
    FreeMem(fList[i]);
  fList.Clear;
end;

{ TMachORelocations }

function TMachORelocations.GetCount: integer;
begin
  Result:=fList.Count;
end;

constructor TMachORelocations.Create(aMachineType : TMachOMachineType;
  aOppositeEndianess : boolean);
begin
  fList:=TFPList.Create;
  fStartOfs:=0;

  case aMachineType of
    mmtpowerpc   : begin
                     fEndianess:=MACH_BIG_ENDIAN;
                     fRelocType:=PPC_RELOC_VANILLA;
                     fRelocSize:=2;
                   end;
    mmtpowerpc64 : begin
                     fEndianess:=MACH_BIG_ENDIAN;
                     fRelocType:=PPC_RELOC_VANILLA;
                     fRelocSize:=3;
                   end;
    mmti386      : begin
                     fEndianess:=MACH_LITTLE_ENDIAN;
                     fRelocType:=GENERIC_RELOC_VANILLA;
                     fRelocSize:=2;
                   end;
    mmtx86_64    : begin
                     fEndianess:=MACH_LITTLE_ENDIAN;
                     fRelocType:=X86_64_RELOC_UNSIGNED;
                     fRelocSize:=3;
                   end;
    mmtarm      : begin
                     fEndianess:=MACH_LITTLE_ENDIAN;
                     fRelocType:=ARM_RELOC_VANILLA;
                     fRelocSize:=2;
                   end;
    mmtarm64    : begin
                    fEndianess:=MACH_LITTLE_ENDIAN;
                    fRelocType:=ARM64_RELOC_UNSIGNED;
                    fRelocSize:=3;
                  end;
  end;
  fOppositeEndianess:=aOppositeEndianess;
end;

destructor TMachORelocations.Destroy;
begin
  Clear;
  fList.Free;
end;

procedure TMachORelocations.Add(addr: longword; symnum: longword);
var p : PRelocationInfo;
begin
  p:=GetMem(sizeof(TRelocationInfo));
  p^.address:=addr;
  //bit fields make things difficult...
  if fEndianess=MACH_BIG_ENDIAN then
  begin
    p^.flags:=symnum shl 8;
    p^.flags:=p^.flags or (fRelocSize shl 5); //length
    p^.flags:=p^.flags or fRelocType;
    { reference via symbol }
    p^.flags:=p^.flags or R_EXTERN_BE;
  end
  else
  begin
    p^.flags:=symnum and R_SYMBOLNUM_LE;
    p^.flags:=p^.flags or (fRelocSize shl 25); //length
    p^.flags:=p^.flags or (fRelocType shl 28);
    { reference via symbol }
    p^.flags:=p^.flags or R_EXTERN_LE;
  end;
  fList.Add(p);
end;

procedure TMachORelocations.Clear;
var i : integer;
begin
  for i:=0 to fList.Count-1 do
    FreeMem(PRelocationInfo(fList[i]));
  fList.Clear;
end;

procedure TMachORelocations.WriteToStream(aStream: TStream);
var rel : TRelocationInfo;
    i : integer;
begin
  for i:=0 to fList.Count-1 do
  begin
    rel:=PRelocationInfo(fList[i])^;
    if fOppositeEndianess then
    begin
      rel.address:=SwapEndian(rel.address);
      rel.flags:=SwapEndian(rel.flags);
    end;
    aStream.WriteBuffer(rel,sizeof(rel));
  end;
end;

{ TAbstractMachOSubWriter }

function TAbstractMachOSubWriter.NextAligned(aBound, aValue: longword): longword;
var topad : longword;
begin
  Result:=aValue;
  topad:=aBound-(aValue mod aBound);
  if topad<>aBound then inc(Result,topad);
end;

procedure TAbstractMachOSubWriter.Align(aBound: integer; aStream: TStream);
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

function TAbstractMachOSubWriter.PrescanNode(aNode: TResourceTreeNode;
  aNodeSize : longword): longword;
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

procedure TAbstractMachOSubWriter.WriteEmptyMachOHeader(aStream: TStream);
begin
  FillByte(fHeader,sizeof(TMachHdr),0);
  aStream.WriteBuffer(fHeader,sizeof(TMachHdr));
  Align(fDataAlignment,aStream);
end;

procedure TAbstractMachOSubWriter.WriteSubNodes(aStream: TStream;
  aNode: TResourceTreeNode);
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

procedure TAbstractMachOSubWriter.WriteResStringTable(aStream: TStream);
begin
  if fResStrTable.Used then
    fResStrTable.WriteToStream(aStream);
  Align(fDataAlignment,aStream);
end;

procedure TAbstractMachOSubWriter.WriteRawData(aStream: TStream);
begin
  WriteResData(aStream,fRoot);
end;

procedure TAbstractMachOSubWriter.WriteResData(aStream: TStream;
  aNode: TResourceTreeNode);
var rawdata : TStream;
    i : integer;
begin
  if aNode.IsLeaf then
  begin
    rawdata:=aNode.Data.RawData;
    rawdata.Position:=0;
    aStream.CopyFrom(rawdata,rawdata.Size);
    Align(fDataAlignment,aStream);
    exit;
  end;
  for i:=0 to aNode.NamedCount-1 do
    WriteResData(aStream,aNode.NamedEntries[i]);
  for i:=0 to aNode.IDCount-1 do
    WriteResData(aStream,aNode.IDEntries[i]);
end;

procedure TAbstractMachOSubWriter.WriteRelocations(aStream: TStream);
begin
  fRelocations.WriteToStream(aStream);
end;

procedure TAbstractMachOSubWriter.WriteSymbolTable(aStream: TStream);
begin
  fSymbolTable.WriteToStream(aStream);
end;

procedure TAbstractMachOSubWriter.WriteMachOStringTable(aStream: TStream);
begin
  fMachOStringTable.WriteToStream(aStream);
  Align(fDataAlignment,aStream);
end;

procedure TAbstractMachOSubWriter.FixHeader(aStream: TStream);
const
  ppcsm2int: array[TMachOSubMachineTypePowerPC] of longint = (CPU_SUBTYPE_POWERPC_ALL);
  ppc64sm2int: array[TMachOSubMachineTypePowerPC64] of longint = (CPU_SUBTYPE_POWERPC_ALL);
  i386sm2int: array[TMachOSubMachineType386] of longint = (CPU_SUBTYPE_I386_ALL);
  x86_64sm2int: array[TMachOSubMachineTypex64] of longint = (CPU_SUBTYPE_X86_64_ALL);
  armsm2int: array[TMachOSubMachineTypeArm] of longint = (CPU_SUBTYPE_ARM_ALL,
    CPU_SUBTYPE_ARM_V4T,CPU_SUBTYPE_ARM_V6,CPU_SUBTYPE_ARM_V5TEJ,
    CPU_SUBTYPE_ARM_XSCALE,CPU_SUBTYPE_ARM_V7);
  arm64sm2int: array[TMachOSubMachineTypeAarch64] of longint = (CPU_SUBTYPE_ARM64_ALL);
begin
  aStream.Position:=0;
  case fMachineType of
    mmtpowerpc   : begin
                     fHeader.magic:=MH_MAGIC;
                     fHeader.cputype:=CPU_TYPE_POWERPC;
                     fHeader.cpusubtype:=ppcsm2int[fSubMachineType.fPpcSubType];
                   end;
    mmtpowerpc64 : begin
                     fHeader.magic:=MH_MAGIC_64;
                     fHeader.cputype:=CPU_TYPE_POWERPC64;
                     fHeader.cpusubtype:=ppc64sm2int[fSubMachineType.fPpc64SubType];
                   end;
    mmti386      : begin
                     fHeader.magic:=MH_MAGIC;
                     fHeader.cputype:=CPU_TYPE_I386;
                     fHeader.cpusubtype:=i386sm2int[fSubMachineType.f386SubType];
                   end;
    mmtx86_64    : begin
                     fHeader.magic:=MH_MAGIC_64;
                     fHeader.cputype:=CPU_TYPE_X86_64;
                     fHeader.cpusubtype:=x86_64sm2int[fSubMachineType.fX64SubType];
                   end;
    mmtarm      : begin
                     fHeader.magic:=MH_MAGIC;
                     fHeader.cputype:=CPU_TYPE_ARM;
                     fHeader.cpusubtype:=armsm2int[fSubMachineType.fArmSubType];
                   end;
    mmtarm64    : begin
                    fHeader.magic:=MH_MAGIC_64;
                    fHeader.cputype:=CPU_TYPE_ARM64;
                    fHeader.cpusubtype:=arm64sm2int[fSubMachineType.fArm64SubType];
                  end;
  end;
  fHeader.filetype:=MH_OBJECT;
  fHeader.ncmds:=3;
  fHeader.flags:=0;
  
  if fOppositeEndianess then
  begin
    fHeader.magic:=SwapEndian(fHeader.magic);
    fHeader.cputype:=SwapEndian(fHeader.cputype);
    fHeader.cpusubtype:=SwapEndian(fHeader.cpusubtype);
    fHeader.filetype:=SwapEndian(fHeader.filetype);
    fHeader.ncmds:=SwapEndian(fHeader.ncmds);
    fHeader.sizeofcmds:=SwapEndian(fHeader.sizeofcmds);
    fHeader.flags:=SwapEndian(fHeader.flags);
  end;
  aStream.WriteBuffer(fHeader,sizeof(fHeader));
  Align(fDataAlignment,aStream);
end;

procedure TAbstractMachOSubWriter.Write(aResources: TResources; aStream: TStream);
begin
  WriteEmptyMachOHeader(aStream);
  AllocateSpaceForLoadCommands(aStream);
  fSectionStart:=aStream.Position;
  fRoot:=TRootResTreeNode(fParent.GetTree(aResources));
  { on AArch64, if you want to refer to a section from another one, you
    have to do it via an explicit symbol reference.

  }
  { dummy text section symbol }
  fSymbolTable.AddLocal('ltmp0',1,0);
  { dummy fpc.resources symbol }
  fSymbolTable.AddLocal('ltmp1',2,0);
  { the offset needs to be the offset in the file, *not* relative to the start
    of the section. We don't know here yet how large the fpcresources section
    will be -> fix up later }
  ffpcreshandlessym:=fSymbolTable.AddGlobal('__fpc_reshandles_internal',3,0);
  { don't add this before any local symbols, as local symbols must be written
    first. We can't reorder while writing the symbol table, because we already
    need the symbol numbers above }
  ffpcresourcessym:=fSymbolTable.AddGlobal('FPC_RESSYMBOL',2,0);

  PrescanResourceTree;
  WriteResHeader(aStream,aResources);
  WriteNodeInfos(aStream);
  WriteResStringTable(aStream);
  WriteRawData(aStream);
  fRelocations.StartOfs:=aStream.Position;
  WriteRelocations(aStream);

  { fix up offset of fpcreshandles symbol }
  fSymbolTable.SetSymbolOffset(ffpcreshandlessym,fDataCurOfs);
  fSymbolTable.StartOfs:=aStream.Position;
  WriteSymbolTable(aStream);
  fMachOStringTable.StartOfs:=aStream.Position;
  WriteMachOStringTable(aStream);
  FixHeader(aStream);
  FixLoadCommands(aStream,aResources);
end;

constructor TAbstractMachOSubWriter.Create(aParent : TMachOResourceWriter;
  const aMachineType : TMachOMachineType; const aSubMachineType:
  TMachoSubMachineType; const aOppositeEndianess : boolean);
begin
  fParent:=aParent;
  fMachineType:=aMachineType;
  fSubMachineType:=aSubMachineType;
  fOppositeEndianess:=aOppositeEndianess;
  fRoot:=nil;
  fMachOStringTable:=TObjectStringTable.Create(nil,0);
  fRelocations:=TMachORelocations.Create(fMachineType,fOppositeEndianess);
  fResStrTable:=TResStringTable.Create;
  fCurOfs:=0;
  fDataCurOfs:=0;
  fSectionStart:=0;
end;

destructor TAbstractMachOSubWriter.Destroy;
begin
  fSymbolTable.Free;
  fResStrTable.Free;
  fRelocations.Free;
  fMachOStringTable.Free;
end;

{ TMachOResourceWriter }

procedure TMachOResourceWriter.SetDefaultTarget;
begin
  {$INCLUDE machodefaulttarget.inc}
end;

procedure TMachOResourceWriter.SetMachineType(const aMachineType: TMachOMachineType);
begin
  case aMachineType of
    mmtpowerpc   : begin fBits:=MACH_32BIT; fEndianess:=MACH_BIG_ENDIAN; end;
    mmtpowerpc64 : begin fBits:=MACH_64BIT; fEndianess:=MACH_BIG_ENDIAN; end;
    mmti386      : begin fBits:=MACH_32BIT; fEndianess:=MACH_LITTLE_ENDIAN; end;
    mmtx86_64    : begin fBits:=MACH_64BIT; fEndianess:=MACH_LITTLE_ENDIAN; end;
    mmtarm       : begin fBits:=MACH_32BIT; fEndianess:=MACH_LITTLE_ENDIAN; end;
    mmtarm64     : begin fBits:=MACH_64BIT; fEndianess:=MACH_LITTLE_ENDIAN; end;
  end;
  fMachineType:=aMachineType;
  fOppositeEndianess:=fNativeEndianess<>fEndianess;
end;

procedure TMachOResourceWriter.SetSubMachineType(const aSubMachineType: TMachoSubMachineType);
begin
  fSubMachineType:=aSubMachineType;
end;

function TMachOResourceWriter.GetExtensions: string;
begin
  Result:=fExtensions;
end;

function TMachOResourceWriter.GetDescription: string;
begin
  Result:=fDescription;
end;

procedure TMachOResourceWriter.Write(aResources: TResources; aStream: TStream);
var subwriter : TAbstractMachOSubWriter;
begin
  case fBits of
    MACH_32BIT : subwriter:=TMachO32SubWriter.Create(self,fMachineType,fSubMachineType,fOppositeEndianess);
    MACH_64BIT : subwriter:=TMachO64SubWriter.Create(self,fMachineType,fSubMachineType,fOppositeEndianess)
  else
    raise EMachOResourceWriterUnknownBitSizeException.Create('');
  end;
  try
    subwriter.Write(aResources,aStream);
  finally
    subwriter.Free;
  end;
end;

constructor TMachOResourceWriter.Create;
begin
  fExtensions:='.o .or';
  fDescription:='Mach-O resource writer';
  SetDefaultTarget;
end;

destructor TMachOResourceWriter.Destroy;
begin

end;

initialization
  TResources.RegisterWriter('.o',TMachOResourceWriter);
  TResources.RegisterWriter('.or',TMachOResourceWriter);

end.
