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

unit elfwriter;

{$MODE OBJFPC} {$H+}

interface

uses
  Classes, SysUtils, resource, elfconsts, elftypes;

type
  EElfResourceWriterException = class(EResourceWriterException);
  EElfResourceWriterUnknownMachineException = class(EElfResourceWriterException);
  EElfResourceWriterUnknownClassException = class(EElfResourceWriterException);
  EElfResourceWriterUnknownSectionException = class(EElfResourceWriterException);

type
  { TElfResourceWriter }

  TElfResourceWriter = class (TAbstractResourceWriter)
  private
    fExtensions : string;
    fDescription : string;
    fMachineTypeInt : integer;
    fMachineType : TElfMachineType;
    fOrder : byte;
    fBits : byte;
    fNativeOrder : integer;
    fOppositeEndianess : boolean;
    procedure SetDefaultTarget;
    procedure SetMachineType(const aMachineType : TElfMachineType);
    
    procedure WriteElfIdent(aStream : TStream);
  protected
    function GetExtensions : string; override;
    function GetDescription : string; override;
    procedure Write(aResources : TResources; aStream : TStream); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property MachineType : TElfMachineType read fMachineType write SetMachineType;
  end;


implementation

uses resourcetree, strtable, fpcrestypes;

type

  { TElfSections }

  TElfSections = class
  private
    fList : TFPList;
    fStringTable : TObjectStringTable;
    function GetCount : integer;
    function GetItem(index : integer) : PElf64SectHdr;
  protected
  public
    constructor Create(aStringTable : TObjectStringTable);
    destructor Destroy; override;
    function Add(const aName: string; const aType, aFlags : longword;
      const aOffset, aSize : qword; const aAddrAlign : longword) : integer; overload;
    function Add(const aName: string; const aType, aFlags : longword;
      const aOffset, aSize, aEntSize : qword; const aLink, aInfo,aAddrAlign
      : longword) : integer; overload;
    procedure Clear;
    property Count : integer read GetCount;
    property Items[index : integer] : PElf64SectHdr read GetItem; default;
  end;

  { TElfSymbolTable }

  TElfSymbolTable = class
  private
    fList : TFPList;
    fSectFree : integer;
    fLocFree : integer;
    fStringTable : TObjectStringTable;
    function CreateSym(const aName : string; const aValue, aSize : qword;
      const aBind, aType : byte; const aSectIdx : integer) : PElf64Symbol;
    procedure Clear;
    function GetCount : integer;
    function GetItem(index : integer) : PElf64Symbol;
  protected
  public
    constructor Create(aStringTable : TObjectStringTable);
    destructor Destroy; override;
    procedure AddSection(const aSectIdx : integer);
    procedure AddLocal(const aName : string; const aValue, aSize : qword;
      const aType : byte; const aSectIdx : integer);
    procedure AddGlobal(const aName : string; const aValue, aSize : qword;
      const aType : byte; const aSectIdx : integer);
    property Count : integer read GetCount;
    property Items[index : integer] : PElf64Symbol read GetItem; default;
    property FirstGlobal : integer read fLocFree;
  end;
  
  { TAbstractElfSubWriter }

  TAbstractElfSubWriter = class
  private
  protected
    fParent : TElfResourceWriter;
    fOppositeEndianess : boolean;
    fMachineType : integer;
    fDataAlignment : longword;
    fMachineFlags : longword;
    fRoot : TRootResTreeNode;
    fSectStringTable : TObjectStringTable;
    fSymStringTable  : TObjectStringTable;
    fResStringTable : TResStringTable;
    fSymbolTable : TElfSymbolTable;
    fSections : TElfSections;
    fShStrTabIdx : integer;
    fSymStrTabIdx : integer;
    fSymTabIdx : integer;
    fSectHdrOffset : qword;
    fCurOfs : longword;
    fDataCurOfs : longword;
    fSectionStart : qword;
    procedure Align(aBound : integer; aStream : TStream);
    function NextAligned(aBound, aValue : longword) : longword;
    procedure PrescanResourceTree; virtual; abstract;
    function PrescanNode(aNode : TResourceTreeNode; aNodeSize : longword) : longword;
    procedure WriteNodeInfo(aStream : TStream; aNode : TResourceTreeNode); virtual; abstract;
    procedure WriteSubNodes(aStream : TStream; aNode : TResourceTreeNode);
    procedure WriteResStringTable(aStream : TStream);
    procedure WriteRawData(aStream : TStream);
    procedure WriteResData(aStream : TStream; aNode : TResourceTreeNode);
    procedure AddEmptySections(aResources : TResources; aStream : TStream);
    procedure WriteStrTab(aStream : TStream);
    procedure WriteShStrTab(aStream : TStream);
    procedure Write(aResources : TResources; aStream : TStream); virtual; abstract;
  public
    constructor Create(aParent : TElfResourceWriter; const aMachineType : integer;
      const aOppositeEndianess : boolean); virtual;
    destructor Destroy; override;
  end;

type
  TElfRelocInfo = record
    RelocType : byte;
    SectionType : integer;
  end;
(*
Almost all differences in 32 and 64 bit elf files lie in record sizes.
Generics don't work with record types, so use macros to do this task
(uglier, but should be the same)
*)

{$MACRO ON}

//Define TElf32RelocTable and TElf32SubWriter

{$DEFINE _TElfRelocTable_:=TElf32RelocTable}
{$DEFINE _TPElfRela_:=PElf32Rela}
{$DEFINE _TElfRela_:=TElf32Rela}
{$DEFINE _TPElfRel_:=PElf32Rel}
{$DEFINE _TElfRel_:=TElf32Rel}
{$DEFINE _Tword_:=longword}
{$DEFINE _TElfSubWriter_:=TElf32SubWriter}
{$DEFINE _TElfHdr_:=TElf32Hdr}
{$DEFINE _TElfSectHdr_:=TElf32SectHdr}
{$DEFINE _TElfSymbol_:=TElf32Symbol}
{$DEFINE _TResHdr_:=TResHdr32}
{$DEFINE _TResInfoNode_:=TResInfoNode32}
{$INCLUDE elfsubwriter.inc}


//Define TElf64RelocTable and TElf32SubWriter

{$DEFINE _TElfRelocTable_:=TElf64RelocTable}
{$DEFINE _TPElfRela_:=PElf64Rela}
{$DEFINE _TElfRela_:=TElf64Rela}
{$DEFINE _TPElfRel_:=PElf64Rel}
{$DEFINE _TElfRel_:=TElf64Rel}
{$DEFINE _Tword_:=qword}
{$DEFINE _TElfSubWriter_:=TElf64SubWriter}
{$DEFINE _TElfHdr_:=TElf64Hdr}
{$DEFINE _TElfSectHdr_:=TElf64SectHdr}
{$DEFINE _TElfSymbol_:=TElf64Symbol}
{$DEFINE _TResHdr_:=TResHdr64}
{$DEFINE _TResInfoNode_:=TResInfoNode64}
{$INCLUDE elfsubwriter.inc}


//Clean all this stuff...

{$UNDEF _TElfRelocTable_}
{$UNDEF _TPElfRela_}
{$UNDEF _TElfRela_}
{$UNDEF _TPElfRel_}
{$UNDEF _TElfRel_}
{$UNDEF _Tword_}
{$UNDEF _TElfSubWriter_}
{$UNDEF _TElfHdr_}
{$UNDEF _TElfSectHdr_}
{$UNDEF _TElfSymbol_}
{$UNDEF _TResHdr_}
{$UNDEF _TResInfoNode_}


{ TElfSections }

function TElfSections.GetCount: integer;
begin
  Result:=fList.Count;
end;

function TElfSections.GetItem(index: integer): PElf64SectHdr;
begin
  Result:=PElf64SectHdr(fList[index]);
end;

constructor TElfSections.Create(aStringTable : TObjectStringTable);
begin
  fList:=TFPList.Create;
  fStringTable:=aStringTable;
  Add('',0,0,0,0,0); //empty section
end;

destructor TElfSections.Destroy;
var i : integer;
    p : PElf64SectHdr;
begin
  for i:=0 to fList.Count-1 do
  begin
    p:=PElf64SectHdr(fList[i]);
    FreeMem(p);
  end;

  fList.Free;
end;

function TElfSections.Add(const aName: string; const aType, aFlags : longword;
  const aOffset, aSize : qword; const aAddrAlign : longword) : integer;
begin
  Result:=Add(aName,aType,aFlags,aOffset,aSize,0,0,0,aAddrAlign);
end;

function TElfSections.Add(const aName: string; const aType, aFlags: longword;
  const aOffset, aSize, aEntSize: qword; const aLink, aInfo, aAddrAlign: longword): integer;
var p : PElf64SectHdr;
begin
  Result:=fList.Count;
  p:=GetMem(sizeof(TElf64SectHdr));
  p^.NameIdx:=fStringTable.Add(aName);
  p^._Type:=aType;
  p^.Flags:=aFlags;
  p^.Address:=0;
  p^.Offset:=aOffset;;
  p^.Size:=aSize;
  p^.Link:=aLink;
  p^.Info:=aInfo;
  p^.AddrAlign:=aAddrAlign;
  p^.EntSize:=aEntSize;
  fList.Add(p);
end;

procedure TElfSections.Clear;
var i : integer;
    p, first : PElf64SectHdr;
begin
  first:=PElf64SectHdr(fList[0]);
  for i:=1 to fList.Count-1 do
  begin
    p:=PElf64SectHdr(fList[i]);
    FreeMem(p);
  end;
  fList.Clear;
  fList.Add(first);
end;

{ TElfSymbolTable }

constructor TElfSymbolTable.Create(aStringTable: TObjectStringTable);
var p : PElf64Symbol;
begin
  fList:=TFPList.Create;
  fStringTable:=aStringTable;
  p:=CreateSym('',0,0,0,0,0);
  fList.Add(p);
  fSectFree:=1;
  fLocFree:=1;
end;

destructor TElfSymbolTable.Destroy;
begin
  Clear;
  fList.Free;
end;

procedure TElfSymbolTable.AddSection(const aSectIdx: integer);
var p : PElf64Symbol;
begin
  p:=CreateSym('',0,0,STB_LOCAL,STT_SECTION,aSectIdx);
  if fSectFree=fList.Count then
    fList.Add(p)
  else
    fList.Insert(fSectFree,p);
  inc(fSectFree);
  inc(fLocFree);
end;

procedure TElfSymbolTable.AddLocal(const aName: string; const aValue,
  aSize: qword; const aType: byte; const aSectIdx: integer);
var p : PElf64Symbol;
begin
  p:=CreateSym(aName,aValue,aSize,STB_LOCAL,aType,aSectIdx);
  if fLocFree=fList.Count then
    fList.Add(p)
  else
    fList.Insert(fLocFree,p);
  inc(fLocFree);
end;

procedure TElfSymbolTable.AddGlobal(const aName: string; const aValue,
  aSize: qword; const aType: byte; const aSectIdx: integer);
var p : PElf64Symbol;
begin
  p:=CreateSym(aName,aValue,aSize,STB_GLOBAL,aType,aSectIdx);
  fList.Add(p)
end;

procedure TElfSymbolTable.Clear;
var p : PElf64Symbol;
    i : integer;
begin
  for i:=0 to fList.Count-1 do
  begin
    p:=PElf64Symbol(fList[i]);
    FreeMem(p);
  end;
  fList.Clear;
end;

function TElfSymbolTable.GetCount: integer;
begin
  Result:=fList.Count;
end;

function TElfSymbolTable.GetItem(index: integer): PElf64Symbol;
begin
  Result:=PElf64Symbol(fList[index]);
end;

function TElfSymbolTable.CreateSym(const aName : string; const aValue, aSize : qword;
      const aBind, aType : byte; const aSectIdx : integer) : PElf64Symbol;
var p : PElf64Symbol;
begin
  p:=GetMem(sizeof(TElf64Symbol));
  p^.Name:=fStringTable.Add(aName);
  p^.Value:=aValue;
  p^.Size:=aSize;
  p^.Info:=aBind shl 4;
  p^.Info:=p^.Info or (aType and $0F);
  p^.Other:=0;
  p^.SectIdx:=aSectIdx;
  Result:=p;
end;

{ TAbstractElfSubWriter }

procedure TAbstractElfSubWriter.Align(aBound : integer; aStream : TStream);
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

function TAbstractElfSubWriter.NextAligned(aBound, aValue : longword) : longword;
var topad : longword;
begin
  Result:=aValue;
  topad:=aBound-(aValue mod aBound);
  if topad<>aBound then inc(Result,topad);
end;

function TAbstractElfSubWriter.PrescanNode(aNode: TResourceTreeNode;
  aNodeSize: longword): longword;
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
    aNode.NameRVA:=fResStringTable.Add(aNode.Desc.Name);

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

procedure TAbstractElfSubWriter.WriteSubNodes(aStream: TStream;
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

procedure TAbstractElfSubWriter.WriteResStringTable(aStream: TStream);
begin
  if fResStringTable.Used then
    fResStringTable.WriteToStream(aStream);
  Align(fDataAlignment,aStream);
end;


procedure TAbstractElfSubWriter.WriteRawData(aStream: TStream);
begin
  WriteResData(aStream,fRoot);
end;

procedure TAbstractElfSubWriter.WriteResData(aStream: TStream;
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

procedure TAbstractElfSubWriter.AddEmptySections(aResources : TResources; aStream: TStream);
begin
  Align(fDataAlignment,aStream);
  fSections.Add(HandlesSectName,SHT_NOBITS,SHF_ALLOC or SHF_WRITE,
    aStream.Position,fDataAlignment*aResources.Count,fDataAlignment);
  fSections.Add('.text',SHT_PROGBITS,SHF_ALLOC or SHF_EXECINSTR,aStream.Position,0,4);
  fSections.Add('.data',SHT_PROGBITS,SHF_ALLOC or SHF_WRITE,aStream.Position,0,4);
  fSections.Add('.bss', SHT_NOBITS,SHF_ALLOC or SHF_WRITE,aStream.Position,0,4);
end;

procedure TAbstractElfSubWriter.WriteStrTab(aStream: TStream);
begin
  fSymStrTabIdx:=fSections.Add('.strtab',SHT_STRTAB,0,aStream.Position,
    fSymStringTable.Size,1);
  fSymStringTable.WriteToStream(aStream);
end;

procedure TAbstractElfSubWriter.WriteShStrTab(aStream: TStream);
const namelen = length('.shstrtab')+1;
begin
  fShStrTabIdx:=fSections.Add('.shstrtab',SHT_STRTAB,0,aStream.Position,
    fSectStringTable.Size+namelen,1);
  fSectStringTable.WriteToStream(aStream);
end;

constructor TAbstractElfSubWriter.Create(aParent : TElfResourceWriter;
  const aMachineType: integer; const aOppositeEndianess: boolean);
begin
  fMachineType:=aMachineType;
  fOppositeEndianess:=aOppositeEndianess;
  fRoot:=nil;
  fParent:=aParent;
  fSectStringTable:=TObjectStringTable.Create(nil,0);
  fSymStringTable:=TObjectStringTable.Create(nil,0);
  fResStringTable:=TResStringTable.Create;
  fSymbolTable:=TElfSymbolTable.Create(fSymStringTable);
  fSections:=TElfSections.Create(fSectStringTable);
  fShStrTabIdx:=0;
  fSymStrTabIdx:=0;
  fSectHdrOffset:=0;
  fCurOfs:=0;
  fDataCurOfs:=0;
  fSectionStart:=0;
end;

destructor TAbstractElfSubWriter.Destroy;
begin
  fSectStringTable.Free;
  fSymStringTable.Free;
  fResStringTable.Free;
  fSymbolTable.Free;
  fSections.Free;
end;

{ TElfResourceWriter }

procedure TElfResourceWriter.SetDefaultTarget;
begin
  {$INCLUDE elfdefaulttarget.inc}
end;

procedure TElfResourceWriter.SetMachineType(const aMachineType: TElfMachineType);
begin
  case aMachineType of
    emtsparc  : begin fMachineTypeInt:=EM_SPARC; fBits:=ELFCLASS32; fOrder:=ELFDATA2MSB; end;
    emti386   : begin fMachineTypeInt:=EM_386; fBits:=ELFCLASS32; fOrder:=ELFDATA2LSB; end;
    emtm68k   : begin fMachineTypeInt:=EM_68K; fBits:=ELFCLASS32; fOrder:=ELFDATA2MSB; end;
    emtppc    : begin fMachineTypeInt:=EM_PPC; fBits:=ELFCLASS32; fOrder:=ELFDATA2MSB; end;
    emtppc64  : begin fMachineTypeInt:=EM_PPC64; fBits:=ELFCLASS64; fOrder:=ELFDATA2MSB; end;
    emtarm    : begin fMachineTypeInt:=EM_ARM; fBits:=ELFCLASS32; fOrder:=ELFDATA2LSB; end;
    emtarmeb  : begin fMachineTypeInt:=EM_ARM; fBits:=ELFCLASS32; fOrder:=ELFDATA2MSB; end;
    emtalpha  : begin fMachineTypeInt:=EM_ALPHA; fBits:=ELFCLASS64; fOrder:=ELFDATA2LSB; end;
    emtia64   : begin fMachineTypeInt:=EM_IA_64; fBits:=ELFCLASS64; fOrder:=ELFDATA2LSB; end;
    emtx86_64 : begin fMachineTypeInt:=EM_X86_64; fBits:=ELFCLASS64; fOrder:=ELFDATA2LSB; end
    else
      raise EElfResourceWriterUnknownMachineException.Create('');
  end;
  fMachineType:=aMachineType;
  fOppositeEndianess:=fNativeOrder<>fOrder;
end;

procedure TElfResourceWriter.WriteElfIdent(aStream: TStream);
var ident : TElfIdent;
begin
  ident.Magic:=ELFMAGIC;
  ident.ElfClass:=fBits;
  ident.ElfData:=fOrder;
  ident.ElfVersion:=EV_CURRENT;
  ident.OsAbi:=ELFOSABI_NONE; // UNIX System V ABI
  ident.AbiVersion:=0;
  FillByte(ident.Padding[9],length(ident.Padding),0);
  
  aStream.WriteBuffer(ident,sizeof(ident));
end;

function TElfResourceWriter.GetExtensions: string;
begin
  Result:=fExtensions;
end;

function TElfResourceWriter.GetDescription: string;
begin
  Result:=fDescription;
end;

procedure TElfResourceWriter.Write(aResources: TResources; aStream: TStream);
var subwriter : TAbstractElfSubWriter;
begin
  WriteElfIdent(aStream);
  case fBits of
    ELFCLASS32 : subwriter:=TElf32SubWriter.Create(self,fMachineTypeInt,fOppositeEndianess);
    ELFCLASS64 : subwriter:=TElf64SubWriter.Create(self,fMachineTypeInt,fOppositeEndianess)
  else
    raise EElfResourceWriterUnknownClassException.Create('');
  end;
  try
    subwriter.Write(aResources,aStream);
  finally
    subwriter.Free;
  end;
end;

constructor TElfResourceWriter.Create;
begin
  fExtensions:='.o .or';
  fDescription:='ELF resource writer';
  SetDefaultTarget;
end;

destructor TElfResourceWriter.Destroy;
begin

end;

initialization
  TResources.RegisterWriter('.o',TElfResourceWriter);
  TResources.RegisterWriter('.or',TElfResourceWriter);

end.
