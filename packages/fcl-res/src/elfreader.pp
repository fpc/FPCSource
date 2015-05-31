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

unit elfreader;

{$MODE OBJFPC} {$H+}

interface

uses
  Classes, SysUtils, resource, elfconsts, elftypes;

type
  EElfResourceReaderException = class(EResourceReaderException);
  EElfResourceReaderUnknownClassException = class(EElfResourceReaderException);
  EElfResourceReaderUnknownVersionException = class(EElfResourceReaderException);
  EElfResourceReaderNoSectionsException = class(EElfResourceReaderException);
  EElfResourceReaderNoStringTableException = class(EElfResourceReaderException);

type

  { TElfResourceReader }

  TElfResourceReader = class (TAbstractResourceReader)
  private
    fDescription: string;
    fExtensions: string;
    fMachineType : TElfMachineType;
    fOrder : byte;
    fBits : byte;
    fNativeOrder : integer;
    fOppositeEndianess : boolean;
    procedure SetDefaultTarget;
    function ReadElfIdent(aStream : TStream) : boolean;
  protected
    function GetExtensions : string; override;
    function GetDescription : string; override;
    procedure Load(aResources : TResources; aStream : TStream); override;
    function CheckMagic(aStream : TStream) : boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property MachineType : TElfMachineType read fMachineType;
  end;

implementation

uses resdatastream, resfactory, resourcetree, strtable, fpcrestypes;

type

  { TAbstractElfSubReader }

  TAbstractElfSubReader = class
  private
  protected
    fParent : TElfResourceReader;
    fRoot : TRootResTreeNode;
    fOppositeEndianess : boolean;
    fMachineType : integer;
    fStringTable : TObjectStringTable;
    fSectIdx : integer;
    fRelSectIdx : integer;
    dummyDesc : TResourceDesc;
    fNeedsReloc : boolean;

    function FindSection(const aName : string) : integer; virtual; abstract;
    function FindResSection : boolean;
    function ReadString(aStream : TStream; aPos : longword) : string;
    procedure ReadNode(aStream : TStream; aParent : TResourceTreeNode;
      aResources : TResources; named : boolean); virtual; abstract;
    procedure ReadResData(aStream : TStream; aNode : TResourceTreeNode;
      aResources : TResources; datasize : longword);
    procedure LoadResources(aResources : TResources; aStream : TStream);
    procedure Load(aResources : TResources; aStream : TStream); virtual; abstract;
  public
    constructor Create(aParent : TElfResourceReader; const aOppositeEndianess : boolean);
    destructor Destroy; override;
    property MachineType : integer read fMachineType;
  end;
  
(*
Almost all differences in 32 and 64 bit elf files lie in record sizes.
Generics don't work with record types, so use macros to do this task
(uglier, but should be the same)
*)

{$MACRO ON}

//Define TElf32RelocTable and TElf32SubReader

{$DEFINE _TElfRelocTable_:=TElf32RelocTable}
{$DEFINE _TPElfRela_:=PElf32Rela}
{$DEFINE _TElfRela_:=TElf32Rela}
{$DEFINE _Tword_:=longword}
{$DEFINE _TElfSubReader_:=TElf32SubReader}
{$DEFINE _TElfHdr_:=TElf32Hdr}
{$DEFINE _TElfSectHdr_:=TElf32SectHdr}
{$DEFINE _TResHdr_:=TResHdr32}
{$DEFINE _TResInfoNode_:=TResInfoNode32}
{$INCLUDE elfsubreader.inc}


//Define TElf64RelocTable and TElf64SubReader

{$DEFINE _TElfRelocTable_:=TElf64RelocTable}
{$DEFINE _TPElfRela_:=PElf64Rela}
{$DEFINE _TElfRela_:=TElf64Rela}
{$DEFINE _Tword_:=qword}
{$DEFINE _TElfSubReader_:=TElf64SubReader}
{$DEFINE _TElfHdr_:=TElf64Hdr}
{$DEFINE _TElfSectHdr_:=TElf64SectHdr}
{$DEFINE _TResHdr_:=TResHdr64}
{$DEFINE _TResInfoNode_:=TResInfoNode64}
{$INCLUDE elfsubreader.inc}


//Clean all this stuff...

{$UNDEF _TElfRelocTable_}
{$UNDEF _TPElfRela_}
{$UNDEF _TElfRela_}
{$UNDEF _Tword_}
{$UNDEF _TElfSubReader_}
{$UNDEF _TElfHdr_}
{$UNDEF _TElfSectHdr_}
{$UNDEF _TResHdr_}
{$UNDEF _TResInfoNode_}


{ TAbstractElfSubReader }

function TAbstractElfSubReader.FindResSection : boolean;
begin
  fSectIdx:=FindSection(RsrcSectName);
  Result:=fSectIdx<>-1;
  if not Result then exit;
  if fNeedsReloc then
  begin
    fRelSectIdx:=FindSection('.rela'+RsrcSectName);
    Result:=fRelSectIdx<>-1;
  end;
end;

function TAbstractElfSubReader.ReadString(aStream: TStream; aPos: longword
  ): string;
var oldpos : int64;
    c : char;
    maxleft : int64;
begin
  Result:='';
  oldpos:=aStream.Position;
  aStream.Position:=aPos;
  aStream.ReadBuffer(c,1);
  maxleft:=aStream.Size-aStream.Position;
  while (c<>#0) and (maxleft>=0) do
  begin
    Result:=Result+c;
    aStream.ReadBuffer(c,1);
    dec(maxleft);
  end;
  aStream.Position:=oldpos;
end;

procedure TAbstractElfSubReader.ReadResData(aStream: TStream;
  aNode: TResourceTreeNode; aResources: TResources; datasize: longword);
var aRes : TAbstractResource;
    RawData : TResourceDataStream;
begin
  aRes:=aNode.CreateResource;
  if aRes=nil then
    raise EResourceDuplicateException.CreateFmt(SResDuplicate,[
      aNode.Data._Type.Name,aNode.Data.Name.Name,aNode.Data.LangID]);
  fParent.SetDataSize(aRes,datasize);
  fParent.SetDataOffset(aRes,aStream.Position);
  RawData:=TResourceDataStream.Create(aStream,aRes,aRes.DataSize,TCachedResourceDataStream);
  fParent.SetRawData(aRes,RawData);
  fParent.AddNoTree(aResources,aRes);
end;

procedure TAbstractElfSubReader.LoadResources(aResources: TResources;
  aStream: TStream);
begin
  fRoot:=TRootResTreeNode(fParent.GetTree(aResources));
  ReadNode(aStream,nil,aResources,false);
end;

constructor TAbstractElfSubReader.Create(aParent : TElfResourceReader;
  const aOppositeEndianess: boolean);
begin
  fParent:=aParent;
  fOppositeEndianess:=aOppositeEndianess;
  fMachineType:=EM_386;
  fSectIdx:=0;
  fRelSectIdx:=0;
  fStringTable:=nil;
  dummyDesc:=TResourceDesc.Create;
  fRoot:=nil;
  fNeedsReloc:=false;
end;

destructor TAbstractElfSubReader.Destroy;
begin
  if fStringTable<>nil then fStringTable.Free;
  dummyDesc.Free;
end;

{ TElfResourceReader }

procedure TElfResourceReader.SetDefaultTarget;
begin
  {$INCLUDE elfdefaulttarget.inc}
end;

function TElfResourceReader.ReadElfIdent(aStream : TStream) : boolean;
var ident : TElfIdent;
begin
  Result:=false;

  try
    aStream.ReadBuffer(ident,sizeof(ident));
  except
    on e : EReadError do exit;
  end;

  if ident.Magic<>ELFMAGIC then exit;
  fBits:=ident.ElfClass;
  fOppositeEndianess:=ident.ElfData<>fNativeOrder;
  fOrder:=ident.ElfData;
  if ident.ElfVersion<>EV_CURRENT then exit;

  Result:=true;
end;

function TElfResourceReader.GetExtensions: string;
begin
  Result:=fExtensions;
end;

function TElfResourceReader.GetDescription: string;
begin
  Result:=fDescription;
end;

procedure TElfResourceReader.Load(aResources: TResources; aStream: TStream);
var subreader : TAbstractElfSubReader;
begin
  if not ReadElfIdent(aStream) then
    raise EResourceReaderWrongFormatException.Create('');
  case fBits of
    ELFCLASS32 : subreader:=TElf32SubReader.Create(self,fOppositeEndianess);
    ELFCLASS64 : subreader:=TElf64SubReader.Create(self,fOppositeEndianess)
  else
    raise EElfResourceReaderUnknownClassException.Create('');
  end;
  try
    try
      subreader.Load(aResources,aStream);
    except
      on e : EReadError do
        raise EResourceReaderUnexpectedEndOfStreamException.Create('');
    end;
    case subreader.MachineType of
      EM_SPARC  : fMachineType:=emtsparc;
      EM_386    : fMachineType:=emti386;
      EM_68K    : fMachineType:=emtm68k;
      EM_PPC    : fMachineType:=emtppc;
      EM_PPC64  : fMachineType:=emtppc64;
      EM_ARM    : if fOrder=ELFDATA2LSB then
                    fMachineType:=emtarm
                  else
                    fMachineType:=emtarmeb;
      EM_AARCH64: fMachineType:=emtaarch64;
      EM_ALPHA  : fMachineType:=emtalpha;
      EM_IA_64  : fMachineType:=emtia64;
      EM_X86_64 : fMachineType:=emtx86_64;
      EM_MIPS   : if fOrder=ELFDATA2LSB then
                    fMachineType:=emtmipsel
                  else
                    fMachineType:=emtmips;
    end;
  finally
    subreader.Free;
  end;
end;

function TElfResourceReader.CheckMagic(aStream: TStream): boolean;
begin
  Result:=ReadElfIdent(aStream);
end;

constructor TElfResourceReader.Create;
begin
  fExtensions:='.o .or';
  fDescription:='ELF resource reader';
  SetDefaultTarget;
end;

destructor TElfResourceReader.Destroy;
begin

end;

initialization
  TResources.RegisterReader('.o',TElfResourceReader);
  TResources.RegisterReader('.or',TElfResourceReader);
  TResources.RegisterReader('',TElfResourceReader);

end.
