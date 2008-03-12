{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Resource reader for Mach-O files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit machoreader;

{$MODE OBJFPC} {$H+}

interface

uses
  Classes, SysUtils, resource, machotypes;
  
type

  { TMachOResourceReader }

  TMachOResourceReader = class (TAbstractResourceReader)
  private
    fDescription: string;
    fExtensions: string;
    fNativeEndianess : integer;
    fEndianess : integer;
    fOppositeEndianess : boolean;
    fMachineType : TMachOMachineType;
    fBits : integer;
    fHeader : TMachHdr;
    procedure SetDefaultTarget;
    function ReadMachOHeader(aStream : TStream) : boolean;
  protected
    function GetExtensions : string; override;
    function GetDescription : string; override;
    procedure Load(aResources : TResources; aStream : TStream); override;
    function CheckMagic(aStream : TStream) : boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property MachineType : TMachOMachineType read fMachineType;
  end;

implementation

uses machoconsts, resfactory, resourcetree, resdatastream, fpcrestypes;

type

  { TAbstractMachOSubReader }

  TAbstractMachOSubReader = class
  private
  protected
    fHeader : TMachHdr;
    fOppositeEndianess : boolean;
    fSegType : longword;
    fRoot : TRootResTreeNode;
    fParent : TMachOResourceReader;
    function ReadString(aStream : TStream; aPos : longword) : string;
    procedure ReadNode(aStream : TStream; aParent : TResourceTreeNode;
      aResources : TResources; named : boolean); virtual; abstract;
    procedure ReadResData(aStream : TStream; aNode : TResourceTreeNode;
      aResources : TResources; datasize : longword);
    procedure LoadResources(aResources : TResources; aStream : TStream);
    procedure Load(aResources : TResources; aStream : TStream); virtual; abstract;
  public
    constructor Create(aParent : TMachOResourceReader; const aHeader : TMachHdr;
      const aOppositeEndianess : boolean); virtual;
    destructor Destroy; override;
  end;
  
(*
Almost all differences in 32 and 64 bit mach-o files lie in record sizes.
Generics don't work with record types, so use macros to do this task
(uglier, but should be the same)
*)

{$MACRO ON}

//Define TMachO32SubReader

{$DEFINE _TMachOSubReader_:=TMachO32SubReader}
{$DEFINE _TSection_:=TSection32}
{$DEFINE _TResHdr_:=TResHdr32}
{$DEFINE _TResInfoNode_:=TResInfoNode32}
{$DEFINE _TSegmentCommand_:=TSegmentCommand32}
{$INCLUDE machosubreader.inc}

//Define TMachO64SubReader

{$DEFINE _TMachOSubReader_:=TMachO64SubReader}
{$DEFINE _TSection_:=TSection64}
{$DEFINE _TResHdr_:=TResHdr64}
{$DEFINE _TResInfoNode_:=TResInfoNode64}
{$DEFINE _TSegmentCommand_:=TSegmentCommand64}
{$INCLUDE machosubreader.inc}

//Clean all this stuff...
{$UNDEF _TMachOSubReader_}
{$UNDEF _TSection_}
{$UNDEF _TResHdr_}
{$UNDEF _TResInfoNode_}
{$UNDEF _TSegmentCommand_}

{ TAbstractMachOSubReader }

function TAbstractMachOSubReader.ReadString(aStream: TStream; aPos: longword
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

procedure TAbstractMachOSubReader.ReadResData(aStream: TStream;
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

procedure TAbstractMachOSubReader.LoadResources(aResources: TResources;
  aStream: TStream);
begin
  fRoot:=TRootResTreeNode(fParent.GetTree(aResources));
  ReadNode(aStream,nil,aResources,false);
end;

constructor TAbstractMachOSubReader.Create(aParent: TMachOResourceReader;
  const aHeader: TMachHdr; const aOppositeEndianess: boolean);
begin
  fParent:=aParent;
  fHeader:=aHeader;
  fOppositeEndianess:=aOppositeEndianess;
  fRoot:=nil;
end;

destructor TAbstractMachOSubReader.Destroy;
begin

end;

{ TMachOResourceReader }

procedure TMachOResourceReader.SetDefaultTarget;
begin
  {$INCLUDE machodefaulttarget.inc}
end;

function TMachOResourceReader.ReadMachOHeader(aStream: TStream): boolean;
var tmp : longword;
begin
  Result:=false;

  try
    aStream.ReadBuffer(fHeader,sizeof(fHeader));
  except
    on e : EReadError do exit;
  end;

  case fHeader.magic of
    MH_MAGIC    : begin fBits:=MACH_32BIT; fOppositeEndianess:=false; end;
    MH_MAGIC_64 : begin fBits:=MACH_64BIT; fOppositeEndianess:=false; end;
    MH_CIGAM    : begin fBits:=MACH_32BIT; fOppositeEndianess:=true; end;
    MH_CIGAM_64 : begin fBits:=MACH_64BIT; fOppositeEndianess:=true; end
    else exit;
  end;
  
  if fOppositeEndianess then
    case fNativeEndianess of
      MACH_BIG_ENDIAN    : fEndianess:=MACH_LITTLE_ENDIAN;
      MACH_LITTLE_ENDIAN : fEndianess:=MACH_BIG_ENDIAN;
    end
  else fEndianess:=fNativeEndianess;
  
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
  
  case fHeader.cputype of
    CPU_TYPE_I386      : fMachineType:=mmti386;
    CPU_TYPE_X86_64    : fMachineType:=mmtx86_64;
    CPU_TYPE_POWERPC   : fMachineType:=mmtpowerpc;
    CPU_TYPE_POWERPC64 : fMachineType:=mmtpowerpc64
    else exit;
  end;
  
  //64-bit mach-o files have 4 bytes of padding after the header
  if fBits=MACH_64BIT then
    try
      aStream.ReadBuffer(tmp,sizeof(tmp));
    except
      on e : EReadError do exit;
    end;

  Result:=true;
end;

function TMachOResourceReader.GetExtensions: string;
begin
  Result:=fExtensions;
end;

function TMachOResourceReader.GetDescription: string;
begin
  Result:=fDescription;
end;

procedure TMachOResourceReader.Load(aResources: TResources; aStream: TStream);
var subreader : TAbstractMachOSubReader;
begin
  if not ReadMachOHeader(aStream) then
    raise EResourceReaderWrongFormatException.Create('');
    
  case fBits of
    MACH_32BIT : subreader:=TMachO32SubReader.Create(self,fHeader,fOppositeEndianess);
    MACH_64BIT : subreader:=TMachO64SubReader.Create(self,fHeader,fOppositeEndianess);
  end;
  try
    try
      subreader.Load(aResources,aStream);
    except
      on e : EReadError do
        raise EResourceReaderUnexpectedEndOfStreamException.Create('');
    end;
  finally
    subreader.Free;
  end;
end;

function TMachOResourceReader.CheckMagic(aStream: TStream): boolean;
begin
  Result:=ReadMachOHeader(aStream);
end;

constructor TMachOResourceReader.Create;
begin
  fExtensions:='.o .or';
  fDescription:='Mach-O resource reader';
  SetDefaultTarget;
end;

destructor TMachOResourceReader.Destroy;
begin

end;

initialization
  TResources.RegisterReader('.o',TMachOResourceReader);
  TResources.RegisterReader('.or',TMachOResourceReader);
  TResources.RegisterReader('',TMachOResourceReader);

end.
