{ *********************************************************************** }
{                                                                         }
{  fpcres2elf - Free Pascal Resource to ELF object compiler               }
{  Part of the Free Pascal and CrossFPC distributions                     }
{                                                                         }
{  Copyright (C) 2005 Simon Kissel                                        }
{                                                                         }
{  See the file COPYING.FPC, included in the FPC distribution,            }
{  for details about the copyright.                                       }
{                                                                         }
{  This program is distributed in the hope that it will be useful,        }
{  but WITHOUT ANY WARRANTY; without even the implied warranty of         }
{  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                   }
{                                                                         }
{ *********************************************************************** }

{
This unit will compile an ELF object file out of a supplied .res resource
file. Optionally, Delphi/Kylix dfm/xfm form files are also accepted as
input. These will automatically be converted into resources internally.

fpcres2elf builds with Delphi, Kylix and FPC.

Currently this only works on 32Bit targets, but support for 64Bit targets
is in the works. Support for big endian systems is completely missing,
though.

Format used for the various resource sections:

 fpc.resptrs:   This section is contained in resptrs.o and always linked to the executable by
                FPC. It containes an exported label fpcrespointers, which is used at runtime
                to find the resptrs section in memory. The resptrs contains pointers to all the
                sections and their sizes. These are updated in a post-precessing step by the
                compiler and by the external resource embedder when applied to an ELF file.
                This section always is 128 Bytes long and initially filled with zeros.
                The first integer (32/64 Bit) value in this section contains the version
                number of the resource system. Currently this value is 1.
                The second integer (32/64 Bit) value in this section contains the number of
                resources.
                After this follows a version-defined number of TFPCResourceSectionInfo entries.
 fpc.ressym:    Contains the resource names. This simply is a stream of zero-terminated strings.
                Only textual names are supported, numeric IDs get autoconverted to #ID's.
                The reshash table has got a byte index into ressym to quickly get that data if needed
 fpc.reshash:   n TFPCResourceInfo records. (number of entries is defined in fpc.resptrs)
 fpc.resdata:   Contains the plain resource data stream. A byte index into the data stream
                is given for each resource entry in TResourceInfo
 fpc.resspare:  An empty section which is resized to make room if the size of any of the previous
                sections gets changed. NOT USED IN VERSION 1 (SIZE WILL BE 0)
 fpc.resstr:    This section is completely seperated from the rest and contains a block of
                resourcestrings in internal FPC format.

resptr TFPCResourceSectionInfo list for FPC resources version 1:

Index   Pointer to
0       ressym
1       reshash
2       resdata
3       resspare
4       resstr
5       reserved for future extension (stabs)
6       reserved for future extension
}
{$ifdef fpc}
{$mode objfpc}
{$endif}
{$H+}
unit elfres;

interface

uses
  elfbfd,
  SysUtils,
  Classes;

const fpcres2elf_version=1;

// Do not change the following consts, they are dummy tables to generate an .o that makes ld happy
const shstrtab = #0+'.symtab'+#0+'.strtab'+#0+'.shstrtab'+#0+'.text'+#0+'.data'+#0+
                 '.bss'+#0+'fpc.ressym'+#0+'fpc.resstr'+#0+'fpc.reshash'+#0+
                 'fpc.resdata'+#0+'fpc.resspare'+#0+#0;
      symtab =   #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00+
                 #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$03#$00#$01#$00+
                 #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$03#$00#$02#$00+
                 #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$03#$00#$03#$00+
                 #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$03#$00#$04#$00+
                 #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$03#$00#$05#$00+
                 #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$03#$00#$06#$00+
                 #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$03#$00#$07#$00+
                 #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$03#$00#$08#$00;
      strtab =   #$00#$00; // this actually is just one byte long
      zeros  =   #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00;
      fake   =   'fakefakefakefakefakefakefakefake';

      // header of a windows 32 bit .res file (16 bytes)
      reshdr =   #$00#$00#$00#$00#$20#$00#$00#$00#$FF#$FF#$00#$00#$FF#$FF#$00#$00+
                 #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00;

      FilerSignature: array[1..4] of Char = 'TPF0';

      SDefaultExtension = '.or';
      
Type
  { TElfResCreator }

  TElfResCreator = Class(TObject)
  private
    FDestFileName: String;
    FExtension: string;
    FSourceFileName: String;
    FOverwrite: Boolean;
    FVerbose: Boolean;
    FVersion: Integer;
  Protected
    FSectionStream: TMemoryStream;
    FDataStream: TMemoryStream;
    FSymStream: TMemoryStream;
    FHashStream: TMemoryStream;
    sectionheader_ofs: integer;
    shstrtab_ofs: integer;
    CurrentResource:integer;
    resheader: string;
    Signature: byte;
    Procedure AllocateData; virtual;
    Procedure FreeData; virtual;
    Procedure DoAlign(const a: integer);
  Public
    Constructor Create;
    Procedure Convert(Const Source,Destination : String);
    Procedure ConvertStreams(Source,Dest : TStream);
    Procedure DoConvertStreams(Source,Dest : TStream); virtual;Abstract;
    Property  Verbose : Boolean Read FVerbose Write FVerbose;
    Property  SourceFileName : String Read FSourceFileName;
    Property  DestFileName : String Read FDestFileName;
    Property  Overwrite : Boolean Read FOverwrite Write FOverWrite;
    Property  Version : Integer Read FVersion Write FVersion;
    Property  Extension : string Read FExtension Write FExtension;
  end;
  
  { TElf32ResCreator }

  TElf32ResCreator = Class(TElfResCreator)
  Private
    ResourceEntries: array of TELF32ResourceInfo;
  Protected
    Procedure AllocateData; override;
    Procedure FreeData; override;
  public
    procedure LoadBinaryDFMEntry(const rs:TStream; const DataStream:TMemoryStream; const SymStream:TMemoryStream; var resinfo:TELF32ResourceInfo);
    procedure LoadTextDFMEntry(const rs:TStream; const DataStream:TMemoryStream; const SymStream:TMemoryStream; var resinfo:TELF32ResourceInfo);
    procedure LoadRESEntry(const rs:TStream; const DataStream:TMemoryStream; const SymStream:TMemoryStream; var resinfo:TELF32ResourceInfo);
    Procedure DoConvertStreams(Source,Dest : TStream); override;
  end;
  
  { TElf64Creator }

  TElf64ResCreator = Class(TElfResCreator)
    Procedure DoConvertStreams(Source,Dest : TStream); override;
  end;

  EElfResError = Class(Exception);

implementation

resourcestring
  SErrUnrecognizedFormat = 'Unrecognized file format for input file "%s"';

Procedure DoError (Msg : String);

begin
  Raise EElfResError.Create(Msg);
end;

Procedure DoErrorFmt (Msg : String; Args : Array of const);

begin
  Raise EElfResError.CreateFmt(Msg,Args);
end;

function HashELF(const S : string) : longint;
{Note: this hash function is described in "Practical Algorithms For
       Programmers" by Andrew Binstock and John Rex, Addison Wesley,
       with modifications in Dr Dobbs Journal, April 1996}
var
  G : longint;
  i : integer;
begin
  Result := 0;
  for i := 1 to length(S) do begin
    Result := (Result shl 4) + ord(S[i]);
    G := Result and $F0000000;
    if (G <> 0) then
      Result := Result xor (G shr 24);
    Result := Result and (not G);
  end;
end;

{ TElfResCreator }

procedure TElfResCreator.AllocateData;
begin
  FSectionStream:=TMemoryStream.Create;
  FDataStream:=TMemoryStream.Create;
  FSymStream:=TMemoryStream.Create;
  FHashStream:=TMemoryStream.Create;
end;

procedure TElfResCreator.FreeData;
begin
  FreeAndNil(FSectionStream);
  FreeAndNil(FDataStream);
  FreeAndNil(FSymStream);
  FreeAndNil(FHashStream);
end;

constructor TElfResCreator.Create;
begin
  FVersion:=fpcres2elf_version;
  FOverwrite:=True;
  FVerbose:=False;
  FExtension:=SDefaultExtension;
end;

// fill the memorystream so it is aligned, max supported align is 16
procedure TElfResCreator.DoAlign(const a: integer);
var i: integer;
begin
  i:=(4 - (FSectionStream.position MOD a)) MOD a;
  if (i>0) then FSectionStream.Write(zeros,i);
end;

procedure TElfResCreator.Convert(const Source, Destination: String);

Var
  Src,Dest : TFileStream;

begin
  FSourceFileName:=Source;
  FDestFileName:=Destination;
  if FDestFileName='' then
    FDestFileName:=ChangeFileExt(Source,FExtension);
  Src:=TFileStream.Create(FSourceFileName,fmOpenRead or fmShareDenyWrite);
  try
    Dest:=TFileStream.Create(FDestFileName,fmCreate or fmShareDenyWrite);
    Try
      ConvertStreams(Src,Dest);
    Finally
      Dest.Free;
    end;
  Finally
    Src.Free;
  end;
end;

procedure TElfResCreator.ConvertStreams(Source, Dest: TStream);
begin
  AllocateData;
  Try
    DoConvertStreams(Source,Dest);
  Finally
    FreeData;
  end;
end;

{ ---------------------------------------------------------------------
  TElf32ResCreator
  ---------------------------------------------------------------------}

procedure TElf32ResCreator.AllocateData;
begin
  inherited AllocateData;
  // reserve space for 1024 resource entries for now
  SetLength(ResourceEntries,1024);
  CurrentResource:=0;
end;

procedure TElf32ResCreator.FreeData;
begin
  inherited FreeData;
  SetLength(ResourceEntries,0);
end;

procedure TElf32ResCreator.LoadRESEntry(const rs:TStream; const DataStream:TMemoryStream; const SymStream:TMemoryStream; var resinfo:TELF32ResourceInfo);
var l:longint;
    w:word;
    ws:WideString;
    wc:WideChar;
    name:string;
    i: integer;
    headersize:integer;
    headerstart:integer;
begin
  headerstart:=rs.Position;
  resinfo.ptr:=DataStream.Position;
  rs.Read(resinfo.size,4);
  rs.Read(headersize,4);
  rs.Read(l,4); // Type
  if (l AND $0000FFFF)=$0000FFFF then
  begin // Type is stored as ID
    resinfo.restype:=(l AND $FFFF0000) shr 16; // kill the marker, we always use IDs
  end
  else
  begin
    // we don't support text IDs for now, skip until we have reached the end
    // of the widestring, and set rcdata (10) in this case.
    repeat
      rs.Read(w,2);
    until w=0;
    resinfo.restype:=10;
  end;

  rs.Read(l,4); // Name
  if (l AND $0000FFFF)=$0000FFFF then
  begin // Name is stored as ID.
    l:=(l AND $FFFF0000) shr 16; // kill the marker
    // We don't want to support integer names, we'll instead convert them to a #id string
    // which is more common.
    name:='#'+inttostr(l);
  end
  else
  begin
    // Ok, it's a widestring ID
    ws:=widechar(l AND $0000FFFF);
    ws:=ws+widechar((l AND $FFFF0000) shr 16);
    // get the rest of it
    repeat
      rs.Read(wc,2);
      if wc<>#0 then ws:=ws+wc;
    until wc=#0;
    // convert to ANSI
    name:=ws;
  end;

  // create a hash of the name
  resinfo.reshash:=HashELF(name);

  // save the name plus a trailing #0 to the SymStream, also save
  // the position of this name in the SymStream
  resinfo.name:=SymStream.Position;
  name:=name+#0;
  SymStream.Write(name[1],length(name));

  // We don't care about the rest of the header
  rs.Seek(headersize-(rs.position-headerstart),soFromCurrent);

  // Now copy over the resource data into our internal memory stream
  DataStream.CopyFrom(rs,resinfo.size);

  // Align the resource stream on a dword boundary
  i:=(4 - (rs.Position MOD 4)) MOD 4;
  if (i>0) then rs.Seek(i,soFromCurrent);


  // Align the data stream on a dword boundary
  i:=(4 - (DataStream.Position MOD 4)) MOD 4;
  if (i>0) then DataStream.Write(zeros,i);
end;

procedure TElf32ResCreator.LoadBinaryDFMEntry(const rs:TStream; const DataStream:TMemoryStream; const SymStream:TMemoryStream; var resinfo:TELF32ResourceInfo);
var name: string;
    i: integer;
begin
  resinfo.ptr:=0;
  resinfo.restype:=10; // RCDATA

  // Skip the header
  rs.Position:=3;

  // Read the name
  setlength(name,64); // Component names can be 64 chars at max
  rs.Read(name[1],64);

  // Find end of name
  i:=pos(#0,name);
  name:=copy(name,1,i-1);

  // Seek to after the name and skip other crap
  rs.Position:=i+9;

  resinfo.size:=rs.Size-rs.Position;

  // ...this is followed by the data.

  // create a hash of the name
  resinfo.reshash:=HashELF(name);

  // save the name plus a trailing #0 to the SymStream, also save
  // the position of this name in the SymStream
  resinfo.name:=SymStream.Position;
  name:=name+#0;
  SymStream.Write(name[1],length(name));

  // Now copy over the resource data into our internal memory stream
  DataStream.CopyFrom(rs,resinfo.size);

  // Align the data stream on a dword boundary
  i:=(4 - (DataStream.Position MOD 4)) MOD 4;
  if (i>0) then DataStream.Write(zeros,i);
end;

procedure TElf32ResCreator.LoadTextDFMEntry(const rs:TStream; const DataStream:TMemoryStream; const SymStream:TMemoryStream; var resinfo:TELF32ResourceInfo);
var ms:TMemoryStream;
begin
  ms:=nil;
  try
    ms:=TMemoryStream.Create;
    ObjectTextToResource(rs,ms);
    LoadBinaryDFMEntry(ms, DataStream, SymStream, resinfo);
  finally
    ms.free;
  end;
end;

procedure TElf32ResCreator.DoConvertStreams(Source, Dest: TStream);

Var
  I : Integer;
  ElfHeader: TElf32Header;
  SectionHeader: TElf32sechdr;
  ressym: TELF32ResourceSectionInfo;
  resstr: TELF32ResourceSectionInfo;
  reshash: TELF32ResourceSectionInfo;
  resdata: TELF32ResourceSectionInfo;
  resspare: TELF32ResourceSectionInfo;
  
begin
  // Read and check the header of the input file. First check if it's a 32bit resource
  // file...
  SetLength(resheader,32);
  Source.Read(resheader[1],32);
  if (resheader<>reshdr) then
    begin
    // ...not a 32Bit resource file. Now let's see if it's a text or binary dfm/xfm/lfm file.
    Source.Position:=0;
    Source.Read(Signature,1);
    if (Signature=$FF) then
      begin
      Source.Position:=0;
      LoadBinaryDFMEntry(Source, FDataStream, FSymStream, ResourceEntries[CurrentResource]);
      end
    else if char(Signature) in ['o','O','i','I',' ',#13,#11,#9] then
      begin
      Source.Position:=0;
      LoadTextDFMEntry(Source, FDataStream, FSymStream, ResourceEntries[CurrentResource]);
      end
    else
      DoErrorFmt(SErrUnrecognizedFormat,[SourceFileName]);
    inc(CurrentResource,1);
    end
  else // ...yes, it's a resource file.
    while Source.Position<Source.Size do
      begin
      // Load Resource info, and copy the resource data into the DataStream
      LoadRESEntry(Source, FDataStream, FSymStream, ResourceEntries[CurrentResource]);
      inc(CurrentResource,1);
      // if we hit the current limit of allocated ResourceEntries in the
      // array, allocate some more space
      if (CurrentResource>=length(ResourceEntries)) then
        setlength(ResourceEntries,length(ResourceEntries)+1024);
      end;

  // downsize the ResourceEntries to the really needed size
  SetLength(ResourceEntries,CurrentResource);

  // Write the symbol table - ressym
  ressym.ptr:=FSectionStream.Position+sizeof(TElf32Header);
  FSymStream.Position:=0;
  FSectionStream.CopyFrom(FSymStream,FSymStream.Size);
  doalign(4);

  // resstr
  resstr.ptr:=FSectionStream.Position+sizeof(TElf32Header);
  resstr.size:=0;
  // TODO: Load string data here
  doalign(4);

  // Now write the ResourceInfos.
  reshash.ptr:=FSectionStream.Position+sizeof(TElf32Header);
  for i:=0 to high(ResourceEntries) do
  begin
    FSectionStream.Write(ResourceEntries[i],sizeof(TELF32ResourceInfo));
  end;
  doalign(4);

  // Next write the resource data stream
  resdata.ptr:=FSectionStream.Position+sizeof(TElf32Header);
  FDataStream.Position:=0;
  FSectionStream.CopyFrom(FDataStream,FDataStream.Size);
  doalign(4);

  // resspare
  resspare.ptr:=FSectionStream.Position+sizeof(TElf32Header);
  // don't write anything, this is an empty section

  // shstrtab - this is not aligned
  shstrtab_ofs:=FSectionStream.Position+sizeof(TElf32Header);
  FSectionStream.Write(shstrtab,length(shstrtab));

  // Write 12 section headers. The headers itself don't need to be aligned,
  // as their size can be divided by 4. As shstrtab is uneven and not aligned,
  // we however need to align the start of the section header table
  doalign(4);
  sectionheader_ofs:=FSectionStream.Position+sizeof(TElf32Header);;

  // empty one
  fillchar(SectionHeader,sizeof(SectionHeader),0);
  FSectionStream.Write(SectionHeader,sizeOf(SectionHeader));

  // .text
  SectionHeader.sh_name:=$1B;
  SectionHeader.sh_type:=1; // PROGBITS
  SectionHeader.sh_flags:=6; // AX
  SectionHeader.sh_addr:=0;
  SectionHeader.sh_offset:=sizeof(TElf32Header); // after header, dummy as size is 0
  SectionHeader.sh_size:=0; // yep, pretty empty it is
  SectionHeader.sh_link:=0;
  SectionHeader.sh_info:=0;
  SectionHeader.sh_addralign:=4; // alignment
  SectionHeader.sh_entsize:=0;
  FSectionStream.Write(SectionHeader,sizeOf(SectionHeader));

  // .data
  SectionHeader.sh_name:=$21;
  SectionHeader.sh_type:=1; // PROGBITS
  SectionHeader.sh_flags:=3; // WA
  SectionHeader.sh_addr:=0;
  SectionHeader.sh_offset:=sizeof(TElf32Header); // after header, dummy as size is 0
  SectionHeader.sh_size:=0; // yep, pretty empty it is
  SectionHeader.sh_link:=0;
  SectionHeader.sh_info:=0;
  SectionHeader.sh_addralign:=4; // alignment
  SectionHeader.sh_entsize:=0;
  FSectionStream.Write(SectionHeader,sizeOf(SectionHeader));

  // .bss
  SectionHeader.sh_name:=$27;
  SectionHeader.sh_type:=8; // NOBITS
  SectionHeader.sh_flags:=3; // WA
  SectionHeader.sh_addr:=0;
  SectionHeader.sh_offset:=sizeof(TElf32Header); // after header, dummy as size is 0
  SectionHeader.sh_size:=0; // yep, pretty empty it is
  SectionHeader.sh_link:=0;
  SectionHeader.sh_info:=0;
  SectionHeader.sh_addralign:=4; // alignment
  SectionHeader.sh_entsize:=0;
  FSectionStream.Write(SectionHeader,sizeOf(SectionHeader));

  // fpc.ressym
  SectionHeader.sh_name:=$2C;
  SectionHeader.sh_type:=1; // PROGBITS
  SectionHeader.sh_flags:=2; // A
  SectionHeader.sh_addr:=0;
  SectionHeader.sh_offset:=ressym.ptr; // directly after header
  SectionHeader.sh_size:=FSymStream.Size;
  SectionHeader.sh_link:=0;
  SectionHeader.sh_info:=0;
  SectionHeader.sh_addralign:=4; // alignment
  SectionHeader.sh_entsize:=0;
  FSectionStream.Write(SectionHeader,sizeOf(SectionHeader));

  // fpc.resstr
  SectionHeader.sh_name:=$37;
  SectionHeader.sh_type:=1; // PROGBITS
  SectionHeader.sh_flags:=2; // A
  SectionHeader.sh_addr:=0;
  SectionHeader.sh_offset:=resstr.ptr;
  SectionHeader.sh_size:=0; // currently empty
  SectionHeader.sh_link:=0;
  SectionHeader.sh_info:=0;
  SectionHeader.sh_addralign:=4; // alignment
  SectionHeader.sh_entsize:=0;
  FSectionStream.Write(SectionHeader,sizeOf(SectionHeader));

  // fpc.reshash
  SectionHeader.sh_name:=$42;
  SectionHeader.sh_type:=1; // PROGBITS
  SectionHeader.sh_flags:=2; // A
  SectionHeader.sh_addr:=0;
  SectionHeader.sh_offset:=reshash.ptr;
  SectionHeader.sh_size:=length(ResourceEntries)*sizeof(TELF32ResourceInfo)+4;
  SectionHeader.sh_link:=0;
  SectionHeader.sh_info:=0;
  SectionHeader.sh_addralign:=4; // alignment
  SectionHeader.sh_entsize:=0;
  FSectionStream.Write(SectionHeader,sizeOf(SectionHeader));

  // fpc.resdata
  SectionHeader.sh_name:=$4e;
  SectionHeader.sh_type:=1; // PROGBITS
  SectionHeader.sh_flags:=2; // A
  SectionHeader.sh_addr:=0;
  SectionHeader.sh_offset:=resdata.ptr;
  SectionHeader.sh_size:=FDataStream.Size;
  SectionHeader.sh_link:=0;
  SectionHeader.sh_info:=0;
  SectionHeader.sh_addralign:=4; // alignment
  SectionHeader.sh_entsize:=0;
  FSectionStream.Write(SectionHeader,sizeOf(SectionHeader));

  // fpc.resspare
  // Not used in V1
  SectionHeader.sh_name:=$5a;
  SectionHeader.sh_type:=8; // NOBITS
  SectionHeader.sh_flags:=2; // A
  SectionHeader.sh_addr:=0;
  SectionHeader.sh_offset:=resspare.ptr; // fake, as it's empty, should be equal to shstrtab's offset
  SectionHeader.sh_size:=0; //DataStream.Size; // Leave as much room as we currently have in resdata section
  SectionHeader.sh_link:=0;
  SectionHeader.sh_info:=0;
  SectionHeader.sh_addralign:=4; // alignment
  SectionHeader.sh_entsize:=0;
  FSectionStream.Write(SectionHeader,sizeOf(SectionHeader));

  // .shstrtab
  SectionHeader.sh_name:=$11;
  SectionHeader.sh_type:=3; // STRTAB
  SectionHeader.sh_flags:=0;
  SectionHeader.sh_addr:=0;
  SectionHeader.sh_offset:=shstrtab_ofs;  // $3E
  SectionHeader.sh_size:=$67;
  SectionHeader.sh_link:=0;
  SectionHeader.sh_info:=0;
  SectionHeader.sh_addralign:=1; // alignment
  SectionHeader.sh_entsize:=0;
  FSectionStream.Write(SectionHeader,sizeOf(SectionHeader));

  // .symtab
  SectionHeader.sh_name:=$01;
  SectionHeader.sh_type:=2; // SYMTAB
  SectionHeader.sh_flags:=0;
  SectionHeader.sh_addr:=0;
  SectionHeader.sh_offset:=FSectionStream.Position+sizeof(TElf32Header)+sizeOf(SectionHeader)+sizeOf(SectionHeader); // will come directly after this and the next section. $0288;
  SectionHeader.sh_size:=$90;
  SectionHeader.sh_link:=$0B;
  SectionHeader.sh_info:=$09;
  SectionHeader.sh_addralign:=4; // alignment
  SectionHeader.sh_entsize:=$10;
  FSectionStream.Write(SectionHeader,sizeOf(SectionHeader));

  // .strtab
  SectionHeader.sh_name:=$09;
  SectionHeader.sh_type:=3; // STRTAB
  SectionHeader.sh_flags:=0;
  SectionHeader.sh_addr:=0;
  SectionHeader.sh_offset:=FSectionStream.Position+sizeof(TElf32Header)+sizeOf(SectionHeader)+$90; // will come after this sectionheader and the $90 bytes symtab - $0318; end of file
  SectionHeader.sh_size:=1;
  SectionHeader.sh_link:=0;
  SectionHeader.sh_info:=0;
  SectionHeader.sh_addralign:=1; // alignment
  SectionHeader.sh_entsize:=$0;
  FSectionStream.Write(SectionHeader,sizeOf(SectionHeader));

  // now write the symbol table
  FSectionStream.Write(symtab,length(symtab));
  // We don't need to align it, as it's $90 in size

  // now write the string table, it's just a single byte
  FSectionStream.Write(strtab,1);

  // Ok, we are done, now let's really write something to disk...

  // First write the ELF header

  fillchar(ElfHeader,sizeof(ElfHeader),0);
  ElfHeader.magic0123:=$464c457f; { = #127'ELF' }
  ElfHeader.file_class:=1;
  ElfHeader.data_encoding:=1;
  ElfHeader.file_version:=1;
  ElfHeader.e_type:=1;
  ElfHeader.e_machine:=3;
  ElfHeader.e_version:=1;
  ElfHeader.e_shoff:=sectionheader_ofs;
  ElfHeader.e_shstrndx:=9;
  ElfHeader.e_shnum:=12;
  ElfHeader.e_ehsize:=sizeof(TElf32header);
  ElfHeader.e_shentsize:=sizeof(TElf32sechdr);

  Dest.Write(ElfHeader,sizeof(TElf32header));

  // And now let's dump our whole memorystream into it.
  Dest.CopyFrom(FSectionStream,0);
end;


{ TElf64Creator }

procedure TElf64ResCreator.DoConvertStreams(Source, Dest: TStream);
begin
  DoError('64 bits resources not yet supported')
end;

end.
