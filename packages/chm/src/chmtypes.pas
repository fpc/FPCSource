{ Copyright (C) <2005> <Andrew Haines> chmtypes.pas

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
{
  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.
}
unit chmtypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSectionName = (snMSCompressed, snUnCompressed);

  TSectionNames = set of TSectionName;

   { TDirectoryChunk }

  TDirectoryChunk = class(TObject)
  private
    FHeaderSize: Integer;
    FQuickRefEntries: Word;
    Buffer: array[0..$1000-1] of byte;
    CurrentPos: Integer;
    FItemCount: Word;
    FClearCount: Integer;
  public
    function CanHold(ASize: Integer): Boolean;
    function FreeSpace: Integer;
    procedure WriteHeader(AHeader: Pointer);
    procedure WriteEntry(Size: Integer; Data: Pointer);
    procedure WriteChunkToStream(Stream: TStream); overload;
    procedure Clear;
    property ItemCount: Word read FItemCount;
    constructor Create(AHeaderSize: Integer);
  end;

  { TPMGIDirectoryChunk }

  TPMGIDirectoryChunk = class(TDirectoryChunk)
  private
    FChunkLevelCount: Integer;
    FParentChunk: TPMGIDirectoryChunk;
  public
    procedure WriteChunkToStream(Stream: TStream; var AIndex: Integer; Final: Boolean = False); overload;
    property ParentChunk: TPMGIDirectoryChunk read FParentChunk write FParentChunk;
    property ChunkLevelCount: Integer read FChunkLevelCount write FChunkLevelCount;
  end;

  PFileEntryRec = ^TFileEntryRec;
  TFileEntryRec = record
    Path: String;
    Name: String;
    DecompressedOffset: QWord;
    DecompressedSize: QWord;
    Compressed: Boolean; // True means it goes in section1 False means section0
  end;

  { TFileEntryList }

  TFileEntryList = class(TList)
  private
    FPaths: TStringList;
    function GetFileEntry(Index: Integer): TFileEntryRec;
    procedure SetFileEntry(Index: Integer; const AValue: TFileEntryRec);
  public
    function AddEntry(AFileEntry: TFileEntryRec; CheckPathIsAdded: Boolean = True): Integer;
    procedure Delete(Index: Integer);
    property FileEntry[Index: Integer]: TFileEntryRec read GetFileEntry write SetFileEntry;
    procedure Sort;
    constructor Create;
    destructor Destroy; override;

  end;

  TTOCIdxHeader = record
    BlockSize: DWord; // 4096
    EntriesOffset: DWord;
    EntriesCount: DWord;
    TopicsOffset: DWord;
    EmptyBytes: array[0..4079] of byte;
  end;

const
  TOC_ENTRY_HAS_NEW      = 2;
  TOC_ENTRY_HAS_CHILDREN = 4;
  TOC_ENTRY_HAS_LOCAL    = 8;

type
  PTOCEntryPageBookInfo = ^TTOCEntryPageBookInfo;
  TTOCEntryPageBookInfo = record
    Unknown1: Word; //  = 0
    EntryIndex: Word; // multiple entry info's can have this value but the TTocEntry it points to points back to the first item with this number. Wierd.
    Props: DWord; // BitField. See TOC_ENTRY_*
    TopicsIndexOrStringsOffset: DWord; // if TOC_ENTRY_HAS_LOCAL is in props it's the Topics Index
                                       // else it's the Offset In Strings of the Item Text
    ParentPageBookInfoOffset: DWord;
    NextPageBookOffset: DWord; // same level of tree only

    // Only if TOC_ENTRY_HAS_CHILDREN is set are these written
    FirstChildOffset: DWord;
    Unknown3: DWord; // = 0
  end;

  TTocEntry = record
    PageBookInfoOffset: DWord;
    IncrementedInt: DWord; // first is $29A
    TopicsIndex: DWord; // Index of Entry in #TOPICS file
  end;

  TTopicEntry = record
    TocOffset,
    StringsOffset,
    URLTableOffset: DWord;
    InContents: Word;// 2 = in contents 6 = not in contents
    Unknown: Word; // 0,2,4,8,10,12,16,32
  end;

  TBtreeHeader = packed record
                        ident          : array[0..1] of ansichar; // $3B $29
                        flags          : word;	// bit $2 is always 1, bit $0400 1 if dir? (always on)
                        blocksize      : word;  // size of blocks (2048)
                        dataformat     : array[0..15] of ansichar;  // "X44" always the same, see specs.
                        unknown0       : dword; // always 0
			lastlstblock   : dword; // index of last listing block in the file;
                        indexrootblock : dword; // Index of the root block in the file.
                        unknown1       : dword; // always -1
                        nrblock	       : dword; // Number of blocks
                        treedepth      : word;  // The depth of the tree of blocks (1 if no index blocks, 2 one level of index blocks, ...)
                        nrkeywords     : dword; // number of keywords in the file.
                        codepage       : dword; // Windows code page identifier (usually 1252 - Windows 3.1 US (ANSI))
			lcid	       : dword; // LCID from the HHP file.
                        ischm	       : dword; // 0 if this a BTREE and is part of a CHW file, 1 if it is a BTree and is part of a CHI or CHM file
                        unknown2       : dword; // Unknown. Almost always 10031. Also 66631 (accessib.chm, ieeula.chm, iesupp.chm, iexplore.chm, msoe.chm, mstask.chm, ratings.chm, wab.chm).
                        unknown3       : dword; // unknown 0
		        unknown4       : dword; // unknown 0
			unknown5       : dword; // unknown 0
                      end;
  PBTreeBlockHeader = ^TBtreeBlockHeader;
  TBtreeBlockHeader = packed record
                        Length             : word;  // Length of free space at the end of the block.
                        NumberOfEntries    : word;  // Number of entries in the block.
                        IndexOfPrevBlock   : dword; // Index of the previous block. -1 if this is the first listing block.
                        IndexOfNextBlock   : dword; // Index of the next block. -1 if this is the last listing block.
                      end;

  PBtreeBlockEntry = ^TBtreeBlockEntry;
  TBtreeBlockEntry = packed record
                        isseealso  : word; // 2 if this keyword is a See Also keyword, 0 if it is not.
                        entrydepth : word; // Depth of this entry into the tree.
                        charindex  : dword;// Character index of the last keyword in the ", " separated list.
                        unknown0   : dword;// 0 (unknown)
                        NrPairs    : dword;// Number of Name, Local pairs
                      end;

  PBtreeIndexBlockHeader = ^TBtreeIndexBlockHeader;
  TBtreeIndexBlockHeader = packed record
                        length             : word;  // Length of free space at the end of the block.
                        NumberOfEntries    : word;  // Number of entries in the block.
                        IndexOfChildBlock  : dword; // Index of Child Block
                      end;

  PBtreeIndexBlockEntry = ^TBtreeIndexBlockEntry;
  TBtreeIndexBlockEntry = packed record
                        isseealso  : word; // 2 if this keyword is a See Also keyword, 0 if it is not.
                        entrydepth : word; // Depth of this entry into the tree.
                        charindex  : dword;// Character index of the last keyword in the ", " separated list.
                        unknown0   : dword;// 0 (unknown)
                        NrPairs    : dword;// Number of Name, Local pairs
                      end;

function PageBookInfoRecordSize(ARecord: PTOCEntryPageBookInfo): Integer;

implementation
uses chmbase;

function PageBookInfoRecordSize(ARecord: PTOCEntryPageBookInfo): Integer;
begin
  if (TOC_ENTRY_HAS_CHILDREN and ARecord^.Props) > 0 then
    Result := 28
  else
    Result := 20;
end;

{ TDirectoryChunk }

function TDirectoryChunk.CanHold(ASize: Integer): Boolean;
begin
  Result := CurrentPos < $1000 - ASize - (SizeOf(Word) * (FQuickRefEntries+2));
end;

function TDirectoryChunk.FreeSpace: Integer;
begin
  Result := $1000 - CurrentPos;
end;

procedure TDirectoryChunk.WriteHeader(AHeader: Pointer);
begin
  Move(AHeader^, Buffer[0], FHeaderSize);
end;

procedure TDirectoryChunk.WriteEntry(Size: Integer; Data: Pointer);
var
  ReversePos: Integer;
  Value: Word;
begin
  if not CanHold(Size) then Raise Exception.Create('Trying to write past the end of the buffer');
  Move(Data^, Buffer[CurrentPos], Size);
  Inc(CurrentPos, Size);
  Inc(FItemCount);

  // now put a quickref entry if needed
  if ItemCount mod 5 = 0 then begin
    Inc(FQuickRefEntries);
    ReversePos := ($1000) - SizeOf(Word) - (SizeOf(Word)*FQuickRefEntries);
    Value := NtoLE(Word(CurrentPos - Size - FHeaderSize));
    Move(Value, Buffer[ReversePos], SizeOf(Word));
  end;
end;

procedure TDirectoryChunk.WriteChunkToStream(Stream: TStream);
var
  ReversePos: Integer;
  TmpItemCount: Word;
begin
  ReversePos := $1000 - SizeOf(Word);
  TmpItemCount := NtoLE(Word(FItemCount));
  Move(TmpItemCount, Buffer[ReversePos], SizeOf(Word));

  Stream.Write(Buffer[0], $1000);
  {$IFDEF DEBUG_CHM_CHUNKS}
  WriteLn('Writing ', Copy(PChar(@Buffer[0]),0,4),' ChunkToStream');
  {$ENDIF}
end;

procedure TDirectoryChunk.Clear;
begin
  FillChar(Buffer, $1000, 0);
  FItemCount := 0;
  CurrentPos := FHeaderSize;
  FQuickRefEntries := 0;
  Inc(FClearCount);
end;

constructor TDirectoryChunk.Create(AHeaderSize: Integer);
begin
  FHeaderSize := AHeaderSize;
  CurrentPos := FHeaderSize;
end;

{ TFileEntryList }

function TFileEntryList.GetFileEntry(Index: Integer): TFileEntryRec;
begin
  Result := PFileEntryRec(Items[Index])^;
end;

procedure TFileEntryList.SetFileEntry(Index: Integer; const AValue: TFileEntryRec);
begin
  PFileEntryRec(Items[Index])^ := AValue;
end;

function TFileEntryList.AddEntry(AFileEntry: TFileEntryRec; CheckPathIsAdded: Boolean = True): Integer;
var
  TmpEntry: PFileEntryRec;
begin
  New(TmpEntry);
  //WriteLn('Adding: ', AFileEntry.Path+AFileEntry.Name,' Size = ', AFileEntry.DecompressedSize,' Offset = ', AFileEntry.DecompressedOffset);
  if CheckPathIsAdded and (FPaths.IndexOf(AFileEntry.Path) < 0) then begin
    // all paths are included in the list of files in section 0 with a size and offset of 0
    FPaths.Add(AFileEntry.Path);
    TmpEntry^.Path := AFileEntry.Path;
    TmpEntry^.Name := '';
    TmpEntry^.DecompressedOffset := 0;
    TmpEntry^.DecompressedSize := 0;
    TmpEntry^.Compressed := False;
    (Self as TList).Add(TmpEntry);
    New(TmpEntry);
  end;
  TmpEntry^ := AFileEntry;
  Result := (Self as TList).Add(TmpEntry);
end;

procedure TFileEntryList.Delete(Index: Integer);
begin
  Dispose(PFileEntryRec(Items[Index]));
  Inherited Delete(Index);
end;

function FileEntrySortFunc(Item1, Item2: PFileEntryRec): Integer;
var
  Str1, Str2: String;
begin
  Str1 := Item1^.Path + Item1^.Name;
  Str2 := Item2^.Path + Item2^.Name;
  Result := ChmCompareText(Str1, Str2);
end;

procedure TFileEntryList.Sort;
begin
  Inherited Sort(TListSortCompare(@FileEntrySortFunc));
end;

constructor TFileEntryList.Create;
begin
  Inherited Create;
  FPaths := TStringList.Create;
end;

destructor TFileEntryList.Destroy;
var
  I: Integer;
begin
  for I := Count-1 downto 0 do
    Delete(I);
  FPaths.Free;
  inherited Destroy;
end;

{ TPMGIDirectoryChunk }
procedure TPMGIDirectoryChunk.WriteChunkToStream(Stream: TStream; var AIndex: Integer
  ; Final: Boolean = False);
var
  NewBuffer: array[0..512] of byte;
  EntryLength,
  WriteSize: Integer;
  OldPos, NewPos, NewStart: Int64;
  procedure FinishBlock;
  var
    Header: TPMGIIndexChunk;
  begin
    Inc(AIndex);
    Header.PMGIsig := 'PMGI';
    Header.UnusedSpace := FParentChunk.FreeSpace;
    FParentChunk.WriteHeader(@Header);
    FParentChunk.WriteChunkToStream(Stream, AIndex, Final);
    FParentChunk.Clear;
  end;
begin
  if FItemCount < 1 then begin
    WriteLn('WHAT ARE YOU DOING!!');
    Dec(AIndex);
    Exit;
  end;
  OldPos := Stream.Position;
  WriteChunkToStream(Stream);
  NewPos := Stream.Position;
  Inc(FChunkLevelCount);

  if Final and (ChunkLevelCount < 2) then begin
    FParentChunk.Free;
    FParentChunk := nil;
    Exit;
  end;
  if FParentChunk = nil then FParentChunk := TPMGIDirectoryChunk.Create(FHeaderSize);

  NewStart := OldPos+FHeaderSize;
  Stream.Position := NewStart;
  EntryLength := GetCompressedInteger(Stream);
  WriteSize := (Stream.Position - NewStart) + EntryLength;
  Move(Buffer[FHeaderSize], NewBuffer[0], WriteSize);
  Inc(WriteSize, WriteCompressedInteger(@NewBuffer[WriteSize], AIndex));

  Stream.Position := NewPos;

  if not FParentChunk.CanHold(WriteSize) then begin
    FinishBlock;
  end;

  FParentChunk.WriteEntry(WriteSize, @NewBuffer[0]);
  if Final then FinishBlock;
  //WriteLn(ChunkLevelCount);
end;

end.

