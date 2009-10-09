{ Copyright (C) <2005> <Andrew Haines> chmwriter.pas

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
unit chmwriter;
{$MODE OBJFPC}{$H+}
{ $DEFINE LZX_USETHREADS}

interface
uses Classes, ChmBase, chmtypes, chmspecialfiles, HtmlIndexer, chmsitemap, Avl_Tree{$IFDEF LZX_USETHREADS}, lzxcompressthread{$ENDIF};

type

  TGetDataFunc = function (const DataName: String; out PathInChm: String; out FileName: String; var Stream: TStream): Boolean of object;
  //  DataName :  A FileName or whatever so that the getter can find and open the file to add
  //  PathInChm:  This is the absolute path in the archive. i.e. /home/user/helpstuff/
  //              becomes '/' and /home/user/helpstuff/subfolder/ > /subfolder/
  //  FileName :  /home/user/helpstuff/index.html > index.html
  //  Stream   :  the file opened with DataName should be written to this stream

Type
   TStringIndex = Class    // AVLTree needs wrapping in non automated reference type
                      TheString : String;
                      StrId     : Integer;
                    end;
   TUrlStrIndex = Class
                      UrlStr    : String;
                      UrlStrId  : Integer;
                    end;

  { TChmWriter }

  TChmWriter = class(TObject)
    FOnLastFile: TNotifyEvent;
  private
    FHasBinaryTOC: Boolean;
    FHasBinaryIndex: Boolean;
    ForceExit: Boolean;

    FDefaultFont: String;
    FDefaultPage: String;
    FFullTextSearch: Boolean;
    FInternalFiles: TFileEntryList; // Contains a complete list of files in the chm including
    FFrameSize: LongWord;           // uncompressed files and special internal files of the chm
    FCurrentStream: TStream; // used to buffer the files that are to be compressed
    FCurrentIndex: Integer;
    FOnGetFileData: TGetDataFunc;
    FSearchTitlesOnly: Boolean;
    FStringsStream: TMemoryStream; // the #STRINGS file
    FTopicsStream: TMemoryStream;  // the #TOPICS file
    FURLTBLStream: TMemoryStream;  // the #URLTBL file. has offsets of strings in URLSTR
    FURLSTRStream: TMemoryStream;  // the #URLSTR file
    FFiftiMainStream: TMemoryStream;
    FContextStream: TMemoryStream; // the #IVB file
    FSection0: TMemoryStream;
    FSection1: TStream; // Compressed Stream
    FSection1Size: QWord;
    FSection1ResetTable: TMemoryStream; // has a list of frame positions NOT window positions
    FDirectoryListings: TStream;
    FOutStream: TStream;
    FFileNames: TStrings;
    FDestroyStream: Boolean;
    FTempStream: TStream;
    FPostStream: TStream;
    FTitle: String;
    FHasTOC: Boolean;
    FHasIndex: Boolean;
    FWindowSize: LongWord;
    FReadCompressedSize: QWord; // Current Size of Uncompressed data that went in Section1 (compressed)
    FIndexedFiles: TIndexedWordList;
    FPostStreamActive: Boolean;
    // Linear order of file
    ITSFHeader: TITSFHeader;
    HeaderSection0Table: TITSFHeaderEntry;  // points to HeaderSection0
    HeaderSection1Table: TITSFHeaderEntry; // points to HeaderSection1
    HeaderSuffix: TITSFHeaderSuffix; //contains the offset of CONTENTSection0 from zero
    HeaderSection0: TITSPHeaderPrefix;
    HeaderSection1: TITSPHeader; // DirectoryListings header
    FAvlStrings   : TAVLTree;    // dedupe strings
    FAvlURLStr    : TAVLTree;    // dedupe urltbl + binindex must resolve URL to topicid
    SpareString   : TStringIndex;
    SpareUrlStr   : TUrlStrIndex;
    // DirectoryListings
    // CONTENT Section 0 (section 1 is contained in section 0)
    // EOF
    // end linear header parts
    procedure InitITSFHeader;
    procedure InitHeaderSectionTable;
    procedure SetTempRawStream(const AValue: TStream);
    procedure WriteHeader(Stream: TStream);
    procedure CreateDirectoryListings;
    procedure WriteDirectoryListings(Stream: TStream);
    procedure StartCompressingStream;
    procedure WriteSYSTEM;
    procedure WriteITBITS;
    procedure WriteSTRINGS;
    procedure WriteTOPICS;
    procedure WriteIVB; // context ids
    procedure WriteURL_STR_TBL;
    procedure WriteOBJINST;
    procedure WriteFiftiMain;
    procedure WriteREADMEFile;
    procedure WriteFinalCompressedFiles;
    procedure WriteSection0;
    procedure WriteSection1;
    procedure WriteDataSpaceFiles(const AStream: TStream);
    function AddString(AString: String): LongWord;
    function AddURL(AURL: String; TopicsIndex: DWord): LongWord;
    procedure CheckFileMakeSearchable(AStream: TStream; AFileEntry: TFileEntryRec);
    function NextTopicIndex: Integer;
    // callbacks for lzxcomp
    function  AtEndOfData: Longbool;
    function  GetData(Count: LongInt; Buffer: PByte): LongInt;
    function  WriteCompressedData(Count: Longint; Buffer: Pointer): LongInt;
    procedure MarkFrame(UnCompressedTotal, CompressedTotal: LongWord);
    // end callbacks
    {$IFDEF LZX_USETHREADS}
    // callbacks for lzx compress threads
    function  LTGetData(Sender: TLZXCompressor; WantedByteCount: Integer; Buffer: Pointer): Integer;
    function  LTIsEndOfFile(Sender: TLZXCompressor): Boolean;
    procedure LTChunkDone(Sender: TLZXCompressor; CompressedSize: Integer; UncompressedSize: Integer; Buffer: Pointer);
    procedure LTMarkFrame(Sender: TLZXCompressor; CompressedTotal: Integer; UncompressedTotal: Integer);
    {$ENDIF}
    // end callbacks
  public
    constructor Create(OutStream: TStream; FreeStreamOnDestroy: Boolean);
    destructor Destroy; override;
    procedure Execute;
    procedure AppendTOC(AStream: TStream);
    procedure AppendBinaryTOCFromSiteMap(ASiteMap: TChmSiteMap);
    procedure AppendBinaryTOCStream(AStream: TStream);
    procedure AppendIndex(AStream: TStream);
    procedure AppendSearchDB(AName: String; AStream: TStream);
    procedure AddStreamToArchive(AFileName, APath: String; AStream: TStream; Compress: Boolean = True);
    procedure PostAddStreamToArchive(AFileName, APath: String; AStream: TStream; Compress: Boolean = True);
    procedure AddContext(AContext: DWord; ATopic: String);
    property WindowSize: LongWord read FWindowSize write FWindowSize default 2; // in $8000 blocks
    property FrameSize: LongWord read FFrameSize write FFrameSize default 1; // in $8000 blocks
    property FilesToCompress: TStrings read FFileNames;
    property OnGetFileData: TGetDataFunc read FOnGetFileData write FOnGetFileData;
    property OnLastFile: TNotifyEvent read FOnLastFile write FOnLastFile;
    property OutStream: TStream read FOutStream;
    property Title: String read FTitle write FTitle;
    property FullTextSearch: Boolean read FFullTextSearch write FFullTextSearch;
    property SearchTitlesOnly: Boolean read FSearchTitlesOnly write FSearchTitlesOnly;
    property HasBinaryTOC: Boolean read FHasBinaryTOC write FHasBinaryTOC;
   property HasBinaryIndex: Boolean read FHasBinaryIndex write FHasBinaryIndex;
   property DefaultFont: String read FDefaultFont write FDefaultFont;
    property DefaultPage: String read FDefaultPage write FDefaultPage;
    property TempRawStream: TStream read FTempStream write SetTempRawStream;
    //property LocaleID: dword read ITSFHeader.LanguageID write ITSFHeader.LanguageID;
  end;

implementation
uses dateutils, sysutils, paslzxcomp, chmFiftiMain;

const
  LZX_WINDOW_SIZE = 16; // 16 = 2 frames = 1 shl 16
  LZX_FRAME_SIZE = $8000;

{$I chmobjinstconst.inc}


Function CompareStrings(Node1, Node2: Pointer): integer;
var n1,n2 : TStringIndex;
begin
  n1:=TStringIndex(Node1); n2:=TStringIndex(Node2);
  Result := CompareText(n1.TheString, n2.TheString);
  if Result < 0 then Result := -1
  else if Result > 0 then Result := 1;
end;


Function CompareUrlStrs(Node1, Node2: Pointer): integer;
var n1,n2 : TUrlStrIndex;
begin
  n1:=TUrlStrIndex(Node1); n2:=TUrlStrIndex(Node2);
  Result := CompareText(n1.UrlStr, n2.UrlStr);
  if Result < 0 then Result := -1
  else if Result > 0 then Result := 1;
end;

{ TChmWriter }

procedure TChmWriter.InitITSFHeader;
begin
  with ITSFHeader do begin
    ITSFsig := ITSFFileSig;
    Version := NToLE(DWord(3));
    // we fix endian order when this is written to the stream
    HeaderLength := NToLE(DWord(SizeOf(TITSFHeader) + (SizeOf(TITSFHeaderEntry)*2) + SizeOf(TITSFHeaderSuffix)));
    Unknown_1 := NToLE(DWord(1));
    TimeStamp:= NToBE(MilliSecondOfTheDay(Now)); //bigendian
    LanguageID := NToLE(DWord($0409)); // English / English_US
    Guid1 := ITSFHeaderGUID;
    Guid2 := ITSFHeaderGUID;
  end;
end;

procedure TChmWriter.InitHeaderSectionTable;
begin
  // header section 0
  HeaderSection0Table.PosFromZero := LEToN(ITSFHeader.HeaderLength);
  HeaderSection0Table.Length := SizeOf(TITSPHeaderPrefix);
  // header section 1
  HeaderSection1Table.PosFromZero := HeaderSection0Table.PosFromZero + HeaderSection0Table.Length;
  HeaderSection1Table.Length := SizeOf(TITSPHeader)+FDirectoryListings.Size;

  //contains the offset of CONTENT Section0 from zero
  HeaderSuffix.Offset := HeaderSection1Table.PosFromZero + HeaderSection1Table.Length;

  // now fix endian stuff
  HeaderSection0Table.PosFromZero := NToLE(HeaderSection0Table.PosFromZero);
  HeaderSection0Table.Length := NToLE(HeaderSection0Table.Length);
  HeaderSection1Table.PosFromZero := NToLE(HeaderSection1Table.PosFromZero);
  HeaderSection1Table.Length := NToLE(HeaderSection1Table.Length);

  with HeaderSection0 do begin // TITSPHeaderPrefix;
    Unknown1 := NToLE(DWord($01FE));
    Unknown2 := 0;
    // at this point we are putting together the headers. content sections 0 and 1 are complete
    FileSize := NToLE(HeaderSuffix.Offset + FSection0.Size + FSection1Size);
    Unknown3 := 0;
    Unknown4 := 0;
  end;
  with HeaderSection1 do begin // TITSPHeader; // DirectoryListings header
    ITSPsig := ITSPHeaderSig;
    Version := NToLE(DWord(1));
    DirHeaderLength := NToLE(DWord(SizeOf(TITSPHeader)));  // Length of the directory header
    Unknown1 := NToLE(DWord($0A));
    ChunkSize := NToLE(DWord($1000));
    Density := NToLE(DWord(2));
    // updated when directory listings were created
    //IndexTreeDepth := 1 ; // 1 if there is no index 2 if there is one level of PMGI chunks. will update as
    //IndexOfRootChunk := -1;// if no root chunk
    //FirstPMGLChunkIndex,
    //LastPMGLChunkIndex: LongWord;

    Unknown2 := NToLE(Longint(-1));
    //DirectoryChunkCount: LongWord;
    LanguageID := NToLE(DWord($0409));
    GUID := ITSPHeaderGUID;
    LengthAgain := NToLE(DWord($54));
    Unknown3 := NToLE(Longint(-1));
    Unknown4 := NToLE(Longint(-1));
    Unknown5 := NToLE(Longint(-1));
  end;

  // more endian stuff
  HeaderSuffix.Offset := NToLE(HeaderSuffix.Offset);
end;

procedure TChmWriter.SetTempRawStream(const AValue: TStream);
begin
  if (FCurrentStream.Size > 0) or (FSection1.Size > 0) then
    raise Exception.Create('Cannot set the TempRawStream once data has been written to it!');
  if AValue = nil then
    raise Exception.Create('TempRawStream cannot be nil!');
  if FCurrentStream = AValue then
    exit;
  FCurrentStream.Free;
  FCurrentStream := AValue;
end;

procedure TChmWriter.WriteHeader(Stream: TStream);
begin
  Stream.Write(ITSFHeader, SizeOf(TITSFHeader));
  Stream.Write(HeaderSection0Table, SizeOf(TITSFHeaderEntry));
  Stream.Write(HeaderSection1Table, SizeOf(TITSFHeaderEntry));
  Stream.Write(HeaderSuffix, SizeOf(TITSFHeaderSuffix));
  Stream.Write(HeaderSection0, SizeOf(TITSPHeaderPrefix));

end;

procedure TChmWriter.CreateDirectoryListings;
type
  TFirstListEntry = record
    Entry: array[0..511] of byte;
    Size: Integer;
  end;
var
  Buffer: array [0..511] of Byte;
  IndexBlock: TPMGIDirectoryChunk;
  ListingBlock: TDirectoryChunk;
  I: Integer;
  Size: Integer;
  FESize: Integer;
  FileName: String;
  FileNameSize: Integer;
  LastListIndex: Integer;
  FirstListEntry: TFirstListEntry;
  ChunkIndex: Integer;
  ListHeader: TPMGListChunk;
const
  PMGL = 'PMGL';
  PMGI = 'PMGI';
  procedure UpdateLastListChunk;
  var
    Tmp: QWord;
  begin
    if ChunkIndex < 1 then begin
      Exit;
    end;
    Tmp := FDirectoryListings.Position;
    FDirectoryListings.Position := (LastListIndex) * $1000;
    FDirectoryListings.Read(ListHeader, SizeOf(TPMGListChunk));
    FDirectoryListings.Position := (LastListIndex) * $1000;
    ListHeader.NextChunkIndex := NToLE(ChunkIndex);
    FDirectoryListings.Write(ListHeader, SizeOf(TPMGListChunk));
    FDirectoryListings.Position := Tmp;
  end;
  procedure WriteIndexChunk(ShouldFinish: Boolean = False);
  var
    IndexHeader: TPMGIIndexChunk;
    ParentIndex,
    TmpIndex: TPMGIDirectoryChunk;
  begin
    with IndexHeader do
    begin
      PMGIsig := PMGI;
      UnusedSpace := NToLE(IndexBlock.FreeSpace);
    end;
    IndexBlock.WriteHeader(@IndexHeader);
    IndexBlock.WriteChunkToStream(FDirectoryListings, ChunkIndex, ShouldFinish);
    IndexBlock.Clear;
    if HeaderSection1.IndexOfRootChunk < 0 then HeaderSection1.IndexOfRootChunk := ChunkIndex;
    if ShouldFinish then
    begin
      HeaderSection1.IndexTreeDepth := 2;
      ParentIndex := IndexBlock.ParentChunk;
      if ParentIndex <> nil then
      repeat // the parent index is notified by our child index when to write
        HeaderSection1.IndexOfRootChunk := ChunkIndex;
        TmpIndex := ParentIndex;
        ParentIndex := ParentIndex.ParentChunk;
        TmpIndex.Free;
        Inc(HeaderSection1.IndexTreeDepth);
        Inc(ChunkIndex);
      until ParentIndex = nil;
    end;
    Inc(ChunkIndex);

  end;
  procedure WriteListChunk;
  begin
    with ListHeader do begin
      PMGLsig := PMGL;
      UnusedSpace := NToLE(ListingBlock.FreeSpace);
      Unknown1 :=  0;
      PreviousChunkIndex := NToLE(LastListIndex);
      NextChunkIndex := NToLE(Longint(-1)); // we update this when we write the next chunk
    end;
    if HeaderSection1.FirstPMGLChunkIndex <= 0 then
      HeaderSection1.FirstPMGLChunkIndex := NToLE(ChunkIndex);
    HeaderSection1.LastPMGLChunkIndex := NToLE(ChunkIndex);
    ListingBlock.WriteHeader(@ListHeader);
    ListingBlock.WriteChunkToStream(FDirectoryListings);
    ListingBlock.Clear;
    UpdateLastListChunk;

    LastListIndex := ChunkIndex;
    Inc(ChunkIndex);
    // now add to index
    if not IndexBlock.CanHold(FirstListEntry.Size) then
      WriteIndexChunk;
    IndexBlock.WriteEntry(FirstListEntry.Size, @FirstListEntry.Entry[0])
  end;
begin
  // first sort the listings
  FInternalFiles.Sort;
  HeaderSection1.IndexTreeDepth := 1;
  HeaderSection1.IndexOfRootChunk := -1;

  ChunkIndex := 0;

  IndexBlock := TPMGIDirectoryChunk.Create(SizeOf(TPMGIIndexChunk));
  ListingBlock := TDirectoryChunk.Create(SizeOf(TPMGListChunk));

  LastListIndex  := -1;

  // add files to a pmgl block until it is full.
  // after the block is full make a pmgi block and add the first entry of the pmgl block
  // repeat until the index block is full and start another.
  // the pmgi chunks take care of needed parent chunks in the tree
  for I := 0 to FInternalFiles.Count-1 do begin
    Size := 0;
    FileName := FInternalFiles.FileEntry[I].Path + FInternalFiles.FileEntry[I].Name;
    FileNameSize := Length(FileName);
    // filename length
    Inc(Size, WriteCompressedInteger(@Buffer[Size], FileNameSize));
    // filename
    Move(FileName[1], Buffer[Size], FileNameSize);
    Inc(Size, FileNameSize);
    FESize := Size;
    // File is compressed...
    Inc(Size, WriteCompressedInteger(@Buffer[Size], Ord(FInternalFiles.FileEntry[I].Compressed)));
    // Offset from section start
    Inc(Size, WriteCompressedInteger(@Buffer[Size], FInternalFiles.FileEntry[I].DecompressedOffset));
    // Size when uncompressed
    Inc(Size, WriteCompressedInteger(@Buffer[Size], FInternalFiles.FileEntry[I].DecompressedSize));

    if not ListingBlock.CanHold(Size) then
      WriteListChunk;

    ListingBlock.WriteEntry(Size, @Buffer[0]);

    if ListingBlock.ItemCount = 1 then begin // add the first list item to the index
      Move(Buffer[0], FirstListEntry.Entry[0], FESize);
      FirstListEntry.Size := FESize + WriteCompressedInteger(@FirstListEntry.Entry[FESize], ChunkIndex);
    end;
  end;
  if ListingBlock.ItemCount > 0 then WriteListChunk;

  if ChunkIndex > 1 then begin
    if (IndexBlock.ItemCount > 1)
    or ( (IndexBlock.ItemCount > 0) and (HeaderSection1.IndexOfRootChunk > -1) )
    then WriteIndexChunk(True);
  end;

  HeaderSection1.DirectoryChunkCount := NToLE(DWord(FDirectoryListings.Size div $1000));

  IndexBlock.Free;
  ListingBlock.Free;

  //now fix some endian stuff
  HeaderSection1.IndexOfRootChunk := NToLE(HeaderSection1.IndexOfRootChunk);
  HeaderSection1.IndexTreeDepth := NtoLE(HeaderSection1.IndexTreeDepth);
end;

procedure TChmWriter.WriteDirectoryListings(Stream: TStream);
begin
  Stream.Write(HeaderSection1, SizeOf(HeaderSection1));
  FDirectoryListings.Position := 0;
  Stream.CopyFrom(FDirectoryListings, FDirectoryListings.Size);
  FDirectoryListings.Position := 0;
  //TMemoryStream(FDirectoryListings).SaveToFile('dirlistings.pmg');
end;

procedure TChmWriter.WriteSystem;
var
  Entry: TFileEntryRec;
  TmpStr: String;
  TmpTitle: String;
const
  VersionStr = 'HHA Version 4.74.8702'; // does this matter?
begin


  // this creates the /#SYSTEM file
  Entry.Name := '#SYSTEM';
  Entry.Path := '/';
  Entry.Compressed := False;
  Entry.DecompressedOffset := FSection0.Position;

 { if FileExists('#SYSTEM') then
  begin
    TmpStream := TMemoryStream.Create;
    TmpStream.LoadFromFile('#SYSTEM');
    TmpStream.Position := 0;
    FSection0.CopyFrom(TmpStream, TmpStream.Size);
  end;                                    }
  // EntryCodeOrder: 10 9 4 2 3 16 6 0 1 5
  FSection0.WriteDWord(NToLE(Word(3))); // Version
  if Title <> '' then
    TmpTitle := Title
  else
    TmpTitle := 'default';

  // Code -> Length -> Data
  // 10
  FSection0.WriteWord(NToLE(Word(10)));
  FSection0.WriteWord(NToLE(Word(SizeOf(DWord))));
  FSection0.WriteDWord(NToLE(MilliSecondOfTheDay(Now)));
  // 9
  FSection0.WriteWord(NToLE(Word(9)));
  FSection0.WriteWord(NToLE(Word(SizeOf(VersionStr)+1)));
  FSection0.Write(VersionStr, SizeOf(VersionStr));
  FSection0.WriteByte(0);
  // 4 A struct that is only needed to set if full text search is on.
  FSection0.WriteWord(NToLE(Word(4)));
  FSection0.WriteWord(NToLE(Word(36))); // size

  FSection0.WriteDWord(NToLE(DWord($0409)));
  FSection0.WriteDWord(1);
  FSection0.WriteDWord(NToLE(DWord(Ord(FFullTextSearch))));
  FSection0.WriteDWord(0);
  FSection0.WriteDWord(0);

  // two for a QWord
  FSection0.WriteDWord(0);
  FSection0.WriteDWord(0);

  FSection0.WriteDWord(0);
  FSection0.WriteDWord(0);




  ////////////////////////<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  // 2  default page to load
  if FDefaultPage <> '' then begin
    FSection0.WriteWord(NToLE(Word(2)));
    FSection0.WriteWord(NToLE(Word(Length(FDefaultPage)+1)));
    FSection0.Write(FDefaultPage[1], Length(FDefaultPage));
    FSection0.WriteByte(0);
  end;
  // 3  Title
  if FTitle <> '' then begin
    FSection0.WriteWord(NToLE(Word(3)));
    FSection0.WriteWord(NToLE(Word(Length(FTitle)+1)));
    FSection0.Write(FTitle[1], Length(FTitle));
    FSection0.WriteByte(0);
  end;

  // 16 Default Font
  if FDefaultFont <> '' then begin
    FSection0.WriteWord(NToLE(Word(16)));
    FSection0.WriteWord(NToLE(Word(Length(FDefaultFont)+1)));
    FSection0.Write(FDefaultFont[1], Length(FDefaultFont));
    FSection0.WriteByte(0);
  end;

  // 6
  // unneeded. if output file is :  /somepath/OutFile.chm the value here is outfile(lowercase)
  {FSection0.WriteWord(6);
  FSection0.WriteWord(Length('test1')+1);
  Fsection0.Write('test1', 5);
  FSection0.WriteByte(0);}

  // 0 Table of contents filename
  if FHasTOC then begin
    TmpStr := 'default.hhc';
    FSection0.WriteWord(0);
    FSection0.WriteWord(NToLE(Word(Length(TmpStr)+1)));
    FSection0.Write(TmpStr[1], Length(TmpStr));
    FSection0.WriteByte(0);
  end;
  // 1
  // hhk Index
  if FHasIndex then begin
    TmpStr := 'default.hhk';
    FSection0.WriteWord(NToLE(Word(1)));
    FSection0.WriteWord(NToLE(Word(Length(TmpStr)+1)));
    FSection0.Write(TmpStr[1], Length(TmpStr));
    FSection0.WriteByte(0);
  end;
  // 5 Default Window.
  // Not likely needed
// }
  Entry.DecompressedSize := FSection0.Position - Entry.DecompressedOffset;
  FInternalFiles.AddEntry(Entry);

  // 7 Binary Index
  if FHasBinaryIndex then
  begin
    FSection0.WriteWord(NToLE(Word(7)));
    FSection0.WriteWord(NToLE(Word(4)));
    FSection0.WriteDWord(DWord(0)); // what is this number to be?
  end;

  // 11 Binary TOC
  if FHasBinaryTOC then
  begin
    FSection0.WriteWord(NToLE(Word(11)));
    FSection0.WriteWord(NToLE(Word(4)));
    FSection0.WriteDWord(DWord(0)); // what is this number to be?
  end;
end;

procedure TChmWriter.WriteITBITS;
var
  Entry: TFileEntryRec;
begin
  // This is an empty and useless file
  Entry.Name := '#ITBITS';
  Entry.Path := '/';
  Entry.Compressed := False;
  Entry.DecompressedOffset :=0;// FSection0.Position;
  Entry.DecompressedSize := 0;

  FInternalFiles.AddEntry(Entry);
end;

procedure TChmWriter.WriteSTRINGS;
begin
  if FStringsStream.Size = 0 then;
    FStringsStream.WriteByte(0);
  FStringsStream.Position := 0;
  PostAddStreamToArchive('#STRINGS', '/', FStringsStream);
end;

procedure IterateWord(aword:TIndexedWord;State:pointer);
var i,cnt : integer;
begin
  cnt:=pinteger(state)^;
  for i := 0 to AWord.DocumentCount-1 do
    Inc(cnt, AWord.GetLogicalDocument(i).NumberOfIndexEntries);
          // was commented in original procedure, seems to list index entries per doc.
            //WriteLn(AWord.TheWord,'             documents = ', AWord.DocumentCount, ' h
  pinteger(state)^:=cnt;
end;

procedure TChmWriter.WriteTOPICS;
var
  FHits: Integer;
  i: Integer;
begin
  if FTopicsStream.Size = 0 then
    Exit;
  FTopicsStream.Position := 0;
  PostAddStreamToArchive('#TOPICS', '/', FTopicsStream);
 // I commented the code below since the result seemed unused
 // FHits:=0;
 //   FIndexedFiles.ForEach(@IterateWord,FHits);
end;

procedure TChmWriter.WriteIVB;
begin
  if FContextStream = nil then exit;

  FContextStream.Position := 0;
  // the size of all the entries
  FContextStream.WriteDWord(NToLE(DWord(FContextStream.Size-SizeOf(dword))));

  FContextStream.Position := 0;
  AddStreamToArchive('#IVB', '/', FContextStream);
end;

procedure TChmWriter.WriteURL_STR_TBL;
begin
  if FURLSTRStream.Size <> 0 then begin
    FURLSTRStream.Position := 0;
    PostAddStreamToArchive('#URLSTR', '/', FURLSTRStream);
  end;
  if FURLTBLStream.Size <> 0 then begin
    FURLTBLStream.Position := 0;
    PostAddStreamToArchive('#URLTBL', '/', FURLTBLStream);
  end;
end;

procedure TChmWriter.WriteOBJINST;
var
  i: Integer;
  ObjStream: TMemoryStream;
  //Flags: Word;
begin
  ObjStream := TMemorystream.Create;
  // this file is needed to enable searches for the ms reader
  ObjStream.WriteDWord(NtoLE($04000000));
  ObjStream.WriteDWord(NtoLE(Dword(2))); // two entries

  ObjStream.WriteDWord(NtoLE(DWord(24))); // offset into file of entry
  ObjStream.WriteDWord(NtoLE(DWord(2691))); // size

  ObjStream.WriteDWord(NtoLE(DWord(2715))); // offset into file of entry
  ObjStream.WriteDWord(NtoLE(DWord(36))); // size

  // first entry
  // write guid 4662DAAF-D393-11D0-9A56-00C04FB68BF7
  ObjStream.WriteDWord(NtoLE($4662DAAF));
  ObjStream.WriteWord(NtoLE($D393));
  ObjStream.WriteWord(NtoLE($11D0));
  ObjStream.WriteWord(NtoLE($569A));
  ObjStream.WriteByte($00);
  ObjStream.WriteByte($C0);
  ObjStream.WriteByte($4F);
  ObjStream.WriteByte($B6);
  ObjStream.WriteByte($8B);
  ObjStream.WriteByte($F7);

  ObjStream.WriteDWord(NtoLE($04000000));
  ObjStream.WriteDWord(NtoLE(11));  // bit flags
  ObjStream.WriteDWord(NtoLE(DWord(1252)));
  ObjStream.WriteDWord(NtoLE(DWord(1033)));
  ObjStream.WriteDWord(NtoLE($00000000));
  ObjStream.WriteDWord(NtoLE($00000000));
  ObjStream.WriteDWord(NtoLE($00145555));
  ObjStream.WriteDWord(NtoLE($00000A0F));
  ObjStream.WriteWord(NtoLE($0100));
  ObjStream.WriteDWord(NtoLE($00030005));
  for i := 0 to 5 do
    ObjStream.WriteDWord($00000000);
  ObjStream.WriteWord($0000);
  // okay now the fun stuff
  for i := 0 to $FF do
  ObjStream.Write(ObjInstEntries[i], SizeOF(TObjInstEntry));
  {begin
    if i = 1 then
      Flags := 7
    else
      Flags := 0;
    if (i >= $41) and (i <= $5A) then
      Flags := Flags or 2;
    if (i >= $61) and (i <= $7A) then
      Flags := Flags or 1;
    if i = $27 then
      Flags := Flags or 6;
    ObjStream.WriteWord(NtoLE(Flags));
    ObjStream.WriteWord(NtoLE(Word(i)));
    if (i >= $41) and (i <= $5A) then
      ObjStream.WriteByte(NtoLE(i+$20))
    else
      ObjStream.WriteByte(NtoLE(i));
    ObjStream.WriteByte(NtoLE(i));
    ObjStream.WriteByte(NtoLE(i));
    ObjStream.WriteByte(NtoLE(i));
    ObjStream.WriteWord(NtoLE($0000));
  end;}
  ObjStream.WriteDWord(NtoLE($E66561C6));
  ObjStream.WriteDWord(NtoLE($73DF6561));
  ObjStream.WriteDWord(NtoLE($656F8C73));
  ObjStream.WriteWord(NtoLE($6F9C));
  ObjStream.WriteByte(NtoLE($65));
  // third bit of second entry
  // write guid 8FA0D5A8-DEDF-11D0-9A61-00C04FB68BF7
  ObjStream.WriteDWord(NtoLE($8FA0D5A8));
  ObjStream.WriteWord(NtoLE($DEDF));
  ObjStream.WriteWord(NtoLE($11D0));
  ObjStream.WriteWord(NtoLE($619A));
  ObjStream.WriteByte($00);
  ObjStream.WriteByte($C0);
  ObjStream.WriteByte($4F);
  ObjStream.WriteByte($B6);
  ObjStream.WriteByte($8B);
  ObjStream.WriteByte($F7);

  ObjStream.WriteDWord(NtoLE($04000000));
  ObjStream.WriteDWord(NtoLE(DWord(1)));
  ObjStream.WriteDWord(NtoLE(DWord(1252)));
  ObjStream.WriteDWord(NtoLE(DWord(1033)));
  ObjStream.WriteDWord(NtoLE(DWord(0)));

  // second entry
  // write guid 4662DAB0-D393-11D0-9A56-00C04FB68B66
  ObjStream.WriteDWord(NtoLE($4662DAB0));
  ObjStream.WriteWord(NtoLE($D393));
  ObjStream.WriteWord(NtoLE($11D0));
  ObjStream.WriteWord(NtoLE($569A));
  ObjStream.WriteByte($00);
  ObjStream.WriteByte($C0);
  ObjStream.WriteByte($4F);
  ObjStream.WriteByte($B6);
  ObjStream.WriteByte($8B);
  ObjStream.WriteByte($66);

  ObjStream.WriteDWord(NtoLE(DWord(666))); // not kidding
  ObjStream.WriteDWord(NtoLE(DWord(1252)));
  ObjStream.WriteDWord(NtoLE(DWord(1033)));
  ObjStream.WriteDWord(NtoLE(DWord(10031)));
  ObjStream.WriteDWord(NtoLE(DWord(0)));

  ObjStream.Position := 0;
  AddStreamToArchive('$OBJINST', '/', ObjStream, True);
  ObjStream.Free;

end;

procedure TChmWriter.WriteFiftiMain;
var
  SearchWriter: TChmSearchWriter;
begin
  if FTopicsStream.Size = 0 then
    Exit;
  SearchWriter := TChmSearchWriter.Create(FFiftiMainStream, FIndexedFiles);
  SearchWriter.WriteToStream;
  SearchWriter.Free;

  if FFiftiMainStream.Size = 0 then
    Exit;

  FFiftiMainStream.Position := 0;
  PostAddStreamToArchive('$FIftiMain', '/', FFiftiMainStream);
end;

procedure TChmWriter.WriteREADMEFile;
const DISCLAIMER_STR = 'This archive was not made by the MS HTML Help Workshop(r)(tm) program.';
var
  Entry: TFileEntryRec;
begin
  // This procedure puts a file in the archive that says it wasn't compiled with the MS compiler
  Entry.Compressed := False;
  Entry.DecompressedOffset := FSection0.Position;
  FSection0.Write(DISCLAIMER_STR, SizeOf(DISCLAIMER_STR));
  Entry.DecompressedSize := FSection0.Position - Entry.DecompressedOffset;
  Entry.Path := '/';
  Entry.Name := '_#_README_#_'; //try to use a name that won't conflict with normal names
  FInternalFiles.AddEntry(Entry);
end;

procedure TChmWriter.WriteFinalCompressedFiles;
begin
  WriteTOPICS;
  WriteURL_STR_TBL;
  WriteSTRINGS;
  WriteFiftiMain;
end;


procedure TChmWriter.WriteSection0;
begin
  FSection0.Position := 0;
  FOutStream.CopyFrom(FSection0, FSection0.Size);
end;

procedure TChmWriter.WriteSection1;
begin
  WriteContentToStream(FOutStream, FSection1);
end;

procedure TChmWriter.WriteDataSpaceFiles(const AStream: TStream);
var
  Entry: TFileEntryRec;
begin
  // This procedure will write all files starting with ::
  Entry.Compressed := False; // None of these files are compressed

  //  ::DataSpace/NameList
  Entry.DecompressedOffset := FSection0.Position;
  Entry.DecompressedSize := WriteNameListToStream(FSection0, [snUnCompressed,snMSCompressed]);
  Entry.Path := '::DataSpace/';
  Entry.Name := 'NameList';
  FInternalFiles.AddEntry(Entry, False);

  //  ::DataSpace/Storage/MSCompressed/ControlData
  Entry.DecompressedOffset := FSection0.Position;
  Entry.DecompressedSize := WriteControlDataToStream(FSection0, 2, 2, 1);
  Entry.Path := '::DataSpace/Storage/MSCompressed/';
  Entry.Name := 'ControlData';
  FInternalFiles.AddEntry(Entry, False);

  //  ::DataSpace/Storage/MSCompressed/SpanInfo
  Entry.DecompressedOffset := FSection0.Position;
  Entry.DecompressedSize := WriteSpanInfoToStream(FSection0, FReadCompressedSize);
  Entry.Path := '::DataSpace/Storage/MSCompressed/';
  Entry.Name := 'SpanInfo';
  FInternalFiles.AddEntry(Entry, False);

  //  ::DataSpace/Storage/MSCompressed/Transform/List
  Entry.DecompressedOffset := FSection0.Position;
  Entry.DecompressedSize := WriteTransformListToStream(FSection0);
  Entry.Path := '::DataSpace/Storage/MSCompressed/Transform/';
  Entry.Name := 'List';
  FInternalFiles.AddEntry(Entry, False);

  //  ::DataSpace/Storage/MSCompressed/Transform/{7FC28940-9D31-11D0-9B27-00A0C91E9C7C}/
  //  ::DataSpace/Storage/MSCompressed/Transform/{7FC28940-9D31-11D0-9B27-00A0C91E9C7C}/InstanceData/ResetTable
  Entry.DecompressedOffset := FSection0.Position;
  Entry.DecompressedSize := WriteResetTableToStream(FSection0, FSection1ResetTable);
  Entry.Path := '::DataSpace/Storage/MSCompressed/Transform/{7FC28940-9D31-11D0-9B27-00A0C91E9C7C}/InstanceData/';
  Entry.Name := 'ResetTable';
  FInternalFiles.AddEntry(Entry, True);


  //  ::DataSpace/Storage/MSCompressed/Content do this last
  Entry.DecompressedOffset := FSection0.Position;
  Entry.DecompressedSize := FSection1Size; // we will write it directly to FOutStream later
  Entry.Path := '::DataSpace/Storage/MSCompressed/';
  Entry.Name := 'Content';
  FInternalFiles.AddEntry(Entry, False);


end;

function TChmWriter.AddString(AString: String): LongWord;
var
  NextBlock: DWord;
  Pos: DWord;
  n  : TAVLTreeNode;
  StrRec : TStringIndex;
begin
  // #STRINGS starts with a null char
  if FStringsStream.Size = 0 then FStringsStream.WriteByte(0);

  SpareString.TheString:=AString;
  n:=fAvlStrings.FindKey(SpareString,@CompareStrings);
  if assigned(n) then
   exit(TStringIndex(n.data).strid);

  // each entry is a null terminated string
  Pos := DWord(FStringsStream.Position);

  // Strings are contained in $1000 byte blocks and cannot cross blocks
  NextBlock := ($0000F000 and Pos) + $00001000;
  if Length(AString) + 1 > NextBlock then
  begin
    FStringsStream.Size:= NextBlock;
    FStringsStream.Position := NextBlock;
  end;

  Result := FStringsStream.Position;
  FStringsStream.WriteBuffer(AString[1], Length(AString));
  FStringsStream.WriteByte(0);

  StrRec:=TStringIndex.Create;
  StrRec.TheString:=AString;
  StrRec.Strid    :=Result;
  fAvlStrings.Add(StrRec);
end;

function TChmWriter.AddURL ( AURL: String; TopicsIndex: DWord ) : LongWord;

  procedure CheckURLStrBlockCanHold(Const AString: String);
  var
    Rem: LongWord;
    Len: LongWord;
  begin
    Rem := $4000 - (FURLSTRStream.Size mod $4000);
    Len := 9 + Length(AString);  // 2 dwords the string and NT
    if Rem < Len then
      while Rem > 0 do
      begin
        FURLSTRStream.WriteByte(0);
        Dec(Rem);
      end;
  end;

  function AddURLString(Const AString: String): DWord;
  var urlstrrec : TUrlStrIndex;
  begin
    CheckURLStrBlockCanHold(AString);
    if FURLSTRStream.Size mod $4000 = 0 then
      FURLSTRStream.WriteByte(0);
      Result := FURLSTRStream.Position;
      UrlStrRec:=TUrlStrIndex.Create;
      UrlStrRec.UrlStr:=AString;
      UrlStrRec.UrlStrid:=result;
      FAvlUrlStr.Add(UrlStrRec);
      FURLSTRStream.WriteDWord(NToLE(DWord(0))); // URL Offset for topic after the the "Local" value
      FURLSTRStream.WriteDWord(NToLE(DWord(0))); // Offset of FrameName??
      FURLSTRStream.Write(AString[1], Length(AString));
      FURLSTRStream.WriteByte(0); //NT
  end;

  function LookupUrlString(const AUrl : String):DWord;
  var n :TAvlTreeNode;
  begin
    SpareUrlStr.UrlStr:=AUrl;
    n:=FAvlUrlStr.FindKey(SpareUrlStr,@CompareUrlStrs);
    if assigned(n) Then
      result:=TUrlStrIndex(n.data).UrlStrId
    else
      result:=AddUrlString(AUrl);
  end;


var UrlIndex : Integer;

begin
  if AURL[1] = '/' then Delete(AURL,1,1);
  UrlIndex:=LookupUrlString(AUrl);

  //if $1000 - (FURLTBLStream.Size mod $1000) = 4 then // we are at 4092
  if FURLTBLStream.Size and $FFC = $FFC then // faster :)
    FURLTBLStream.WriteDWord(0);
  Result := FURLTBLStream.Position;
  FURLTBLStream.WriteDWord(0);//($231e9f5c); //unknown
  FURLTBLStream.WriteDWord(NtoLE(TopicsIndex)); // Index of topic in #TOPICS
  FURLTBLStream.WriteDWord(NtoLE(UrlIndex));
end;

function _AtEndOfData(arg: pointer): LongBool; cdecl;
begin
  Result := TChmWriter(arg).AtEndOfData;
end;

function TChmWriter.AtEndOfData: LongBool;
begin
  Result := ForceExit or (FCurrentIndex >= FFileNames.Count-1);
  if Result then
    Result := Integer(FCurrentStream.Position) >= Integer(FCurrentStream.Size)-1;
end;

function _GetData(arg: pointer; Count: LongInt; Buffer: Pointer): LongInt; cdecl;
begin
  Result := TChmWriter(arg).GetData(Count, PByte(Buffer));
end;

function TChmWriter.GetData(Count: LongInt; Buffer: PByte): LongInt;
var
  FileEntry: TFileEntryRec;
begin
  Result := 0;
  while (Result < Count) and (not AtEndOfData) do begin
    Inc(Result, FCurrentStream.Read(Buffer[Result], Count-Result));
    if (Result < Count) and (not AtEndOfData)
    then begin
      // the current file has been read. move to the next file in the list
      FCurrentStream.Position := 0;
      Inc(FCurrentIndex);
      ForceExit := OnGetFileData(FFileNames[FCurrentIndex], FileEntry.Path, FileEntry.Name, FCurrentStream);
      FileEntry.DecompressedSize := FCurrentStream.Size;
      FileEntry.DecompressedOffset := FReadCompressedSize; //269047723;//to test writing really large numbers
      FileEntry.Compressed := True;

      if FullTextSearch then
        CheckFileMakeSearchable(FCurrentStream, FileEntry);

      FInternalFiles.AddEntry(FileEntry);
      // So the next file knows it's offset
      Inc(FReadCompressedSize,  FileEntry.DecompressedSize);
      FCurrentStream.Position := 0;
    end;

    // this is intended for programs to add perhaps a file
    // after all the other files have been added.
    if (AtEndOfData)
    and (FCurrentStream <> FPostStream) then
    begin
      FPostStreamActive := True;
      if Assigned(FOnLastFile) then
        FOnLastFile(Self);
      FCurrentStream.Free;
      WriteFinalCompressedFiles;
      FCurrentStream := FPostStream;
      FCurrentStream.Position := 0;
      Inc(FReadCompressedSize, FCurrentStream.Size);
    end;
  end;
end;

function _WriteCompressedData(arg: pointer; Count: LongInt; Buffer: Pointer): LongInt; cdecl;
begin
  Result := TChmWriter(arg).WriteCompressedData(Count, Buffer);
end;

function TChmWriter.WriteCompressedData(Count: Longint; Buffer: Pointer): LongInt;
begin
  // we allocate a MB at a time to limit memory reallocation since this
  // writes usually 2 bytes at a time
  if (FSection1 is TMemoryStream) and (FSection1.Position >= FSection1.Size-1) then begin
    FSection1.Size := FSection1.Size+$100000;
  end;
  Result := FSection1.Write(Buffer^, Count);
  Inc(FSection1Size, Result);
end;

procedure _MarkFrame(arg: pointer; UncompressedTotal, CompressedTotal: LongWord); cdecl;
begin
  TChmWriter(arg).MarkFrame(UncompressedTotal, CompressedTotal);
end;

procedure TChmWriter.MarkFrame(UnCompressedTotal, CompressedTotal: LongWord);
  procedure WriteQWord(Value: QWord);
  begin
    FSection1ResetTable.Write(NToLE(Value), 8);
  end;
  procedure IncEntryCount;
  var
    OldPos: QWord;
    Value: DWord;
  begin
    OldPos := FSection1ResetTable.Position;
    FSection1ResetTable.Position := $4;
    Value := LeToN(FSection1ResetTable.ReadDWord)+1;
    FSection1ResetTable.Position := $4;
    FSection1ResetTable.WriteDWord(NToLE(Value));
    FSection1ResetTable.Position := OldPos;
  end;
  procedure UpdateTotalSizes;
  var
    OldPos: QWord;
  begin
    OldPos := FSection1ResetTable.Position;
    FSection1ResetTable.Position := $10;
    WriteQWord(FReadCompressedSize); // size of read data that has been compressed
    WriteQWord(CompressedTotal);
    FSection1ResetTable.Position := OldPos;
  end;
begin
  if FSection1ResetTable.Size = 0 then begin
    // Write the header
    FSection1ResetTable.WriteDWord(NtoLE(DWord(2)));
    FSection1ResetTable.WriteDWord(0); // number of entries. we will correct this with IncEntryCount
    FSection1ResetTable.WriteDWord(NtoLE(DWord(8))); // Size of Entries (qword)
    FSection1ResetTable.WriteDWord(NtoLE(DWord($28))); // Size of this header
    WriteQWord(0); // Total Uncompressed Size
    WriteQWord(0); // Total Compressed Size
    WriteQWord(NtoLE($8000)); // Block Size
    WriteQWord(0); // First Block start
  end;
  IncEntryCount;
  UpdateTotalSizes;
  WriteQWord(CompressedTotal); // Next Block Start
  // We have to trim the last entry off when we are done because there is no next block in that case
end;

{$IFDEF LZX_USETHREADS}
function TChmWriter.LTGetData(Sender: TLZXCompressor; WantedByteCount: Integer;
  Buffer: Pointer): Integer;
begin
  Result := GetData(WantedByteCount, Buffer);
  //WriteLn('Wanted ', WantedByteCount, ' got ', Result);
end;

function TChmWriter.LTIsEndOfFile(Sender: TLZXCompressor): Boolean;
begin
  Result := AtEndOfData;
end;

procedure TChmWriter.LTChunkDone(Sender: TLZXCompressor;
  CompressedSize: Integer; UncompressedSize: Integer; Buffer: Pointer);
begin
  WriteCompressedData(CompressedSize, Buffer);
end;

procedure TChmWriter.LTMarkFrame(Sender: TLZXCompressor;
  CompressedTotal: Integer; UncompressedTotal: Integer);
begin
  MarkFrame(UncompressedTotal, CompressedTotal);
  //WriteLn('Mark Frame C = ', CompressedTotal, ' U = ', UncompressedTotal);
end;
{$ENDIF}

procedure TChmWriter.CheckFileMakeSearchable(AStream: TStream; AFileEntry: TFileEntryRec);

  var
    TopicEntry: TTopicEntry;
    ATitle: String;
begin
  if Pos('.ht', AFileEntry.Name) > 0 then
  begin
    ATitle := FIndexedFiles.IndexFile(AStream, NextTopicIndex, FSearchTitlesOnly);
    if ATitle <> '' then
      TopicEntry.StringsOffset := AddString(ATitle)
    else
      TopicEntry.StringsOffset := $FFFFFFFF;
    TopicEntry.URLTableOffset := AddURL(AFileEntry.Path+AFileEntry.Name, NextTopicIndex);
    TopicEntry.InContents := 2;
    TopicEntry.Unknown := 0;
    TopicEntry.TocOffset := 0;
    FTopicsStream.WriteDWord(LEtoN(TopicEntry.TocOffset));
    FTopicsStream.WriteDWord(LEtoN(TopicEntry.StringsOffset));
    FTopicsStream.WriteDWord(LEtoN(TopicEntry.URLTableOffset));
    FTopicsStream.WriteWord(LEtoN(TopicEntry.InContents));
    FTopicsStream.WriteWord(LEtoN(TopicEntry.Unknown));
  end;
end;

function TChmWriter.NextTopicIndex: Integer;
begin
  Result := FTopicsStream.Size div 16;
end;

constructor TChmWriter.Create(OutStream: TStream; FreeStreamOnDestroy: Boolean);
begin
  if OutStream = nil then Raise Exception.Create('TChmWriter.OutStream Cannot be nil!');
  FCurrentStream := TMemoryStream.Create;
  FCurrentIndex := -1;
  FOutStream := OutStream;
  FInternalFiles := TFileEntryList.Create;
  FStringsStream := TmemoryStream.Create;
  FTopicsStream := TMemoryStream.Create;
  FURLSTRStream := TMemoryStream.Create;
  FURLTBLStream := TMemoryStream.Create;
  FFiftiMainStream := TMemoryStream.Create;
  FSection0 := TMemoryStream.Create;
  FSection1 := TMemoryStream.Create;
  FSection1ResetTable := TMemoryStream.Create;
  FDirectoryListings := TMemoryStream.Create;
  FPostStream := TMemoryStream.Create;;
  FDestroyStream := FreeStreamOnDestroy;
  FFileNames := TStringList.Create;
  FIndexedFiles := TIndexedWordList.Create;
  FAvlStrings   := TAVLTree.Create(@CompareStrings);    // dedupe strings
  FAvlURLStr    := TAVLTree.Create(@CompareUrlStrs);    // dedupe urltbl + binindex must resolve URL to topicid
  SpareString   := TStringIndex.Create;                 // We need an object to search in avltree
  SpareUrlStr   := TUrlStrIndex.Create;                 //    to avoid create/free circles we keep one in spare
                                                        //    for searching purposes
end;

destructor TChmWriter.Destroy;
begin
  if FDestroyStream then FOutStream.Free;
  if Assigned(FContextStream) then FContextStream.Free;
  FInternalFiles.Free;
  FCurrentStream.Free;
  FStringsStream.Free;
  FTopicsStream.Free;
  FURLSTRStream.Free;
  FURLTBLStream.Free;
  FFiftiMainStream.Free;
  FSection0.Free;
  FSection1.Free;
  FSection1ResetTable.Free;
  FDirectoryListings.Free;
  FFileNames.Free;
  FIndexedFiles.Free;
  SpareString.free;
  SpareUrlStr.free;
  FAvlUrlStr.FreeAndClear;
  FAvlUrlStr.Free;
  FAvlStrings.FreeAndClear;
  FAvlStrings.Free;
  inherited Destroy;
end;

procedure TChmWriter.Execute;
begin
  InitITSFHeader;
  FOutStream.Position := 0;
  FSection1Size := 0;

  // write any internal files to FCurrentStream that we want in the compressed section
  WriteIVB;

  // written to Section0 (uncompressed)
  WriteREADMEFile;

  WriteOBJINST;

  // move back to zero so that we can start reading from zero :)
  FReadCompressedSize := FCurrentStream.Size;
  FCurrentStream.Position := 0;  // when compressing happens, first the FCurrentStream is read
                                 // before loading user files. So we can fill FCurrentStream with
                                 // internal files first.

  // this gathers ALL files that should be in section1 (the compressed section)
  StartCompressingStream;
  FSection1.Size := FSection1Size;

  // This creates and writes the #ITBITS (empty) file to section0
  WriteITBITS;
  // This creates and writes the #SYSTEM file to section0
  WriteSystem;


  //this creates all special files in the archive that start with ::DataSpace
  WriteDataSpaceFiles(FSection0);

  // creates all directory listings including header
  CreateDirectoryListings;

  // do this after we have compressed everything so that we know the values that must be written
  InitHeaderSectionTable;

  // Now we can write everything to FOutStream
  WriteHeader(FOutStream);
  WriteDirectoryListings(FOutStream);
  WriteSection0; //does NOT include section 1 even though section0.content IS section1
  WriteSection1; // writes section 1 to FOutStream
end;

procedure TChmWriter.AppendTOC(AStream: TStream);
begin
  FHasTOC := True;
  PostAddStreamToArchive('default.hhc', '/', AStream, True);
end;

procedure TChmWriter.AppendBinaryTOCFromSiteMap(ASiteMap: TChmSiteMap);
var
  Header: TTOCIdxHeader;
  Entry: TTocEntry;
  EntryInfo: TTOCEntryPageBookInfo;


  EntryInfoStream,
  EntryTopicOffsetStream,
  EntryStream: TMemoryStream;

  TOCIDXStream: TMemoryStream;

  NextLevelItems,
  CurrentLevelItems: TFPList;
  i,j: Integer;
  MenuItem: TChmSiteMapItem;
  MenuItems: TChmSiteMapItems;
  TopicEntry: TTopicEntry;
  EntryCount: DWord = $29A;
  procedure FixParentBookFirstChildOffset(AChildOffset: DWord);
  var
    ParentEntry: TTOCEntryPageBookInfo;
  begin
    // read parent entry
    EntryInfoStream.Position := MenuItems.InternalData;
    EntryInfoStream.Read(ParentEntry, SizeOf(ParentEntry));
    // update child offset
    ParentEntry.FirstChildOffset:= NtoLE(DWord(4096 + AChildOffset));
    // write back to stream
    EntryInfoStream.Position := MenuItems.InternalData;
    EntryInfoStream.Write(ParentEntry, SizeOf(ParentEntry));
    // move to end of stream
    EntryInfoStream.Position := AChildOffset;
  end;

begin
  FillChar(Header, 4096, 0);
  // create streams
  TOCIDXStream := TMemoryStream.Create;
  EntryInfoStream := TMemoryStream.Create;
  EntryTopicOffsetStream := TMemoryStream.Create;
  EntryStream := TMemoryStream.Create;

  NextLevelItems := TFPList.Create;

  NextLevelItems.Add(ASiteMap.Items);

  if NextLevelItems.Count = 0 then
      FreeAndNil(NextLevelItems);

  while NextLevelItems <> nil do
  begin
    CurrentLevelItems := NextLevelItems;
    NextLevelItems := TFPList.Create;

    for i := 0 to CurrentLevelItems.Count-1 do
    begin
      MenuItems := TChmSiteMapItems(CurrentLevelItems.Items[i]);

      for j := 0 to MenuItems.Count-1 do
      begin
        MenuItem := MenuItems.Item[j];
        // first figure out the props
        EntryInfo.Props := 0;
        if MenuItem.Children.Count > 0 then
          EntryInfo.Props := EntryInfo.Props or TOC_ENTRY_HAS_CHILDREN;
        if Length(MenuItem.Local) > 0 then
          EntryInfo.Props := EntryInfo.Props or TOC_ENTRY_HAS_LOCAL;


      if EntryInfo.Props and TOC_ENTRY_HAS_LOCAL > 0 then
      begin
        // Write #TOPICS entry
        TopicEntry.TocOffset      := NtoLE(DWord(4096 + EntryInfoStream.Position));
        TopicEntry.StringsOffset  := NtoLE(AddString(MenuItem.Text));
        TopicEntry.URLTableOffset := NtoLE(AddURL(MenuItem.Local, NextTopicIndex));
        TopicEntry.InContents     := NtoLE(Word( 2 ));
        TopicEntry.Unknown        := 0;
        EntryInfo.TopicsIndexOrStringsOffset := NtoLE(Dword(NextTopicIndex));;
        FTopicsStream.Write(TopicEntry, SizeOf(TopicEntry));
        EntryTopicOffsetStream.WriteDWord(EntryInfo.TopicsIndexOrStringsOffset);

        // write TOCEntry
        Entry.PageBookInfoOffset:= NtoLE(4096 + EntryInfoStream.Position);
        Entry.IncrementedInt  := NtoLE(EntryCount);
        EntryStream.Write(Entry, SizeOf(Entry));
        Inc(EntryCount);

      end
      else
      begin
        EntryInfo.TopicsIndexOrStringsOffset := NtoLE(AddString(MenuItem.Text));
      end;


        // write TOCEntryInfo

        EntryInfo.Unknown1 := 0;
        EntryInfo.EntryIndex := NtoLE(Word(EntryCount - $29A)); //who knows how useful any of this is

        if MenuItems.InternalData <> maxLongint then
          EntryInfo.ParentPageBookInfoOffset := MenuItems.InternalData
        else
          EntryInfo.ParentPageBookInfoOffset := 0;

        if j = MenuItems.Count-1 then
          EntryInfo.NextPageBookOffset := 0
        else if (EntryInfo.Props and TOC_ENTRY_HAS_CHILDREN) > 0 then
          EntryInfo.NextPageBookOffset := 4096 + EntryInfoStream.Position + 28
        else
          EntryInfo.NextPageBookOffset := 4096 + EntryInfoStream.Position + 20;

        // Only if TOC_ENTRY_HAS_CHILDREN is set are these written
        EntryInfo.FirstChildOffset := 0; // we will update this when the child is written
        // in fact lets update the *parent* of this item now if needed
        if (j = 0) and (MenuItems.InternalData <> maxLongint) then
          FixParentBookFirstChildOffset(EntryInfoStream.Position);

        EntryInfo.Unknown3 := 0;

        // fix endian order
        EntryInfo.Props := NtoLE(EntryInfo.Props);
        EntryInfo.ParentPageBookInfoOffset := NtoLE(EntryInfo.ParentPageBookInfoOffset);
        EntryInfo.NextPageBookOffset := NtoLE(EntryInfo.NextPageBookOffset);

        if MenuItem.Children.Count > 0 then
        begin
          NextLevelItems.Add(MenuItem.Children);
          MenuItem.Children.InternalData := EntryInfoStream.Position;
        end;

        // write to stream
        EntryInfoStream.Write(EntryInfo, PageBookInfoRecordSize(@EntryInfo));
      end;
    end;

    FreeAndNil(CurrentLevelItems);
    if NextLevelItems.Count = 0 then
      FreeAndNil(NextLevelItems);
  end;

  // write all streams to TOCIdxStream and free everything
  EntryInfoStream.Position:=0;
  EntryTopicOffsetStream.Position:=0;
  EntryStream.Position:=0;

  Header.BlockSize := NtoLE(DWord(4096));
  Header.EntriesCount := NtoLE(DWord(EntryCount - $29A));
  Header.EntriesOffset := NtoLE(DWord(4096 + EntryInfoStream.Size + EntryTopicOffsetStream.Size));
  Header.TopicsOffset := NtoLE(DWord(4096 + EntryInfoStream.Size));

  TOCIDXStream.Write(Header, SizeOf(Header));

  TOCIDXStream.CopyFrom(EntryInfoStream, EntryInfoStream.Size);
  EntryInfoStream.Free;

  TOCIDXStream.CopyFrom(EntryTopicOffsetStream, EntryTopicOffsetStream.Size);
  EntryTopicOffsetStream.Free;

  TOCIDXStream.CopyFrom(EntryStream, EntryStream.Size);
  EntryStream.Free;

  TOCIDXStream.Position := 0;
  AppendBinaryTOCStream(TOCIDXStream);
  TOCIDXStream.Free;
end;

procedure TChmWriter.AppendBinaryTOCStream(AStream: TStream);
begin
  AddStreamToArchive('#TOCIDX', '/', AStream, True);
end;

procedure TChmWriter.AppendIndex(AStream: TStream);
begin
  FHasIndex := True;
  PostAddStreamToArchive('default.hhk', '/', AStream, True);
end;

procedure TChmWriter.AppendSearchDB(AName: String; AStream: TStream);
begin
  PostAddStreamToArchive(AName, '/', AStream);
end;


// this procedure is used to manually add files to compress to an internal stream that is
// processed before FileToCompress is called. Files added this way should not be
// duplicated in the FilesToCompress property.
procedure TChmWriter.AddStreamToArchive(AFileName, APath: String; AStream: TStream; Compress: Boolean = True);
var
  TargetStream: TStream;
  Entry: TFileEntryRec;
begin
  // in case AddStreamToArchive is used after we should be writing to the post stream
  if FPostStreamActive then
  begin
    PostAddStreamToArchive(AFileName, APath, AStream, Compress);
    Exit;
  end;
  if AStream = nil then Exit;
  if Compress then
    TargetStream := FCurrentStream
  else
    TargetStream := FSection0;

  Entry.Name := AFileName;
  Entry.Path := APath;
  Entry.Compressed :=  Compress;
  Entry.DecompressedOffset := TargetStream.Position;
  Entry.DecompressedSize := AStream.Size;
  if FullTextSearch then
    CheckFileMakeSearchable(AStream, Entry); // Must check before we add it to the list so we know if the name needs to be added to #STRINGS
  FInternalFiles.AddEntry(Entry);
  AStream.Position := 0;
  TargetStream.CopyFrom(AStream, AStream.Size);
end;

procedure TChmWriter.PostAddStreamToArchive(AFileName, APath: String;
  AStream: TStream; Compress: Boolean);
var
  TargetStream: TStream;
  Entry: TFileEntryRec;
begin
  if AStream = nil then Exit;
  if Compress then
    TargetStream := FPostStream
  else
    TargetStream := FSection0;

  Entry.Name := AFileName;
  Entry.Path := APath;
  Entry.Compressed :=  Compress;
  if not Compress then
    Entry.DecompressedOffset := TargetStream.Position
  else
    Entry.DecompressedOffset := FReadCompressedSize + TargetStream.Position;
  Entry.DecompressedSize := AStream.Size;
  FInternalFiles.AddEntry(Entry);
  AStream.Position := 0;
  TargetStream.CopyFrom(AStream, AStream.Size);
  if FullTextSearch then
    CheckFileMakeSearchable(AStream, Entry);
end;

procedure TChmWriter.AddContext(AContext: DWord; ATopic: String);
var
  Offset: DWord;
begin
  if FContextStream = nil then begin
    // #IVB starts with a dword which is the size of the stream - sizeof(dword)
    FContextStream.WriteDWord(0);
    // we will update this when we write the file to the final stream
  end;
  // an entry is a context id and then the offset of the name of the topic in the strings file
  FContextStream.WriteDWord(NToLE(AContext));
  Offset := NToLE(AddString(ATopic));
  FContextStream.WriteDWord(Offset);
end;

procedure TChmWriter.StartCompressingStream;
var
  {$IFNDEF LZX_USETHREADS}
  LZXdata: Plzx_data;
  WSize: LongInt;
  {$ELSE}
  Compressor: TLZXCompressor;
  {$ENDIF}
begin
 {$IFNDEF LZX_USETHREADS}
  lzx_init(@LZXdata, LZX_WINDOW_SIZE, @_GetData, Self, @_AtEndOfData,
              @_WriteCompressedData, Self, @_MarkFrame, Self);

  WSize := 1 shl LZX_WINDOW_SIZE;
  while not AtEndOfData do begin
    lzx_reset(LZXdata);
    lzx_compress_block(LZXdata, WSize, True);
  end;

  //we have to mark the last frame manually
  MarkFrame(LZXdata^.len_uncompressed_input, LZXdata^.len_compressed_output);

  lzx_finish(LZXdata, nil);
  {$ELSE}
  Compressor := TLZXCompressor.Create(10);
  Compressor.OnChunkDone  :=@LTChunkDone;
  Compressor.OnGetData    :=@LTGetData;
  Compressor.OnIsEndOfFile:=@LTIsEndOfFile;
  Compressor.OnMarkFrame  :=@LTMarkFrame;
  Compressor.Execute(True);
  //Sleep(20000);
  Compressor.Free;
  {$ENDIF}
end;

end.

