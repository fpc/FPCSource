{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 2000 by Berczi Gabor

    Help support for Windows Help (.HLP) files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$R-}
unit WWinHelp;

interface

uses Objects,
     WUtils,WHelp;

const
     WinHelpMagicNo             = $00035F3F;
     WinHelpBTreeHeaderMagicNo  = $293B;
     WinHelpSystemHeaderMagicNo = $036c;

     { WinHelp Btree dataformat descriptors }
     wh_bdf_Long           = 'L';
     wh_bdf_ASCIIZ         = 'F';
     wh_bdf_ASCIIZ2        = 'i';
     wh_bdf_Short          = '2';
     wh_bdf_Long2          = '4';
     wh_bdf_ASCIIZ3        = 'z';

     { WinHelp system record type }
     wh_srt_Title          =  1;
     wh_srt_Copyright      =  2;
     wh_srt_Contents       =  3;
     wh_srt_Config         =  4;
     wh_srt_Icon           =  5;
     wh_srt_Window         =  6;
     wh_srt_Citation       =  8;
     wh_srt_LangID         =  9;
     wh_srt_Content        = 10;
     wh_srt_DefFont        = 11;
     wh_srt_FtIndex        = 12;
     wh_srt_Groups         = 13;
     wh_srt_KeyIndex       = 14;
     wh_srt_Language       = 18;
     wh_srt_DLLMaps        = 19;

type
      PInteger = ^integer;

      TWinHelpHeader = packed record
        MagicNo          : longint;
        DirectoryStart   : longint;
        NonDirectoryStart: longint;
        EntireFileSize   : longint;
      end;

      TWinHelpFileEntryHeader = packed record
        ReservedSpace    : longint;
        UsedSpace        : longint;
        FileFlags        : byte;
      end;

      TWinHelpRecordHeader = packed record
        RecordType       : word;
        DataSize         : word;
      end;

      TWinHelpBTreeHeader = packed record
        Magic            : word;
        Flags            : word; { 0x0002 always set, 0x0400 set if directory }
        PageSize         : word;
        DataFormat       : array[1..16] of char;
        MustBeZero       : word; { $0000 }
        PageSplits       : word;
        RootPage         : word;
        MustBeNegOne     : integer; { $ffff }
        TotalPages       : word;
        NumLevels        : word;
        TotalEntries     : word;
        Pages            : record end;
      end;

      TWinHelpBTreeIndexHeader = packed record
        Unknown          : longint;
        NumEntries       : word; { no of index-entries }
        PrevPage         : integer;
      end;

      {
      TWinHelpBTreeIndexEntry = packed record
        FileName   : STRINGZ;
        PageNumber : word;
      end;
      }

      TWinHelpBTreeNodeHeader = packed record
        Unknown          : longint;
        NumEntries       : word; { no of index-entries }
        PrevPage         : integer;
        NextPage         : integer;
      end;

      TWinHelpSystemHeader = packed record
        Magic            : word;
        Version          : word;
        Always1          : word;
        GenDate          : longint;
        Flags            : word;
      end;

      TWinHelpPhrIndexHeader = packed record
        Magic            : longint;
        NumEntries       : longint;
        CompressedSize   : longint;
        PhrImageSize     : longint;
        PhrImageCompressedSize: longint;
        Always0          : longint;
        BitCount_Unk     : word; { BitCount = lower 4 bits }
        Dunno            : word;
      end;

      TWinHelpTopicBlockHeader = packed record
        LastTopicLink    : longint;
        FirstTopicLink   : longint;
        LastTopicHeader  : longint;
      end;

      TWinHelpTopicBlock = packed record
        Header           : TWinHelpTopicBlockHeader;
        Data             : record end;
      end;

      TWinHelpTopicHeader = packed record
        BlockSize        : longint;
        PrevOffset       : longint; { prev topic }
        NextOffset       : longint; { next topic }
        TopicNumber      : longint;
        NonScrollRgnOfs  : longint; { start of non-scrolling region (topic ofs) }
        ScrollRgnOfs     : longint; { topic ofs }
        NextTopic        : longint; { next type 2 record }
      end;

      TWinHelpTopicLink = packed record
        BlockSize        : longint;
        DataLen2         : longint;
        PrevBlock        : longint;
        NextBlock        : longint;
        DataLen1         : longint;
        RecordType       : byte;
      end;

      TTopicBlock = object
        Header       : TWinHelpTopicBlockHeader;
        DataSize     : longint;
        DataPtr      : PByteArray;
      private
        CurOfs: longint;
        procedure Seek(Pos: longint);
        function  GetPos: longint;
        function  GetSize: longint;
        procedure Read(var Buf; Count: longint);
      end;

      PTopicEnumData = ^TTopicEnumData;
      TTopicEnumData = record
        TB      : TTopicBlock;
        BlockNo : longint;
        TopicPos: longint;
        TopicOfs: longint;
        TL      : TWinHelpTopicLink;
        LinkData1Size: longint;
        LinkData1: PByteArray;
        LinkData2Size: longint;
        LinkData2: PByteArray;
      end;

      PWinHelpFile = ^TWinHelpFile;
      TWinHelpFile = object(THelpFile)
        constructor Init(AFileName: string; AID: word);
        destructor  Done; virtual;
      public
        function    LoadIndex: boolean; virtual;
        function    ReadTopic(T: PTopic): boolean; virtual;
      private
        F: PStream;
        Header: TWinHelpHeader;
        SysHeader: TWinHelpSystemHeader;
        Title: string;
        CNTFileName: string;
        PhrasesStart: longint;
        TTLBTreeStart: longint;
        TopicFileStart: longint;
        PhrIndexStart: longint;
        PhrImageStart: longint;
        Phrases: PUnsortedStringCollection;
        TreeDone: boolean;
        IndexLoaded: boolean;
        function ReadHeader: boolean;
        function ReadInternalDirectory: boolean;
        function ProcessLeafPage(PagesBase: longint; PageSize,PageNo,TotalPages: word;
                   ReadLeafEntryMethod: pointer; ScannedPages: PIntCollection): boolean;
        function ProcessIndexPage(CurLevel,MaxLevel: integer; PagesBase: longint; PageSize: word; PageNo,
                   TotalPages: word; ReadIndexEntryMethod, ReadLeafEntryMethod: pointer;
                   ScannedPages: PIntCollection): boolean;
        function ProcessTree(ReadIndexEntryMethod, ReadLeafEntryMethod: pointer; NoDuplicates: boolean): boolean;
        function IDIRReadIndexEntry(SubPageNo: PInteger): boolean;
        function IDIRReadLeafEntry(P: pointer): boolean;
        function IDIRProcessFile(const FileName: string; FileOfs: longint): boolean;
        function TTLBReadIndexEntry(SubPageNo: PInteger): boolean;
        function TTLBReadLeafEntry(P: pointer): boolean;
        function TTLBProcessTopicEntry(const TopicTitle: string; FileOfs: longint): boolean;
        function ReadSystemFile: boolean;
        function ReadPhraseFile: boolean;
        function ReadTTLBTree: boolean;
        function ReadPhrIndexFile(PhraseOfs: PIntCollection; var IH: TWinHelpPhrIndexHeader): boolean;
        function ReadPhrImageFile(PhraseOfs: PIntCollection; const IH: TWinHelpPhrIndexHeader): boolean;
        function TopicBlockSize: word;
        function LZ77Compressed: boolean;
        function UsesHallCompression: boolean;
        procedure ExtractTopicOffset(TopicOffset: longint; var TopicBlockNo, TopicBlockOffset: word);
        function  ReadTopicBlock(BlockNo: word; var T: TTopicBlock; ReadData: boolean): boolean;
        function  ProcessTopicBlock(BlockNo: longint; EnumProc: pointer): boolean;
        procedure PhraseDecompress(SrcBufP: pointer; SrcBufSize: longint; DestBufP: pointer; DestBufSize: longint);
        procedure HallDecompress(SrcBufP: pointer; SrcBufSize: longint; DestBufP: pointer; DestBufSize: longint);
      end;

procedure RegisterHelpType;

implementation

uses Strings;

function ReadString(F: PStream): string;
var S: string;
    C: char;
begin
  S:='';
  if Assigned(F) then
  repeat
    F^.Read(C,sizeof(C));
    if (F^.Status=stOK) and (C<>#0) then
      S:=S+C;
  until (C=#0) or (F^.Status<>stOK);
  ReadString:=S;
end;

function TTopicBlock.GetPos: longint;
begin
  GetPos:=CurOfs;
end;

function TTopicBlock.GetSize: longint;
begin
  GetSize:=DataSize;
end;

procedure TTopicBlock.Seek(Pos: longint);
begin
  CurOfs:=Pos;
end;

procedure TTopicBlock.Read(var Buf; Count: longint);
begin
  FillChar(Buf,Count,0);
  if Count>(DataSize-CurOfs) then
  begin
    Count:=Max(0,DataSize-CurOfs);
  end;
  Move(DataPtr^[CurOfs],Buf,Count);
  Inc(CurOfs,Count);
end;

constructor TWinHelpFile.Init(AFileName: string; AID: word);
var OK: boolean;
begin
  if inherited Init(AID)=false then Fail;
  New(Phrases, Init(1000,1000));
  F:=New(PFastBufStream, Init(AFileName, stOpenRead, HelpStreamBufSize));
  OK:=F<>nil;
  if OK then OK:=(F^.Status=stOK);
  if OK then
    begin
      OK:=ReadHeader;
    end;
  if OK=false then
  begin
    Done;
    Fail;
  end;
end;

function TWinHelpFile.ReadHeader: boolean;
var OK: boolean;
begin
  F^.Read(Header,sizeof(Header));
  OK:=(F^.Status=stOK);
  OK:=OK and (Header.MagicNo=WinHelpMagicNo);
  if OK then
  begin
    F^.Seek(Header.DirectoryStart);
    OK:=(F^.Status=stOK);
  end;
  if OK then
    OK:=ReadInternalDirectory;
  ReadHeader:=OK;
end;

function TWinHelpFile.TopicBlockSize: word;
var Size: word;
begin
  if (SysHeader.Version<=16) then
    Size:=2048 else
  if (SysHeader.Flags in[0,4]) then
    Size:=4096
  else
    Size:=2048;
  TopicBlockSize:=Size;
end;

function TWinHelpFile.LZ77Compressed: boolean;
begin
  LZ77Compressed:=(SysHeader.Version>16) and (SysHeader.Flags<>0);
end;

function TWinHelpFile.UsesHallCompression: boolean;
begin
  UsesHallCompression:=(PhrIndexStart<>0) and (PhrImageStart<>0);
end;

function TWinHelpFile.ReadSystemFile: boolean;
var OK: boolean;
    FH: TWinHelpFileEntryHeader;
    RH: TWinHelpRecordHeader;
    StartOfs,RecStartOfs: longint;
begin
  F^.Read(FH,sizeof(FH));
  OK:=(F^.Status=stOK);
  StartOfs:=F^.GetPos;
  F^.Read(SysHeader,sizeof(SysHeader));
  OK:=OK and (F^.Status=stOK);
  OK:=OK and (SysHeader.Magic=WinHelpSystemHeaderMagicNo);
  if OK then
  if SysHeader.Version>16 then
  begin
    repeat
      F^.Read(RH,sizeof(RH));
      OK:=(F^.Status=stOK);
      RecStartOfs:=F^.GetPos;
      if OK then
      begin
        case RH.RecordType of
          wh_srt_Title   : Title:=ReadString(F);
          wh_srt_Content : CNTFileName:=ReadString(F);
        end;
        if F^.GetPos<>RecStartOfs+RH.DataSize then
          F^.Seek(RecStartOfs+RH.DataSize);
        OK:=(F^.Status=stOK);
      end;
    until (OK=false) or (F^.GetPos>=StartOfs+FH.UsedSpace);
  end
 else
  Title:=ReadString(F);
  OK:=OK and (F^.Status=stOK);
  ReadSystemFile:=OK;
end;

function LZ77Decompress(SrcBufP: pointer; SrcSize: longint; DestBufP: pointer; DestSize: longint): longint;
var SrcBuf: PByteArray absolute SrcBufP;
    DestBuf: PByteArray absolute DestBufP;
var SrcOfs: longint;
function GetByte: byte;
begin
  GetByte:=PByteArray(SrcBuf)^[SrcOfs];
  Inc(SrcOfs);
end;
var DestOfs: longint;
procedure PutByte(B: byte);
begin
  PByteArray(DestBuf)^[DestOfs]:=B;
  Inc(DestOfs);
end;
var B,Mask: byte;
    Len,J: word;
    I: integer;
const N = 4096; F=16;
begin
  SrcOfs:=0; DestOfs:=0; I:=N-F;
  while (SrcOfs<SrcSize) do
  begin
    B:=GetByte;
    for Mask:=0 to 7 do
    begin
      if SrcOfs=SrcSize then Break;
      if (B and (1 shl Mask))<>0 then
        begin
          J:=GetByte;
          Len:=GetByte;
          J:=J+(Len and $0f) shl 8;
          Len:=(Len and $f0) shr 4+3;
          while (Len>0) do
          begin
            PutByte(PByteArray(DestBuf)^[DestOfs-J-1]);
            {Inc(J); }Inc(I);
            Dec(Len);
          end;
        end
      else
        begin
          PutByte(GetByte);
          I:=(I+1) and (N-1);
        end;
    end;
  end;
  LZ77Decompress:=DestOfs;
end;

function TWinHelpFile.ReadPhraseFile: boolean;
var OK: boolean;
    FH: TWinHelpFileEntryHeader;
    NumPhrases: word;
    DecompSize: longint;
    W: word;
    PhraseOfss: PWordArray;
    PhraseOfssSize: word;
    PhraseBuf: PByteArray;
    TempBuf: pointer;
    TempSize: longint;
    I,PhraseBufSize,PhraseOfs,PhraseSize: longint;
    S: string;
begin
  F^.Read(FH,sizeof(FH));
  OK:=(F^.Status=stOK);
  F^.Read(NumPhrases,sizeof(NumPhrases));
  F^.Read(W,sizeof(W));
  OK:=(F^.Status=stOK) and (W=$0100);
  if OK then
  begin
    PhraseOfssSize:=(NumPhrases+1)*sizeof(word);
    GetMem(PhraseOfss,PhraseOfssSize);
    F^.Read(W,sizeof(W));
    if W=2*(NumPhrases+1) then
      begin
        { uncompressed data }
        PhraseOfss^[0]:=W;
        F^.Read(PhraseOfss^[1],PhraseOfssSize-sizeof(word)*1);
        DecompSize:=FH.UsedSpace-(PhraseOfssSize+2+2+2);
        PhraseBufSize:=DecompSize;
        GetMem(PhraseBuf,PhraseBufSize);
        F^.Read(PhraseBuf^,DecompSize);
      end
    else
      begin
        DecompSize:=W;
        F^.Read(W,sizeof(W));
        DecompSize:=DecompSize+longint(W) shl 16;
        Inc(DecompSize);
        PhraseOfss^[0]:=DecompSize;
        F^.Read(PhraseOfss^[1],PhraseOfssSize);
        PhraseBufSize:=DecompSize+10;
        GetMem(PhraseBuf,PhraseBufSize); FillChar(PhraseBuf^,DecompSize,0);
        TempSize:=FH.UsedSpace-(PhraseOfssSize+2+2+2);
        GetMem(TempBuf,TempSize);
        F^.Read(TempBuf^,TempSize);
        LZ77Decompress(TempBuf,TempSize,PhraseBuf,DecompSize);
        FreeMem(TempBuf,TempSize);
      end;
    for I:=1 to NumPhrases do
    begin
      PhraseOfs:=PhraseOfss^[I]-PhraseOfss^[1];
      if I=NumPhrases then
        PhraseSize:=DecompSize-PhraseOfs
      else
        PhraseSize:=PhraseOfss^[I+1]-PhraseOfss^[I];
      S:=MemToStr(PhraseBuf^[PhraseOfs],PhraseSize);
      Phrases^.InsertStr(S);
    end;
    FreeMem(PhraseOfss,PhraseOfssSize);
    FreeMem(PhraseBuf,PhraseBufSize);
  end;
  ReadPhraseFile:=OK;
end;

function TWinHelpFile.ProcessLeafPage(PagesBase: longint; PageSize,PageNo,TotalPages: word;
          ReadLeafEntryMethod: pointer; ScannedPages: PIntCollection): boolean;
var OK: boolean;
    BNH: TWinHelpBTreeNodeHeader;
    Count: longint;
    PageOfs: longint;
    CurPage: longint;
begin
  CurPage:=PageNo;
  repeat
    PageOfs:=PagesBase+longint(PageSize)*CurPage;
    F^.Seek(PageOfs);
    OK:=(F^.Status=stOK);
    if OK then
    begin
      F^.Read(BNH,sizeof(BNH));
      OK:=(F^.Status=stOK);
    end;
    if OK then
    if (ScannedPages<>nil) and ScannedPages^.Contains(CurPage) then
      Break
    else
    begin
      if Assigned(ScannedPages) then ScannedPages^.Add(CurPage);
      Count:=0;
      repeat
        TreeDone:=not (longint(CallPointerMethod(ReadLeafEntryMethod,@Self,nil))<>0);
        Inc(Count);
      until (OK=false) or (F^.Status<>stOK) or TreeDone or (Count>=BNH.NumEntries){ or
            (F^.GetPos>=PageOfs+PageSize-BNH.NumEntries)};
    end;

    if (BNH.PrevPage<>-1) and (TreeDone=false) then
      CurPage:=BNH.PrevPage;
  until (OK=false) or TreeDone or (BNH.PrevPage=-1);
  ProcessLeafPage:=OK;
end;

function TWinHelpFile.ProcessIndexPage(CurLevel,MaxLevel: integer; PagesBase: longint; PageSize: word; PageNo,
         TotalPages: word; ReadIndexEntryMethod, ReadLeafEntryMethod: pointer; ScannedPages: PIntCollection): boolean;
var BIH: TWinHelpBTreeIndexHeader;
    I: integer;
    SubPageNo: integer;
    OK: boolean;
    OldPos: longint;
    CurPage: longint;
begin
  CurPage:=PageNo;
  repeat
    F^.Seek(PagesBase+longint(PageSize)*CurPage);
    OK:=(F^.Status=stOK);
    if OK then
    begin
      F^.Read(BIH,sizeof(BIH));
      OK:=(F^.Status=stOK);
    end;
    if OK then
    if (ScannedPages<>nil) and ScannedPages^.Contains(CurPage) then
      Break
    else
    begin
      if Assigned(ScannedPages) then ScannedPages^.Add(CurPage);
      for I:=1 to BIH.NumEntries do
      begin
        SubPageNo:=-1;
        OK:=(longint(CallPointerMethod(ReadIndexEntryMethod,@Self,@SubPageNo))<>0);
        OK:=OK and (F^.Status=stOK);
        if OK then
          if CurLevel<MaxLevel-1 then
            begin
              if (0<=SubPageNo) then
               if (ScannedPages=nil) or (ScannedPages^.Contains(SubPageNo)=false) then
              begin
                OldPos:=F^.GetPos;
                OK:=ProcessIndexPage(CurLevel+1,MaxLevel,PagesBase,PageSize,SubPageNo,TotalPages,
                  ReadIndexEntryMethod,ReadLeafEntryMethod,ScannedPages);
                if F^.GetPos<>OldPos then
                  F^.Seek(OldPos);
              end
            end
          else
            { process leaf page }
            if (0<=SubPageNo) then
             if (ScannedPages=nil) or (ScannedPages^.Contains(SubPageNo)=false) then
              begin
                OldPos:=F^.GetPos;
                OK:=ProcessLeafPage(PagesBase,PageSize,SubPageNo,TotalPages,ReadLeafEntryMethod,ScannedPages);
                if F^.GetPos<>OldPos then
                  F^.Seek(OldPos);
              end;
        if TreeDone then
          Break;
      end;
    end;

    if (BIH.PrevPage>0) and (TreeDone=false) then
      CurPage:=BIH.PrevPage
    else
      Break;
  until (OK=false) or TreeDone;
  ProcessIndexPage:=OK;
end;

function TWinHelpFile.ProcessTree(ReadIndexEntryMethod, ReadLeafEntryMethod: pointer; NoDuplicates: boolean): boolean;
var BTH: TWinHelpBTreeHeader;
    OK: boolean;
    PagesBase: longint;
    ScannedPages: PIntCollection;
begin
  ScannedPages:=nil;
  TreeDone:=false;
  F^.Read(BTH,sizeof(BTH));
  OK:=(F^.Status=stOK);
  PagesBase:=F^.GetPos;
  if OK then
  begin
    OK:=(BTH.Magic=WinHelpBTreeHeaderMagicNo) and
        (BTH.MustBeZero=0) and
        (BTH.MustBeNegOne=-1);
  end;
  if OK then
  begin
    if NoDuplicates then
      New(ScannedPages, Init(500,100));
    if BTH.NumLevels>1 then
      begin
        OK:=ProcessIndexPage(1,BTH.NumLevels,PagesBase,BTH.PageSize,BTH.RootPage,BTH.TotalPages,
             ReadIndexEntryMethod,ReadLeafEntryMethod,ScannedPages);
      end
    else
      OK:=ProcessLeafPage(PagesBase,BTH.PageSize,BTH.RootPage,BTH.TotalPages,ReadLeafEntryMethod,ScannedPAges);
    if Assigned(ScannedPages) then
      Dispose(ScannedPages, Done);
  end;
  ProcessTree:=OK;
end;

(*function TWinHelpFile.IDIRProcessFile(const FileName: string; FileOfs: longint): boolean;
var OK: boolean;
begin
  OK:=true;
  if FileName='|SYSTEM' then
    begin
      F^.Seek(FileOfs); OK:=(F^.Status=stOK);
      if OK then OK:=ReadSystemFile;
    end else
  if (FileName='|Phrases') then
    begin
      PhrasesStart:=FileOfs;
    end else
  ;
  IDIRProcessFile:=OK;
end;

function TWinHelpFile.IDIRProcessLeafPage(PagesBase: longint; PageSize,PageNo,TotalPages: word): boolean;
var OK: boolean;
    BNH: TWinHelpBTreeNodeHeader;
    FileOfs: longint;
    Count: integer;
    S: string;
    CurOfs,PageOfs,OldPos: longint;
begin
  PageOfs:=PagesBase+PageSize*PageNo;
  F^.Seek(PageOfs);
  OK:=(F^.Status=stOK);
  repeat
    if OK then
    begin
      F^.Read(BNH,sizeof(BNH));
      OK:=(F^.Status=stOK);
    end;
    if OK then
    begin
      Count:=0;
      repeat
        S:=ReadString(F);
        F^.Read(FileOfs,sizeof(FileOfs)); { longint }
        OK:=OK and (F^.Status=stOK);
        if OK then
        begin
          OldPos:=F^.GetPos;
          OK:=IDIRProcessFile(S,FileOfs);
          if F^.GetPos<>OldPos then
            F^.Seek(OldPos);
        end;
        Inc(Count);
      until (OK=false) or (Count=BNH.NumEntries){ or (F^.GetPos>=PageOfs+PageSize-BNH.NumEntries)};
    end;

    if BNH.PrevPage<>-1 then
      begin
        F^.Seek(PagesBase+PageSize*BNH.PrevPage);
        OK:=(F^.Status=stOK);
      end;
  until (OK=false) or (BNH.PrevPage=-1);
  IDIRProcessLeafPage:=OK;
end;

function TWinHelpFile.IDIRProcessIndexPage(CurLevel,MaxLevel: integer; PagesBase: longint; PageSize: word; PageNo,
         TotalPages: word): boolean;
var BIH: TWinHelpBTreeIndexHeader;
    I: integer;
    SubPageNo: integer;
    OK: boolean;
    S: string;
    OldPos: longint;
begin
  F^.Seek(PagesBase+PageSize*PageNo);
  OK:=(F^.Status=stOK);
  repeat
    if OK then
    begin
      F^.Read(BIH,sizeof(BIH));
      OK:=(F^.Status=stOK);
    end;
    if OK then
    for I:=1 to BIH.NumEntries do
    begin
      S:=ReadString(F);
      F^.Read(SubPageNo,sizeof(SubPageNo)); { word }
      OK:=OK and (F^.Status=stOK);
      if OK then
        if CurLevel<MaxLevel-1 then
          begin
            if (0<=SubPageNo) then
            begin
              OldPos:=F^.GetPos;
              OK:=IDIRProcessIndexPage(CurLevel+1,MaxLevel,PagesBase,PageSize,SubPageNo,TotalPages);
              if F^.GetPos<>OldPos then
                F^.Seek(OldPos);
            end
          end
        else
          { process leaf page }
          if (0<=SubPageNo) then
            OK:=IDIRProcessLeafPage(PagesBase,PageSize,SubPageNo,TotalPages);
    end;

    if (BIH.PrevPage>0) then
      begin
        F^.Seek(PagesBase+PageSize*BIH.PrevPage);
        OK:=(F^.Status=stOK);
      end
    else
      Break;
  until (OK=false);
  IDIRProcessIndexPage:=OK;
end;

function TWinHelpFile.ReadInternalDirectory: boolean;
var BTH: TWinHelpBTreeHeader;
    OK: boolean;
    PagesBase: longint;
begin
  F^.Read(BTH,sizeof(BTH));
  OK:=(F^.Status=stOK);
  PagesBase:=F^.GetPos;
  if OK then
  begin
    OK:=(BTH.Magic=WinHelpBTreeHeaderMagicNo) and
        (BTH.MustBeZero=0) and
        (BTH.MustBeNegOne=-1);
  end;
  if BTH.NumLevels>1 then
    OK:=IDIRProcessIndexPage(1,BTH.NumLevels,PagesBase,BTH.PageSize,BTH.RootPage,BTH.TotalPages)
  else
    OK:=IDIRProcessLeafPage(PagesBase,BTH.PageSize,BTH.RootPage,BTH.TotalPages);
  ReadInternalDirectory:=OK;
end;
*)
function TWinHelpFile.IDIRProcessFile(const FileName: string; FileOfs: longint): boolean;
var OK: boolean;
begin
  OK:=true;
  if FileName='|SYSTEM' then
    begin
      F^.Seek(FileOfs); OK:=(F^.Status=stOK);
      if OK then OK:=ReadSystemFile;
    end else
  if (FileName='|Phrases') then begin PhrasesStart:=FileOfs; end else
  if (FileName='|TOPIC') then begin TopicFileStart:=FileOfs; end else
  if (FileName='|TTLBTREE') then begin TTLBTreeStart:=FileOfs; end else
  if (FileName='|PhrIndex') then begin PhrIndexStart:=FileOfs; end else
  if (FileName='|PhrImage') then begin PhrImageStart:=FileOfs; end else
  ;
  IDIRProcessFile:=OK;
end;

function TWinHelpFile.IDIRReadIndexEntry(SubPageNo: PInteger): boolean;
var {S: string;}
    OK: boolean;
begin
  {S:=}ReadString(F);
  F^.Read(SubPageNo^,sizeof(SubPageNo^));
  OK:=(F^.Status=stOK);
  IDIRReadIndexEntry:=OK;
end;

function TWinHelpFile.IDIRReadLeafEntry(P: pointer): boolean;
var OK: boolean;
    S: string;
    FileOfs,OldPos: longint;
begin
  S:=ReadString(F);
  F^.Read(FileOfs,sizeof(FileOfs)); { longint }
  OK:=(F^.Status=stOK);
  if OK then
  begin
    OldPos:=F^.GetPos;
    OK:=IDIRProcessFile(S,FileOfs);
    if F^.GetPos<>OldPos then
      F^.Seek(OldPos);
    OK:=OK and (F^.Status=stOK);
  end;
  IDIRReadLeafEntry:=OK;
end;

function TWinHelpFile.ReadInternalDirectory: boolean;
var OK: boolean;
    FH: TWinHelpFileEntryHeader;
begin
  F^.Read(FH,sizeof(FH));
  OK:=(F^.Status=stOK);
  if OK then
    OK:=ProcessTree(@TWinHelpFile.IDIRReadIndexEntry,@TWinHelpFile.IDIRReadLeafEntry,true);
  ReadInternalDirectory:=OK;
end;

function TWinHelpFile.TTLBReadIndexEntry(SubPageNo: PInteger): boolean;
var TopicOffset: longint;
    OK: boolean;
begin
  F^.Read(TopicOffset,sizeof(TopicOffset));
  F^.Read(SubPageNo^,sizeof(SubPageNo^));
  OK:=(F^.Status=stOK);
  TTLBReadIndexEntry:=OK;
end;

function TWinHelpFile.TTLBReadLeafEntry(P: pointer): boolean;
var OK: boolean;
    S: string;
    TopicOfs,OldPos: longint;
begin
  F^.Read(TopicOfs,sizeof(TopicOfs)); { longint }
  S:=ReadString(F);
  OK:=(F^.Status=stOK);
  if OK then
  begin
    OldPos:=F^.GetPos;
    OK:=TTLBProcessTopicEntry(S,TopicOfs);
    if F^.GetPos<>OldPos then
      F^.Seek(OldPos);
    OK:=OK and (F^.Status=stOK);
  end;
  TTLBReadLeafEntry:=OK;
end;

function TWinHelpFile.TTLBProcessTopicEntry(const TopicTitle: string; FileOfs: longint): boolean;
var OK: boolean;
{const Count: longint = 0;}
begin
{  Inc(Count);
  if (Count mod 100)=1 then
  begin
    gotoxy(1,1); write(Count,' - ',IndexEntries^.Count,' - ',Topics^.Count);
  end;}
  OK:=(IndexEntries^.Count<MaxCollectionSize-10);
  if OK then
  begin
    if (TopicTitle<>'') and (FileOfs>=0) then
    begin
      AddIndexEntry(TopicTitle,FileOfs);
      AddTopic(FileOfs,FileOfs,'',nil,0);
    end;
  end;
  TTLBProcessTopicEntry:=OK;
end;

function TWinHelpFile.ReadTTLBTree: boolean;
var OK: boolean;
    FH: TWinHelpFileEntryHeader;
begin
  F^.Read(FH,sizeof(FH));
  OK:=(F^.Status=stOK);
  if OK then
    OK:=ProcessTree(@TWinHelpFile.TTLBReadIndexEntry,@TWinHelpFile.TTLBReadLeafEntry,true);
  ReadTTLBTree:=OK;
end;

function TWinHelpFile.ReadPhrIndexFile(PhraseOfs: PIntCollection; var IH: TWinHelpPhrIndexHeader): boolean;
var OK: boolean;
    FH: TWinHelpFileEntryHeader;
    TotalBitPos,BufBitPos: longint;
    BitBuf: array[0..1023] of byte;
    CurFrag: word;
function GetBit: integer;
begin
  BufBitPos:=(TotalBitPos mod ((High(BitBuf)-Low(BitBuf)+1)*8));
  if (BufBitPos=0) then
  begin
    CurFrag:=Min(sizeof(BitBuf),FH.UsedSpace-(TotalBitPos div 8));
    F^.Read(BitBuf,CurFrag);
    OK:=OK and (F^.Status=stOK);
  end;
  if (BitBuf[Low(BitBuf)+BufBitPos div 8] and (1 shl (BufBitPos mod 8)))<>0 then
    GetBit:=1
  else
    GetBit:=0;
  Inc(TotalBitPos);
end;
var Delta: longint;
    I,J,LastOfs: longint;
    BitCount: integer;
begin
  F^.Read(FH,sizeof(FH));
  OK:=(F^.Status=stOK);
  if OK then
  begin
    F^.Read(IH,sizeof(IH));
    OK:=(F^.Status=stOK) and (IH.Magic=1);
  end;
  if OK then
  begin
    PhraseOfs^.Add(0);
    TotalBitPos:=0; LastOfs:=0; BitCount:=(IH.BitCount_Unk and $0f);
    for I:=1 to IH.NumEntries do
    begin
      Delta:=1;
      while GetBit=1 do
        Delta:=Delta+(1 shl BitCount);
      for J:=0 to BitCount-1 do
        Delta:=Delta+(1 shl J)*GetBit;
      Inc(LastOfs,Delta);
      PhraseOfs^.Add(LastOfs);
    end;
  end;
  ReadPhrIndexFile:=OK;
end;

function TWinHelpFile.ReadPhrImageFile(PhraseOfs: PIntCollection; const IH: TWinHelpPhrIndexHeader): boolean;
var OK: boolean;
    FH: TWinHelpFileEntryHeader;
    PhraseBufSize: longint;
    PhraseBuf: PByteArray;
    TempBufSize: longint;
    TempBuf: pointer;
    CurOfs,NextOfs: longint;
    I: longint;
begin
  F^.Read(FH,sizeof(FH));
  OK:=(F^.Status=stOK);
  OK:=OK and (IH.PhrImageCompressedSize=FH.UsedSpace);
  if OK then
  begin
    PhraseBufSize:=IH.PhrImageSize;
    GetMem(PhraseBuf,PhraseBufSize);
    if IH.PhrImageSize=IH.PhrImageCompressedSize then
      begin
        F^.Read(PhraseBuf^,PhraseBufSize);
      end
    else
      begin
        TempBufSize:=IH.PhrImageCompressedSize;
        GetMem(TempBuf,TempBufSize);
        F^.Read(TempBuf^,TempBufSize);
        OK:=(F^.Status=stOK);
        if OK then LZ77Decompress(TempBuf,TempBufSize,PhraseBuf,PhraseBufSize);
        FreeMem(TempBuf,TempBufSize);
      end;
    if OK then
    begin
      for I:=1 to IH.NumEntries do
      begin
        CurOfs:=PhraseOfs^.AtInt(I-1);
        NextOfs:=PhraseOfs^.AtInt(I);
        Phrases^.InsertStr(MemToStr(PhraseBuf^[CurOfs],NextOfs-CurOfs));
      end;
    end;
    FreeMem(PhraseBuf,PhraseBufSize);
  end;
  ReadPhrImageFile:=OK;
end;

function TWinHelpFile.LoadIndex: boolean;
var OK: boolean;
    PO: PIntCollection;
    IH: TWinHelpPhrIndexHeader;
begin
  if IndexLoaded then OK:=true else
  begin
    if PhrasesStart<>0 then
    begin
      F^.Seek(PhrasesStart); OK:=(F^.Status=stOK);
      if OK then OK:=ReadPhraseFile;
    end else
    if (PhrIndexStart<>0) and (PhrImageStart<>0) then
    begin
      New(PO, Init(1000,1000));
      F^.Seek(PhrIndexStart); OK:=(F^.Status=stOK);
      if OK then OK:=ReadPhrIndexFile(PO,IH);
      if OK then begin F^.Seek(PhrImageStart); OK:=(F^.Status=stOK); end;
      if OK then OK:=ReadPhrImageFile(PO,IH);
      Dispose(PO, Done);
    end;
    if TTLBTreeStart<>0 then
    begin
      F^.Seek(TTLBTreeStart); OK:=(F^.Status=stOK);
      if OK then OK:=ReadTTLBTree;
    end;
    IndexLoaded:=OK;
  end;
  LoadIndex:=OK;
end;

procedure TWinHelpFile.ExtractTopicOffset(TopicOffset: longint; var TopicBlockNo, TopicBlockOffset: word);
var {OfsBitCount: longint;
    OfsMask: longint;}
    BS: longint;
begin
{  if LZ77Compressed then
    BS:=32768
  else
    BS:=TopicBlockSize;}
  BS:=32768;

{  for OfsBitCount:=0 to 31 do
   if (1 shl OfsBitCount)=BS then
     Break;
  OfsMask:=(1 shl OfsBitCount)-1;
  TopicBlockNo:=(TopicOffset and not OfsMask) shr OfsBitCount;
  TopicBlockOffset:=(TopicOffset and OfsMask);}

  TopicBlockNo:=TopicOffset div BS;
  TopicBlockOffset:=TopicOffset mod BS;
end;

function TWinHelpFile.ReadTopicBlock(BlockNo: word; var T: TTopicBlock; ReadData: boolean): boolean;
var TempBuf: pointer;
    BlockBuf: ^TWinHelpTopicBlock;
    OK: boolean;
    RS,DecompSize: longint;
const TempBufSize = 16384;
begin
  F^.Reset;
  FillChar(T,sizeof(T),0);
  F^.Seek(TopicFileStart+sizeof(TWinHelpFileEntryHeader)+longint(BlockNo)*TopicBlockSize);
  OK:=(F^.Status=stOK);
  if OK then
 if ReadData=false then
  begin
    F^.Read(T.Header,sizeof(T.Header));
    OK:=(F^.Status=stOK);
  end
 else
  begin
    GetMem(BlockBuf, TopicBlockSize);
    F^.Read(BlockBuf^,TopicBlockSize);
    OK:=(F^.Status=stOK);
    if OK then
    begin
      Move(BlockBuf^.Header,T.Header,sizeof(T.Header));
      if LZ77Compressed then
        begin
          GetMem(TempBuf,TempBufSize);
          DecompSize:=LZ77Decompress(@BlockBuf^.Data,TopicBlockSize-sizeof(BlockBuf^.Header),TempBuf,TempBufSize);
          T.DataSize:=DecompSize;
          GetMem(T.DataPtr,T.DataSize);
          Move(TempBuf^,T.DataPtr^,T.DataSize);
          FreeMem(TempBuf,TempBufSize);
        end
      else
        begin
          T.DataSize:=TopicBlockSize-sizeof(BlockBuf^.Header);
          GetMem(T.DataPtr,T.DataSize);
          Move(BlockBuf^.Data,T.DataPtr^,T.DataSize);
        end;
    end;
    FreeMem(BlockBuf,TopicBlockSize);
  end;
  ReadTopicBlock:=OK;
end;

procedure FreeTopicBlock(T: TTopicBlock);
begin
  if (T.DataSize>0) and (T.DataPtr<>nil) then
  begin
    FreeMem(T.DataPtr,T.DataSize);
    T.DataPtr:=nil;
  end;
end;

procedure TWinHelpFile.PhraseDecompress(SrcBufP: pointer; SrcBufSize: longint; DestBufP: pointer; DestBufSize: longint);
var SrcBuf: PByteArray absolute SrcBufP;
    DestBuf: PByteArray absolute DestBufP;
var SrcOfs: longint;
function GetByte: byte;
begin
  GetByte:=SrcBuf^[SrcOfs];
  Inc(SrcOfs);
end;
var DestOfs: longint;
procedure PutByte(B: byte);
begin
  if DestOfs<DestBufSize then
    DestBuf^[DestOfs]:=B;
  Inc(DestOfs);
end;
var B: byte;
    I,Index: longint;
    S: string;
begin
  SrcOfs:=0; DestOfs:=0;
  while (SrcOfs<SrcBufSize) do
  begin
    B:=GetByte;
    if (B=0) or (B>15) then
      PutByte(B)
    else
      begin
        Index:=longint(B)*256-256+GetByte;
        S:=GetStr(Phrases^.At(Index div 2));
        if (Index mod 2)=1 then S:=S+' ';
        for I:=1 to length(S) do
          PutByte(ord(S[I]));
      end;
  end;
end;

procedure TWinHelpFile.HallDecompress(SrcBufP: pointer; SrcBufSize: longint; DestBufP: pointer; DestBufSize: longint);
var SrcBuf: PByteArray absolute SrcBufP;
    DestBuf: PByteArray absolute DestBufP;
var SrcOfs: longint;
function GetByte: byte;
begin
  GetByte:=SrcBuf^[SrcOfs];
  Inc(SrcOfs);
end;
var DestOfs: longint;
procedure PutByte(B: byte);
begin
  if DestOfs<DestBufSize then
    DestBuf^[DestOfs]:=B;
  Inc(DestOfs);
end;
procedure EmitStr(const S: string);
var I: longint;
begin
  for I:=1 to length(S) do
    PutByte(ord(S[I]));
end;
procedure EmitStrIndex(Index: longint);
begin
  EmitStr(GetStr(Phrases^.At(Index)));
end;
var B: longint;
    I,Index: longint;
    S: string;
begin
  SrcOfs:=0; DestOfs:=0;
  while (SrcOfs<SrcBufSize) do
  begin
    B:=GetByte;
    if (B and 1)=0 then
      EmitStrIndex(B div 2)
    else
      if (B and 3)=1 then
        EmitStrIndex(B*64+64+GetByte)
      else
        if (B and 7)=3 then
          for I:=1 to (B div 8)+1 do
            PutByte(GetByte)
        else
          if (B and 15)=7 then
            EmitStr(CharStr(' ',B div 16+1))
          else
            EmitStr(CharStr(#0,B div 16+1));
  end;
end;

function TWinHelpFile.ProcessTopicBlock(BlockNo: longint; EnumProc: pointer): boolean;
var TB: TTopicBlock;
    TL: TWinHelpTopicLink;
    BlockFileOfs: longint;
    LinkData1Size: longint;
    LinkData1: PByteArray;
    LinkData2Size: longint;
    LinkData2: PByteArray;
    TempBufSize: longint;
    TempBuf: PByteArray;
    CurBlockOfs,LastLinkOfs: longint;
    TopicPos,TopicOfs: longint;
    OK: boolean;
    TEN: TTopicEnumData;
    DoCont: boolean;
begin
  OK:=ReadTopicBlock(BlockNo,TB,true);
  if OK then
  begin
    TopicOfs:=0; DoCont:=true;
    BlockFileOfs:=longint(BlockNo)*TopicBlockSize;
    if TB.Header.FirstTopicLink>0 then
      TB.Seek((TB.Header.FirstTopicLink and $3fff)-sizeof(TB.Header));
    if TB.Header.LastTopicLink=-1 then
      LastLinkOfs:=TB.DataSize-1-sizeof(TL)
    else
      LastLinkOfs:={(TB.Header.LastTopicLink-BlockFileOfs-sizeof(TB.Header))}TB.GetSize-1;
    while (DoCont) and OK and (TB.GetPos<=LastLinkOfs) do
    begin
      CurBlockOfs:=TB.GetPos;
      TopicPos:=TB.GetPos+sizeof(TB.Header);
      TB.Read(TL,sizeof(TL));
      if (TL.BlockSize=0) or (TL.DataLen1=0) or (TB.GetPos>LastLinkOfs) or
         (TB.GetSize-TB.GetPos<TL.BlockSize) then
        Break;
      LinkData1Size:=TL.DataLen1-sizeof(TL);
      GetMem(LinkData1,LinkData1Size);
      TB.Read(LinkData1^,LinkData1Size);
      LinkData2Size:=TL.DataLen2;
      GetMem(LinkData2,LinkData2Size);
      if TL.DataLen2>TL.BlockSize-TL.DataLen1 then
        begin
          TempBufSize:=TL.BlockSize-TL.DataLen1;
          GetMem(TempBuf,TempBufSize);
          TB.Read(TempBuf^,TempBufSize);
          if UsesHallCompression then
            HallDecompress(TempBuf,TempBufSize,LinkData2,LinkData2Size)
          else
            PhraseDecompress(TempBuf,TempBufSize,LinkData2,LinkData2Size);
          FreeMem(TempBuf,TempBufSize);
        end
      else
        TB.Read(LinkData2^,TL.DataLen2);
      FillChar(TEN,sizeof(TEN),0);
      TEN.TB:=TB;
      TEN.BlockNo:=BlockNo;
      TEN.TopicPos:=TopicPos;
      TEN.TopicOfs:=TopicOfs;
      TEN.TL:=TL;
      TEN.LinkData1Size:=LinkData1Size;
      TEN.LinkData1:=LinkData1;
      TEN.LinkData2Size:=LinkData2Size;
      TEN.LinkData2:=LinkData2;
      DoCont:=(longint(CallPointerLocal(EnumProc,get_caller_frame(get_frame),@TEN)) and $ff)<>0;
      case TL.RecordType of
        $02: ;
        $20,$23:
          begin
            Inc(TopicOfs,TL.DataLen2);
          end;
      end;
      FreeMem(LinkData1,LinkData1Size);
      FreeMem(LinkData2,LinkData2Size);
    end;
    FreeTopicBlock(TB);
  end;
  ProcessTopicBlock:=OK;
end;

function TWinHelpFile.ReadTopic(T: PTopic): boolean;
var OK: boolean;
    BlockNo,BlockOfs: word;
    TopicStartPos: longint;
    GotIt: boolean;
    TH: TWinHelpTopicHeader;
    CurLine: string;
    Lines: PUnsortedStringCollection;
    EmitSize: longint;
    LastEmittedChar: integer;
procedure FlushLine;
begin
  Lines^.InsertStr(CurLine); CurLine:='';
end;
procedure EmitText(const S: string);
begin
  Inc(EmitSize,length(S));
  if length(CurLine)+length(S)>High(S) then
    FlushLine;
  CurLine:=CurLine+S;
  if length(S)>0 then
    LastEmittedChar:=ord(S[length(S)]);
end;
procedure EmitTextC(C: PChar);
var RemSize,CurOfs,CurFrag: longint;
    S: string;
begin
  if C=nil then Exit;
  RemSize:=StrLen(C); CurOfs:=0;
  while (RemSize>0) do
  begin
    CurFrag:=Min(RemSize,255);
    S[0]:=chr(CurFrag);
    Move(PByteArray(C)^[CurOfs],S[1],CurFrag);
    EmitText(S);
    Dec(RemSize,CurFrag); Inc(CurOfs,CurFrag);
  end;
end;
function SearchTopicStart(P: PTopicEnumData): boolean; {$ifndef FPC}far;{$endif}
begin
  case P^.TL.RecordType of
    $02 : TopicStartPos:=P^.TopicPos;
  end;
  GotIt:=(P^.TL.RecordType in [$20,$23]) and (P^.TopicOfs<=BlockOfs) and (BlockOfs<P^.TopicOfs+P^.LinkData2Size);
  SearchTopicStart:=not GotIt;
end;
function RenderTopicProc(P: PTopicEnumData): boolean; {$ifndef FPC}far;{$endif}
var LinkData1Ofs: longint;
    LinkData2Ofs: longint;
function ReadUCHAR: byte;
begin
  ReadUCHAR:=P^.LinkData1^[LinkData1Ofs];
  Inc(LinkData1Ofs);
end;
function ReadCHAR: shortint;
var B: byte;
    U: shortint absolute B;
begin
  B:=ReadUCHAR;
  ReadCHAR:=U;
end;
function ReadUSHORT: word;
begin
  ReadUSHORT:=ReadUCHAR+longint(ReadUCHAR)*256;
end;
function ReadSHORT: integer;
var W: word;
    I: integer absolute W;
begin
  W:=ReadUSHORT;
  ReadSHORT:=I;
end;
function ReadComprUSHORT: word;
var B: byte;
begin
  B:=ReadUCHAR;
  if (B mod 2)=0 then
    ReadComprUSHORT:=(B div 2)
  else
    ReadComprUSHORT:=(B div 2)+longint(ReadUCHAR)*128;
end;
{$Q-}
function ReadLONG: longint;
begin
  ReadLONG:=ReadUSHORT+longint(ReadUSHORT)*65536;
end;
function ReadULONG: longint;
begin
  ReadULONG:=ReadLONG;
end;
{$Q+}
function ReadComprSHORT: integer;
var B: byte;
begin
  B:=ReadUCHAR;
  if (B mod 2)=0 then
    ReadComprSHORT:=longint(B div 2)-64
  else
    ReadComprSHORT:=((B div 2)+longint(ReadUCHAR)*128)-16384;
end;
function ReadComprULONG: longint;
var W: word;
begin
  W:=ReadUSHORT;
  if (W mod 2)=0 then
    ReadComprULONG:=(W div 2)
  else
    ReadComprULONG:=(W div 2)+longint(ReadUSHORT)*32768;
end;
function ReadComprLONG: longint;
var W: word;
begin
  W:=ReadUSHORT;
  if (W mod 2)=0 then
    ReadComprLONG:=longint(W div 2)-16384
  else
    ReadComprLONG:=(W div 2)+longint(ReadUSHORT)*32768-67108864;
end;
function ReadString: string;
var S: string;
    B: byte;
begin
  S:='';
  repeat
    B:=ReadUCHAR;
    if B<>0 then
      S:=S+chr(B);
  until B=0;
  ReadString:=S;
end;
procedure EmitDebugText(const S: string);
begin
{$ifdef DEBUGMSG}
  EmitText(S);
{$endif}
end;
var Finished: boolean;
    S: string;
    { ---- }
    Cmd: integer;
    I,TopicSize: longint;
    NumberOfCols,TableType: byte;
    MinTableWidth: integer;
    Flags: longint;
    NumberOfTabStops: integer;
    TabStop: longint;
    PType: integer;
    Len: word;
    SLen,LinkOfs: longint;
    SPtr: pointer;
    SBuf: PChar;
    PictureSize,PictureStartOfs: longint;
    FontNumber: integer;
begin
  Finished:=((P^.TopicPos>TopicStartPos) or (P^.BlockNo>BlockNo)) and
            (P^.TL.RecordType=$02); { next topic header found }
  if (Finished=false) and (P^.TopicPos>=TopicStartPos) then
  case P^.TL.RecordType of
    $02 :
      begin
        S[0]:=chr(Min(StrLen(pointer(P^.LinkData2)),P^.LinkData2Size));
        Move(P^.LinkData2^,S[1],ord(S[0]));
        if S<>'' then
        begin
          EmitText('  '+S+' Ü'+hscLineBreak);
          EmitText(' '+CharStr('ß',length(S)+3)+hscLineBreak);
        end;
      end;
    $20,$23 :
      begin
        EmitDebugText(hscLineBreak+'<------ new record ------>'+hscLineBreak);
        LinkData1Ofs:=0; LinkData2Ofs:=0; EmitSize:=0;
        { ---- }
        MinTableWidth:=0;
        TopicSize:=ReadComprULONG;
        if P^.TL.RecordType in[$20,$23] then
          {TopicLen:=}ReadComprUSHORT;
        if P^.TL.RecordType=$23 then
        begin
          NumberOfCols:=ReadUCHAR; TableType:=ReadUCHAR;
          if TableType in[0,2] then
            MinTableWidth:=ReadSHORT;
          for I:=1 to NumberOfCols do
          begin
            {GapWidth:=}ReadSHORT;
            {ColWidth:=}ReadSHORT;
          end;
        end;

        if P^.TL.RecordType=$23 then
        begin
          {Column:=}ReadSHORT; {-1 = end of topic}
          {Unknown:=}ReadSHORT; {Always0:=}ReadCHAR;
        end;
        {Unknown:=}ReadUCHAR; {Uknown:=}ReadCHAR;
        ID:=ReadUSHORT;
        Flags:=ReadUSHORT;
        if (Flags and 1)<>0 then
          {Unknown:=}ReadComprLONG;
        if (Flags and 2)<>0 then
          {SpacingAbove:=}ReadComprSHORT;
        if (Flags and 4)<>0 then
          {SpacingBelow:=}ReadComprSHORT;
        if (Flags and 8)<>0 then
          {SpacingLines:=}ReadComprSHORT;
        if (Flags and 16)<>0 then
          {LeftIndent:=}ReadComprSHORT;
        if (Flags and 32)<>0 then
          {RightIndent:=}ReadComprSHORT;
        if (Flags and 64)<>0 then
          {FirstLineIndent:=}ReadComprSHORT;
        if (Flags and 256)<>0 then {BorderInfo}
          begin
            {BorderFlags:=}ReadUCHAR;
            {BorderWidth:=}ReadSHORT;
          end;
        if (Flags and 512)<>0 then {TabInfo}
          begin
            NumberOfTabStops:=ReadComprSHORT;
            for I:=1 to NumberOfTabStops do
            begin
              TabStop:=ReadComprUSHORT;
              if (TabStop and $4000)<>0 then
                {TabType:=}ReadComprUSHORT;
            end;
          end;
        for I:=10 to 14 do
          if (Flags and (1 shl I))<>0 then
            ReadUCHAR;
{
        if (TH.NonScrollRgnOfs<>-1) then
          if (P^.TopicPos=(TH.ScrollRgnOfs and $3fff)) then
            begin
              EmitText(hscLineBreak);
              EmitText(CharStr('Ä',80));
              EmitText(hscLineBreak);
            end;
}
        while (LinkData2Ofs<P^.LinkData2Size) do
        begin
          LinkOfs:=-1;
          SPtr:=@(P^.LinkData2^[LinkData2Ofs]);
          SLen:=StrLen(SPtr);
          if SLen>0 then
            SBuf:=SPtr
          else
            SBuf:=nil;
          Inc(LinkData2Ofs,SLen+1);

          Cmd:=-1;
          if (LinkData1Ofs<P^.LinkData1Size) then
          begin
            Cmd:=ReadUCHAR;
            case Cmd of
              $ff : { End of formatting }
                    EmitDebugText('[blockend]');
              $20 : begin
                      EmitDebugText('[vfld]');
                      {vfldNumber:=}ReadLONG;
                    end;
              $21 : begin
                      EmitDebugText('[dtype]');
                      {dtypeNumber:=}ReadSHORT;
                    end;
              $3a,
              $3c : {????}
                    begin
                      if LastEmittedChar<>ord(hscLineBreak) then
                        EmitText(hscLineBreak);
                      EmitDebugText('[tag0x'+hexstr(Cmd,2)+']');
                    end;
              $80 : begin
                      FontNumber:=ReadSHORT;
                      EmitDebugText('[font'+inttostr(FontNumber)+']');
                    end;
              $81 : {LineBreak}
                    begin
                      EmitDebugText('[br]');
                      EmitText(hscLineBreak);
                    end;
              $82 : {End Of Paragraph}
                    begin
                      EmitDebugText('[eop]');
                      EmitText(hscLineBreak);
                    end;
              $83 : {TAB}
                    begin
                      EmitDebugText('[tab]');
                      EmitText(' ');
                    end;
              $86,
              $87,
              $88 : { ewc or bmc or bmcwd or bmct or button or mci }
                    begin
                      PType:=ReadUCHAR;
                      PictureSize:=ReadComprLONG;
                      if PType=$22 then
                        {NumberOfHotSpots:=}ReadComprSHORT;
                      PictureStartOfs:=LinkData1Ofs;
                      PictureSize:=Min(PictureSize,P^.LinkData1Size-LinkData1Ofs);
                      if PType in[$03,$22] then
                      begin
                        {PictureIsEmbedded:=}ReadSHORT;
                        {PictureNumber:=}ReadSHORT;
                        for I:=1 to PictureSize-4 do
                          {PictureData[I-1]:=}ReadCHAR;
                      end;
                      if PType=$05 then
                      begin
                        {Unknown1:=}ReadSHORT;
                        {Unknown2:=}ReadSHORT;
                        {Unknown3:=}ReadSHORT;
                        { +??? }
                      end;
                      while (LinkData1Ofs<PictureStartOfs+PictureSize) do
                        {}ReadCHAR;
                      EmitText('[img]');
                    end;
              $89 : { end of hotspot }EmitDebugText('[ehs]');
              $8b : { non-break space }; { does not appear in LinkData2 !!!! }
              $8c : { non-break hypen };
              $c6 : {????}
                    ReadLONG;
              $c8,  { macro }
              $cc : { macro without font change }
                    begin
                      Len:=ReadSHORT;
                      for I:=1 to longint(Len)-3 do
                        {C:=}ReadUCHAR; { string }
                    end;
              $e0,  { popup jump } { start with underlined green }
              $e1 : { topic jump } { start with underlined green }
                    begin
                      EmitDebugText('[linkgr]');
                      LinkOfs:=ReadLONG;
                      if LinkOfs>0 then
                      begin
                        EmitText(hscLink);
                        AddLinkToTopic(T,ID,LinkOfs);
                      end;
                    end;
              $e2,  { popup jump }
              $e3,  { topic jump }
              $e6,  { popup jump without font change }
              $e7 : { topic jump without font change }
                    begin
                      EmitDebugText('[link]');
                      LinkOfs:=ReadLONG;
                      if LinkOfs>0 then
                      begin
                        EmitText(hscLink);
                        AddLinkToTopic(T,ID,LinkOfs);
                      end;
                    end;
              $ea,  { popup jump into external file }
              $eb,  { popup jump into external file without font change }
              $ee,  { popup jump into external file / secondary window }
              $ef : { popup jump into external file / secondary window }
                    begin
                      EmitDebugText('[linkext]');
                      Len:=ReadSHORT;
                      PType:=ReadUCHAR;
                      LinkOfs:=ReadLONG;
                      {WindowNo:=}ReadUCHAR;
                      {NameOfExternalFile:=}ReadString;
                      {WindowName:=}ReadString;
                      if LinkOfs>0 then
                      begin
                        EmitText(hscLink);
                        AddLinkToTopic(T,ID,LinkOfs);
                      end;
                    end;
              else EmitDebugText('[tag0x'+hexstr(Cmd,2)+']');
            end;
          end;
          if SLen>0 then
            EmitTextC(SPtr);
{          case Cmd of
            $81 : EmitText(hscLineBreak);
            $82 : EmitText(hscLineBreak);
          end;}
          if LinkOfs>0 then
          begin
            EmitText(hscLink);
            EmitDebugText('[eol]');
          end;
        end;
      end;
  end;
  RenderTopicProc:=not Finished;
end;
begin
  F^.Reset;
  OK:=(TopicFileStart<>0) and (T<>nil);
  if OK then
  begin
    ExtractTopicOffset(T^.FileOfs,BlockNo,BlockOfs);
    TopicStartPos:=-1; GotIt:=false;
    OK:=ProcessTopicBlock(BlockNo,@SearchTopicStart);
    OK:=OK and GotIt and (TopicStartPos<>-1);
    if OK then
    begin
      CurLine:='';
      New(Lines, Init(1000,1000));
      LastEmittedChar:=-1;
      OK:=ProcessTopicBlock(BlockNo,@RenderTopicProc);
      FlushLine;
      BuildTopic(Lines,T);
      Dispose(Lines, Done);
    end;
  end;
  ReadTopic:=OK;
end;

destructor TWinHelpFile.Done;
begin
  if Assigned(F) then Dispose(F, Done); F:=nil;
  if Assigned(Phrases) then Dispose(Phrases, Done); Phrases:=nil;
  inherited Done;
end;

function CreateProc(const FileName,Param: string;Index : longint): PHelpFile; {$ifndef FPC}far;{$endif}
begin
  CreateProc:=New(PWinHelpFile, Init(FileName,Index));
end;

procedure RegisterHelpType;
begin
  RegisterHelpFileType({$ifdef FPC}@{$endif}CreateProc);
end;

END.
