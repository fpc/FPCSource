{
    $Id$
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

      TWinHelpTopicBlockHeader = packed record
        LastTopicLink    : longint;
        FirstTopicLink   : longint;
        LastTopicHeader  : longint;
      end;

      TWinHelpTopicBlock = packed record
        Header           : TWinHelpTopicBlockHeader;
        Data             : record end;
      end;

      TTopicBlock = record
        Header       : TWinHelpTopicBlockHeader;
        DataSize     : longint;
        DataPtr      : pointer;
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
        function TopicBlockSize: word;
        function LZ77Compressed: boolean;
        procedure ExtractTopicOffset(TopicOffset: longint; var TopicBlockNo, TopicBlockOffset: word);
        function  ReadTopicBlock(BlockNo: word; var T: TTopicBlock; ReadData: boolean): boolean;
      end;

implementation

uses Crt,CallSpec;

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
  if (FileName='|Phrases') then
    begin
      PhrasesStart:=FileOfs;
    end else
  ;
  if (FileName='|TOPIC') then
    begin
      TopicFileStart:=FileOfs;
    end else
  ;
  if (FileName='|TTLBTREE') then
    begin
      TTLBTreeStart:=FileOfs;
    end else
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
const Count: longint = 0;
begin
  Inc(Count);
  if (Count mod 100)=1 then
  begin
    gotoxy(1,1); write(Count,' - ',IndexEntries^.Count,' - ',Topics^.Count);
  end;
  OK:=(IndexEntries^.Count<MaxCollectionSize-10);
  if OK then
  begin
    if (TopicTitle<>'') and (FileOfs>=0) then
    begin
      AddIndexEntry(TopicTitle,FileOfs);
      AddTopic(FileOfs,FileOfs,'');
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

function TWinHelpFile.LoadIndex: boolean;
var OK: boolean;
begin
  if IndexLoaded then OK:=true else
  begin
    if PhrasesStart<>0 then
    begin
      F^.Seek(PhrasesStart); OK:=(F^.Status=stOK);
      if OK then OK:=ReadPhraseFile;
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
  if LZ77Compressed then
    BS:=16384
  else
    BS:=TopicBlockSize;
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

function TWinHelpFile.ReadTopic(T: PTopic): boolean;
var OK: boolean;
    BlockNo,BlockOfs: word;
    TB: TTopicBlock;
begin
  OK:=(TopicFileStart<>0) and (T<>nil);
  if OK then
  begin
    ExtractTopicOffset(T^.FileOfs,BlockNo,BlockOfs);
    OK:=ReadTopicBlock(BlockNo,TB,true);
  end;
  if OK then
  begin
    FreeTopicBlock(TB);
  end;
  ReadTopic:=OK;
end;

destructor TWinHelpFile.Done;
begin
  if Assigned(F) then Dispose(F, Done); F:=nil;
  if Assigned(Phrases) then Dispose(Phrases, Done); Phrases:=nil;
  inherited Done;
end;

END.
{
  $Log$
  Revision 1.1  2000-06-26 07:29:23  pierre
   * new bunch of Gabor's changes


}