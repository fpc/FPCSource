{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Help support & Borland OA .HLP reader objects and routines

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$R-}
unit WHelp;

interface

uses
{$ifdef Win32}
   { placed here to avoid TRect to be found in windows unit
     for win32 target whereas its found in objects unit for other targets PM }
     windows,
{$endif Win32}
     Objects,
     WUtils;

const
      MinFormatVersion  = $04; { was $34 }

      TP55FormatVersion = $04;
      TP70FormatVersion = $34;

      Signature      = '$*$* &&&&$*$'#0;
      ncRawChar      = $F;
      ncRepChar      = $E;

      oa_rtFileHeader   = Byte ($0);
      oa_rtContext      = Byte ($1);
      oa_rtText         = Byte ($2);
      oa_rtKeyWord      = Byte ($3);
      oa_rtIndex        = Byte ($4);
      oa_rtCompression  = Byte ($5);
      oa_rtIndexTags    = Byte ($6);

      ctNone         = $00;
      ctNibble       = $02;

      hscLineBreak   = #0;
      hscLink        = #2;
      hscLineStart   = #3;
      hscCode        = #5;
      hscCenter      = #10;
      hscRight       = #11;
      hscNamedMark   = #12;
      hscTextAttr    = #13;
      hscTextColor   = #14;
      hscNormText    = #15;

type
      FileStamp      = array [0..32] of char; {+ null terminator + $1A }
      FileSignature  = array [0..12] of char; {+ null terminator }

      THelpCtx = longint;

      THLPVersion = packed record
        FormatVersion : byte;
        TextVersion   : byte;
      end;

      THLPRecordHeader = packed record
        RecType       : byte; {TPRecType}
        RecLength     : word;
      end;

      THLPContextPos = packed record
        LoW: word;
        HiB: byte;
      end;

      THLPContexts = packed record
        ContextCount : word;
        Contexts     : array[0..0] of THLPContextPos;
      end;

      THLPFileHeader = packed record
        Options         : word;
        MainIndexScreen : word;
        MaxScreenSize   : word;
        Height          : byte;
        Width           : byte;
        LeftMargin      : byte;
      end;

      THLPCompression = packed record
        CompType      : byte;
        CharTable     : array [0..13] of byte;
      end;

      THLPIndexDescriptor = packed record
        LengthCode    : byte;
        UniqueChars   : array [0..0] of byte;
        Context       : word;
      end;

      THLPIndexTable = packed record
        IndexCount    : word;
        Entries       : record end;
      end;

      THLPKeywordDescriptor = packed record
        KwContext     : word;
      end;

      THLPKeyWordRecord = packed record
        UpContext     : word;
        DownContext   : word;
        KeyWordCount  : word;
        Keywords      : array[0..0] of THLPKeywordDescriptor;
      end;

      THLPKeywordDescriptor55 = packed record
        PosY          : byte;
        StartX        : byte;
        EndX          : byte;
        Dunno         : array[0..1] of word;
        KwContext     : word;
      end;

      THLPKeyWordRecord55 = packed record
        UpContext     : word;
        DownContext   : word;
        KeyWordCount  : byte;
        Keywords      : array[0..0] of THLPKeywordDescriptor55;
      end;

      TRecord = packed record
        SClass   : word;
        Size     : word;
        Data     : pointer;
      end;

      PIndexEntry = ^TIndexEntry;
      TIndexEntry = packed record
        Tag        : PString;
        HelpCtx    : THelpCtx;
        FileID     : word;
      end;

      PKeywordDescriptor = ^TKeywordDescriptor;
      TKeywordDescriptor = packed record
        FileID     : word;
        Context    : THelpCtx;
      end;

      PKeywordDescriptors = ^TKeywordDescriptors;
      TKeywordDescriptors = array[0..MaxBytes div sizeof(TKeywordDescriptor)-1] of TKeywordDescriptor;

      PTopic = ^TTopic;
      TTopic = object
        HelpCtx       : THelpCtx;
        FileOfs       : longint;
        TextSize      : sw_word;
        Text          : PByteArray;
        LinkCount     : sw_word;
        Links         : PKeywordDescriptors;
        LastAccess    : longint;
        FileID        : word;
        Param         : PString;
        StartNamedMark: integer;
        NamedMarks    : PUnsortedStringCollection;
        ExtData       : pointer;
        ExtDataSize   : longint;
        function LinkSize: sw_word;
        function GetNamedMarkIndex(const MarkName: string): sw_integer;
      end;

      PTopicCollection = ^TTopicCollection;
      TTopicCollection = object(TSortedCollection)
        function   At(Index: sw_Integer): PTopic;
        procedure  FreeItem(Item: Pointer); virtual;
        function   Compare(Key1, Key2: Pointer): Sw_Integer; virtual;
        function   SearchTopic(AHelpCtx: THelpCtx): PTopic;
      end;

      PIndexEntryCollection = ^TIndexEntryCollection;
      TIndexEntryCollection = object(TSortedCollection)
        function   At(Index: Sw_Integer): PIndexEntry;
        procedure  FreeItem(Item: Pointer); virtual;
        function   Compare(Key1, Key2: Pointer): Sw_Integer; virtual;
      end;

      PUnsortedIndexEntryCollection = ^TUnsortedIndexEntryCollection;
      TUnsortedIndexEntryCollection = object(TCollection)
        function   At(Index: Sw_Integer): PIndexEntry;
        procedure  FreeItem(Item: Pointer); virtual;
      end;

      PHelpFile = ^THelpFile;
      THelpFile = object(TObject)
        ID           : word;
        Topics       : PTopicCollection;
        IndexEntries : PUnsortedIndexEntryCollection;
        constructor Init(AID: word);
        function    LoadTopic(HelpCtx: THelpCtx): PTopic; virtual;
        procedure   AddTopic(HelpCtx: THelpCtx; Pos: longint; const Param: string;
                    ExtData: pointer; ExtDataSize: longint);
        procedure   AddIndexEntry(const Text: string; AHelpCtx: THelpCtx);
        destructor  Done; virtual;
      public
        function    LoadIndex: boolean; virtual;
        function    SearchTopic(HelpCtx: THelpCtx): PTopic; virtual;
        function    ReadTopic(T: PTopic): boolean; virtual;
      private
        procedure MaintainTopicCache;
      end;

      POAHelpFile = ^TOAHelpFile;
      TOAHelpFile = object(THelpFile)
        Version      : THLPVersion;
        Header       : THLPFileHeader;
        Compression  : THLPCompression;
        constructor Init(AFileName: string; AID: word);
        destructor  Done; virtual;
      public
        function    LoadIndex: boolean; virtual;
        function    ReadTopic(T: PTopic): boolean; virtual;
      public { protected }
        F: PStream;
        TopicsRead     : boolean;
        IndexTableRead : boolean;
        CompressionRead: boolean;
        IndexTagsRead  : boolean;
        IndexTagsPos   : longint;
        IndexTablePos  : longint;
        function  ReadHeader: boolean;
        function  ReadTopics: boolean;
        function  ReadIndexTable: boolean;
        function  ReadCompression: boolean;
        function  ReadIndexTags: boolean;
        function  ReadRecord(var R: TRecord; ReadData: boolean): boolean;
      end;

      PHelpFileCollection = PCollection;

      PHelpFacility = ^THelpFacility;
      THelpFacility = object(TObject)
        HelpFiles: PHelpFileCollection;
        IndexTabSize: sw_integer;
        constructor Init;
        function    AddOAHelpFile(const FileName: string): boolean;
        function    AddHTMLHelpFile(const FileName, TOCEntry: string): boolean;
        function    AddNGHelpFile(const FileName: string): boolean;
        function    AddOS2HelpFile(const FileName: string): boolean;
        function    AddWinHelpFile(const FileName: string): boolean;
        function    AddHTMLIndexHelpFile(const FileName: string): boolean;
        function    LoadTopic(SourceFileID: word; Context: THelpCtx): PTopic; virtual;
        function    TopicSearch(Keyword: string; var FileID: word; var Context: THelpCtx): boolean; virtual;
        function    BuildIndexTopic: PTopic; virtual;
        destructor  Done; virtual;
      private
        LastID: word;
        function  SearchFile(ID: byte): PHelpFile;
        function  SearchTopicInHelpFile(F: PHelpFile; Context: THelpCtx): PTopic;
        function  SearchTopicOwner(SourceFileID: word; Context: THelpCtx): PHelpFile;
        function  AddFile(H: PHelpFile): boolean;
      end;

const TopicCacheSize    : sw_integer = 10;
      HelpStreamBufSize : sw_integer = 4096;
      HelpFacility      : PHelpFacility = nil;
      MaxHelpTopicSize  : sw_word = {$ifdef FPC}3*65520{$else}65520{$endif};

function  NewTopic(FileID: byte; HelpCtx: THelpCtx; Pos: longint; Param: string;
          ExtData: pointer; ExtDataSize: longint): PTopic;
procedure DisposeTopic(P: PTopic);

procedure RenderTopic(Lines: PUnsortedStringCollection; T: PTopic);
procedure BuildTopic(Lines: PUnsortedStringCollection; T: PTopic);
procedure AddLinkToTopic(T: PTopic; AFileID: word; ACtx: THelpCtx);

function  NewIndexEntry(Tag: string; FileID: word; HelpCtx: THelpCtx): PIndexEntry;
procedure DisposeIndexEntry(P: PIndexEntry);

procedure DisposeRecord(var R: TRecord);

implementation

uses
{$ifdef Unix}
  linux,
{$endif Unix}
{$IFDEF OS2}
  DosCalls,
{$ENDIF OS2}
  WConsts,WHTMLHlp,WNGHelp,WWinHelp,WOS2Help;


Function GetDosTicks:longint; { returns ticks at 18.2 Hz, just like DOS }
{$IFDEF OS2}
  const
    QSV_MS_COUNT = 14;
  var
    L: longint;
  begin
    DosQuerySysInfo (QSV_MS_COUNT, QSV_MS_COUNT, L, 4);
    GetDosTicks := L div 55;
  end;
{$ENDIF}
{$IFDEF Unix}
  var
    tv : TimeVal;
    tz : TimeZone;
  begin
    GetTimeOfDay(tv); {Timezone no longer used?}
    GetDosTicks:=((tv.Sec mod 86400) div 60)*1092+((tv.Sec mod 60)*1000000+tv.USec) div 54945;
  end;
{$endif Unix}
{$ifdef Win32}
  begin
    GetDosTicks:=(Windows.GetTickCount*5484) div 100;
  end;
{$endif Win32}
{$ifdef go32v2}
  begin
    GetDosTicks:=MemL[$40:$6c];
  end;
{$endif go32v2}
{$ifdef TP}
  begin
    GetDosTicks:=MemL[$40:$6c];
  end;
{$endif go32v2}

procedure DisposeRecord(var R: TRecord);
begin
  with R do
  if (Size>0) and (Data<>nil) then FreeMem(Data, Size);
  FillChar(R, SizeOf(R), 0);
end;

function NewTopic(FileID: byte; HelpCtx: THelpCtx; Pos: longint; Param: string;
         ExtData: pointer; ExtDataSize: longint): PTopic;
var P: PTopic;
begin
  New(P); FillChar(P^,SizeOf(P^), 0);
  P^.HelpCtx:=HelpCtx; P^.FileOfs:=Pos; P^.FileID:=FileID;
  P^.Param:=NewStr(Param);
  if Assigned(ExtData) and (ExtDataSize>0) then
  begin
    P^.ExtDataSize:=ExtDataSize;
    GetMem(P^.ExtData,ExtDataSize);
    Move(ExtData^,P^.ExtData^,ExtDataSize);
  end;
  New(P^.NamedMarks, Init(100,100));
  NewTopic:=P;
end;

procedure DisposeTopic(P: PTopic);
begin
  if P<>nil then
  begin
    if (P^.TextSize>0) and (P^.Text<>nil) then
       FreeMem(P^.Text,P^.TextSize);
    P^.Text:=nil;
    if (P^.LinkCount>0) and (P^.Links<>nil) then
       FreeMem(P^.Links,P^.LinkSize);
    P^.Links:=nil;
    if P^.Param<>nil then DisposeStr(P^.Param); P^.Param:=nil;
    if Assigned(P^.ExtData) then
      FreeMem(P^.ExtData{$ifndef FPC},P^.ExtDataSize{$endif});
    if Assigned(P^.NamedMarks) then Dispose(P^.NamedMarks, Done); P^.NamedMarks:=nil;
    Dispose(P);
  end;
end;

function CloneTopic(T: PTopic): PTopic;
var NT: PTopic;
procedure CloneMark(P: PString); {$ifndef FPC}far;{$endif}
begin
  NT^.NamedMarks^.InsertStr(GetStr(P));
end;
begin
  New(NT); Move(T^,NT^,SizeOf(NT^));
  if NT^.Text<>nil then
     begin GetMem(NT^.Text,NT^.TextSize); Move(T^.Text^,NT^.Text^,NT^.TextSize); end;
  if NT^.Links<>nil then
     begin GetMem(NT^.Links,NT^.LinkSize); Move(T^.Links^,NT^.Links^,NT^.LinkSize); end;
  if NT^.Param<>nil then
     NT^.Param:=NewStr(T^.Param^);
  if Assigned(T^.NamedMarks) then
  begin
    New(NT^.NamedMarks, Init(T^.NamedMarks^.Count,10));
    T^.NamedMarks^.ForEach(@CloneMark);
  end;
  NT^.ExtDataSize:=T^.ExtDataSize;
  if Assigned(T^.ExtData) and (T^.ExtDataSize>0) then
  begin
    GetMem(NT^.ExtData,NT^.ExtDataSize);
    Move(T^.ExtData^,NT^.ExtData^,NT^.ExtDataSize);
  end;
  CloneTopic:=NT;
end;

procedure RenderTopic(Lines: PUnsortedStringCollection; T: PTopic);
var Size,CurPtr,I,MSize: sw_word;
    S: string;
begin
  CurPtr:=0;
  for I:=0 to Lines^.Count-1 do
  begin
    S:=GetStr(Lines^.At(I));
    Size:=length(S)+1;
    Inc(CurPtr,Size);
  end;
  Size:=CurPtr;
  T^.TextSize:=Size; GetMem(T^.Text,T^.TextSize);
  CurPtr:=0;
  for I:=0 to Lines^.Count-1 do
  begin
    S:=GetStr(Lines^.At(I)); Size:=length(S); MSize:=Size;
    if CurPtr+Size>=T^.TextSize then
      MSize:=T^.TextSize-CurPtr;
    Move(S[1],PByteArray(T^.Text)^[CurPtr],MSize);
    if MSize<>Size then
      Break;
    Inc(CurPtr,Size);
    PByteArray(T^.Text)^[CurPtr]:=ord(hscLineBreak);
    Inc(CurPtr);
    if CurPtr>=T^.TextSize then Break;
  end;
end;

procedure BuildTopic(Lines: PUnsortedStringCollection; T: PTopic);
var Size,CurPtr,MSize: sw_word;
    I: sw_integer;
    S: string;
begin
  CurPtr:=0;
  for I:=0 to Lines^.Count-1 do
  begin
    S:=GetStr(Lines^.At(I));
    Size:=length(S);
    Inc(CurPtr,Size);
  end;
  Size:=CurPtr;
  T^.TextSize:=Size; GetMem(T^.Text,T^.TextSize);
  CurPtr:=0;
  for I:=0 to Lines^.Count-1 do
  begin
    S:=GetStr(Lines^.At(I)); Size:=length(S); MSize:=Size;
    if Size>0 then
    begin
      if CurPtr+Size>=T^.TextSize then
        MSize:=T^.TextSize-CurPtr;
      Move(S[1],PByteArray(T^.Text)^[CurPtr],MSize);
      if MSize<>Size then
        Break;
      Inc(CurPtr,Size);
    end;
    if CurPtr>=T^.TextSize then Break;
  end;
end;

procedure AddLinkToTopic(T: PTopic; AFileID: word; ACtx: THelpCtx);
var NewSize: word;
    NewPtr: pointer;
begin
  NewSize:=longint(T^.LinkCount+1)*sizeof(T^.Links^[0]);
  GetMem(NewPtr,NewSize);
  if Assigned(T^.Links) then
  begin
    Move(T^.Links^,NewPtr^,T^.LinkSize);
    FreeMem(T^.Links,T^.LinkSize);
  end;
  T^.Links:=NewPtr;
  with T^.Links^[T^.LinkCount] do
  begin
    FileID:=AFileID;
    Context:=ACtx;
  end;
  Inc(T^.LinkCount);
end;

function NewIndexEntry(Tag: string; FileID: word; HelpCtx: THelpCtx): PIndexEntry;
var P: PIndexEntry;
begin
  New(P); FillChar(P^,SizeOf(P^), 0);
  P^.Tag:=NewStr(Tag); P^.FileID:=FileID; P^.HelpCtx:=HelpCtx;
  NewIndexEntry:=P;
end;

procedure DisposeIndexEntry(P: PIndexEntry);
begin
  if P<>nil then
  begin
    if P^.Tag<>nil then DisposeStr(P^.Tag);
    Dispose(P);
  end;
end;

function TTopic.LinkSize: sw_word;
begin
  LinkSize:=LinkCount*SizeOf(Links^[0]);
end;

function TTopic.GetNamedMarkIndex(const MarkName: string): sw_integer;
var I,Index: sw_integer;
begin
  Index:=-1;
  if Assigned(NamedMarks) then
  for I:=0 to NamedMarks^.Count-1 do
    if CompareText(GetStr(NamedMarks^.At(I)),MarkName)=0 then
     begin
       Index:=I;
       Break;
     end;
  GetNamedMarkIndex:=Index;
end;

function TTopicCollection.At(Index: sw_Integer): PTopic;
begin
  At:=inherited At(Index);
end;

procedure TTopicCollection.FreeItem(Item: Pointer);
begin
  if Item<>nil then DisposeTopic(Item);
end;

function TTopicCollection.Compare(Key1, Key2: Pointer): Sw_Integer;
var K1: PTopic absolute Key1;
    K2: PTopic absolute Key2;
    R: Sw_integer;
begin
  if K1^.HelpCtx<K2^.HelpCtx then R:=-1 else
  if K1^.HelpCtx>K2^.HelpCtx then R:= 1 else
  R:=0;
  Compare:=R;
end;

function TTopicCollection.SearchTopic(AHelpCtx: THelpCtx): PTopic;
var T: TTopic;
    P: PTopic;
    Index: sw_integer;
begin
  T.HelpCtx:=AHelpCtx;
  if Search(@T,Index) then
    P:=At(Index)
  else
    P:=nil;
  SearchTopic:=P;
end;

function TIndexEntryCollection.At(Index: Sw_Integer): PIndexEntry;
begin
  At:=inherited At(Index);
end;

procedure TIndexEntryCollection.FreeItem(Item: Pointer);
begin
  if Item<>nil then DisposeIndexEntry(Item);
end;

function TUnsortedIndexEntryCollection.At(Index: Sw_Integer): PIndexEntry;
begin
  At:=inherited At(Index);
end;

procedure TUnsortedIndexEntryCollection.FreeItem(Item: Pointer);
begin
  if Item<>nil then DisposeIndexEntry(Item);
end;

function TIndexEntryCollection.Compare(Key1, Key2: Pointer): Sw_Integer;
var K1: PIndexEntry absolute Key1;
    K2: PIndexEntry absolute Key2;
    R: Sw_integer;
    S1,S2: string;
begin
  S1:=UpcaseStr(K1^.Tag^); S2:=UpcaseStr(K2^.Tag^);
  if S1<S2 then R:=-1 else
  if S1>S2 then R:=1 else
  if K1^.FileID<K2^.FileID then R:=-1 else
  if K1^.FileID>K2^.FileID then R:= 1 else
  R:=0;
  Compare:=R;
end;

constructor THelpFile.Init(AID: word);
begin
  inherited Init;
  ID:=AID;
  New(Topics, Init(2000,1000));
  New(IndexEntries, Init(2000,1000));
end;

procedure THelpFile.AddTopic(HelpCtx: THelpCtx; Pos: longint; const Param: string; ExtData: pointer; ExtDataSize: longint);
begin
  Topics^.Insert(NewTopic(ID,HelpCtx,Pos,Param,ExtData,ExtDataSize));
end;

procedure THelpFile.AddIndexEntry(const Text: string; AHelpCtx: THelpCtx);
begin
  IndexEntries^.Insert(NewIndexEntry(Text,ID,AHelpCtx));
end;

function THelpFile.LoadTopic(HelpCtx: THelpCtx): PTopic;
var T: PTopic;
begin
  T:=SearchTopic(HelpCtx);
  if (T<>nil) then
   if T^.Text=nil then
     begin
       MaintainTopicCache;
       if ReadTopic(T)=false then
           T:=nil;
       if (T<>nil) and (T^.Text=nil) then T:=nil;
     end;
  if T<>nil then
     begin T^.LastAccess:=GetDosTicks; T:=CloneTopic(T); end;
  LoadTopic:=T;
end;

function THelpFile.LoadIndex: boolean;
begin
  Abstract;
  LoadIndex:=false; { remove warning }
end;

function THelpFile.SearchTopic(HelpCtx: THelpCtx): PTopic;
var T: PTopic;
begin
  T:=Topics^.SearchTopic(HelpCtx);
  SearchTopic:=T;
end;

function THelpFile.ReadTopic(T: PTopic): boolean;
begin
  Abstract;
  ReadTopic:=false; { remove warning }
end;

procedure THelpFile.MaintainTopicCache;
var Count: sw_integer;
    MinLRU: longint;
procedure CountThem(P: PTopic); {$ifndef FPC}far;{$endif}
begin if (P^.Text<>nil) or (P^.Links<>nil) then Inc(Count); end;
procedure SearchLRU(P: PTopic); {$ifndef FPC}far;{$endif}
begin if P^.LastAccess<MinLRU then begin MinLRU:=P^.LastAccess; end; end;
var P: PTopic;
begin
  Count:=0; Topics^.ForEach(@CountThem);
  if (Count>=TopicCacheSize) then
  begin
    MinLRU:=MaxLongint; P:=nil; Topics^.ForEach(@SearchLRU);
    if P<>nil then
    begin
      FreeMem(P^.Text,P^.TextSize); P^.TextSize:=0; P^.Text:=nil;
      FreeMem(P^.Links,P^.LinkSize); P^.LinkCount:=0; P^.Links:=nil;
    end;
  end;
end;

destructor THelpFile.Done;
begin
  if Topics<>nil then Dispose(Topics, Done);
  if IndexEntries<>nil then Dispose(IndexEntries, Done);
  inherited Done;
end;

constructor TOAHelpFile.Init(AFileName: string; AID: word);
var OK: boolean;
    FS,L: longint;
    R: TRecord;
begin
  if inherited Init(AID)=false then Fail;
  F:=New(PFastBufStream, Init(AFileName, stOpenRead, HelpStreamBufSize));
  OK:=F<>nil;
  if OK then OK:=(F^.Status=stOK);
  if OK then
    begin
      FS:=F^.GetSize;
      OK:=ReadHeader;
    end;
  while OK do
  begin
    L:=F^.GetPos;
    if (L>=FS) then Break;
    OK:=ReadRecord(R,false);
    if (OK=false) or (R.SClass=0) or (R.Size=0) then Break;
    case R.SClass of
      oa_rtContext     : begin F^.Seek(L); OK:=ReadTopics; end;
      oa_rtText        : {Skip};
      oa_rtKeyword     : {Skip};
      oa_rtIndex       : begin IndexTablePos:=L; {OK:=ReadIndexTable; }end;
      oa_rtCompression : begin F^.Seek(L); OK:=ReadCompression; end;
      oa_rtIndexTags   : begin IndexTagsPos:=L; {OK:=ReadIndexTags; }end;
    else
     begin
     {$ifdef DEBUGMSG}
       ClearFormatParams;
       AddFormatParamInt(R.SClass);
       AddFormatParamInt(L);
       AddFormatParamInt(R.Size);
       ErrorBox('Uknown help record tag %x encountered, '+
                'offset %x, size %d',@FormatParams);
     {$else}
       {Skip};
     {$endif}
     end;
    end;
    if OK then
       begin Inc(L, SizeOf(THLPRecordHeader)); Inc(L, R.Size); F^.Seek(L); OK:=(F^.Status=stOK); end
  end;
  OK:=OK and (TopicsRead=true);
  if OK=false then Fail;
end;

function TOAHelpFile.LoadIndex: boolean;
begin
  LoadIndex:=ReadIndexTable;
end;

function TOAHelpFile.ReadHeader: boolean;
var S: string;
    P: longint;
    R: TRecord;
    OK: boolean;
begin
  F^.Seek(0);
  F^.Read(S[1],128); S[0]:=#255;
  OK:=(F^.Status=stOK); P:=Pos(Signature,S);
  OK:=OK and (P>0);
  if OK then
  begin
    F^.Seek(P+length(Signature)-1);
    F^.Read(Version,SizeOf(Version));
    OK:=(F^.Status=stOK) and (Version.FormatVersion>=MinFormatVersion);
    if OK then
    begin
      OK:=ReadRecord(R,true);
      OK:=OK and (R.SClass=oa_rtFileHeader) and (R.Size=SizeOf(Header));
      if OK then Move(R.Data^,Header,SizeOf(Header));
      DisposeRecord(R);
    end;
  end;
  ReadHeader:=OK;
end;

function TOAHelpFile.ReadTopics: boolean;
var OK: boolean;
    R: TRecord;
    L,I: longint;
function GetCtxPos(C: THLPContextPos): longint;
begin
  GetCtxPos:=longint(C.HiB) shl 16 + C.LoW;
end;
begin
  OK:=ReadRecord(R, true);
  if OK then
  with THLPContexts(R.Data^) do
  for I:=1 to longint(ContextCount)-1 do
  begin
    if Topics^.Count=MaxCollectionSize then Break;
    L:=GetCtxPos(Contexts[I]);
    if (L and $800000)<>0 then L:=not L;
    if (L=-1) and (Header.MainIndexScreen>0) then
       L:=GetCtxPos(Contexts[Header.MainIndexScreen]);
    if (L>0) then
      AddTopic(I,L,'',nil,0);
  end;
  DisposeRecord(R);
  TopicsRead:=OK;
  ReadTopics:=OK;
end;

function TOAHelpFile.ReadIndexTable: boolean;
var OK: boolean;
    R: TRecord;
    I: longint;
    LastTag,S: string;
    CurPtr: sw_word;
    HelpCtx: THelpCtx;
    LenCode,CopyCnt,AddLen: byte;
type pword = ^word;
begin
  if IndexTableRead then OK:=true else
 begin
  LastTag:=''; CurPtr:=0;
  OK:=(IndexTablePos<>0);
  if OK then begin F^.Seek(IndexTablePos); OK:=F^.Status=stOK; end;
  if OK then OK:=ReadRecord(R, true);
  if OK then
  with THLPIndexTable(R.Data^) do
  for I:=0 to IndexCount-1 do
  begin
    LenCode:=PByteArray(@Entries)^[CurPtr];
    AddLen:=LenCode and $1f; CopyCnt:=LenCode shr 5;
    S[0]:=chr(AddLen); Move(PByteArray(@Entries)^[CurPtr+1],S[1],AddLen);
    LastTag:=copy(LastTag,1,CopyCnt)+S;
    HelpCtx:=PWord(@PByteArray(@Entries)^[CurPtr+1+AddLen])^;
    AddIndexEntry(LastTag,HelpCtx);
    Inc(CurPtr,1+AddLen+2);
  end;
  DisposeRecord(R);
  IndexTableRead:=OK;
 end;
  ReadIndexTable:=OK;
end;

function TOAHelpFile.ReadCompression: boolean;
var OK: boolean;
    R: TRecord;
begin
  OK:=ReadRecord(R, true);
  OK:=OK and (R.Size=SizeOf(THLPCompression));
  if OK then Move(R.Data^,Compression,SizeOf(Compression));
  DisposeRecord(R);
  CompressionRead:=OK;
  ReadCompression:=OK;
end;

function TOAHelpFile.ReadIndexTags: boolean;
var OK: boolean;
begin
  OK:={ReadRecord(R, true)}true;
  IndexTagsRead:=OK;
  ReadIndexTags:=OK;
end;

function TOAHelpFile.ReadRecord(var R: TRecord; ReadData: boolean): boolean;
var OK: boolean;
    H: THLPRecordHeader;
begin
  FillChar(R, SizeOf(R), 0);
  F^.Read(H,SizeOf(H));
  OK:=F^.Status=stOK;
  if OK then
  begin
    R.SClass:=H.RecType; R.Size:=H.RecLength;
    if (R.Size>0) and ReadData then
    begin
      GetMem(R.Data,R.Size);
      F^.Read(R.Data^,R.Size);
      OK:=F^.Status=stOK;
    end;
    if OK=false then DisposeRecord(R);
  end;
  ReadRecord:=OK;
end;

function TOAHelpFile.ReadTopic(T: PTopic): boolean;
var SrcPtr,DestPtr,TopicSize: sw_word;
    NewR: TRecord;
    LinkPosCount: integer;
    LinkPos: array[1..50] of TRect;
function IsLinkPosStart(X,Y: integer): boolean;
var OK: boolean;
    I: integer;
begin
  OK:=false;
  for I:=1 to LinkPosCount do
    with LinkPos[I] do
      if (A.X=X) and (A.Y=Y) then
        begin
          OK:=true;
          Break;
        end;
  IsLinkPosStart:=OK;
end;
function IsLinkPosEnd(X,Y: integer): boolean;
var OK: boolean;
    I: integer;
begin
  OK:=false;
  for I:=1 to LinkPosCount do
    with LinkPos[I] do
      if (B.X=X) and (B.Y=Y) then
        begin
          OK:=true;
          Break;
        end;
  IsLinkPosEnd:=OK;
end;
function ExtractTextRec(var R: TRecord): boolean;
function GetNextNibble: byte;
var B,N: byte;
begin
  B:=PByteArray(R.Data)^[SrcPtr div 2];
  N:=( B and ($0f shl (4*(SrcPtr mod 2))) ) shr (4*(SrcPtr mod 2));
  Inc(SrcPtr);
  GetNextNibble:=N;
end;
procedure RealAddChar(C: char);
begin
  if Assigned(NewR.Data) then
    PByteArray(NewR.Data)^[DestPtr]:=ord(C);
  Inc(DestPtr);
end;
var CurX,CurY: integer;
    InLink: boolean;
procedure AddChar(C: char);
begin
  if IsLinkPosStart(CurX+2,CurY) then
    begin
      RealAddChar(hscLink);
      InLink:=true;
    end
  else
    if (C=hscLineBreak) and (InLink) then
      begin
        RealAddChar(hscLink);
        InLink:=false;
      end;
  RealAddChar(C);
  if IsLinkPosEnd(CurX+2,CurY) then
    begin
      RealAddChar(hscLink);
      InLink:=false;
    end;
  if C<>hscLineBreak then
    Inc(CurX)
  else
    begin
      CurX:=0;
      Inc(CurY);
    end;
end;
var OK: boolean;
    C: char;
    P: pointer;
function GetNextChar: char;
var C: char;
    I,N,Cnt: byte;
begin
  N:=GetNextNibble;
  case N of
    $00       : C:=#0;
    $01..$0D  : C:=chr(Compression.CharTable[N]);
    ncRawChar : begin
                  I:=GetNextNibble;
                  C:=chr(I+GetNextNibble shl 4);
                end;
    ncRepChar : begin
                  Cnt:=2+GetNextNibble;
                  C:=GetNextChar{$ifdef FPC}(){$endif};
                  for I:=1 to Cnt-1 do AddChar(C);
                end;
  end;
  GetNextChar:=C;
end;
begin
  OK:=Compression.CompType in[ctNone,ctNibble];
  if OK then
  case Compression.CompType of
       ctNone   : ;
       ctNibble :
         begin
           CurX:=0; CurY:=0; InLink:=false;
           NewR.SClass:=0;
           NewR.Size:=0;
           NewR.Data:=nil;
           SrcPtr:=0; DestPtr:=0;
           while SrcPtr<(R.Size*2) do
           begin
             C:=GetNextChar;
             AddChar(C);
           end;
           if InLink then AddChar(hscLineBreak);
           TopicSize:=DestPtr;

           CurX:=0; CurY:=0; InLink:=false;
           NewR.SClass:=R.SClass;
           NewR.Size:=Min(MaxHelpTopicSize,TopicSize);
           GetMem(NewR.Data, NewR.Size);
           SrcPtr:=0; DestPtr:=0;
           while SrcPtr<(R.Size*2) do
           begin
             C:=GetNextChar;
             AddChar(C);
           end;
           if InLink then AddChar(hscLineBreak);
           DisposeRecord(R); R:=NewR;
           if (R.Size>DestPtr) then
           begin
             P:=R.Data; GetMem(R.Data,DestPtr);
             Move(P^,R.Data^,DestPtr); FreeMem(P,R.Size); R.Size:=DestPtr;
           end;
         end;
  else OK:=false;
  end;
  ExtractTextRec:=OK;
end;
var OK: boolean;
    TextR,KeyWR: TRecord;
    I: sw_word;
begin
  OK:=T<>nil;
  if OK and (T^.Text=nil) then
  begin
    LinkPosCount:=0; FillChar(LinkPos,Sizeof(LinkPos),0);
    FillChar(TextR,SizeOf(TextR),0); FillChar(KeyWR,SizeOf(KeyWR),0);
    F^.Seek(T^.FileOfs); OK:=F^.Status=stOK;
    if OK then OK:=ReadRecord(TextR,true);
    OK:=OK and (TextR.SClass=oa_rtText);
    if OK then OK:=ReadRecord(KeyWR,true);
    OK:=OK and (KeyWR.SClass=oa_rtKeyword);

    if OK then
    begin
      case Version.FormatVersion of
        TP55FormatVersion :
           with THLPKeywordRecord55(KeyWR.Data^) do
           begin
             T^.LinkCount:=KeywordCount;
             GetMem(T^.Links,T^.LinkSize);
             if T^.LinkCount>0 then
             for I:=0 to T^.LinkCount-1 do
             with Keywords[I] do
             begin
               T^.Links^[I].Context:=KwContext;
               T^.Links^[I].FileID:=ID;
               Inc(LinkPosCount);
               with LinkPos[LinkPosCount] do
               begin
                 A.Y:=PosY-1; B.Y:=PosY-1;
                 A.X:=StartX-1; B.X:=EndX-1;
               end;
             end;
           end;
      else
           with THLPKeywordRecord(KeyWR.Data^) do
           begin
             T^.LinkCount:=KeywordCount;
             GetMem(T^.Links,T^.LinkSize);
             if KeywordCount>0 then
             for I:=0 to KeywordCount-1 do
             begin
               T^.Links^[I].Context:=Keywords[I].KwContext;
               T^.Links^[I].FileID:=ID;
             end;
           end;
      end;
    end;

    if OK then OK:=ExtractTextRec(TextR);
    if OK then
      if TextR.Size>0 then
      begin
        T^.Text:=TextR.Data; T^.TextSize:=TextR.Size;
        TextR.Data:=nil; TextR.Size:=0;
      end;

    DisposeRecord(TextR); DisposeRecord(KeyWR);
  end;
  ReadTopic:=OK;
end;

destructor TOAHelpFile.Done;
begin
  if F<>nil then Dispose(F, Done);
  inherited Done;
end;

constructor THelpFacility.Init;
begin
  inherited Init;
  New(HelpFiles, Init(10,10));
  IndexTabSize:=40;
end;


function THelpFacility.AddOAHelpFile(const FileName: string): boolean;
var H: PHelpFile;
begin
  H:=New(POAHelpFile, Init(FileName, LastID+1));
  AddOAHelpFile:=AddFile(H);
end;

function THelpFacility.AddHTMLHelpFile(const FileName, TOCEntry: string): boolean;
var H: PHelpFile;
begin
  H:=New(PHTMLHelpFile, Init(FileName, LastID+1, TOCEntry));
  AddHTMLHelpFile:=AddFile(H);;
end;

function THelpFacility.AddNGHelpFile(const FileName: string): boolean;
var H: PHelpFile;
begin
  H:=New(PNGHelpFile, Init(FileName, LastID+1));
  AddNGHelpFile:=AddFile(H);;
end;

function THelpFacility.AddOS2HelpFile(const FileName: string): boolean;
var H: PHelpFile;
begin
  H:=New(POS2HelpFile, Init(FileName, LastID+1));
  AddOS2HelpFile:=AddFile(H);;
end;

function THelpFacility.AddWinHelpFile(const FileName: string): boolean;
var H: PHelpFile;
begin
  H:=New(PWinHelpFile, Init(FileName, LastID+1));
  AddWinHelpFile:=AddFile(H);;
end;

function THelpFacility.AddHTMLIndexHelpFile(const FileName: string): boolean;
var H: PHelpFile;
begin
  H:=New(PHTMLIndexHelpFile, Init(FileName, LastID+1));
  AddHTMLIndexHelpFile:=AddFile(H);;
end;

function THelpFacility.AddFile(H: PHelpFile): boolean;
begin
  if H<>nil then
    begin
      HelpFiles^.Insert(H);
      Inc(LastID);
    end;
  AddFile:=H<>nil;
end;

function THelpFacility.SearchTopicOwner(SourceFileID: word; Context: THelpCtx): PHelpFile;
var P: PTopic;
    HelpFile: PHelpFile;
function Search(F: PHelpFile): boolean; {$ifndef FPC}far;{$endif}
begin
  P:=SearchTopicInHelpFile(F,Context); if P<>nil then HelpFile:=F;
  Search:=P<>nil;
end;
begin
  HelpFile:=nil;
  if SourceFileID=0 then P:=nil else
     begin
       HelpFile:=SearchFile(SourceFileID);
       P:=SearchTopicInHelpFile(HelpFile,Context);
     end;
  if P=nil then HelpFiles^.FirstThat(@Search);
  if P=nil then HelpFile:=nil;
  SearchTopicOwner:=HelpFile;
end;

function THelpFacility.LoadTopic(SourceFileID: word; Context: THelpCtx): PTopic;
var P: PTopic;
    H: PHelpFile;
begin
  if (SourceFileID=0) and (Context=0) then
     P:=BuildIndexTopic else
  begin
    H:=SearchTopicOwner(SourceFileID,Context);
    if (H=nil) then P:=nil else
       P:=H^.LoadTopic(Context);
  end;
  LoadTopic:=P;
end;

function THelpFacility.TopicSearch(Keyword: string; var FileID: word; var Context: THelpCtx): boolean;
function ScanHelpFile(H: PHelpFile): boolean; {$ifndef FPC}far;{$endif}
function Search(P: PIndexEntry): boolean; {$ifndef FPC}far;{$endif}
begin
  Search:=copy(UpcaseStr(P^.Tag^),1,length(Keyword))=Keyword;
end;
var P: PIndexEntry;
begin
  H^.LoadIndex;
  P:=H^.IndexEntries^.FirstThat(@Search);
  if P<>nil then begin FileID:=H^.ID; Context:=P^.HelpCtx; end;
  ScanHelpFile:=P<>nil;
end;
begin
  Keyword:=UpcaseStr(Keyword);
  TopicSearch:=HelpFiles^.FirstThat(@ScanHelpFile)<>nil;
end;

function THelpFacility.BuildIndexTopic: PTopic;
var T: PTopic;
    Keywords: PIndexEntryCollection;
    Lines: PUnsortedStringCollection;
procedure InsertKeywordsOfFile(H: PHelpFile); {$ifndef FPC}far;{$endif}
function InsertKeywords(P: PIndexEntry): boolean; {$ifndef FPC}far;{$endif}
begin
  Keywords^.Insert(P);
  InsertKeywords:=Keywords^.Count>=MaxCollectionSize;
end;
begin
  H^.LoadIndex;
  if Keywords^.Count<MaxCollectionSize then
  H^.IndexEntries^.FirstThat(@InsertKeywords);
end;
procedure AddLine(S: string);
begin
  if S='' then S:=' ';
  Lines^.Insert(NewStr(S));
end;
var Line: string;
procedure FlushLine;
begin
  if Line<>'' then AddLine(Line); Line:='';
end;
var KWCount,NLFlag: sw_integer;
    LastFirstChar: char;
procedure NewSection(FirstChar: char);
begin
  if FirstChar<=#64 then FirstChar:=#32;
  FlushLine;
  AddLine('');
  AddLine(FirstChar);
  AddLine('');
  LastFirstChar:=FirstChar;
  NLFlag:=0;
end;
function FormatAlias(Alias: string): string;
var StartP,EndP: sw_integer;
begin
  repeat
    StartP:=Pos('  ',Alias);
    if StartP>0 then
    begin
      EndP:=StartP;
      while (EndP+1<=length(Alias)) and (Alias[EndP+1]=' ') do Inc(EndP);
      Alias:=copy(Alias,1,StartP-1)+' | '+copy(Alias,EndP+1,High(Alias));
    end;
  until StartP=0;
  if Assigned(HelpFacility) then
    if length(Alias)>IndexTabSize-4 then
      Alias:=Trim(copy(Alias,1,IndexTabSize-4-2))+'..';
  FormatAlias:=Alias;
end;
procedure AddKeyword(KWS: string);
begin
  Inc(KWCount); if KWCount=1 then NLFlag:=0;
  if (KWCount=1) or
     ( (Upcase(KWS[1])<>LastFirstChar) and ( (LastFirstChar>#64) or (KWS[1]>#64) ) ) then
     NewSection(Upcase(KWS[1]));

  KWS:=FormatAlias(KWS);

  if (NLFlag mod 2)=0
     then Line:=' '+#2+KWS+#2
     else begin
            Line:=RExpand(Line,IndexTabSize)+#2+KWS+#2;
            FlushLine;
          end;
  Inc(NLFlag);
end;
var KW: PIndexEntry;
    I: sw_integer;
begin
  New(Keywords, Init(5000,5000));
  HelpFiles^.ForEach(@InsertKeywordsOfFile);
  New(Lines, Init((Keywords^.Count div 2)+100,1000));
  T:=NewTopic(0,0,0,'',nil,0);
  if HelpFiles^.Count=0 then
    begin
      AddLine('');
      AddLine(' '+msg_nohelpfilesinstalled)
    end else
  begin
    AddLine(' '+msg_helpindex);
    KWCount:=0; Line:='';
    T^.LinkCount:=Min(Keywords^.Count,MaxBytes div sizeof(T^.Links^[0])-1);
    GetMem(T^.Links,T^.LinkSize);

    for I:=0 to T^.LinkCount-1 do
    begin
      KW:=Keywords^.At(I);
      AddKeyword(KW^.Tag^);
      T^.Links^[I].Context:=longint(KW^.HelpCtx);
      T^.Links^[I].FileID:=KW^.FileID;
    end;
    FlushLine;
    AddLine('');
  end;
  RenderTopic(Lines,T);
  Dispose(Lines, Done);
  Keywords^.DeleteAll; Dispose(Keywords, Done);
  BuildIndexTopic:=T;
end;

function THelpFacility.SearchFile(ID: byte): PHelpFile;
function Match(P: PHelpFile): boolean; {$ifndef FPC}far;{$endif}
begin
  Match:=(P^.ID=ID);
end;
begin
  SearchFile:=HelpFiles^.FirstThat(@Match);
end;

function THelpFacility.SearchTopicInHelpFile(F: PHelpFile; Context: THelpCtx): PTopic;
var P: PTopic;
begin
  if F=nil then P:=nil else
  P:=F^.SearchTopic(Context);
  SearchTopicInHelpFile:=P;
end;

destructor THelpFacility.Done;
begin
  inherited Done;
  Dispose(HelpFiles, Done);
end;

END.
{
  $Log$
  Revision 1.5  2000-11-15 00:14:11  pierre
   new merge

  Revision 1.1.2.3  2000/11/14 09:08:51  marco
   * First batch IDE renamefest

  Revision 1.4  2000/11/13 17:37:43  pierre
   merges from fixes branch

  Revision 1.1.2.2  2000/11/12 19:48:20  hajny
    * OS/2 implementation of GetDosTicks added

  Revision 1.3  2000/11/11 23:05:31  hajny

  Revision 1.2  2000/10/31 22:35:56  pierre
   * New big merge from fixes branch

  Revision 1.1.2.1  2000/09/18 13:20:56  pierre
   New bunch of Gabor changes

  Revision 1.1  2000/07/13 09:48:37  michael
  + Initial import

  Revision 1.26  2000/07/03 08:54:54  pierre
   * Some enhancements for WinHelp support by G	abor

  Revision 1.25  2000/06/26 07:29:23  pierre
   * new bunch of Gabor's changes

  Revision 1.24  2000/06/22 09:07:14  pierre
   * Gabor changes: see fixes.txt

  Revision 1.23  2000/06/16 08:50:44  pierre
   + new bunch of Gabor's changes

  Revision 1.22  2000/05/31 20:42:02  pierre
   * fixthe TRect problem by 'using' windows before objects

  Revision 1.21  2000/05/30 07:18:33  pierre
   + colors for HTML help by Gabor

  Revision 1.20  2000/05/29 10:44:59  pierre
   + New bunch of Gabor's changes: see fixes.txt

  Revision 1.19  2000/04/25 08:42:35  pierre
   * New Gabor changes : see fixes.txt

  Revision 1.18  2000/04/18 11:42:38  pierre
   lot of Gabor changes : see fixes.txt

  Revision 1.17  2000/02/07 11:47:25  pierre
   * Remove 64Kb limitation for FPC by Gabor

  Revision 1.16  2000/01/03 14:59:03  marco
   * Fixed Linux code that got time of day. Removed Timezone parameter

  Revision 1.15  1999/08/16 18:25:29  peter
    * Adjusting the selection when the editor didn't contain any line.
    * Reserved word recognition redesigned, but this didn't affect the overall
      syntax highlight speed remarkably (at least not on my Amd-K6/350).
      The syntax scanner loop is a bit slow but the main problem is the
      recognition of special symbols. Switching off symbol processing boosts
      the performance up to ca. 200%...
    * The editor didn't allow copying (for ex to clipboard) of a single character
    * 'File|Save as' caused permanently run-time error 3. Not any more now...
    * Compiler Messages window (actually the whole desktop) did not act on any
      keypress when compilation failed and thus the window remained visible
    + Message windows are now closed upon pressing Esc
    + At 'Run' the IDE checks whether any sources are modified, and recompiles
      only when neccessary
    + BlockRead and BlockWrite (Ctrl+K+R/W) implemented in TCodeEditor
    + LineSelect (Ctrl+K+L) implemented
    * The IDE had problems closing help windows before saving the desktop

  Revision 1.14  1999/07/18 16:26:42  florian
    * IDE compiles with for Win32 and basic things are working

  Revision 1.13  1999/04/13 10:47:51  daniel
  * Fixed for Linux

  Revision 1.12  1999/04/07 21:56:00  peter
    + object support for browser
    * html help fixes
    * more desktop saving things
    * NODEBUG directive to exclude debugger

  Revision 1.11  1999/03/16 12:38:16  peter
    * tools macro fixes
    + tph writer
    + first things for resource files

  Revision 1.10  1999/03/08 14:58:19  peter
    + prompt with dialogs for tools

  Revision 1.9  1999/03/03 16:44:05  pierre
   * TPH reader fix from Peter

  Revision 1.8  1999/03/01 15:42:11  peter
    + Added dummy entries for functions not yet implemented
    * MenuBar didn't update itself automatically on command-set changes
    * Fixed Debugging/Profiling options dialog
    * TCodeEditor converts spaces to tabs at save only if efUseTabChars is
 set
    * efBackSpaceUnindents works correctly
    + 'Messages' window implemented
    + Added '$CAP MSG()' and '$CAP EDIT' to available tool-macros
    + Added TP message-filter support (for ex. you can call GREP thru
      GREP2MSG and view the result in the messages window - just like in TP)
    * A 'var' was missing from the param-list of THelpFacility.TopicSearch,
      so topic search didn't work...
    * In FPHELP.PAS there were still context-variables defined as word instead
      of THelpCtx
    * StdStatusKeys() was missing from the statusdef for help windows
    + Topic-title for index-table can be specified when adding a HTML-files

  Revision 1.6  1999/02/20 15:18:35  peter
    + ctrl-c capture with confirm dialog
    + ascii table in the tools menu
    + heapviewer
    * empty file fixed
    * fixed callback routines in fpdebug to have far for tp7

  Revision 1.5  1999/02/19 15:43:22  peter
    * compatibility fixes for FV

  Revision 1.4  1999/02/18 13:44:37  peter
    * search fixed
    + backward search
    * help fixes
    * browser updates

  Revision 1.3  1999/02/08 10:37:46  peter
    + html helpviewer

  Revision 1.2  1998/12/28 15:47:56  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

  Revision 1.4  1998/12/22 10:39:55  peter
    + options are now written/read
    + find and replace routines

}
