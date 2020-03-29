{
    wThis file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 2000 by Berczi Gabor

    Borland OA .HLP reader objects and routines

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$R-}
unit WOAHelp;

interface

uses Objects,WUtils,WHelp;

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

type
      FileStamp      = array [0..32] of char; {+ null terminator + $1A }
      FileSignature  = array [0..12] of char; {+ null terminator }

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

procedure RegisterHelpType;

implementation


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
  if OK=false then
    Begin
      Done;
      Fail;
    End;
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
      Header.Options        :=LEToN(Header.Options);
      Header.MainIndexScreen:=LEToN(Header.MainIndexScreen);
      Header.MaxScreenSize  :=LEToN(Header.MaxScreenSize );
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
  c.LoW:=LEToN(Word(C.LoW));
  GetCtxPos:=longint(C.HiB) shl 16 + C.LoW;
end;
begin
  OK:=ReadRecord(R, true);
  if OK then
  with THLPContexts(R.Data^) do
  begin
  ContextCount:=LEToN(ContextCount);
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
  FillChar(R, SizeOf(R), 0);
  LastTag:=''; CurPtr:=0;
  OK:=(IndexTablePos<>0);
  if OK then begin F^.Seek(IndexTablePos); OK:=F^.Status=stOK; end;
  if OK then OK:=ReadRecord(R, true);
  if OK then
  with THLPIndexTable(R.Data^) do
  begin
  IndexCount:=LEToN(IndexCount);
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
  H.RecLength:=LEToN(H.RecLength);
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
                  C:=GetNextChar();
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
             UpContext:=LEToN(UpContext);
             DownContext:=LEToN(DownContext);
             T^.LinkCount:=KeywordCount;
             GetMem(T^.Links,T^.LinkSize);
             if T^.LinkCount>0 then
             for I:=0 to T^.LinkCount-1 do
             with Keywords[I] do
             begin
               KwContext:=LEToN(KwContext);
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
             KeywordCount:=LEToN(KeywordCount);
             UpContext:=LEToN(UpContext);
             DownContext:=LEToN(DownContext);
             T^.LinkCount:=KeywordCount;
             GetMem(T^.Links,T^.LinkSize);
             if KeywordCount>0 then
             for I:=0 to KeywordCount-1 do
             begin
               Keywords[I].KwContext:=LEToN(Keywords[I].KwContext);
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

function CreateProc(const FileName,Param: string;Index : longint): PHelpFile;
begin
  CreateProc:=New(POAHelpFile, Init(FileName,Index));
end;

procedure RegisterHelpType;
begin
  RegisterHelpFileType(@CreateProc);
end;

END.
