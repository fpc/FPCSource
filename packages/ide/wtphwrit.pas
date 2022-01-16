{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Routines to create .tph files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit WTPHWriter;

interface

uses
    Objects, WoaHelp, WHelp;

const
     HelpStamp = 'TURBO PASCAL HelpFile.';

     DefFormatVersion = $34;

type
    PHelpFileWriter = ^THelpFileWriter;
    THelpFileWriter = object(TOAHelpFile)
      constructor Init(AFileName: string; AID: word);
      function    CreateTopic(HelpCtx: THelpCtx): PTopic; virtual;
      procedure   AddTopicToIndex(IndexTag: string; P: PTopic); virtual;
      procedure   AddLineToTopic(P: PTopic; Line: string); virtual;
      procedure   AddLinkToTopic(P: PTopic; AHelpCtx: THelpCtx);
      procedure   AddIndexEntry(Tag: string; P: PTopic); virtual;
      function    WriteFile: boolean; virtual;
      destructor  Done; virtual;
    private
      procedure   CompleteContextNo;
      procedure   CalcTopicOfs;
      procedure   WriteHeader(var S: TStream);
      procedure   WriteCompressionRecord(var S: TStream);
      procedure   WriteContextTable(var S: TStream);
      procedure   WriteIndexTable(var S: TStream);
      procedure   WriteTopic(var S: TStream; T: PTopic);
      procedure   WriteRecord(var S: TStream; RecType: byte; var Buf; Size: word);
    end;

implementation

constructor THelpFileWriter.Init(AFileName: string; AID: word);
var OK: boolean;
begin
  THelpFile.Init(AID);
  New(F, Init(AFileName, stCreate, HelpStreamBufSize));
  OK:=F<>nil;
  if OK then OK:=(F^.Status=stOK);
  if OK=false then Fail;
end;

function THelpFileWriter.CreateTopic(HelpCtx: THelpCtx): PTopic;
var P: PTopic;
begin
  if (HelpCtx<>0) and (SearchTopic(HelpCtx)<>nil) then
    P:=nil
  else
    begin
      P:=NewTopic(ID,HelpCtx,0,'');
      Topics^.Insert(P);
    end;
  CreateTopic:=P;
end;

procedure THelpFileWriter.AddTopicToIndex(IndexTag: string; P: PTopic);
begin
  IndexEntries^.Insert(NewIndexEntry(IndexTag,P^.FileID,P^.HelpCtx));
end;

procedure THelpFileWriter.AddLineToTopic(P: PTopic; Line: string);
var OldText: pointer;
    OldSize: word;
begin
  if P=nil then Exit;
  OldText:=P^.Text; OldSize:=P^.TextSize;
  Inc(P^.TextSize,length(Line)+1);
  GetMem(P^.Text,P^.TextSize);
  if OldText<>nil then Move(OldText^,P^.Text^,OldSize);
  Move(Line[1],P^.Text^[OldSize],length(Line));
  P^.Text^[OldSize+length(Line)]:=0;
  if OldText<>nil then FreeMem(OldText,OldSize);
end;

procedure THelpFileWriter.AddLinkToTopic(P: PTopic; AHelpCtx: THelpCtx);
var OldEntries: pointer;
    OldCount  : word;
    OldSize   : word;
begin
  if P=nil then Exit;
  OldEntries:=P^.Links; OldCount:=P^.LinkCount; OldSize:=P^.LinkSize;
  Inc(P^.LinkCount);
  GetMem(P^.Links,P^.LinkSize);
  if OldEntries<>nil then Move(OldEntries^,P^.Links^,OldSize);
  with P^.Links^[P^.LinkCount-1] do
    begin
      FileID:=ID;
      Context:=AHelpCtx;
    end;
  if OldEntries<>nil then FreeMem(OldEntries,OldSize);
end;

procedure THelpFileWriter.AddIndexEntry(Tag: string; P: PTopic);
begin
  if P=nil then Exit;
  IndexEntries^.Insert(NewIndexEntry(Tag,P^.FileID,P^.HelpCtx));
end;

function THelpFileWriter.WriteFile: boolean;
var I: sw_integer;
    CtxStart: longint;
begin
  CompleteContextNo;
  CalcTopicOfs;

  WriteHeader(F^);
  WriteCompressionRecord(F^);
  CtxStart:=F^.GetPos;
  WriteContextTable(F^);
  WriteIndexTable(F^);
  for I:=0 to Topics^.Count-1 do
    begin
      WriteTopic(F^,Topics^.At(I));
    end;
  F^.Seek(CtxStart);
  WriteContextTable(F^);
end;

procedure THelpFileWriter.WriteHeader(var S: TStream);
var St: string;
begin
  Version.FormatVersion:=DefFormatVersion;

  St:=HelpStamp+#0#$1a;
  F^.Write(St[1],length(St));
  St:=Signature;
  F^.Write(St[1],length(St));
  F^.Write(Version,SizeOf(Version));

  WriteRecord(F^,rtFileHeader,Header,SizeOf(Header));
end;

procedure THelpFileWriter.WriteCompressionRecord(var S: TStream);
var CR: THLPCompression;
begin
  FillChar(CR,SizeOf(CR),0);
  WriteRecord(F^,rtCompression,CR,SizeOf(CR));
end;

procedure THelpFileWriter.WriteIndexTable(var S: TStream);
const BufSize = 65000;
var P: ^THLPIndexTable;
    TableSize: word;
procedure AddByte(B: byte);
begin
  PByteArray(@P^.Entries)^[TableSize]:=B;
  Inc(TableSize);
end;
procedure AddEntry(Tag: string; HelpCtx: word);
var Len,I: byte;
begin
  Len:=length(Tag); if Len>31 then Len:=31;
  AddByte(Len);
  for I:=1 to Len do
    AddByte(ord(Tag[I]));
  AddByte(Lo(HelpCtx)); AddByte(Hi(HelpCtx));
end;
var I: sw_integer;
begin
  if IndexEntries^.Count=0 then Exit;
  GetMem(P,BufSize);

  TableSize:=0;
  P^.IndexCount:=IndexEntries^.Count;
  for I:=0 to IndexEntries^.Count-1 do
    with IndexEntries^.At(I)^ do
    AddEntry(Tag^,HelpCtx);
  Inc(TableSize,SizeOf(P^.IndexCount));
  WriteRecord(F^,rtIndex,P^,TableSize);

  FreeMem(P,BufSize);
end;

procedure THelpFileWriter.WriteContextTable(var S: TStream);
var Ctxs: ^THLPContexts;
    CtxSize,I: word;
    T: PTopic;
    MaxCtx: longint;
begin
  if Topics^.Count=0 then MaxCtx:=1 else
    MaxCtx:=Topics^.At(Topics^.Count-1)^.HelpCtx;
  CtxSize:=SizeOf(Ctxs^.ContextCount)+SizeOf(Ctxs^.Contexts[0])*(MaxCtx+1);
  GetMem(Ctxs,CtxSize); FillChar(Ctxs^,CtxSize,0);
  Ctxs^.ContextCount:=MaxCtx+1;
  for I:=1 to Topics^.Count do
    begin
      T:=Topics^.At(I-1);
      with Ctxs^.Contexts[T^.HelpCtx] do
       begin
         LoW:=(T^.FileOfs and $ffff);
         HiB:=(T^.FileOfs shr 16) and $ff;
       end;
    end;
  WriteRecord(F^,rtContext,Ctxs^,CtxSize);
  FreeMem(Ctxs,CtxSize);
end;

procedure THelpFileWriter.WriteTopic(var S: TStream; T: PTopic);
var TextBuf: PByteArray;
    TextSize: word;
    KWBuf: ^THLPKeywordRecord;
    I,KWBufSize: word;
begin
  T^.FileOfs:=S.GetPos;
  TextBuf:=T^.Text; TextSize:=T^.TextSize;
  WriteRecord(F^,rtText,TextBuf^,TextSize);
  { write keyword record here }
  KWBufSize:=SizeOf(KWBuf^)+SizeOf(KWBuf^.Keywords[0])*T^.LinkCount;
  GetMem(KWBuf,KWBufSize); FillChar(KWBuf^,KWBufSize,0);
  KWBuf^.KeywordCount:=T^.LinkCount;
  for I:=0 to T^.LinkCount-1 do
    KWBuf^.Keywords[I].kwContext:=T^.Links^[I].Context;
  WriteRecord(F^,rtKeyword,KWBuf^,KWBufSize);
  FreeMem(KWBuf,KWBufSize);
end;

procedure THelpFileWriter.CompleteContextNo;
var P: PTopic;
    NextTopicID: THelpCtx;
function SearchNextFreeTopicID: THelpCtx;
begin
  while Topics^.SearchTopic(NextTopicID)<>nil do
    Inc(NextTopicID);
  SearchNextFreeTopicID:=NextTopicID;
end;
begin
  NextTopicID:=1;
  repeat
    P:=Topics^.SearchTopic(0);
    if P<>nil then
      begin
        Topics^.Delete(P);
        P^.HelpCtx:=SearchNextFreeTopicID;
        Topics^.Insert(P);
      end;
  until P=nil;
end;

procedure THelpFileWriter.CalcTopicOfs;
begin
end;

procedure THelpFileWriter.WriteRecord(var S: TStream; RecType: byte; var Buf; Size: word);
var RH: THLPRecordHeader;
begin
  RH.RecType:=RecType; RH.RecLength:=Size;
  S.Write(RH,SizeOf(RH));
  S.Write(Buf,Size);
end;

destructor THelpFileWriter.Done;
begin
  inherited Done;
end;

END.
