{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 2000 by Berczi Gabor

    Help support for Norton Guide (.NG) files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$R-}
unit WNGHelp;

interface

uses Objects,
     WUtils,WHelp;

const
      NGFileSignature = 'NG';
      NGXORByte       = $1a;

      NGMinRecordSize = $1a;

      ng_rtContainer    = Byte ($0);
      ng_rtTopic        = Byte ($1);

type
      TNGFileHeader = packed record
        Signature     : array[1..2] of char;
        Unknown       : word;
        Version       : word;
        MenuCount     : word;
        GuideName     : array[8..47] of char;
        Credits       : array[48..377] of char;
      end;

      TNGRecordHeader = packed record
        RecType      : word;
        RecLength    : word;
      end;

      TNGContainerItem = packed record
        EntryNameOfs  : word; { relative in record }
        SubItemsOfs   : longint; { file offset to a new record header }
      end;

      PNGContainerRecord = ^TNGContainerRecord;
      TNGContainerRecord = packed record
        ItemCount     : word;
        Unknown       : word;
        IndexInParent : integer;
        ParentOfs     : longint;
        MenuNo        : integer;{ belongs to menu # }
        MenuItemNo    : integer;{ belongs to menu item # }
        Unknown2      : array[18..25] of byte;
        Items         : array[0..0] of TNGContainerItem;
      end;

      TNGTopicRecord = packed record
        NumberOfLines : word;
        SeeAlsoOfs    : word;
        IndexInParent : integer;
        ParentOfs     : longint;
        MenuNo        : integer;{ belongs to menu # }
        MenuItemNo    : integer;{ belongs to menu item # }
        PrevTopicOfs  : longint;
        NextTopicOfs  : longint;
        TopicLines    : record end;
        { array of TNGSeeAlsoRec }
      end;

      TNGSeeAlsoRec = packed record
        EntryCount    : word;
        Entries       : record end;
        { array of LinkedRecOfs  : longint; }
        { array of LinkNames : ASCIIZ; }
      end;

      PContainerItemRec = ^TContainerItemRec;
      TContainerItemRec = record
        Name     : string;
        FilePos  : longint;
        Container: PNGContainerRecord;
      end;

      PNGHelpFile = ^TNGHelpFile;
      TNGHelpFile = object(THelpFile)
        constructor Init(AFileName: string; AID: word);
        destructor  Done; virtual;
      public
        function    LoadIndex: boolean; virtual;
        function    ReadTopic(T: PTopic): boolean; virtual;
      private
        F: PStream;
        Header: TNGFileHeader;
{        NextHelpCtx: longint;}
        function ReadHeader: boolean;
        function ReadContainer(EnumProc: pointer): boolean;
        function ReadTopicRec(Lines: PUnsortedStringCollection): boolean;
        function ReadRecord(var R: TRecord; ReadData: boolean): boolean;
      end;

implementation

uses CallSpec;

function NGDecompressStr(const S: string): string;
var NS: string;
    I: sw_integer;
begin
  NS:='';
  I:=1;
  while (I<=length(S)) do
  begin
    if S[I]=#255 then
      begin
        NS:=NS+CharStr(' ',ord(S[I+1]));
        Inc(I);
      end
    else
      NS:=NS+S[I];
    Inc(I);
  end;
  NGDecompressStr:=NS;
end;

function TranslateStr(const S: string): string;
var NS: string;
    I: sw_integer;
    InHiLite: boolean;
begin
  NS:=''; InHiLite:=false;
  I:=1;
  while (I<=length(S)) do
  begin
    case S[I] of
      '^'  : begin
               Inc(I);
               case S[I] of
                 '^' : NS:=NS+'^';
                 'B' : begin
                         if InHiLite then
                           NS:=NS+hscNormText
                         else
                           NS:=NS+hscTextColor+chr(15);
                         InHiLite:=not InHiLite;
                       end;
                 'b' : begin
                         if InHiLite then
                           NS:=NS+hscNormText
                         else
                           NS:=NS+hscTextColor+chr(11);
                         InHiLite:=not InHiLite;
                       end;
                 'U' : begin
                         if InHiLite then
                           NS:=NS+hscNormText
                         else
                           NS:=NS+hscTextColor+chr(3);
                         InHiLite:=not InHiLite;
                       end;
               else
                 NS:=NS;
               end;
             end;
    else NS:=NS+S[I];
    end;
    Inc(I);
  end;
  if InHiLite then NS:=NS+hscNormText;
  TranslateStr:=NS;
end;

procedure TranslateLines(P: PUnsortedStringCollection);
var S: string;
    I: sw_integer;
begin
  for I:=0 to P^.Count-1 do
  begin
    S:=GetStr(P^.At(I));
    P^.AtFree(I);
    P^.AtInsert(I,NewStr(TranslateStr(S)));
  end;
end;

constructor TNGHelpFile.Init(AFileName: string; AID: word);
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
    if length(Alias)>HelpFacility^.IndexTabSize-4 then
      Alias:=Trim(copy(Alias,1,HelpFacility^.IndexTabSize-4-2))+'..';
  FormatAlias:=Alias;
end;
procedure AddToIndex(P: PContainerItemRec); {$ifndef FPC}far;{$endif}
var S: string;
begin
  S:=Trim(P^.Name);
  S:=TranslateStr(S);
  S:=Trim(FormatAlias(S));
  if (S<>'') and (P^.FilePos<>-1) then
    begin
{      Inc(NextHelpCtx);}
      AddIndexEntry(S,P^.FilePos);
      AddTopic(P^.FilePos,P^.FilePos,'');
    end;
end;
var OK: boolean;
    FS: longint;
    R: TRecord;
    L: longint;
begin
  if inherited Init(AID)=false then Fail;
  F:=New(PBufStream, Init(AFileName, stOpenRead, HelpStreamBufSize));
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
    if (OK=false) then Break;
    case R.SClass of
      ng_rtContainer : begin F^.Seek(L); OK:=ReadContainer(@AddToIndex); end;
{      ng_rtTopic     : begin F^.Seek(L); OK:=ReadTopicRec; end;}
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
       begin
         Inc(L, sizeof(TNGRecordHeader)+R.Size); F^.Seek(L);
         OK:=(F^.Status=stOK);
       end;
  end;
  if OK=false then
  begin
    Done;
    Fail;
  end;
end;

function TNGHelpFile.ReadHeader: boolean;
var OK: boolean;
begin
  F^.Read(Header,sizeof(Header));
  OK:=(F^.Status=stOK);
  OK:=OK and (Header.Signature=NGFileSignature);
  ReadHeader:=OK;
end;

function KillSpecChars(const S: string): string;
var I: sw_integer;
    RS: string;
begin
  RS:='';
  for I:=1 to length(S) do
    if S[I]>=#32 then
      RS:=RS+S[I];
  KillSpecChars:=RS;
end;

function TNGHelpFile.ReadContainer(EnumProc: pointer): boolean;
var OK: boolean;
    R: TRecord;
    I,L: longint;
    CI: TNGContainerItem;
    P: pointer;
    CIR: TContainerItemRec;
begin
  OK:=ReadRecord(R, true);
  if OK then
  with TNGContainerRecord(R.Data^) do
  begin
    I:=0;
    while (I<ItemCount) do
    with Items[I] do
    begin
      P:=@(PByteArray(R.Data)^[NGMinRecordSize-sizeof(TNGRecordHeader)+EntryNameOfs]);
      FillChar(CIR,sizeof(CIR),0);
      with CIR do
      begin
        Container:=R.Data;
        Name:=NGDecompressStr(StrPas(P));
        FilePos:=SubItemsOfs;
      end;
      CallPointerLocal(EnumProc,PreviousFramePointer,@CIR);
      Inc(I);
    end;
  end;
  DisposeRecord(R);
  ReadContainer:=OK;
end;

function TNGHelpFile.ReadTopicRec(Lines: PUnsortedStringCollection): boolean;
var OK: boolean;
    R: TRecord;
    I: sw_integer;
    LineP: pointer;
    S: string;
begin
  OK:=ReadRecord(R, true);
  if OK then
  with TNGTopicRecord(R.Data^) do
  begin
    LineP:=@TopicLines;
    for I:=1 to NumberOfLines do
    begin
      S:=StrPas(LineP);
      Lines^.InsertStr(NGDecompressStr(S));
      LineP:=pointer(longint(LineP)+length(S)+1);
    end;
  end;
  DisposeRecord(R);
  ReadTopicRec:=OK;
end;

function TNGHelpFile.ReadRecord(var R: TRecord; ReadData: boolean): boolean;
var OK: boolean;
    H: TNGRecordHeader;
    I: sw_integer;
begin
  FillChar(R, SizeOf(R), 0);
  F^.Read(H,SizeOf(H));
  OK:=F^.Status=stOK;
  if OK then
    for I:=0 to SizeOf(H)-1 do
      PByteArray(@H)^[I]:=PByteArray(@H)^[I] xor NGXORByte;
  if OK then
  begin
    R.SClass:=H.RecType; R.Size:=H.RecLength+(NGMinRecordSize-sizeof(TNGRecordHeader));
    if (R.Size>0) and ReadData then
    begin
      GetMem(R.Data,R.Size);
      F^.Read(R.Data^,R.Size);
      if R.Size>0 then
      for I:=0 to R.Size-1 do
        PByteArray(R.Data)^[I]:=PByteArray(R.Data)^[I] xor NGXORByte;
      OK:=F^.Status=stOK;
    end;
    if OK=false then DisposeRecord(R);
  end;
  ReadRecord:=OK;
end;

function TNGHelpFile.LoadIndex: boolean;
begin
  LoadIndex:=false;
end;

function TNGHelpFile.ReadTopic(T: PTopic): boolean;
var Lines: PUnsortedStringCollection;
procedure AddToTopic(P: PContainerItemRec); {$ifndef FPC}far;{$endif}
begin
  Lines^.InsertStr(hscLink+P^.Name+hscLink);
  AddLinkToTopic(T,ID,P^.FilePos);
end;
var OK: boolean;
    R: TRecord;
begin
  New(Lines, Init(100,100));
  F^.Seek(T^.FileOfs); OK:=F^.Status=stOK;
  if OK then OK:=ReadRecord(R,false);
  case R.SClass of
      ng_rtContainer :
        begin
          F^.Seek(T^.FileOfs);
          Lines^.InsertStr(' ');
          OK:=ReadContainer(@AddToTopic);
          RenderTopic(Lines,T);
        end;
      ng_rtTopic     :
        begin
          F^.Seek(T^.FileOfs);
          Lines^.InsertStr(' ');
          OK:=ReadTopicRec(Lines);
          TranslateLines(Lines);
          Lines^.InsertStr(' ');
          RenderTopic(Lines,T);
        end;
  else OK:=false;
  end;
  Dispose(Lines, Done);
  ReadTopic:=OK;
end;

destructor TNGHelpFile.Done;
begin
  if Assigned(F) then Dispose(F, Done); F:=nil;
  inherited Done;
end;

END.
{
  $Log$
  Revision 1.1  2000-06-22 09:07:15  pierre
   * Gabor changes: see fixes.txt

}