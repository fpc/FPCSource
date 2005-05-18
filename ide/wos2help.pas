{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 2000 by Berczi Gabor

    Help support for OS/2 (.INF) help files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$R-}
unit WOS2Help;

interface

uses Objects,
     WUtils,WHelp;

const
      INFFileSignature = 'HS';

      { TOCEntry flags }
      inf_tef_HasChildren    = $80; { following nodes are a higher level }
      inf_tef_Hidden         = $40; { this entry doesn't appear in VIEW.EXE's presentation of the toc }
      inf_tef_Extended       = $20; { extended entry format }
      inf_tef_LevelMask      = $0f;

type
      TINFFileHeader = packed record
        Signature   : array[1..2] of char;{ ID magic word (5348h = "HS")                 }
        Unknown1    : byte;      { unknown purpose, could be third letter of ID }
        Flags       : byte;      { probably a flag word...                      }
                                 {  bit 0: set if INF style file                }
                                 {  bit 4: set if HLP style file                }
                                 { patching this byte allows reading HLP files  }
                                 { using the VIEW command, while help files     }
                                 { seem to work with INF settings here as well. }
        HeaderSize  : word;      { total size of header                         }
        Unknown2    : word;      { unknown purpose                              }
        NumTOC      : word;      { 16 bit number of entries in the tocarray     }
        TOCStrTabOfs: longint;   { 32 bit file offset of the start of the       }
                                 { strings for the table-of-contents            }
        TOCStrTabSize: longint;  { number of bytes in file occupied by the      }
                                 { table-of-contents strings                    }
        TOCArrayOfs : longint;   { 32 bit file offset of the start of tocarray  }
        NumResPanels: word;      { number of panels with ressource numbers      }
        ResTabOfs   : longint;   { 32 bit file offset of ressource number table }
        NumNames    : word;      { number of panels with textual name           }
        NameTabOfs  : longint;   { 32 bit file offset to panel name table       }
        NumIndexes  : word;      { number of index entries                      }
        IndexTabOfs : longint;   { 32 bit file offset to index table            }
        IndexTabSize: longint;   { size of index table                          }
        Unknown3: array[1..10] of byte; { unknown purpose                       }
        SearchTabOfs: longint;   { 32 bit file offset of full text search table }
        SearchTabSize:longint;   { size of full text search table               }
        NumSlots    : word;      { number of "slots"                            }
        SlotTabOfs  : longint;   { file offset of the slots array               }
        DictSize    : longint;   { number of bytes occupied by the "dictionary" }
        NumDictEntries: word;    { number of entries in the dictionary          }
        DictOfs     : longint;   { file offset of the start of the dictionary   }
        ImageOfs    : longint;   { file offset of image data                    }
        Unknown4    : byte;      { unknown purpose                              }
        NLSTabOfs   : longint;   { 32 bit file offset of NLS table              }
        NLSTabSize  : longint;   { size of NLS table                            }
        ExtBlockOfs : longint;   { 32 bit file offset of extended data block    }
        Unknown5: array[1..12] of byte;{ unknown purpose                        }
        Title       : array[1..48] of char;{ ASCII title of database            }
      end;

      PINFTOCArray = ^TINFTOCArray;
      TINFTOCArray = packed array[0..16382] of longint;

      PINFSlotArray = ^TINFSlotArray;
      TINFSlotArray = packed array[0..32766] of word;

      PSlotArray = ^TSlotArray;
      TSlotArray = packed array[0..16382] of longint;

      TINFTOCEntry = packed record
        Size     : byte; { length of the entry including this byte    }
        Flags    : byte; { flag byte, description folows (MSB first)  }
        NumSlots : byte; { number of "slots" occupied by the text for }
                         { this toc entry                             }
        Slots    : record end;
      end;

      TINFTopicHeader = packed record
        Stuff        : byte;      { ??                                   }
        LocalDictPos : longint;   { file offset of the local dictionary  }
        NumLocalDict : byte;      { number of entries in the local dict  }
        TextSize     : word;      { number of bytes in the text          }
        Text         : record end;{ encoded text of the article          }
      end;

      TINFEscHeader = packed record
        EscLen : byte;  { length of the sequence (including esclen but excluding FF) }
        EscCode: byte;  { which escape function }
      end;

      POS2HelpFile = ^TOS2HelpFile;
      TOS2HelpFile = object(THelpFile)
        constructor Init(AFileName: string; AID: word);
        destructor  Done; virtual;
      public
        function    LoadIndex: boolean; virtual;
        function    ReadTopic(T: PTopic): boolean; virtual;
      private
        F: PStream;
        Header: TINFFileHeader;
        Dictionary: PUnsortedStringCollection;
        Slots: PSlotArray;
        SlotsSize: longint;
        function ReadHeader: boolean;
        function ReadSlots: boolean;
        function ReadDictionary: boolean;
        function ReadTOC: boolean;
        function ReadTopicRec(FileOfs: longint; Topic: PTopic; Lines: PUnsortedStringCollection): boolean;
      end;

      TINFGetAttrColorProc = function(TextStyle, TextColor: byte; var Color: byte): boolean;

function DefINFGetAttrColor(TextStyle, TextColor: byte; var Color: byte): boolean;

const INFGetAttrColor : TINFGetAttrColorProc = {$ifdef fpc}@{$endif}DefINFGetAttrColor;

procedure RegisterHelpType;

implementation


function DefINFGetAttrColor(TextStyle, TextColor: byte; var Color: byte): boolean;
{
          style;    // 1,2,3: same as :hp#.
                    // 4,5,6: same as :hp5,6,7.
                    // 0 returns to plain text
          color;   // 1,2,3: same as :hp4,8,9.
                   // 0: default color

          :hp4 text is light blue
          :hp8 text is red
          :hp9 text is magenta

          :hp1 is italic font
          :hp2 is bold font
          :hp3 is bold italic font
          :hp5 is normal underlined font
          :hp6 is italic underlined font
          :hp7 is bold underlined font
}
begin
  DefINFGetAttrColor:=false;
end;

function KillNonASCII(S: string): string;
var I: sw_integer;
begin
  for I:=1 to length(S) do
    if S[I]<#32 then
      S[I]:='.';
  KillNonASCII:=S;
end;

function ContainsNonASCIIChar(const S: string): boolean;
var I: sw_integer;
begin
  ContainsNonASCIIChar:=false;
  for I:=1 to length(S) do
    if S[I]<#32 then
      begin
        ContainsNonASCIIChar:=true;
        Break;
      end;
end;

constructor TOS2HelpFile.Init(AFileName: string; AID: word);
var OK: boolean;
begin
  if inherited Init(AID)=false then Fail;
  New(Dictionary, Init(100,1000));
  F:=New(PFastBufStream, Init(AFileName, stOpenRead, HelpStreamBufSize));
  OK:=F<>nil;
  if OK then OK:=(F^.Status=stOK);
  if OK then OK:=ReadHeader;
  if OK=false then
  begin
    Done;
    Fail;
  end;
end;

function TOS2HelpFile.ReadHeader: boolean;
var OK: boolean;
begin
  F^.Read(Header,sizeof(Header));
  OK:=(F^.Status=stOK);
  OK:=OK and (Header.Signature=INFFileSignature);
  ReadHeader:=OK;
end;

function TOS2HelpFile.LoadIndex: boolean;
var OK: boolean;
begin
  OK:=ReadDictionary;
  if OK then OK:=ReadSlots;
  if OK then OK:=ReadTOC;
  LoadIndex:=OK;
end;

function TOS2HelpFile.ReadDictionary: boolean;
var OK: boolean;
    I: longint;
    C: array[0..255] of char;
    B: byte;
begin
  F^.Seek(Header.DictOfs);
  OK:=(F^.Status=stOK);
  I:=0;
  while OK and (I<Header.NumDictEntries) do
  begin
    FillChar(C,sizeof(C),0);
    F^.Read(B,sizeof(B)); F^.Read(C,B-1);
    OK:=(F^.Status=stOK);
    if OK then
      Dictionary^.InsertStr(StrPas(@C));
    Inc(I);
  end;
  ReadDictionary:=OK;
end;

function TOS2HelpFile.ReadSlots: boolean;
var OK: boolean;
begin
  SlotsSize:=Header.NumSlots*sizeof(Slots^[0]);
  GetMem(Slots,SlotsSize);
  F^.Seek(Header.SlotTabOfs);
  OK:=(F^.Status=stOK);
  if OK then
  begin
    F^.Read(Slots^,SlotsSize);
    OK:=(F^.Status=stOK);
  end;
  ReadSlots:=OK;
end;

function TOS2HelpFile.ReadTOC: boolean;
var OK: boolean;
    I,Count: longint;
    TE: TINFTOCEntry;
    W: word;
    C: array[0..255] of char;
    StartOfs: longint;
    S: string;
    SubSlots: PWordArray;
    SubSlotsSize: longint;
    TOC: PINFTOCArray;
    TOCSize: longint;
begin
  F^.Seek(Header.TOCArrayOfs); TOCSize:=Header.TOCStrTabSize;
  OK:=(F^.Status=stOK);
  if OK then
  begin
    GetMem(TOC,TOCSize);
    F^.Read(TOC^,TOCSize);
    OK:=(F^.Status=stOK);
    I:=0;
    while OK and (I<Header.NumTOC) do
    begin
      F^.Seek(TOC^[I]);
      OK:=(F^.Status=stOK);
      if OK then
      begin
        StartOfs:=F^.GetPos;
        F^.Read(TE,sizeof(TE));
        OK:=(F^.Status=stOK);
      end;
      if OK and ((TE.Flags and inf_tef_Extended)<>0) then
      begin
        F^.Read(W,sizeof(W));
        Count:=0;
        if (W and 1)<>0 then Inc(Count,5);
        if (W and 2)<>0 then Inc(Count,5);
        if (W and 4)<>0 then Inc(Count,2);
        if (W and 8)<>0 then Inc(Count,2);
        F^.Seek(F^.GetPos+Count);
        OK:=(F^.Status=stOK);
      end;
      if OK then
      begin
        SubSlotsSize:=sizeof(Word)*TE.NumSlots;
        GetMem(SubSlots,SubSlotsSize);
        F^.Read(SubSlots^,SubSlotsSize);
        FillChar(C,sizeof(C),0);
        F^.Read(C,Max(0,TE.Size-(F^.GetPos-StartOfs)));
        OK:=(F^.Status=stOK);
        if OK then
        begin
          S:=StrPas(@C);
          AddTopic(I,StartOfs,S,SubSlots,SubSlotsSize);
          if (S<>'') and (not ContainsNonASCIIChar(S)) then
           AddIndexEntry(S,I);
 {         !}
        end;
        FreeMem(SubSlots,SubSlotsSize);
      end;
      Inc(I);
    end;
    FreeMem(TOC,TOCSize); TOC:=nil;
  end;
  ReadTOC:=OK;
end;

function TOS2HelpFile.ReadTopicRec(FileOfs: longint; Topic: PTopic; Lines: PUnsortedStringCollection): boolean;
var Line: string;
    CharsInLine: sw_integer;
    LeftMargin: byte;
    TextStyle,TextColor: byte;
    InMonospace: boolean;
    Align: (alLeft,alRight,alCenter);
    LineNo: longint;
procedure FlushLine;
begin
  if Line<>'' then Lines^.InsertStr(Line);
  Line:='';
end;
procedure AddChar(C: char);
begin
  if length(Line)>=255 then FlushLine;
  Line:=Line+C;
end;
procedure AddString(const S: string);
var I: sw_integer;
begin
  for I:=1 to length(S) do
    AddChar(S[I]);
end;
procedure AddTextChar(C: char);
begin
  if (C=hscLineBreak) then
  begin
    case Align of
      alRight : AddChar(hscRight);
      alCenter: AddChar(hscCenter);
    end;
  end;
  if (CharsInLine=0) and (C<>hscLineBreak) then
  begin
    if (LeftMargin>0) then
      begin
        AddString(CharStr(#255,LeftMargin)+hscLineStart);
        Inc(CharsInLine,LeftMargin);
      end else
    if InMonospace then
      begin
        AddChar(' ');
        Inc(CharsInLine);
      end;
  end;
  AddChar(C);
  if C=hscLineBreak then
    begin
      CharsInLine:=0;
      Inc(LineNo);
    end
  else
    Inc(CharsInLine);
end;
procedure AddText(const S: string);
var I: sw_integer;
begin
  for I:=1 to length(S) do
    AddTextChar(S[I]);
end;
var H: TINFTopicHeader;
    Text: PByteArray;
    TextOfs: longint;
    Dict: PWordArray;
    Spacing: boolean;
function NextByte: byte;
begin
  NextByte:=Text^[TextOfs];
  Inc(TextOfs);
end;
procedure ReadBytes(DataPtr: pointer; NumBytes: sw_integer);
var I: sw_integer;
begin
  for I:=1 to NumBytes do
    if Assigned(DataPtr) then
      PByteArray(DataPtr)^[I-1]:=NextByte
    else
      NextByte;
end;
procedure AddWord(LocalIndex: word);
begin
  AddText(GetStr(Dictionary^.At(Dict^[LocalIndex])));
  if Spacing and not InMonospace then AddTextChar(' ');
end;
var
    DictSize,EscStartOfs: longint;
    OK: boolean;
    EH: TINFEscHeader;
    B,Color: byte;
    W: word;
    CurLinkCtx: longint;
    InTempMargin: boolean;
begin
  F^.Reset;
  F^.Seek(FileOfs);
  OK:=(F^.Status=stOK);
  if OK then
  begin
    F^.Read(H,sizeof(H));
    OK:=(F^.Status=stOK);
  end;
  if OK then
  begin
    LineNo:=0;
    Line:=''; LeftMargin:=0;
    InTempMargin:=false;
    CharsInLine:=0; TextStyle:=0; TextColor:=0; Align:=alLeft;
    CurLinkCtx:=-1; InMonospace:=false;
    DictSize:=H.NumLocalDict*sizeof(Dict^[0]);
    GetMem(Text,H.TextSize);
    GetMem(Dict,DictSize);
    F^.Read(Text^,H.TextSize);
    F^.Seek(H.LocalDictPos);
    F^.Read(Dict^,DictSize);
    OK:=(F^.Status=stOK);

    TextOfs:=0; Spacing:=true;
    while OK and (TextOfs<H.TextSize) do
    begin
      B:=NextByte;
      if (B<H.NumLocalDict) then
      begin
        AddWord(B);
      end else
      case B of
        $fa : begin
                if (LineNo>0) then
                  AddTextChar(hscLineBreak);
                if InTempMargin then
                  LeftMargin:=0;
                AddTextChar(hscLineBreak);
                Spacing:=true;
              end;
        $fb : { ??? };
        $fc : Spacing:=not Spacing;
        $fd : begin
                AddTextChar(hscLineBreak);
                Spacing:=true;
              end;
        $fe : AddChar(' ');
        $ff : begin
                EscStartOfs:=TextOfs;
                ReadBytes(@EH,sizeof(EH));
                case EH.EscCode of
                  $02,$11,$12 :
                    begin
                      { set left margin }
                      if EH.EscCode=$11 then
                        AddTextChar(hscLineBreak);
                      LeftMargin:=NextByte;
                    end;
                  $03 :
                    { right margin, not used }
                    NextByte;
                  $04 :
                    begin
                      TextStyle:=NextByte;
                      if (TextStyle=0) or (not INFGetAttrColor(TextStyle,TextColor,Color)) then
                        AddChar(hscNormText)
                      else
                        AddText(hscTextColor+chr(Color));
                    end;
                  $05 :
                    begin
                      W:=word(NextByte)*256+NextByte;
                      AddChar(hscLink);
                      CurLinkCtx:=W;
                    end;
                  $08 :
                    if CurLinkCtx<>-1 then
                    begin
                      AddChar(hscLink);
                      AddLinkToTopic(Topic,ID,CurLinkCtx);
                    end;
                  $0b :
                    begin
                      if CharsInLine>0 then
                        AddTextChar(hscLineBreak);
                      AddTextChar(hscLineBreak);
                      AddChar(hscCode);
                      InMonospace:=true;
                    end;
                  $0c :
                    begin
                      AddChar(hscCode);
                      InMonospace:=false;
                    end;
                  $0d :
                    begin
                      TextColor:=NextByte;
                      if (TextColor=0) or (not INFGetAttrColor(TextStyle,TextColor,Color)) then
                        AddChar(hscNormText)
                      else
                        AddText(hscTextColor+chr(Color));
                    end;
                  $0e :
                    begin
                      AddText(hscLineBreak+'[img]'+hscLineBreak);
                    end;
                  $1a :
                    begin
                      if CharsInLine>0 then AddText(hscLineBreak);
                      case NextByte of
                        1 : Align:=alLeft;
                        2 : Align:=alRight;
                        4 : Align:=alCenter;
                      end;
                      Spacing:=false;
                    end;
                  $1b :
                    begin
                      Spacing:=true;
                    end;
                  $1c :
                    begin
                      LeftMargin:=CharsInLine;
                      InTempMargin:=true;
                    end;
                end;
                TextOfs:=EscStartOfs+EH.EscLen;
              end;
      end;
    end;
    if CharsInLine>0 then
      AddTextChar(hscLineBreak);
    AddTextChar(hscLineBreak);
    FlushLine;

    FreeMem(Dict,DictSize);
    FreeMem(Text,H.TextSize);
  end;
  F^.Reset;
  ReadTopicRec:=OK;
end;

function TOS2HelpFile.ReadTopic(T: PTopic): boolean;
var OK: boolean;
    I,NumSlots,Idx: sw_integer;
    TopicOfs: longint;
    L: PUnsortedStringCollection;
    Title: string;
begin
  OK:=false;
  NumSlots:=T^.ExtDataSize div 2; { extdata is array of word }
  New(L, Init(100,100));
  Title:=GetStr(T^.Param);
  if Title<>'' then
  begin
    L^.InsertStr('  '+Title+' Ü'+hscLineBreak);
    L^.InsertStr(' '+CharStr('ß',length(Title)+3)+hscLineBreak);
  end;
  if 0<T^.HelpCtx then
  begin
    L^.InsertStr(hscLink+'[previous topic]'+hscLink+'  ');
    AddLinkToTopic(T,ID,T^.HelpCtx-1);
  end;
  if T^.HelpCtx<Header.NumTOC then
  begin
    L^.InsertStr(hscLink+'[next topic]'+hscLink);
    AddLinkToTopic(T,ID,T^.HelpCtx+1);
  end;
  L^.InsertStr(hscLineBreak);
  for I:=0 to NumSlots-1 do
  begin
    Idx:=PWordArray(T^.ExtData)^[I];
    TopicOfs:=Slots^[Idx];
    OK:=ReadTopicRec(TopicOfs,T,L);
    if not OK then
      Break;
  end;
  if OK then BuildTopic(L,T);
  Dispose(L, Done);
  ReadTopic:=OK;
end;

destructor TOS2HelpFile.Done;
begin
  if Assigned(Slots) then FreeMem(Slots, SlotsSize); Slots:=nil;
  if Assigned(Dictionary) then Dispose(Dictionary, Done); Dictionary:=nil;
  if Assigned(F) then Dispose(F, Done); F:=nil;
  inherited Done;
end;

function CreateProc(const FileName,Param: string;Index : longint): PHelpFile; {$ifndef FPC}far;{$endif}
begin
  CreateProc:=New(POS2HelpFile, Init(FileName,Index));
end;

procedure RegisterHelpType;
begin
  RegisterHelpFileType({$ifdef FPC}@{$endif}CreateProc);
end;

END.
