{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1999-2000 by Berczi Gabor

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit WHTMLHlp;

interface

uses Objects,WHTML,WAnsi,WHelp;

const
     extHTML              = '.htm';
     extHTMLIndex         = '.htx';

     ListIndent = 2;
     DefIndent  = 4;

     MaxTopicLinks = 4000; { maximum number of links on a single HTML page }

type
    THTMLSection = (hsNone,hsHeading1,hsHeading2,hsHeading3,hsHeading4,hsHeading5,hsHeading6);

    PTopicLinkCollection = ^TTopicLinkCollection;
    TTopicLinkCollection = object(TStringCollection)
      procedure   Insert(Item: Pointer); virtual;
      function    At(Index: sw_Integer): PString;
      function    AddItem(Item: string): integer;
    end;

    TParagraphAlign = (paLeft,paCenter,paRight);

    PTableElement = ^TTableElement;
    TTableElement = object(Tobject)
      TextBegin,TextEnd, TextLength, NumNL : sw_word;
      Alignment : TParagraphAlign;
      NextEl : PTableElement;
      constructor init(AAlignment : TParagraphAlign);
    end;

    PTableLine = ^TTableLine;
    TTableLine = object(Tobject)
      NumElements : sw_word;
      Nextline : PTableLine;
      FirstEl,LastEl : PTableElement;
      constructor Init;
      procedure AddElement(PTE : PTableElement);
      destructor Done; virtual;
    end;

    PHTMLTopicRenderer = ^THTMLTopicRenderer;
    PTable = ^TTable;
    TTable = object(Tobject)
      NumLines,NumCols : sw_word;
      GlobalOffset,
      GlobalTextBegin : sw_word;
      WithBorder : boolean;
      IsBar : boolean;
      FirstLine : PTableLine;
      LastLine : PTableLine;
      PreviousTable : PTable;
      Renderer : PHTMLTopicRenderer;
      constructor Init(Previous : PTable);
      procedure AddLine(PL : PTableLine);
      procedure AddElement(PTE : PTableElement);
      procedure TextInsert(Pos : sw_word;const S : string);
      procedure FormatTable;
      destructor Done; virtual;
    end;

    THTMLTopicRenderer = object(THTMLParser)
      function  BuildTopic(P: PTopic; AURL: string; HTMLFile: PTextFile; ATopicLinks: PTopicLinkCollection): boolean;
    public
      function  DocAddTextChar(C: char): boolean; virtual;
      procedure DocSoftBreak; virtual;
      procedure DocTYPE; virtual;
      procedure DocHTML(Entered: boolean); virtual;
      procedure DocHEAD(Entered: boolean); virtual;
      procedure DocMETA; virtual;
      procedure DocTITLE(Entered: boolean); virtual;
      procedure DocBODY(Entered: boolean); virtual;
      procedure DocAnchor(Entered: boolean); virtual;
      procedure DocUnknownTag; virtual;
      procedure DocHeading(Level: integer; Entered: boolean); virtual;
      procedure DocParagraph(Entered: boolean); virtual;
      procedure DocBreak; virtual;
      procedure DocImage; virtual;
      procedure DocProcessComment(Comment: string); virtual;
      procedure DocBold(Entered: boolean); virtual;
      procedure DocCite(Entered: boolean); virtual;
      procedure DocCode(Entered: boolean); virtual;
      procedure DocEmphasized(Entered: boolean); virtual;
      procedure DocItalic(Entered: boolean); virtual;
      procedure DocKbd(Entered: boolean); virtual;
      procedure DocPreformatted(Entered: boolean); virtual;
      procedure DocSample(Entered: boolean); virtual;
      procedure DocStrong(Entered: boolean); virtual;
      procedure DocTeleType(Entered: boolean); virtual;
      procedure DocVariable(Entered: boolean); virtual;
      procedure DocSpan(Entered: boolean); virtual;
      procedure DocList(Entered: boolean); virtual;
      procedure DocOrderedList(Entered: boolean); virtual;
      procedure DocListItem(Entered: boolean); virtual;
      procedure DocDefList(Entered: boolean); virtual;
      procedure DocDefTerm(Entered: boolean); virtual;
      procedure DocDefExp(Entered: boolean); virtual;
      procedure DocTable(Entered: boolean); virtual;
      procedure DocTableRow(Entered: boolean); virtual;
      procedure DocTableHeaderItem(Entered: boolean); virtual;
      procedure DocTableItem(Entered: boolean); virtual;
      procedure DocHorizontalRuler; virtual;
    public
      function  GetSectionColor(Section: THTMLSection; var Color: byte): boolean; virtual;
    private
      URL: string;
      Topic: PTopic;
      TopicLinks: PTopicLinkCollection;
      TextPtr: sw_word;
      InTitle: boolean;
      InBody: boolean;
      InAnchor: boolean;
      InParagraph: boolean;
      InPreformatted: boolean;
      SuppressOutput: boolean;
      SuppressUntil : string;
      InDefExp: boolean;
      TopicTitle: string;
      Indent: integer;
      AnyCharsInLine,
      LastAnsiLoadFailed: boolean;
      CurHeadLevel: integer;
      PAlign: TParagraphAlign;
      LinkIndexes: array[0..MaxTopicLinks] of sw_integer;
      LinkPtr: sw_integer;
      LastTextChar: char;
{      Anchor: TAnchor;}
      { Table stuff }
      CurrentTable : PTable;
      procedure AddText(const S: string);
      procedure AddChar(C: char);
      procedure AddCharAt(C: char;AtPtr : sw_word);
      function AddTextAt(const S: string;AtPtr : sw_word) : sw_word;
      function ComputeTextLength(TStart,TEnd : sw_word) : sw_word;

    end;

    PCustomHTMLHelpFile = ^TCustomHTMLHelpFile;
    TCustomHTMLHelpFile = object(THelpFile)
      constructor Init(AID: word);
      destructor  Done; virtual;
    public
      function    GetTopicInfo(T: PTopic) : string; virtual;
      function    SearchTopic(HelpCtx: THelpCtx): PTopic; virtual;
      function    ReadTopic(T: PTopic): boolean; virtual;
    private
      Renderer: PHTMLTopicRenderer;
      DefaultFileName: string;
      CurFileName: string;
      TopicLinks: PTopicLinkCollection;
    end;

    PHTMLHelpFile = ^THTMLHelpFile;
    THTMLHelpFile = object(TCustomHTMLHelpFile)
      constructor Init(AFileName: string; AID: word; ATOCEntry: string);
    public
      function    LoadIndex: boolean; virtual;
    private
      TOCEntry: string;
    end;

    PHTMLIndexHelpFile = ^THTMLIndexHelpFile;
    THTMLIndexHelpFile = object(TCustomHTMLHelpFile)
      constructor Init(AFileName: string; AID: word);
      function    LoadIndex: boolean; virtual;
    private
      IndexFileName: string;
    end;

    PHTMLAnsiView    = ^THTMLAnsiView;
    PHTMLAnsiConsole = ^THTMLAnsiConsole;

    THTMLAnsiConsole = Object(TAnsiViewConsole)
      MaxX,MaxY : integer;
      procedure   GotoXY(X,Y: integer); virtual;
    end;

    THTMLAnsiView = Object(TAnsiView)
    private
      HTMLOwner : PHTMLTopicRenderer;
      HTMLConsole : PHTMLAnsiConsole;
    public
      constructor Init(AOwner: PHTMLTopicRenderer);
      procedure   CopyToHTML;
    end;

    THTMLGetSectionColorProc = function(Section: THTMLSection; var Color: byte): boolean;

function DefHTMLGetSectionColor(Section: THTMLSection; var Color: byte): boolean;

const HTMLGetSectionColor : THTMLGetSectionColorProc = {$ifdef fpc}@{$endif}DefHTMLGetSectionColor;

procedure RegisterHelpType;

implementation

uses
  Views,WConsts,WUtils,WViews,WHTMLScn;



constructor TTableElement.init(AAlignment : TParagraphAlign);
begin
  Alignment:=AAlignment;
  NextEl:=nil;
  TextBegin:=0;
  TextEnd:=0;
end;


{ TTableLine methods }

constructor TTableLine.Init;
begin
  NumElements:=0;
  NextLine:=nil;
  Firstel:=nil;
  LastEl:=nil;
end;

procedure TTableLine.AddElement(PTE : PTableElement);
begin
  if not assigned(FirstEl) then
    FirstEl:=PTE;
  if assigned(LastEl) then
    LastEl^.NextEl:=PTE;
  LastEl:=PTE;
  Inc(NumElements);
end;

destructor TTableLine.Done;
begin
  LastEl:=FirstEl;
  while assigned(LastEl) do
    begin
      LastEl:=FirstEl^.NextEl;
      Dispose(FirstEl,Done);
      FirstEl:=LastEl;
    end;
  inherited Done;
end;

{ TTable methods }
constructor TTable.Init(Previous : PTable);
begin
  PreviousTable:=Previous;
  NumLines:=0;
  NumCols:=0;
  GlobalOffset:=0;
  GlobalTextBegin:=0;
  FirstLine:=nil;
  LastLine:=nil;

  WithBorder:=false;
  IsBar:=false;
end;

procedure TTable.AddLine(PL : PTableLine);
begin
  If not assigned(FirstLine) then
    FirstLine:=PL;
  if Assigned(LastLine) then
    LastLine^.NextLine:=PL;
  LastLine:=PL;
  Inc(NumLines);
end;

procedure TTable.AddElement(PTE : PTableElement);
begin
  if assigned(LastLine) then
    begin
      LastLine^.AddElement(PTE);
      If LastLine^.NumElements>NumCols then
        NumCols:=LastLine^.NumElements;
    end;
end;

procedure TTable.TextInsert(Pos : sw_word;const S : string);
var
  i : sw_word;
begin
  if S='' then
    exit;
  i:=Renderer^.AddTextAt(S,Pos+GlobalOffset);
  GlobalOffset:=GlobalOffset+i;
end;

procedure TTable.FormatTable;
const
  MaxCols = 200;
type
  TLengthArray = Array [ 1 .. MaxCols] of sw_word;
  PLengthArray = ^TLengthArray;
var
  ColLengthArray : PLengthArray;
  RowSizeArray : PLengthArray;
  CurLine : PTableLine;
  CurEl : PTableElement;
  Align : TParagraphAlign;
  TextBegin,TextEnd : sw_word;
  i,j,k,Length : sw_word;
begin
  { do nothing for single cell tables }
  if (NumCols=1) and (NumLines=1) then
    exit;
  GetMem(ColLengthArray,Sizeof(sw_word)*NumCols);
  FillChar(ColLengthArray^,Sizeof(sw_word)*NumCols,#0);
  GetMem(RowSizeArray,Sizeof(sw_word)*NumLines);
  FillChar(RowSizeArray^,Sizeof(sw_word)*NumLines,#0);
  { Compute the largest cell }
  CurLine:=FirstLine;
  For i:=1 to NumLines do
    begin
      CurEl:=CurLine^.FirstEl;
      RowSizeArray^[i]:=1;
      For j:=1 to NumCols do
        begin
          if not assigned(CurEl) then
            break;
          Length:=CurEl^.TextLength;
          if assigned(CurEl^.NextEl) and
             (CurEl^.NextEl^.TextBegin>CurEl^.TextEnd) then
            Inc(Length,Renderer^.ComputeTextLength(
               CurEl^.NextEl^.TextBegin+GlobalOffset,
               CurEl^.TextBegin+GlobalOffset));

          if Length>ColLengthArray^[j] then
            ColLengthArray^[j]:=Length;
          { We need to handle multiline cells... }
          if CurEl^.NumNL>=RowSizeArray^[i] then
            RowSizeArray^[i]:=CurEl^.NumNL;
          { We don't handle multiline cells yet... }
          if CurEl^.NumNL>=1 then
            begin
              for k:=CurEl^.TextBegin+GlobalOffset to
                     CurEl^.TextEnd+GlobalOffset do
                if Renderer^.Topic^.Text^[k]=ord(hscLineBreak) then
                  Renderer^.Topic^.Text^[k]:=ord(' ');
            end;

          CurEl:=CurEl^.NextEl;
        end;
      CurLine:=CurLine^.NextLine;
    end;
  { Adjust to largest cell }
  CurLine:=FirstLine;
  TextBegin:=GlobalTextBegin;
  If (NumLines>0) and WithBorder then
    Begin
      TextInsert(TextBegin,#218);
      For j:=1 to NumCols do
        begin
          TextInsert(TextBegin,CharStr(#196,ColLengthArray^[j]));
          if j<NumCols then
            TextInsert(TextBegin,#194);
        end;
      TextInsert(TextBegin,#191);
      TextInsert(TextBegin,hscLineBreak);
    End;
  For i:=1 to NumLines do
    begin
      CurEl:=CurLine^.FirstEl;
      For j:=1 to NumCols do
        begin
          if not assigned(CurEl) then
            begin
              Length:=0;
              Align:=paLeft;
            end
          else
            begin
              TextBegin:=CurEl^.TextBegin;
              TextEnd:=CurEl^.TextEnd;
              {While (TextEnd>TextBegin) and
                    (Renderer^.Topic^.Text^[TextEnd+GlobalOffset]=ord(hscLineBreak)) do
                dec(TextEnd); }
              Length:=CurEl^.TextLength;
              Align:=CurEl^.Alignment;
            end;
          if WithBorder then
            TextInsert(TextBegin,#179)
          else
            TextInsert(TextBegin,' ');
          if Length<ColLengthArray^[j] then
            begin
              case Align of
                paLeft:
                  TextInsert(TextEnd,CharStr(' ',ColLengthArray^[j]-Length));
                paRight:
                  TextInsert(TextBegin,CharStr(' ',ColLengthArray^[j]-Length));
                paCenter:
                  begin
                    TextInsert(TextBegin,CharStr(' ',(ColLengthArray^[j]-Length) div 2));
                    TextInsert(TextEnd,CharStr(' ',(ColLengthArray^[j]-Length)- ((ColLengthArray^[j]-Length) div 2)));
                  end;
                end;
            end;
          if Assigned(CurEl) then
            CurEl:=CurEl^.NextEl;
        end;
      if WithBorder then
        TextInsert(TextEnd,#179);
      //TextInsert(TextEnd,hscLineBreak);
      CurLine:=CurLine^.NextLine;
    end;
  If (NumLines>0) and WithBorder then
    Begin
      TextInsert(TextEnd,hscLineBreak);
      TextInsert(TextEnd,#192);
      For j:=1 to NumCols do
        begin
          TextInsert(TextEnd,CharStr(#196,ColLengthArray^[j]));
          if j<NumCols then
            TextInsert(TextEnd,#193);
        end;
      TextInsert(TextEnd,#217);
      TextInsert(TextEnd,hscLineBreak);
    End;

  FreeMem(ColLengthArray,Sizeof(sw_word)*NumCols);
  FreeMem(RowSizeArray,Sizeof(sw_word)*NumLines);
end;

destructor TTable.Done;
begin
  LastLine:=FirstLine;
  while assigned(LastLine) do
    begin
      LastLine:=FirstLine^.NextLine;
      Dispose(FirstLine,Done);
      FirstLine:=LastLine;
    end;
  if Assigned(PreviousTable) then
    Inc(PreviousTable^.GlobalOffset,GlobalOffset);
  inherited Done;
end;


{    THTMLAnsiConsole methods      }

procedure THTMLAnsiConsole.GotoXY(X,Y : integer);
begin
  if X>MaxX then MaxX:=X-1;
  if Y>MaxY then MaxY:=Y-1;
  inherited GotoXY(X,Y);
end;

{    THTMLAnsiView methods      }

constructor THTMLAnsiView.Init(AOwner : PHTMLTopicRenderer);
var
  R : TRect;
begin
  if not assigned(AOwner) then
    fail;
  R.Assign(0,0,80,25);
  inherited init(R,nil,nil);
  HTMLOwner:=AOwner;
  HTMLConsole:=New(PHTMLAnsiConsole,Init(@Self));
  HTMLConsole^.HighVideo;
  Dispose(Console,Done);
  Console:=HTMLConsole;
  HTMLConsole^.Size.X:=80;
  HTMLConsole^.Size.Y:=25;
  HTMLConsole^.ClrScr;
  HTMLConsole^.MaxX:=-1;
  HTMLConsole^.MaxY:=-1;
  HTMLConsole^.BoundChecks:=0;
end;

procedure THTMLAnsiView.CopyToHTML;
var
  Attr,NewAttr : byte;
  c : char;
  X,Y,Pos : longint;
begin
   Attr:=(Buffer^[1] shr 8);
   HTMLOwner^.AddChar(hscLineBreak);
   HTMLOwner^.AddText(hscTextAttr+chr(Attr));
   for Y:=0 to HTMLConsole^.MaxY-1 do
     begin
       for X:=0 to HTMLConsole^.MaxX-1 do
         begin
           Pos:=(Delta.Y*MaxViewWidth)+X+Y*MaxViewWidth;
           NewAttr:=(Buffer^[Pos] shr 8);
           if NewAttr <> Attr then
             begin
               Attr:=NewAttr;
               HTMLOwner^.AddText(hscTextAttr+chr(Attr));
             end;
           c:= chr(Buffer^[Pos] and $ff);
           if ord(c)>16 then
             HTMLOwner^.AddChar(c)
           else
             begin
               HTMLOwner^.AddChar(hscDirect);
               HTMLOwner^.AddChar(c);
             end;
         end;
       { Write start of next line in normal color, for correct alignment }
       HTMLOwner^.AddChar(hscNormText);
       { Force to set attr again at start of next line }
       Attr:=0;
       HTMLOwner^.AddChar(hscLineBreak);
     end;
end;

function DefHTMLGetSectionColor(Section: THTMLSection; var Color: byte): boolean;
begin
  Color:=0;
  DefHTMLGetSectionColor:=false;
end;

function EncodeHTMLCtx(FileID: integer; LinkNo: word): longint;
var Ctx: longint;
begin
  Ctx:=(longint(FileID) shl 16)+LinkNo;
  EncodeHTMLCtx:=Ctx;
end;

procedure DecodeHTMLCtx(Ctx: longint; var FileID: word; var LinkNo: word);
begin
  if (Ctx shr 16)=0 then
    begin
      FileID:=$ffff; LinkNo:=0;
    end
  else
    begin
      FileID:=Ctx shr 16; LinkNo:=Ctx and $ffff;
    end;
end;

function CharStr(C: char; Count: byte): string;
var S: string;
begin
  S[0]:=chr(Count);
  if Count>0 then FillChar(S[1],Count,C);
  CharStr:=S;
end;

procedure TTopicLinkCollection.Insert(Item: Pointer);
begin
  AtInsert(Count,Item);
end;

function TTopicLinkCollection.At(Index: sw_Integer): PString;
begin
  At:=inherited At(Index);
end;

function TTopicLinkCollection.AddItem(Item: string): integer;
var Idx: sw_integer;
begin
  if Item='' then Idx:=-1 else
  if Search(@Item,Idx)=false then
    begin
      AtInsert(Count,NewStr(Item));
      Idx:=Count-1;
    end;
  AddItem:=Idx;
end;

function THTMLTopicRenderer.DocAddTextChar(C: char): boolean;
var Added: boolean;
begin
  Added:=false;
  if InTitle then
    begin
      TopicTitle:=TopicTitle+C;
      Added:=true;
    end
  else
  if InBody then
    begin
      if (InPreFormatted) or (C<>#32) or (LastTextChar<>C) then
      if (C<>#32) or (AnyCharsInLine=true) or (InPreFormatted=true) then
        begin
          AddChar(C);
          LastTextChar:=C;
          Added:=true;
        end;
    end;
  DocAddTextChar:=Added;
end;

procedure THTMLTopicRenderer.DocSoftBreak;
begin
  if InPreformatted then DocBreak else
  if AnyCharsInLine and not assigned(CurrentTable) then
    begin
      AddChar(' ');
      LastTextChar:=' ';
    end;
end;

procedure THTMLTopicRenderer.DocTYPE;
begin
end;

procedure THTMLTopicRenderer.DocHTML(Entered: boolean);
begin
end;

procedure THTMLTopicRenderer.DocHEAD(Entered: boolean);
begin
end;

procedure THTMLTopicRenderer.DocMETA;
begin
end;

procedure THTMLTopicRenderer.DocTITLE(Entered: boolean);
begin
  if Entered then
    begin
      TopicTitle:='';
    end
  else
    begin
      { render topic title here }
      if TopicTitle<>'' then
        begin
          AddText('  '+TopicTitle+' Ü'); DocBreak;
          AddText(' '+CharStr('ß',length(TopicTitle)+3)); DocBreak;
        end;
    end;
  InTitle:=Entered;
end;

procedure THTMLTopicRenderer.DocBODY(Entered: boolean);
begin
  InBody:=Entered;
end;

procedure THTMLTopicRenderer.DocAnchor(Entered: boolean);
var HRef,Name: string;
begin
  if Entered and InAnchor then DocAnchor(false);
  if Entered then
    begin
      if DocGetTagParam('HREF',HRef)=false then HRef:='';
      if DocGetTagParam('NAME',Name)=false then Name:='';
      if {(HRef='') and} (Name='') then
        if DocGetTagParam('ID',Name)=false then
          Name:='';
      if Name<>'' then
        begin
          Topic^.NamedMarks^.InsertStr(Name);
{$ifdef DEBUG}
          DebugMessage('',' Adding Name "'+Name+'"',1,1);
{$endif DEBUG}
          AddChar(hscNamedMark);
        end;
      if (HRef<>'')then
          begin
            if (LinkPtr<MaxTopicLinks){ and
               not DisableCrossIndexing}  then
            begin
              InAnchor:=true;
              AddChar(hscLink);
              if pos('#',HRef)=1 then
                Href:=NameAndExtOf(GetFilename)+Href;
              HRef:=CompleteURL(URL,HRef);
              LinkIndexes[LinkPtr]:=TopicLinks^.AddItem(HRef);
{$ifdef DEBUG}
              DebugMessage('',' Adding Link "'+HRef+'"',1,1);
{$endif DEBUG}
              Inc(LinkPtr);
            end;
          end;
    end
  else
    begin
      if InAnchor=true then AddChar(hscLink);
      InAnchor:=false;
    end;
end;

procedure THTMLTopicRenderer.DocUnknownTag;
begin
{$ifdef DEBUG}
  DebugMessage('',' Unknown tag "'+TagName+'" params "'+
    TagParams+'"',1,1);
{$endif DEBUG}
end;

procedure DecodeAlign(Align: string; var PAlign: TParagraphAlign);
begin
  Align:=UpcaseStr(Align);
  if Align='LEFT' then PAlign:=paLeft else
  if Align='CENTER' then PAlign:=paCenter else
  if Align='RIGHT' then PAlign:=paRight;
end;

procedure THTMLTopicRenderer.DocHeading(Level: integer; Entered: boolean);
var Align: string;
    C: byte;
    SC: THTMLSection;
begin
  if Entered then
    begin
      DocBreak;
      CurHeadLevel:=Level;
      PAlign:=paLeft;
      if DocGetTagParam('ALIGN',Align) then
        DecodeAlign(Align,PAlign);
      SC:=hsNone;
      case Level of
        1: SC:=hsHeading1;
        2: SC:=hsHeading2;
        3: SC:=hsHeading3;
        4: SC:=hsHeading4;
        5: SC:=hsHeading5;
        6: SC:=hsHeading6;
      end;
      if GetSectionColor(SC,C) then
        AddText(hscTextAttr+chr(C));
    end
  else
    begin
      AddChar(hscNormText);
      CurHeadLevel:=0;
      DocBreak;
    end;
end;

procedure THTMLTopicRenderer.DocParagraph(Entered: boolean);
var Align: string;
begin
  if Entered and InParagraph then DocParagraph(false);
  if Entered then
    begin
      if AnyCharsInLine then DocBreak;
      if DocGetTagParam('ALIGN',Align) then
        DecodeAlign(Align,PAlign);
    end
  else
    begin
{      if AnyCharsInLine then }DocBreak;
      PAlign:=paLeft;
    end;
  InParagraph:=Entered;
end;

procedure THTMLTopicRenderer.DocBreak;
begin
  if (CurHeadLevel=1) or (PAlign=paCenter) then
    AddChar(hscCenter);
  if (PAlign=paRight) then
    AddChar(hscRight);
  AddChar(hscLineBreak);
  if Indent>0 then
  AddText(CharStr(#255,Indent)+hscLineStart);
  AnyCharsInLine:=false;
end;

procedure THTMLTopicRenderer.DocProcessComment(Comment: string);
var
  src,index : string;
begin
  if pos('tex4ht:',Comment)=0 then
    exit;
{$ifdef DEBUG}
  DebugMessage(GetFileName,'tex4ht comment "'
        +Comment+'"',Line,1);
{$endif DEBUG}
  if SuppressOutput then
    begin
      if (pos(SuppressUntil,Comment)=0) then
        exit
      else
        begin
{$ifdef DEBUG}
          DebugMessage(GetFileName,' Found '+SuppressUntil+'comment "'
            +Comment+'" SuppressOuput reset to false',Line,1);
{$endif DEBUG}
          SuppressOutput:=false;
          SuppressUntil:='';
        end;
    end;
  if (pos('tex4ht:graphics ',Comment)>0) and
     LastAnsiLoadFailed then
    begin
{$ifdef DEBUG}
      DebugMessage(GetFileName,' Using tex4ht comment "'
        +Comment+'"',Line,1);
{$endif DEBUG}
      { Try again with this info }
      TagParams:=Comment;
      DocImage;
    end;
  if (pos('tex4ht:syntaxdiagram ',Comment)>0) then
    begin
{$ifdef DEBUG}
      DebugMessage(GetFileName,' Using tex4ht:syntaxdiagram comment "'
        +Comment+'"',Line,1);
{$endif DEBUG}
      { Try again with this info }
      TagParams:=Comment;
      DocImage;
      if not LastAnsiLoadFailed then
        begin
          SuppressOutput:=true;
          SuppressUntil:='tex4ht:endsyntaxdiagram ';
        end
    end;
  if (pos('tex4ht:mysyntdiag ',Comment)>0) then
    begin
{$ifdef DEBUG}
      DebugMessage(GetFileName,' Using tex4ht:mysyntdiag comment "'
        +Comment+'"',Line,1);
{$endif DEBUG}
      { Try again with this info }
      TagParams:=Comment;
      DocGetTagParam('SRC',src);
      DocGetTagParam('INDEX',index);
      TagParams:='src="../syntax/'+src+'-'+index+'.png"';
      DocImage;
      if not LastAnsiLoadFailed then
        begin
          SuppressOutput:=true;
          SuppressUntil:='tex4ht:endmysyntdiag ';
        end
    end;
end;

procedure THTMLTopicRenderer.DocImage;
var Name,Src,Alt,SrcLine: string;
    f : text;
    attr : byte;
    PA : PHTMLAnsiView;
    StorePreformatted : boolean;
begin
  if SuppressOutput then
    exit;
{$ifdef DEBUG}
  if not DocGetTagParam('NAME',Name) then
     Name:='<No name>';
  DebugMessage(GetFileName,' Image "'+Name+'"',Line,1);
{$endif DEBUG}
  if DocGetTagParam('SRC',src) then
    begin
{$ifdef DEBUG}
      DebugMessage(GetFileName,' Image source tag "'+Src+'"',Line,1);
{$endif DEBUG}
      if src<>'' then
        begin
          src:=CompleteURL(URL,src);
          { this should be a image file ending by .gif or .jpg...
            Try to see if a file with same name and extension .git
            exists PM }
          src:=DirAndNameOf(src)+'.ans';
{$ifdef DEBUG}
          DebugMessage(GetFileName,' Trying "'+Src+'"',Line,1);
{$endif DEBUG}
          if not ExistsFile(src) then
            begin
              DocGetTagParam('SRC',src);
              src:=DirAndNameOf(src)+'.ans';
              src:=CompleteURL(DirOf(URL)+'../',src);
{$ifdef DEBUG}
              DebugMessage(GetFileName,' Trying "'+Src+'"',Line,1);
{$endif DEBUG}
            end;
          if not ExistsFile(src) then
            begin
              LastAnsiLoadFailed:=true;
{$ifdef DEBUG}
              DebugMessage(GetFileName,' "'+Src+'" not found',Line,1);
{$endif DEBUG}
            end
          else
            begin
              PA:=New(PHTMLAnsiView,init(@self));
              PA^.LoadFile(src);
              LastAnsiLoadFailed:=false;
              if AnyCharsInLine then DocBreak;
              StorePreformatted:=InPreformatted;
              InPreformatted:=true;
              {AddText('Image from '+src+hscLineBreak); }
              AddChar(hscInImage);
              PA^.CopyToHTML;
              InPreformatted:=StorePreformatted;
              AddChar(hscInImage);
              AddChar(hscNormText);
              if AnyCharsInLine then DocBreak;
              Dispose(PA,Done);
              Exit;
            end;
          { also look for a raw text file without colors }
          src:=DirAndNameOf(src)+'.txt';
          if not ExistsFile(src) then
            begin
              LastAnsiLoadFailed:=true;
{$ifdef DEBUG}
              DebugMessage(GetFileName,' "'+Src+'" not found',Line,1);
{$endif DEBUG}
            end
          else
            begin
              Assign(f,src);
              Reset(f);
              DocPreformatted(true);
              while not eof(f) do
                begin
                  Readln(f,SrcLine);
                  AddText(SrcLine+hscLineBreak);
                end;
              Close(f);
              LastAnsiLoadFailed:=false;
              DocPreformatted(false);
              LastAnsiLoadFailed:=false;
              Exit;
            end;
        end;
    end;
  if DocGetTagParam('ALT',Alt)=false then
    begin
      DocGetTagParam('SRC',Alt);
      if Alt<>'' then
        Alt:='Can''t display '+Alt
      else
        Alt:='IMG';
    end;
  if Alt<>'' then
    begin
      StorePreformatted:=InPreformatted;
      InPreformatted:=true;
      DocGetTagParam('SRC',src);
      AddChar(hscInImage);
      AddText('[--'+Src+'--'+hscLineBreak);
      AddText(Alt+hscLineBreak+'--]');
      AddChar(hscInImage);
      AddChar(hscNormText);
      InPreformatted:=StorePreformatted;
    end;
end;

procedure THTMLTopicRenderer.DocBold(Entered: boolean);
begin
end;

procedure THTMLTopicRenderer.DocCite(Entered: boolean);
begin
end;

procedure THTMLTopicRenderer.DocCode(Entered: boolean);
begin
  if AnyCharsInLine then DocBreak;
  AddText(hscCode);
  DocBreak;
end;

procedure THTMLTopicRenderer.DocEmphasized(Entered: boolean);
begin
end;

procedure THTMLTopicRenderer.DocItalic(Entered: boolean);
begin
end;

procedure THTMLTopicRenderer.DocKbd(Entered: boolean);
begin
end;

procedure THTMLTopicRenderer.DocPreformatted(Entered: boolean);
begin
  if AnyCharsInLine then DocBreak;
  AddText(hscCode);
  DocBreak;
  InPreformatted:=Entered;
end;

procedure THTMLTopicRenderer.DocSample(Entered: boolean);
begin
end;

procedure THTMLTopicRenderer.DocStrong(Entered: boolean);
begin
end;

procedure THTMLTopicRenderer.DocTeleType(Entered: boolean);
begin
end;

procedure THTMLTopicRenderer.DocVariable(Entered: boolean);
begin
end;

procedure THTMLTopicRenderer.DocSpan(Entered: boolean);
begin
end;

procedure THTMLTopicRenderer.DocList(Entered: boolean);
begin
  if Entered then
    begin
      Inc(Indent,ListIndent);
      DocBreak;
    end
  else
    begin
      Dec(Indent,ListIndent);
      if AnyCharsInLine then DocBreak;
    end;
end;

procedure THTMLTopicRenderer.DocOrderedList(Entered: boolean);
begin
  DocList(Entered);
end;

procedure THTMLTopicRenderer.DocListItem(Entered: boolean);
begin
  if not Entered then
    exit;
  if AnyCharsInLine then
    DocBreak;
  AddText('þ'+hscLineStart);
end;

procedure THTMLTopicRenderer.DocDefList(Entered: boolean);
begin
  if Entered then
    begin
{      if LastChar<>hscLineBreak then DocBreak;}
    end
  else
    begin
      if AnyCharsInLine then DocBreak;
      InDefExp:=false;
    end;
end;

procedure THTMLTopicRenderer.DocDefTerm(Entered: boolean);
begin
  if not Entered then
    exit;
  DocBreak;
end;

procedure THTMLTopicRenderer.DocDefExp(Entered: boolean);
begin
  if not Entered then
    begin
      if InDefExp then
        Dec(Indent,DefIndent);
      InDefExp:=false;
    end
  else
    begin
      if not InDefExp then
        Inc(Indent,DefIndent);
      InDefExp:=true;
      DocBreak;
    end;
end;

procedure THTMLTopicRenderer.DocTable(Entered: boolean);
var
  ATable : PTable;
  Param : String;
begin
  if AnyCharsInLine then
    begin
      AddChar(hscLineBreak);
      AnyCharsInLine:=false;
    end;
  if Entered then
    begin
      DocBreak;
      New(ATable,Init(CurrentTable));
      CurrentTable:=ATable;
      CurrentTable^.Renderer:=@Self;
      if DocGetTagParam('BORDER',Param) then
        if Param<>'0' then
          CurrentTable^.WithBorder:=true;
      if DocGetTagParam('CLASS',Param) then
        if Param='bar' then
          CurrentTable^.IsBar:=true;
    end
  else
    begin
      CurrentTable^.FormatTable;
      ATable:=CurrentTable;
      CurrentTable:=ATable^.PreviousTable;
      Dispose(ATable,Done);
    end;
end;

procedure THTMLTopicRenderer.DocTableRow(Entered: boolean);
var
  ATableLine : PTableLine;
begin
  if AnyCharsInLine or
     (assigned(CurrentTable) and
      assigned(CurrentTable^.FirstLine)) then
    begin
      AddChar(hscLineBreak);
      AnyCharsInLine:=false;
    end;
  if Entered then
    begin
      New(ATableLine,Init);
      if CurrentTable^.GlobalTextBegin=0 then
      CurrentTable^.GlobalTextBegin:=TextPtr;
      CurrentTable^.AddLine(ATableLine);
    end;
end;

procedure THTMLTopicRenderer.DocTableItem(Entered: boolean);
var
  Align : String;
  i : sw_word;
  NewEl : PTableElement;
  PAlignEl : TParagraphAlign;
begin
  if Entered then
    begin
      if assigned(CurrentTable^.LastLine) and Assigned(CurrentTable^.LastLine^.LastEl) and
         (CurrentTable^.LastLine^.LastEl^.TextEnd=sw_word(-1)) then
        begin
          NewEl:=CurrentTable^.LastLine^.LastEl;
          NewEl^.TextEnd:=TextPtr;
          NewEl^.TextLength:=ComputeTextLength(
            NewEl^.TextBegin+CurrentTable^.GlobalOffset,
            TextPtr+CurrentTable^.GlobalOffset);
        end;
      PAlignEl:=paLeft;
      if DocGetTagParam('ALIGN',Align) then
        DecodeAlign(Align,PAlignEl);
      New(NewEl,Init(PAlignEl));
      CurrentTable^.AddElement(NewEl);
      NewEl^.TextBegin:=TextPtr;
      NewEl^.TextEnd:=sw_word(-1);
      { AddText(' - ');}
    end
  else
    begin
      NewEl:=CurrentTable^.LastLine^.LastEl;
      NewEl^.TextEnd:=TextPtr;
      NewEl^.TextLength:=ComputeTextLength(
        NewEl^.TextBegin+CurrentTable^.GlobalOffset,
        TextPtr+CurrentTable^.GlobalOffset);
      NewEl^.NumNL:=0;
      for i:=NewEl^.TextBegin to TextPtr do
        begin
          if Topic^.Text^[i]=ord(hscLineBreak) then
            inc(NewEl^.NumNL);
        end;
    end;
end;

procedure THTMLTopicRenderer.DocTableHeaderItem(Entered: boolean);
begin
  { Treat as a normal item }
  DocTableItem(Entered);
end;


procedure THTMLTopicRenderer.DocHorizontalRuler;
var OAlign: TParagraphAlign;
begin
  OAlign:=PAlign;
  if AnyCharsInLine then DocBreak;
  PAlign:=paCenter;
  DocAddText(' '+CharStr('Ä',60)+' ');
  DocBreak;
  PAlign:=OAlign;
end;

procedure THTMLTopicRenderer.AddChar(C: char);
begin
  if (Topic=nil) or (TextPtr=MaxBytes) or SuppressOutput then Exit;
  Topic^.Text^[TextPtr]:=ord(C);
  Inc(TextPtr);
  if (C>#15) and ((C<>' ') or (InPreFormatted=true)) then
    AnyCharsInLine:=true;
end;

procedure THTMLTopicRenderer.AddCharAt(C: char;AtPtr : sw_word);
begin
  if (Topic=nil) or (TextPtr=MaxBytes) or SuppressOutput then Exit;
  if AtPtr>TextPtr then
    AtPtr:=TextPtr
  else
    begin
      Move(Topic^.Text^[AtPtr],Topic^.Text^[AtPtr+1],TextPtr-AtPtr);
    end;
  Topic^.Text^[AtPtr]:=ord(C);
  Inc(TextPtr);
end;

procedure THTMLTopicRenderer.AddText(const S: string);
var I: sw_integer;
begin
  for I:=1 to length(S) do
    AddChar(S[I]);
end;

function THTMLTopicRenderer.ComputeTextLength(TStart,TEnd : sw_word) : sw_word;
var I,tot: sw_integer;
begin
  tot:=0;
  i:=TStart;
  while i<= TEnd-1 do
    begin
      inc(tot);
      case chr(Topic^.Text^[i]) of
      hscLink,hscCode,
      hscCenter,hscRight,
      hscNamedMark,hscNormText :
        Dec(tot);{ Do not increase tot }
      hscDirect:
        begin
          Inc(i); { Skip next }
          //Inc(tot);
        end;
      hscTextAttr,
      hscTextColor:
        begin
          Inc(i);
          Dec(tot);
        end;
      end;
      inc(i);
    end;
  ComputeTextLength:=tot;

end;
function THTMLTopicRenderer.AddTextAt(const S: String;AtPtr : sw_word) : sw_word;
var
  i,slen,len : sw_word;
begin
  if (Topic=nil) or (TextPtr>=MaxBytes)  or SuppressOutput then Exit;
  slen:=length(s);
  if TextPtr+slen>=MaxBytes then
    slen:=MaxBytes-TextPtr;
  if AtPtr>TextPtr then
    AtPtr:=TextPtr
  else
    begin
      len:=TextPtr-AtPtr;
      Move(Topic^.Text^[AtPtr],Topic^.Text^[AtPtr+slen],len);
    end;
  for i:=1 to slen do
    begin
      Topic^.Text^[AtPtr]:=ord(S[i]);
      Inc(TextPtr);
      inc(AtPtr);
      if (TextPtr=MaxBytes) then Exit;
    end;
  AddTextAt:=slen;
end;

function THTMLTopicRenderer.GetSectionColor(Section: THTMLSection; var Color: byte): boolean;
begin
  GetSectionColor:=HTMLGetSectionColor(Section,Color);
end;

function THTMLTopicRenderer.BuildTopic(P: PTopic; AURL: string; HTMLFile: PTextFile;
           ATopicLinks: PTopicLinkCollection): boolean;
var OK: boolean;
    TP: pointer;
    I: sw_integer;
begin
  URL:=AURL;
  Topic:=P; TopicLinks:=ATopicLinks;
  OK:=Assigned(Topic) and Assigned(HTMLFile) and Assigned(TopicLinks);
  if OK then
    begin
      if (Topic^.TextSize<>0) and Assigned(Topic^.Text) then
        begin
          FreeMem(Topic^.Text,Topic^.TextSize);
          Topic^.TextSize:=0; Topic^.Text:=nil;
        end;
      Topic^.TextSize:=MaxHelpTopicSize;
      GetMem(Topic^.Text,Topic^.TextSize);

      TopicTitle:='';
      InTitle:=false; InBody:={false}true; InAnchor:=false;
      InParagraph:=false; InPreformatted:=false;
      Indent:=0; CurHeadLevel:=0;
      PAlign:=paLeft;
      TextPtr:=0; LinkPtr:=0;
      AnyCharsInLine:=false;
      LastTextChar:=#0;
      SuppressUntil:='';
      SuppressOutput:=false;
      OK:=Process(HTMLFile);

      if OK then
        begin
          { --- topic links --- }
          if (Topic^.Links<>nil) and (Topic^.LinkSize>0) then
            begin
              FreeMem(Topic^.Links,Topic^.LinkSize);
              Topic^.Links:=nil; Topic^.LinkCount:=0;
            end;
          Topic^.LinkCount:=LinkPtr{TopicLinks^.Count}; { <- eeeeeek! }
          GetMem(Topic^.Links,Topic^.LinkSize);
          if Topic^.LinkCount>0 then { FP causes numeric RTE 215 without this }
          for I:=0 to Min(Topic^.LinkCount-1,High(LinkIndexes)-1) do
            begin
              Topic^.Links^[I].FileID:=Topic^.FileID;
              Topic^.Links^[I].Context:=EncodeHTMLCtx(Topic^.FileID,LinkIndexes[I]+1);
            end;
          { --- topic text --- }
          GetMem(TP,TextPtr);
          Move(Topic^.Text^,TP^,TextPtr);
          FreeMem(Topic^.Text,Topic^.TextSize);
          Topic^.Text:=TP; Topic^.TextSize:=TextPtr;
        end
      else
        begin
          DisposeTopic(Topic);
          Topic:=nil;
        end;
    end;
  BuildTopic:=OK;
end;

constructor TCustomHTMLHelpFile.Init(AID: word);
begin
  inherited Init(AID);
  New(Renderer, Init);
  New(TopicLinks, Init(50,500));
end;

function TCustomHTMLHelpFile.SearchTopic(HelpCtx: THelpCtx): PTopic;
function MatchCtx(P: PTopic): boolean;
begin
  MatchCtx:=P^.HelpCtx=HelpCtx;
end;
var FileID,LinkNo: word;
    P: PTopic;
    FName: string;
begin
  DecodeHTMLCtx(HelpCtx,FileID,LinkNo);
  if (HelpCtx<>0) and (FileID<>ID) then P:=nil else
  if (FileID=ID) and (LinkNo>TopicLinks^.Count) then P:=nil else
    begin
      P:=Topics^.FirstThat(@MatchCtx);
      if P=nil then
        begin
          if LinkNo=0 then
            FName:=DefaultFileName
          else
            FName:=TopicLinks^.At(LinkNo-1)^;
          P:=NewTopic(ID,HelpCtx,0,FName,nil,0);
          Topics^.Insert(P);
        end;
    end;
  SearchTopic:=P;
end;

function TCustomHTMLHelpFile.GetTopicInfo(T: PTopic) : string;
var OK: boolean;
    Name: string;
    Link,Bookmark: string;
    P: sw_integer;
begin
  Bookmark:='';
  OK:=T<>nil;
  if OK then
    begin
      if T^.HelpCtx=0 then
        begin
          Name:=DefaultFileName;
          P:=0;
        end
      else
        begin
          Link:=TopicLinks^.At((T^.HelpCtx and $ffff)-1)^;
          Link:=FormatPath(Link);
{$ifdef DEBUG_WHTMLHLP}
          DebugMessage(Link,' looking for',1,1);
{$endif DEBUG_WHTMLHLP}
          P:=Pos('#',Link);
          if P>0 then
          begin
            Bookmark:=copy(Link,P+1,length(Link));
            Link:=copy(Link,1,P-1);
          end;
{          if CurFileName='' then Name:=Link else
          Name:=CompletePath(CurFileName,Link);}
          Name:=Link;
        end;
    end;
  GetTopicInfo:=Name+'#'+BookMark;
end;

function TCustomHTMLHelpFile.ReadTopic(T: PTopic): boolean;
var OK: boolean;
    HTMLFile: PMemoryTextFile;
    Name: string;
    Link,Bookmark: string;
    P: sw_integer;
begin
  Bookmark:='';
  OK:=T<>nil;
  if OK then
    begin
      if T^.HelpCtx=0 then
        begin
          Name:=DefaultFileName;
          P:=0;
        end
      else
        begin
          Link:=TopicLinks^.At((T^.HelpCtx and $ffff)-1)^;
          Link:=FormatPath(Link);
{$ifdef DEBUG}
          DebugMessage(Link,' looking for',1,1);
{$endif DEBUG}
          P:=Pos('#',Link);
          if P>0 then
          begin
            Bookmark:=copy(Link,P+1,length(Link));
            Link:=copy(Link,1,P-1);
          end;
{          if CurFileName='' then Name:=Link else
          Name:=CompletePath(CurFileName,Link);}
          Name:=Link;
        end;
      HTMLFile:=nil;
      if Name<>'' then
        HTMLFile:=New(PDOSTextFile, Init(Name));

      if (HTMLFile=nil) and (CurFileName<>'') then
        begin
          Name:=CurFileName;
          HTMLFile:=New(PDOSTextFile, Init(Name));
        end;
      if (HTMLFile=nil) then
        begin
{$ifdef DEBUG}
          DebugMessage(Link,' filename not known :(',1,1);
{$endif DEBUG}
        end;
      if (p>1) and (HTMLFile=nil) then
        begin
{$ifdef DEBUG}
          if p>0 then
            DebugMessage(Name,Link+'#'+Bookmark+' not found',1,1)
          else
            DebugMessage(Name,Link+' not found',1,1);
{$endif DEBUG}
          New(HTMLFile, Init);
          HTMLFile^.AddLine('<HEAD><TITLE>'+msg_pagenotavailable+'</TITLE></HEAD>');
          HTMLFile^.AddLine(
            '<BODY>'+
            FormatStrStr(msg_cantaccessurl,Name)+'<br><br>'+
            '</BODY>');
        end;
      OK:=Renderer^.BuildTopic(T,Name,HTMLFile,TopicLinks);
      if OK then
        CurFileName:=Name
      else
        begin
{$ifdef DEBUG}
          if p>0 then
            DebugMessage(Name,Link+'#'+Bookmark+' not found',1,1)
          else
            DebugMessage(Name,Link+' not found',1,1);
{$endif DEBUG}
        end;
      if HTMLFile<>nil then Dispose(HTMLFile, Done);
      if BookMark='' then
        T^.StartNamedMark:=0
      else
        begin
          P:=T^.GetNamedMarkIndex(BookMark);
{$ifdef DEBUG}
          if p=-1 then
            DebugMessage(Name,Link+'#'+Bookmark+' bookmark not found',1,1);
{$endif DEBUG}
          T^.StartNamedMark:=P+1;
        end;
    end;
  ReadTopic:=OK;
end;

destructor TCustomHTMLHelpFile.Done;
begin
  inherited Done;
  if Renderer<>nil then Dispose(Renderer, Done);
  if TopicLinks<>nil then Dispose(TopicLinks, Done);
end;

constructor THTMLHelpFile.Init(AFileName: string; AID: word; ATOCEntry: string);
begin
  if inherited Init(AID)=false then Fail;
  DefaultFileName:=AFileName; TOCEntry:=ATOCEntry;
  if DefaultFileName='' then
  begin
    Done;
    Fail;
  end;
end;

function THTMLHelpFile.LoadIndex: boolean;
begin
  IndexEntries^.Insert(NewIndexEntry(TOCEntry,ID,0));
  LoadIndex:=true;
end;

constructor THTMLIndexHelpFile.Init(AFileName: string; AID: word);
begin
  inherited Init(AID);
  IndexFileName:=AFileName;
end;

function THTMLIndexHelpFile.LoadIndex: boolean;
function FormatAlias(Alias: string): string;
begin
  if Assigned(HelpFacility) then
    if length(Alias)>HelpFacility^.IndexTabSize-4 then
      Alias:=Trim(copy(Alias,1,HelpFacility^.IndexTabSize-4-2))+'..';
  FormatAlias:=Alias;
end;
(*procedure AddDoc(P: PHTMLLinkScanDocument);
var I: sw_integer;
    TLI: THelpCtx;
begin
  for I:=1 to P^.GetAliasCount do
  begin
    TLI:=TopicLinks^.AddItem(P^.GetName);
    TLI:=EncodeHTMLCtx(ID,TLI+1);
    IndexEntries^.Insert(NewIndexEntry(FormatAlias(P^.GetAlias(I-1)),ID,TLI));
  end;
end;*)
var S: PBufStream;
    LS: PHTMLLinkScanner;
    OK: boolean;
    TLI: THelpCtx;
    I,J: sw_integer;
begin
  New(S, Init(IndexFileName,stOpenRead,4096));
  OK:=Assigned(S);
  if OK then
  begin
    New(LS, LoadDocuments(S^));
    OK:=Assigned(LS);
    if OK then
    begin
      LS^.SetBaseDir(DirOf(IndexFileName));
      for I:=0 to LS^.GetDocumentCount-1 do
        begin
          TLI:=TopicLinks^.AddItem(LS^.GetDocumentURL(I));
          TLI:=EncodeHTMLCtx(ID,TLI+1);
          for J:=0 to LS^.GetDocumentAliasCount(I)-1 do
            IndexEntries^.Insert(NewIndexEntry(
              FormatAlias(LS^.GetDocumentAlias(I,J)),ID,TLI));
        end;
      Dispose(LS, Done);
    end;
    Dispose(S, Done);
  end;
  LoadIndex:=OK;
end;

function CreateProcHTML(const FileName,Param: string;Index : longint): PHelpFile;
var H: PHelpFile;
begin
  H:=nil;
  if CompareText(copy(ExtOf(FileName),1,length(extHTML)),extHTML)=0 then
    H:=New(PHTMLHelpFile, Init(FileName,Index,Param));
  CreateProcHTML:=H;
end;

function CreateProcHTMLIndex(const FileName,Param: string;Index : longint): PHelpFile;
var H: PHelpFile;
begin
  H:=nil;
  if CompareText(ExtOf(FileName),extHTMLIndex)=0 then
    H:=New(PHTMLIndexHelpFile, Init(FileName,Index));
  CreateProcHTMLIndex:=H;
end;

procedure RegisterHelpType;
begin
  RegisterHelpFileType({$ifdef FPC}@{$endif}CreateProcHTML);
  RegisterHelpFileType({$ifdef FPC}@{$endif}CreateProcHTMLIndex);
end;


END.
