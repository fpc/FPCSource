Unit RTFPars;
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt, Member of the
    Free Pascal development team

    This unit implements a RTF Parser.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}

interface

Uses classes,sysutils;

{$i rtfdata.inc}

type Trtferrorhandler = Procedure (s : string) of object;

{ TRTFParser }

TRTFParser = class(TObject)
  private
    FOnRTFError : TRTFerrorHandler;
    FfontList : PRTFFont;
    FcolorList : PRTFColor;
    FstyleList : PRTFStyle;
    FrtfClass,
    FrtfMajor,
    FrtfMinor,
    FrtfParam : Integer;
    rtfTextBuf : string [rtfBufSiz];
    rtfTextLen : Integer;
    pushedChar : Integer;               { pushback char if read too far }
    pushedClass : Integer;      { pushed token info for RTFUngetToken() }
    pushedMajor,
    pushedMinor,
    pushedParam : Integer;
    pushedTextBuf : String[rtfBufSiz];
    FStream : TStream;
    ccb : array [0..rtfMaxClass] of TRTFFuncPtr;                { class callbacks }
    dcb : array [0..rtfMaxDestination] of TRTFFuncPtr;  { destination callbacks }
    readHook : TRTFFUNCPTR;
    FTokenClass: Integer;
    Procedure Error (msg : String);
    Procedure LookupInit ;
    Procedure ReadFontTbl ;
    Procedure ReadColorTbl;
    Procedure ReadStyleSheet ;
    Procedure ReadInfoGroup ;
    Procedure ReadPictGroup ;
    Function  CheckCM (Aclass, major: Integer) : Boolean;
    Function  CheckCMM (Aclass, major, minor : Integer) : Boolean;
    Function  CheckMM (major, minor : Integer) : Boolean;
    Procedure Real_RTFGetToken;
    Function  GetChar : Integer;
    Procedure Lookup (S : String);
    Function  GetFont (num : Integer) : PRTFFont;
    Function  GetColor (num : Integer) : PRTFColor;
    Function  GetStyle (num : Integer) : PRTFStyle;
    Procedure setClassCallback (Aclass : Integer; Acallback : TRTFFuncPtr);
    Function  GetClassCallback (Aclass : Integer) : TRTFFuncPtr;
    Procedure SetDestinationCallback (ADestination : Integer; Acallback : TRTFFuncPtr);
    Function  GetDestinationCallback (Adestination : Integer) : TRTFFuncPtr ;
    Procedure SetStream (Astream : TStream);
  public
    Constructor Create (AStream : TStream);
    Destructor  Destroy; override;
    Procedure GetReadHook (Var q : TRTFFuncPtr);
    Function  GetToken : Integer;
    Function  PeekToken : Integer;
    Procedure ResetParser;
    Procedure RouteToken;
    Procedure SkipGroup;
    Procedure StartReading;
    Procedure SetReadHook (Hook : TRTFFuncPtr);
    Procedure UngetToken;
    Procedure SetToken (Aclass, major, minor, param : Integer; text : string);
    Procedure ExpandStyle (n : Integer);
    Function GetRtfBuf : String;
    { Properties }
    Property Colors [Index : Integer]: PRTFColor Read GetColor;
    Property ClassCallBacks [AClass : Integer]: TRTFFuncptr
       Read GetClassCallBack
       Write SetClassCallback;
    Property DestinationCallBacks [Adestination : Integer]: TRTFFuncptr
       Read GetdestinationCallBack
       Write SetdestinationCallback;
    Property Fonts [Index : Integer]: PRTFFont Read GetFont;
    Property OnRTFError : TRTFerrorHandler Read FOnRTFError Write FOnRTFError;
    Property rtfClass : Integer Read FrtfClass;
    Property rtfMajor : Integer Read FrtfMajor;
    Property rtfMinor : Integer Read FrtfMinor;
    Property rtfParam : Integer Read FrtfParam;
    Property Stream : TStream Read FStream Write SetStream;
    Property Styles [index : Integer] : PRTFStyle Read GetStyle;
  end;

Implementation

Const EOF = -255;

{ ---------------------------------------------------------------------
         Utility functions
  ---------------------------------------------------------------------}

Function Hash (s : String) : Integer;

var
  val,i : integer;

Begin
val:=0;
for i:=1 to length(s) do
  val:=val+ord(s[i]);
Hash:=val;
End;

Function isalpha (s : integer) : Boolean;

begin
  result:= ( (s>=ord('A')) and (s<=ord('Z')))
             or (((s>=ord('a')) and ((s<=ord('z')) ))
            );
end;

Function isdigit (s : integer) : Boolean;

begin
  result:= ( (s>=ord('0')) and (s<=ord('9')) )
end;

Function HexVal (c : Integer) : Integer;

Begin
  if (c>=ord('A')) and (C<=ord('Z')) then inc (c,32);
  if c<ord ('A') then
    result:=(c - ord('0'))      { '0'..'9' }
  else
    result:= (c - ord('a') + 10);               { 'a'..'f' }
End;

{ ---------------------------------------------------------------------
       Initialize the reader.  This may be called multiple times,
       to read multiple files.  The only thing not reset is the input
       stream; that must be done with RTFSetStream().
  ---------------------------------------------------------------------}

Constructor TRTFParser.Create (Astream : TStream);

Begin
inherited create;
{ initialize lookup table }
LookupInit ;
Fstream := Astream;
FfontList  :=nil;
FcolorList :=nil;
FstyleList :=nil;
onrtferror:=nil;
ResetParser;
end;

Procedure TRTFParser.ResetParser;

var
  cp : PRTFColor;
  fp : PRTFFont;
  sp : PRTFStyle;
  ep,eltlist : PRTFStyleElt;
  i : integer;

begin

for i:=0 to rtfMaxClass-1 do
  setClassCallback (i, Nil);
for i:=0 to rtfMaxDestination-1 do
  SetDestinationCallback (i,nil);

{ install built-in destination readers }
SetDestinationCallback (rtfFontTbl, @ReadFontTbl);
SetDestinationCallback (rtfColorTbl, @ReadColorTbl);
SetDestinationCallback (rtfStyleSheet, @ReadStyleSheet);
SetDestinationCallback (rtfInfo, @ReadInfoGroup);
SetDestinationCallback (rtfPict, @ReadPictGroup);

SetReadHook (Nil);

{ dump old lists if necessary }

while FfontList<>nil do
  Begin
  fp := FfontList^.rtfNextFont;
  dispose (FfontList);
  FfontList := fp;
  End;
while FcolorList<>nil do
  Begin
  cp := FcolorList^.rtfNextColor;
  dispose (FcolorList);
  FcolorList := cp;
  End;
while FstyleList<>nil do
  Begin
  sp := FstyleList^.rtfNextStyle;
  eltList := FstyleList^.rtfSSEList;
  while eltList<>nil do
    Begin
    ep:=eltList^.rtfNextSE;
    dispose(eltList);
    eltList:= ep;
    End;
  Dispose (FstyleList);
  FstyleList := sp;
  End;
FrtfClass := -1;
pushedClass := -1;
pushedChar := EOF;
{ Reset the stream if it is assigned }
if assigned (FStream) then
  FStream.seek(0,soFromBeginning);
End;


Destructor TRTFParser.Destroy;

var
  cp : PRTFColor;
  fp : PRTFFont;
  sp : PRTFStyle;
  ep,eltlist : PRTFStyleElt;

begin
  { Dump the lists. }
  while FfontList<>nil do
    Begin
    fp := FfontList^.rtfNextFont;
    dispose (FfontList);
    FfontList := fp;
    End;
  while FcolorList<>nil do
    Begin
  cp := FcolorList^.rtfNextColor;
  dispose (FcolorList);
    FcolorList := cp;
    End;
  while FstyleList<>nil do
    Begin
    sp := FstyleList^.rtfNextStyle;
    eltList := FstyleList^.rtfSSEList;
    while eltList<>nil do
      Begin
      ep:=eltList^.rtfNextSE;
      dispose(eltList);
      eltList:= ep;
      End;
    Dispose (FstyleList);
    FstyleList := sp;
    End;
  { Dump rest }
  inherited destroy;
end;


{ ---------------------------------------------------------------------
       Callback table manipulation routines
  ---------------------------------------------------------------------}

Procedure TRTFParser.SetClassCallback (Aclass : Integer; Acallback : TRTFFuncPtr);

Begin
  if (aclass>=0) and (Aclass<rtfMaxClass) then
    ccb[Aclass]:= Acallback;
End;


Function TRTFParser.GetClassCallback (Aclass : Integer) : TRTFFuncPtr;

Begin
  if (Aclass>=0) and (Aclass<rtfMaxClass) then
    GetClassCallback :=ccb[Aclass]
  else
    GetClassCallback:=nil;
End;

{ ---------------------------------------------------------------------
   Install or return a writer callback for a destination type
  ---------------------------------------------------------------------}

Procedure TRTFParser.SetDestinationCallback (ADestination : Integer; Acallback : TRTFFuncPtr);

Begin
  if (Adestination>=0) and (Adestination<rtfMaxDestination) then
    dcb[ADestination] := Acallback;
End;


Function TRTFParser.GetDestinationCallback (Adestination : Integer) : TRTFFuncPtr ;

Begin
  if (Adestination>=0) and (ADestination<rtfMaxDestination) then
    Result:=dcb[Adestination]
  Else
    Result:=nil;
End;


{ ---------------------------------------------------------------------
       Token reading routines
  ---------------------------------------------------------------------}

{ Read the input stream, invoking the writer's callbacks where appropriate. }
Procedure TRTFParser.StartReading;

Begin
  { Reset stream. }
  FStream.Seek (0,soFromBeginning);
  { Start reading. }
  while (GetToken<>rtfEOF) do
    RouteToken;
End;


{ Route a token.  If it's a destination for which a reader is
  installed, process the destination internally, otherwise
  pass the token to the writer's class callback. }
Procedure TRTFParser.RouteToken;

Var
  p : TRTFFuncPtr;

Begin
  if (rtfClass < 0) or (rtfClass>=rtfMaxClass) then
    Error ('No such class : '+rtfTextBuf)
  else
    begin
    if (CheckCM (rtfControl, rtfDestination)) then
      Begin
      { invoke destination-specific callback if there is one }
      p:=GetDestinationCallback (rtfMinor);
      if assigned(p) then
        Begin
        p;
        exit
        End;
      End;
    { invoke class callback if there is one }
    p:= GetClassCallback (rtfClass);
    if assigned(p) then
      p;
    end;
End;


{ Skip to the end of the current group.  When this returns,
  writers that maintain a state stack may want to call their
  state unstacker; global vars will still be set to the group's
  closing brace. }
Procedure TRTFParser.SkipGroup;

Var
  level : Integer;
Begin
  level:= 1;
  while (GetToken<>rtfEOF) do
    if (rtfClass=rtfGroup) then
       Begin
       if (rtfMajor=rtfBeginGroup) then
         inc(level)
       else if (rtfMajor=rtfEndGroup) then
          Begin
          dec(level);
          if (level < 1) then
            exit;       { end of initial group }
          End;
       End;
End;

{ Read one token.  Call the read hook if there is one.  The
  token class is the return value.  Returns rtfEOF when there
  are no more tokens. }
Function TRTFParser.GetToken : Integer;

var p : TRTFFuncPTR;

Begin
GetReadHook (p);
while true do
  Begin
  Real_RTFGetToken;
  if (assigned(p)) then
    p;  { give read hook a look at token }
  { Silently discard newlines and carriage returns.  }
  if not ((rtfClass=rtfText) and ((rtfMajor=13) or (rtfmajor=10))) then
    break;
  End;
result:=rtfClass;
End;


{ ---------------------------------------------------------------------
   Install or return a token reader hook.
  ---------------------------------------------------------------------}

Procedure TRTFParser.SetReadHook (Hook : TRTFFuncPtr);

Begin
 readHook := Hook;
End;

Procedure TRTFParser.GetReadHook (Var q : TRTFFuncPtr);

Begin
  Q:=readHook;
End;


Procedure TRTFParser.UngetToken;

Begin
if (pushedClass >= 0) then      { there's already an ungotten token }
        Error ('cannot unget two tokens');
if (rtfClass < 0) then
        Error ('no token to unget');
pushedClass := rtfClass;
pushedMajor := rtfMajor;
pushedMinor := rtfMinor;
pushedParam := rtfParam;
rtfTextBuf  := pushedTextBuf;
End;


Function TRTFParser.PeekToken : Integer;

Begin
  Real_RTFGetToken;
  UngetToken;
  Result:=rtfClass;
End;



Procedure TRTFParser.Real_RTFGetToken;

var sign,c,c2 : Integer;

Begin
{ check for pushed token from RTFUngetToken() }
if (pushedClass >= 0) then
  Begin
  FrtfClass    := pushedClass;
  FrtfMajor    := pushedMajor;
  FrtfMinor    := pushedMinor;
  FrtfParam    := pushedParam;
  rtfTextBuf  := pushedTextBuf;
  rtfTextLen  := length (rtfTextBuf);
  pushedClass := -1;
  exit;
  End;
{ initialize token vars }
FrtfClass   := rtfUnknown;
FrtfParam   := rtfNoParam;
rtfTextBuf := '';
rtfTextLen := 0;
FTokenClass := rtfUnknown;

{ get first character, which may be a pushback from previous token }

if (pushedChar <> EOF) then
  Begin
  c := pushedChar;
  rtfTextBuf:=rtfTextBuf+chr(c);
  inc(rtftextlen);
  pushedChar := EOF;
  End
else
 begin
 c:=GetChar;
 if C=EOF then
   Begin
   FrtfClass := rtfEOF;
   exit;
   End;
 end;
if c=ord('{') then
  Begin
  FrtfClass := rtfGroup;
  FrtfMajor := rtfBeginGroup;
  exit;
  End;
if c=ord('}') then
  Begin
  FrtfClass := RTFGROUP;
  FrtfMajor := rtfEndGroup;
  exit;
  End;
if c<>ord('\') then
  Begin
  { Two possibilities here:
    1) ASCII 9, effectively like \tab control symbol
    2) literal text char }
  if c=ord(#8) then                     { ASCII 9 }
    Begin
    FrtfClass := rtfControl;
    FrtfMajor := rtfSpecialChar;
    FrtfMinor := rtfTab;
    End
  else
    Begin
    FrtfClass := rtfText;
    FrtfMajor := c;
    End;
  exit;
End;
c:=getchar;
if (c=EOF) then
  { early eof, whoops (class is rtfUnknown) }
  exit;
if ( not isalpha (c)) then
  Begin
  { Three possibilities here:
   1) hex encoded text char, e.g., \'d5, \'d3
   2) special escaped text char, e.g., \, \;
   3) control symbol, e.g., \_, \-, \|, \<10> }
  if c=ord('''') then { hex char }
     Begin
     c:=getchar;
     if (c<>EOF) then
       begin
       c2:=getchar;
       if (c2<>EOF) then
         Begin
         { should do isxdigit check! }
         FrtfClass := rtfText;
         FrtfMajor := HexVal (c) * 16 + HexVal (c2);
         exit;
         End;
       end;
       { early eof, whoops (class is rtfUnknown) }
       exit;
       End;
  if pos (chr(c),':{};\')<>0 then { escaped char }
    Begin
    FrtfClass := rtfText;
    FrtfMajor := c;
    exit;
    End;
   { control symbol }
   Lookup (rtfTextBuf); { sets class, major, minor }
   FTokenClass:=rtfControl;
   exit;
  End;
{ control word }
while (isalpha (c)) do
  Begin
  c:=GetChar;
  if (c=EOF) then
    break;
  End;
{ At this point, the control word is all collected, so the
  major/minor numbers are determined before the parameter
  (if any) is scanned.  There will be one too many characters
  in the buffer, though, so fix up before and restore after
  looking up. }
if (c<>EOF) then
  delete(rtfTextBuf,length(rtfTextbuf),1);
Lookup (rtfTextBuf);    { sets class, major, minor }
FTokenClass:=rtfControl;
if (c <>EOF) then
  rtfTextBuf:=rtfTextBuf+chr(c);
{ Should be looking at first digit of parameter if there
  is one, unless it's negative.  In that case, next char
  is '-', so need to gobble next char, and remember sign. }
sign := 1;
if c = ord('-') then
  Begin
  sign := -1;
  c := GetChar;
  End;
if (c<>EOF) then
  if isdigit (c) then
  Begin
  FrtfParam := 0;
  while (isdigit (c)) do        { gobble parameter }
    Begin
    FrtfParam := FrtfParam * 10 + c - ord('0');
    c:=GetChar;
    if (c=EOF) then
      break;
    End;
  FrtfParam:= sign*FrtfParam;
  End;
{ If control symbol delimiter was a blank, gobble it.
 Otherwise the character is first char of next token, so
 push it back for next call.  In either case, delete the
 delimiter from the token buffer. }
if (c<>EOF) then
  Begin
  if c<>ord (' ') then
     pushedChar := c;
  Delete (rtfTextBuf,rtfTextLen,1);
  Dec (rtfTextLen);
  End;
End;

Function TRTFParser.GetChar : Integer;

var c : byte;

Begin
  if FStream.read(c,1)<>0 then
    begin
    if (c and 128)=128 then c:=ord('?');
    Result:=c;
    rtfTextBuf:=rtfTextBuf+chr(c);
    inc(rtfTextLen);
    end
  else
    Result:=EOF;
End;

{ Synthesize a token by setting the global variables to the
  values supplied.  Typically this is followed with a call
  to RTFRouteToken().
  If param is non-negative, it becomes part of the token text. }
Procedure TRTFParser.SetToken (Aclass, major, minor, param : Integer; text : string);

Begin
  FrtfClass := Aclass;
  FrtfMajor := major;
  FrtfMinor := minor;
  FrtfParam := param;
  if (param=rtfNoParam) then
     rtfTextBuf:=text
  else
     rtfTextBuf:=text+IntTostr(param);
  rtfTextLen:=length(rtfTextBuf);
End;

{ ---------------------------------------------------------------------
       Special destination readers.  They gobble the destination so the
       writer doesn't have to deal with them.  That's wrong for any
       translator that wants to process any of these itself.  In that
       case, these readers should be overridden by installing a different
       destination callback.

       NOTE: The last token read by each of these reader will be the
       destination's terminating '', which will then be the current token.
       That 'End;' token is passed to RTFRouteToken() - the writer has already
       seen the 'Begin' that began the destination group, and may have pushed a
       state; it also needs to know at the end of the group that a state
       should be popped.

       It's important that rtfdata.inc and the control token lookup table list
       as many symbols as possible, because these readers unfortunately
       make strict assumptions about the input they expect, and a token
       of class rtfUnknown will throw them off easily.
 ----------------------------------------------------------------------}


{ Read Begin \fonttbl ... End; destination.  Old font tables don't have
  braces around each table entry; try to adjust for that.}
Procedure TRTFParser.ReadFontTbl;

var
  fp : PRTFFont;
  bp : string[rtfbufsiz];
  old : Integer;

Begin
old := -1;
While true do
  Begin
  GetToken;
  if CheckCM (rtfGroup, rtfEndGroup) then
      break;
  if (old < 0) then             { first entry - determine tbl type }
    Begin
    if CheckCMM (rtfControl, rtfCharAttr, rtfFontNum) then
      old:=1    { no brace }
    else if CheckCM (rtfGroup, rtfBeginGroup) then
      old:= 0   { brace }
    else                        { can't tell! }
      Error ('FTErr - Cannot determine format')
    End;
  if (old=0) then       { need to find "Begin" here }
    Begin
    if not CheckCM (rtfGroup, rtfBeginGroup) then
      Error ('FTErr - missing {');
    GetToken;   { yes, skip to next token }
    End;
  new(fp);
  if (fp=nil) then
     Error ('FTErr - cannot allocate font entry');
  fp^.rtfNextFont:= FfontList;
  FfontList:=fp;
  if not CheckCMM (rtfControl, rtfCharAttr, rtfFontNum) then
     Error ('FTErr - missing font number');
  fp^.rtfFNum := rtfParam;
  { Read optionalcommands. Recognize only fontfamily}
  GetToken;
  if not CheckCM (rtfControl, rtfFontFamily) then
    error ('FTErr - missing font family ');
  fp^.rtfFFamily := rtfMinor;
  { Read optional commands/groups. Recognize none at this point..}
  GetToken;
  while (rtfclass=rtfcontrol) or ((rtfclass=rtfgroup) or (rtfclass=rtfunknown)) do
    begin
    if rtfclass=rtfgroup then SkipGroup;
    GetToken
    end;
  { Read font name }
  bp:='';
  while (rtfclass=rtfText) do
    Begin
    if rtfMajor=ord(';') then
       break;
    bp:=bp+chr(rtfMajor);
    GetToken
    End;
  if bp='' then
     Error ('FTErr - missing font name');
  fp^.rtffname:=bp;
  { Read alternate font}
  if rtfclass=rtfgroup then
    begin
    SkipGroup;
    if Not rtfMajor=ord(';') then
      Error('Alternate font badly terminated');
    GetToken;
    end;
  if (old=0) then       { need to see "End;" here }
    Begin
    GetToken;
    if not CheckCM (rtfGroup, rtfEndGroup) then
       Error ('FTErr - missing }');
    End;
  End;
RouteToken;     { feed "End;" back to router }
End;


{ The color table entries have color values of -1 if
  the default color should be used for the entry (only
  a semi-colon is given in the definition, no color values).
  There will be a problem if a partial entry (1 or 2 but
  not 3 color values) is given.  The possibility is ignored
  here. }
Procedure TRTFParser.ReadColorTbl;

var
  cp   : PRTFColor;
  cnum : Integer;

Begin
cnum:=0;
While true do
  Begin
  GetToken;
  if CheckCM (rtfGroup, rtfEndGroup) then
    break;
  new(cp);
  if (cp=nil) then
    Error ('CTErr - cannot allocate color entry');
  cp^.rtfCNum  :=cnum;
  cp^.rtfCRed  :=-1;
  cp^.rtfCGreen:=-1;
  cp^.rtfCBlue :=-1;
  cp^.rtfNextColor := FColorList;
  inc(cnum);
  FcolorList:=cp;
  while true do
    Begin
    if not CheckCM (rtfControl, rtfColorName) then
       break;
    case rtfMinor of
      rtfRed:   cp^.rtfCRed   :=rtfParam;
      rtfGreen: cp^.rtfCGreen :=rtfParam;
      rtfBlue:  cp^.rtfCBlue  :=rtfParam;
    End;
    GetToken;
    End;
  if not CheckCM (rtfText, ord(';')) then
     Error ('CTErr - malformed entry');
  End;
RouteToken;     { feed "End;" back to router }
End;


{ The "Normal" style definition doesn't contain any style number
 (why?), all others do.  Normal style is given style 0. }

Procedure TRTFParser.ReadStyleSheet;

var
  sp          : PRTFStyle;
  sep,sepLast : PRTFStyleElt;
  bp          : string[rtfBufSiz];

Begin
While true do
  Begin
  GetToken;
  if CheckCM (rtfGroup, rtfEndGroup) then
      break;
  new (sp);
  if sp=nil then
     Error ('SSErr - cannot allocate stylesheet entry');
  sp^.rtfSNum := -1;
  sp^.rtfSBasedOn := rtfBasedOnNone;
  sp^.rtfSNextPar := -1;
  sp^.rtfSSEList := nil;
  sepLast:=nil;
  sp^.rtfNextStyle := FstyleList;
  sp^.rtfExpanding := 0;
  FstyleList := sp;
  if not CheckCM (rtfGroup, rtfBeginGroup) then
     Error ('SSErr - missing {');
  while (GetToken=rtfControl) or (FTokenClass=rtfControl) do
    Begin
    if rtfClass=rtfUnknown then
      continue;
    if (CheckMM (rtfParAttr, rtfStyleNum)) then
      Begin
      sp^.rtfSNum:=rtfParam;
      continue;
      End;
    if (CheckMM (rtfStyleAttr, rtfBasedOn)) then
      Begin
      sp^.rtfSBasedOn:=rtfParam;
      continue;
      End;
    if (CheckMM (rtfStyleAttr, rtfNext)) then
      Begin
      sp^.rtfSNextPar:=rtfParam;
      Continue;
      End;
    new(sep);
    if sep=nil then
      Error ('SSErr - cannot allocate style element');
    sep^.rtfSEClass:=rtfClass;
    sep^.rtfSEMajor:=rtfMajor;
    sep^.rtfSEMinor:=rtfMinor;
    sep^.rtfSEParam:=rtfParam;
    sep^.rtfSEText:=rtfTextBuf;
    if sepLast=nil then
       sp^.rtfSSEList:=sep      { first element }
    else                                { add to end }
       sepLast^.rtfNextSE:=sep;
    sep^.rtfNextSE:=nil;
    sepLast:=sep;
  End;
  if sp^.rtfSNextPar=-1 then            { \snext not given }
    sp^.rtfSNextPar:=sp^.rtfSNum;       { next is itself }
  if rtfClass<>rtfText then
     Error ('SSErr - missing style name');
  Bp:='';
  while rtfClass=rtfText do
    Begin
    if rtfMajor=ord(';') then
      Begin
      GetToken;
      break;
      End;
    bp:=bp+chr(rtfMajor);
    GetToken;
    End;
  if (sp^.rtfSNum < 0) then     { no style number was specified }
    Begin                       { (only legal for Normal style) }
    if bp<>'Normal' then
       Error ('SSErr - missing style number');
    sp^.rtfSNum:=0;
    End;
  sp^.rtfSName:=bp;
  if not CheckCM (rtfGroup, rtfEndGroup) then
      Error ('SSErr - missing }');
  End;
RouteToken;     { feed "End;" back to router }
End;


Procedure TRTFParser.ReadInfoGroup;

Begin
  SkipGroup ;
  RouteToken ;  { feed "End;" back to router }
End;


Procedure TRTFParser.ReadPictGroup;

Begin
  SkipGroup ;
  RouteToken ;  { feed "End;" back to router }
End;


{ ----------------------------------------------------------------------
    Routines to return pieces of stylesheet, or font or color tables
  ----------------------------------------------------------------------}


Function TRTFParser.GetStyle (num : Integer) : PRTFStyle;

var
  s : PRTFSTyle;

Begin
s:=Fstylelist;
if num<>1 then
  while s<>nil do
    Begin
    if (s^.rtfSNum=num) then break;
    s:=s^.rtfNextStyle;
    End;
result:=s;              { NULL if not found }
End;


Function TRTFParser.GetFont (num : Integer) : PRTFFont;

Var
  f :PRTFFont;

Begin
f:=FfontList;
if num<>-1 then
  while f<>nil do
    Begin
    if f^.rtfFNum=num then break;
    f:=f^.rtfNextFont;
    End;
result:=f;              { NULL if not found }
End;

Function TRTFParser.GetColor (num : Integer) : PRTFColor;

var
  c : PRTFColor;

Begin
c:=Fcolorlist;
if (num<>-1) then
  while c<>nil do
    Begin
    if c^.rtfCNum=num then break;
    c:=c^.rtfNextColor;
    End;
Result:=c;              { NULL if not found }
End;

{ ---------------------------------------------------------------------
       Expand style n, if there is such a style.
  ---------------------------------------------------------------------}

Procedure TRTFParser.ExpandStyle (n : Integer);

var
  s  : PRTFStyle;
  se : PRTFStyleElt;

Begin
if n=-1 then exit;
s:=GetStyle (n);
if s=nil then exit;

if (s^.rtfExpanding<>0) then
  Error ('Style expansion loop, style '+inttostr(n));
s^.rtfExpanding:=1;     { set expansion flag for loop detection }
{
        Expand "based-on" style.  This is done by synthesizing
        the token that the writer needs to see in order to trigger
        another style expansion, and feeding to token back through
        the router so the writer sees it.
}
SetToken (rtfControl, rtfParAttr, rtfStyleNum, s^.rtfSBasedOn, '\s');
RouteToken;
{
        Now route the tokens unique to this style.  RTFSetToken()
        isn't used because it would add the param value to the end
        of the token text, which already has it in.
}
se:=s^.rtfSSEList;
while se<>nil do
  Begin
  FrtfClass:=se^.rtfSEClass;
  FrtfMajor:=se^.rtfSEMajor;
  FrtfMinor:=se^.rtfSEMinor;
  FrtfParam:=se^.rtfSEParam;
  rtfTextBuf:=se^.rtfSEText;
  rtfTextLen:=length (rtfTextBuf);
  RouteToken;
  se:=se^.rtfNextSE
  End;
s^.rtfExpanding:=0;     { done - clear expansion flag }
End;

function TRTFParser.GetRtfBuf: String;
begin
  Result:=rtfTextBuf;
end;

{ ---------------------------------------------------------------------
       Initialize lookup table hash values.
       Only need to do this the first time it's called.
  ---------------------------------------------------------------------}

Procedure TRTFParser.LookupInit;

var count : Integer;

Begin
count:=0;
while rtfkey[count].rtfKStr<>'' do
  begin
  rtfkey[count].rtfKHash:=Hash (rtfkey[count].rtfKStr);
  inc(count)
  End;
End;


{ ---------------------------------------------------------------------
       Determine major and minor number of control token.  If it's
       not found, the class turns into rtfUnknown.
  ---------------------------------------------------------------------}

Procedure TRTFParser.Lookup (S : String);

var
 thehash,rp : Integer;

Begin
delete(s,1,1);                  { skip over the leading \ character }
thehash:=Hash (s);
rp:=0;
while rtfkey[rp].rtfKstr<>'' do
  Begin
  if (thehash=rtfkey[rp].rtfKHash) and (s=rtfkey[rp].rtfKStr) then
     Begin
     FrtfClass:=rtfControl;
     FrtfMajor:=rtfkey[rp].rtfKMajor;
     FrtfMinor:=rtfkey[rp].rtfKMinor;
     exit;
     End;
  inc(rp);
  End;
FrtfClass:=rtfUnknown;
End;


Procedure TRTFParser.Error (msg : String);

{ Call errorhandler }

begin
  if assigned(onrtferror) then onrtferror(msg);
end;
{ ---------------------------------------------------------------------
       Token comparison routines
  ---------------------------------------------------------------------}

Function TRTFParser.CheckCM (Aclass, major: Integer) : Boolean;
Begin
  Result:=(rtfClass=Aclass) and (rtfMajor=major);
End;


Function TRTFParser.CheckCMM (Aclass, major, minor : Integer) : Boolean;

Begin
  Result:=(rtfClass=Aclass) and ((rtfMajor=major) and (rtfMinor=minor));
End;


Function TRTFParser.CheckMM (major, minor : Integer) : Boolean;

Begin
  Result:=(rtfMajor=major) and (rtfMinor=minor);
End;

Procedure TRTFParser.SetStream (Astream : TStream);

begin
  FStream:=Astream;
end;

end.
