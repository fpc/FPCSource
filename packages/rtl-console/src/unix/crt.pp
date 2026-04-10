{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt and Peter Vreman,
    members of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit Crt;
{$ENDIF FPC_DOTTEDUNITS}

Interface
{$mode fpc} // Shortstring is assumed
 {$i crth.inc}

Const
  { Controlling consts }
  Flushing     = false;               {if true then don't buffer output}
  ConsoleMaxX  = 1024;
  ConsoleMaxY  = 1024;
  ScreenHeight : longint = 25;
  ScreenWidth  : longint = 80;

Type
  TCharAttr=packed record
    ch   : AnsiChar;
    attr : byte;
  end;
  TConsoleBuf=Array[0..ConsoleMaxX*ConsoleMaxY-1] of TCharAttr;
  PConsoleBuf=^TConsoleBuf;

var
  ConsoleBuf : PConsoleBuf;

Implementation

{$IFDEF FPC_DOTTEDUNITS}
uses UnixApi.Base ,UnixApi.Unix, UnixApi.TermIO;
{$ELSE FPC_DOTTEDUNITS}
uses BaseUnix ,unix, termio;
{$ENDIF FPC_DOTTEDUNITS}

Const
  OldTextAttr : byte = $07;
Var
  CurrX,CurrY : Byte;
  OutputRedir, InputRedir : boolean; { is the output/input being redirected (not a TTY) }
{$ifdef debugcrt}
  DebugFile : Text;
{$endif}
{*****************************************************************************
                    Some Handy Functions Not in the System.PP
*****************************************************************************}

{$ifdef debugcrt}
Procedure Debug(Msg : shortstring);

begin
  Writeln(DebugFile,Msg);
end;
{$endif}

Function Str(l:longint):shortstring;
{
  Return a String of the longint
}
var
  hstr : string[32];
begin
  System.Str(l,hstr);
  Str:=hstr;
end;



Function Max(l1,l2:longint):longint;
{
  Return the maximum of l1 and l2
}
begin
  if l1>l2 then
   Max:=l1
  else
   Max:=l2;
end;



Function Min(l1,l2:longint):longint;
{
  Return the minimum of l1 and l2
}
begin
  if l1<l2 then
   Min:=l1
  else
   Min:=l2;
end;


{*****************************************************************************
                      Optimal AnsiString Conversion Routines
*****************************************************************************}

Function XY2Ansi(x,y,ox,oy:longint):shortstring;
{
  Returns a string with the escape sequences to go to X,Y on the screen
}
Begin
  { in case of potential ox overflow, send full position information
    (mantis #20880) }
  if (y=oy) and
     (ox<>$ff) then
   begin
     if x=ox then
      begin
        // this workaround should improve behaviour on some terminals.
        // debian bug 216057 but I also observed this with video on FreeBSD
        if x=screenwidth then
          XY2Ansi:=#27'['+Str(y)+';'+Str(x)+'H'
        else
       // end workaround
          XY2Ansi:='';
        exit;
      end;
    {$ifdef Linux}      // linux CRT shortcut
     if x=1 then
      begin
        XY2Ansi:=#13;
        exit;
      end;
    {$endif}
     if x>ox then
      begin
        XY2Ansi:=#27'['+Str(x-ox)+'C';
        exit;
      end
     else
      begin
        XY2Ansi:=#27'['+Str(ox-x)+'D';
        exit;
      end;
   end;
  if x=ox then
   begin
     if y>oy then
      begin
        XY2Ansi:=#27'['+Str(y-oy)+'B';
        exit;
      end
     else
      begin
        XY2Ansi:=#27'['+Str(oy-y)+'A';
        exit;
      end;
   end;
  {$ifdef Linux}                        // this shortcut isn't for everybody
  if (x=1) and (oy+1=y) then
   XY2Ansi:=#13#10
  else
  {$endif}
   XY2Ansi:=#27'['+Str(y)+';'+Str(x)+'H';
End;



const
  AnsiTbl : string[8]='04261537';
Function Attr2Ansi(Attr,OAttr:longint):shortstring;
{
  Convert Attr to an Ansi String, the Optimal code is calculate
  with use of the old OAttr
}
var
  hstr : string[16];
  OFg,OBg,Fg,Bg : longint;

  procedure AddSep(ch:AnsiChar);
  begin
    if length(hstr)>0 then
     hstr:=hstr+';';
    hstr:=hstr+ch;
  end;

begin
  if Attr=OAttr then
   begin
     Attr2Ansi:='';
     exit;
   end;
  Hstr:='';
  Fg:=Attr and $f;
  Bg:=Attr shr 4;
  OFg:=OAttr and $f;
  OBg:=OAttr shr 4;
  if (OFg<>7) or (Fg=7) or ((OFg>7) and (Fg<8)) or ((OBg>7) and (Bg<8)) then
   begin
     hstr:='0';
     OFg:=7;
     OBg:=0;
   end;
  if (Fg>7) and (OFg<8) then
   begin
     AddSep('1');
     OFg:=OFg or 8;
   end;
  if (Bg and 8)<>(OBg and 8) then
   begin
     AddSep('5');
     OBg:=OBg or 8;
   end;
  if (Fg<>OFg) then
   begin
     AddSep('3');
     hstr:=hstr+AnsiTbl[(Fg and 7)+1];
   end;
  if (Bg<>OBg) then
   begin
     AddSep('4');
     hstr:=hstr+AnsiTbl[(Bg and 7)+1];
   end;
  if hstr='0' then
   hstr:='';
  Attr2Ansi:=#27'['+hstr+'m';
end;



Function Ansi2Attr(Const HStr:shortstring;oattr:longint):longint;
{
  Convert an Escape sequence to an attribute value, uses Oattr as the last
  color written
}
var
  i,j : longint;
begin
  i:=2;
  if (Length(HStr)<3) or (Hstr[1]<>#27) or (Hstr[2]<>'[') then
   i:=255;
  while (i<length(Hstr)) do
   begin
     inc(i);
     case Hstr[i] of
      '0' : OAttr:=7;
      '1' : OAttr:=OAttr or $8;
      '5' : OAttr:=OAttr or $80;
      '3' : begin
              inc(i);
              j:=pos(Hstr[i],AnsiTbl);
              if j>0 then
               OAttr:=(OAttr and $f8) or (j-1);
            end;
      '4' : begin
              inc(i);
              j:=pos(Hstr[i],AnsiTbl);
              if j>0 then
               OAttr:=(OAttr and $8f) or ((j-1) shl 4);
            end;
      'm' : i:=length(HStr);
     end;
   end;
  Ansi2Attr:=OAttr;
end;



{*****************************************************************************
                          Buffered StdIn/StdOut IO
*****************************************************************************}

const
  ttyIn=0;  {Handles for stdin/stdout}
  ttyOut=1;
  ttyFlush:boolean=true;
{Buffered Input/Output}
  InSize=256;
  OutSize=1024;
var
  InBuf  : array[0..InSize-1] of AnsiChar;
  InCnt,
  InHead,
  InTail : longint;
  OutBuf : array[0..OutSize-1] of AnsiChar;
  OutCnt : longint;


{Flush Output Buffer}
procedure ttyFlushOutput;
begin
  if OutCnt>0 then
   begin
     fpWrite(ttyOut,OutBuf,OutCnt);
     OutCnt:=0;
   end;
end;



Function ttySetFlush(b:boolean):boolean;
begin
  ttySetFlush:=ttyFlush;
  ttyFlush:=b;
  if ttyFlush then
   ttyFlushOutput;
end;


{Send AnsiChar to Remote}
Procedure ttySendChar(c:AnsiChar);
Begin
  if OutCnt<OutSize then
   begin
     OutBuf[OutCnt]:=c;
     inc(OutCnt);
   end;
{Full ?}
  if (OutCnt>=OutSize) then
   ttyFlushOutput;
End;



{Send String to Remote}
procedure ttySendStr(const hstr:shortstring);
var
  i : longint;
begin
  for i:=1to length(hstr) do
   ttySendChar(hstr[i]);
  if ttyFlush then
   ttyFlushOutput;
end;



{Get AnsiChar from Remote}
function ttyRecvChar:AnsiChar;
var
  Readed,i : longint;
begin
{Buffer Empty? Yes, Input from StdIn}
  if (InHead=InTail) then
   begin
   {Calc Amount of Chars to Read}
     i:=InSize-InHead;
     if InTail>InHead then
      i:=InTail-InHead;
   {Read}
     Readed:=fpread(TTYIn,InBuf[InHead],i);
   {Increase Counters}
     inc(InCnt,Readed);
     inc(InHead,Readed);
   {Wrap if End has Reached}
     if InHead>=InSize then
      InHead:=0;
   end;
{Check Buffer}
  if (InCnt=0) then
   ttyRecvChar:=#0
  else
   begin
     ttyRecvChar:=InBuf[InTail];
     dec(InCnt);
     inc(InTail);
     if InTail>=InSize then
      InTail:=0;
   end;
end;


{*****************************************************************************
                       Screen Routines not Window Depended
*****************************************************************************}

procedure ttyGotoXY(x,y:longint);
{
  Goto XY on the Screen, if a value is 0 the goto the current
  position of that value and always recalc the ansicode for it
}
begin
  if x=0 then
   begin
     x:=CurrX;
     CurrX:=$ff;
   end;
  if y=0 then
   begin
     y:=CurrY;
     CurrY:=$ff;
   end;
  if OutputRedir then
   begin
     if longint(y)-longint(CurrY)=1 then
      ttySendStr(#10);
   end
  else
   ttySendStr(XY2Ansi(x,y,CurrX,CurrY));
  CurrX:=x;
  CurrY:=y;
end;



procedure ttyColor(a:longint);
{
  Set Attribute to A, only output if not the last attribute is set
}
begin
  if a<>OldTextAttr then
   begin
     if not OutputRedir then
      ttySendStr(Attr2Ansi(a,OldTextAttr));
     TextAttr:=a;
     OldTextAttr:=a;
   end;
end;



procedure ttyWrite(const s:shortstring);
{
  Write a string to the output, memory copy and Current X&Y are also updated
}
var
  idx,i : longint;
begin
  ttySendStr(s);
{Update MemCopy}
  idx:=(CurrY-1)*ScreenWidth-1;
  for i:=1 to length(s) do
   if s[i]=#8 then
    begin
      if CurrX>1 then
       dec(CurrX);
    end
   else
    begin
      ConsoleBuf^[idx+CurrX].ch:=s[i];
      ConsoleBuf^[idx+CurrX].attr:=TextAttr;
      inc(CurrX);
      If CurrX>ScreenWidth then
        CurrX:=$FF; // Mark as invalid.
    end;
end;



Function FullWin:boolean;
{
  Full Screen 80x25? Window(1,1,80,25) is used, allows faster routines
}
begin
  FullWin:=(WindMinX=1) and (WindMinY=1) and
           (WindMaxX=ScreenWidth) and (WindMaxY=ScreenHeight);
end;


procedure LineWrite(const temp:shortstring);
{
  Write a Line to the screen, doesn't write on 80,25 under Dos
  the Current CurrX is set to WindMax. NO MEMORY UPDATE!
}
begin
  CurrX:=WindMaxX+1;
  ttySendStr(Temp);
end;



Procedure DoEmptyLine(y,xl,xh:Longint);
{
  Write an empty line at row Y from column Xl to Xh. Memory is also updated.
}
Var
  len : Longint;
  blank_with_attribute : TCharAttr;
Begin
  ttyGotoXY(xl,y);
  len:=xh-xl+1;
  LineWrite(Space(len));
  blank_with_attribute.ch:=' ';
  blank_with_attribute.attr:=TextAttr;
  FillWord(ConsoleBuf^[(y-1)*ScreenWidth+xl-1],len,word(blank_with_attribute));
End;


procedure DoScrollLine(y1,y2,xl,xh:longint);
{
  Move Line y1 to y2, use only columns Xl-Xh, Memory is updated also
}
var
  Temp    : shortstring;
  idx,
  OldAttr,
  x,attr  : longint;
begin
  ttyGotoXY(xl,y2);
{ precalc ConsoleBuf[] y-offset }
  idx:=(y1-1)*ScreenWidth-1;
{ update screen }
  OldAttr:=$ff;
  Temp:='';
  For x:=xl To xh Do
   Begin
     attr:=ConsoleBuf^[idx+x].attr;
     if (attr<>OldAttr) and (not OutputRedir) then
      begin
        temp:=temp+Attr2Ansi(Attr,OldAttr);
        OldAttr:=Attr;
      end;
     Temp:=Temp+ConsoleBuf^[idx+x].ch;
     if (x=xh) or (length(Temp)>240) then
      begin
        LineWrite(Temp);
        Temp:='';
      end;
   End;
{Update memory copy}
  Move(ConsoleBuf^[(y1-1)*ScreenWidth+xl-1],ConsoleBuf^[(y2-1)*ScreenWidth+xl-1],(xh-xl+1)*2);
end;



Procedure TextColor(Color: Byte);
{
  Switch foregroundcolor
}
  var AddBlink : byte;
Begin
  If (Color>15) Then
    AddBlink:=Blink
  else
    AddBlink:=0;
  ttyColor((Color and $f) or (TextAttr and $70) or AddBlink);
End;



Procedure TextBackground(Color: Byte);
{
  Switch backgroundcolor
}
Begin
  TextAttr:=((Color shl 4) and ($f0 and not Blink)) or (TextAttr and ($0f OR Blink));
  ttyColor(TextAttr);
End;



Procedure HighVideo;
{
  Set highlighted output.
}
Begin
  TextColor(TextAttr Or $08);
End;



Procedure LowVideo;
{
  Set normal output
}
Begin
  TextColor(TextAttr And $77);
End;



Procedure NormVideo;
{
  Set normal back and foregroundcolors.
}
Begin
  TextColor(7);
  TextBackGround(0);
End;



Procedure GotoXy(X: tcrtcoord; Y: tcrtcoord);
{
  Go to coordinates X,Y in the current window.
}
Begin
  If (X>0) and (X<=WindMaxX- WindMinX+1) and
     (Y>0) and (Y<=WindMaxY-WindMinY+1) Then
   Begin
     Inc(X,WindMinX-1);
     Inc(Y,WindMinY-1);
     ttyGotoXY(x,y);
   End;
End;



Procedure Window(X1, Y1, X2, Y2: Byte);
{
  Set screen window to the specified coordinates.
}
Begin
  if (X1>X2) or (X2>ScreenWidth) or
     (Y1>Y2) or (Y2>ScreenHeight) then
   exit;
  WindMinX:=X1;
  WindMaxX:=X2;
  WindMinY:=Y1;
  WindMaxY:=Y2;
  WindMin:=((Y1-1) Shl 8)+(X1-1);
  WindMax:=((Y2-1) Shl 8)+(X2-1);
  GoToXY(1,1);
End;



Procedure ClrScr;
{
  Clear the current window, and set the cursor on 1,1
}
Var
  CY,i      : Longint;
  oldflush  : boolean;
  blank_with_attribute : TCharAttr;

Begin
  { See if color has changed }
  if OldTextAttr<>TextAttr then
   begin
     i:=TextAttr;
     TextAttr:=OldTextAttr;
     ttyColor(i);
   end;
  oldflush:=ttySetFlush(Flushing);
  if FullWin then
   begin
     if not OutputRedir then
      ttySendStr(#27'[H'#27'[2J');
     CurrX:=1;
     CurrY:=1;
     blank_with_attribute.ch   := ' ';
     blank_with_attribute.attr := TextAttr;
     FillWord(ConsoleBuf^,ScreenWidth*ScreenHeight,word(blank_with_attribute));
   end
  else
   begin
     For Cy:=WindMinY To WindMaxY Do
      DoEmptyLine(Cy,WindMinX,WindMaxX);
     GoToXY(1,1);
   end;
  ttySetFlush(oldflush);
End;



Procedure ClrEol;
{
  Clear from current position to end of line.
}
var
  len,i : longint;
  IsLastLine : boolean;
Begin
  { See if color has changed }
  if OldTextAttr<>TextAttr then
   begin
     i:=TextAttr;
     TextAttr:=OldTextAttr;
     ttyColor(i);
   end;
  if FullWin or (WindMaxX = ScreenWidth) then
   begin
     if not OutputRedir then
      ttySendStr(#27'[K');
   end
  else
   begin
   { Tweak WindMaxx and WindMaxy so no scrolling happens }
     len:=WindMaxX-CurrX+1;
     IsLastLine:=false;
     if CurrY=WindMaxY then
      begin
        inc(WindMaxX,3);
        inc(WindMaxY,2);
        IsLastLine:=true;
      end;
     ttySendStr(Space(len));
     if IsLastLine then
      begin
        dec(WindMaxX,3);
        dec(WindMaxY,2);
      end;
     ttyGotoXY(0,0);
   end;
End;



Function WhereX: tcrtcoord;
{
  Return current X-position of cursor.
}
Begin
  WhereX:=CurrX-WindMinX+1;
End;



Function WhereY: tcrtcoord;
{
  Return current Y-position of cursor.
}
Begin
  WhereY:=CurrY-WindMinY+1;
End;



Procedure ScrollScrnRegionUp(xl,yl,xh,yh, count: longint);
{
  Scroll the indicated region count lines up. The empty lines are filled
  with blanks in the current color. The screen position is restored
  afterwards.
}
Var
  y,oldx,oldy : byte;
  oldflush    : boolean;
Begin
  oldflush:=ttySetFlush(Flushing);
  oldx:=CurrX;
  oldy:=CurrY;
{Scroll}
  For y:=yl to yh-count do
   DoScrollLine(y+count,y,xl,xh);
{Restore TextAttr}
  ttySendStr(Attr2Ansi(TextAttr,$ff));
{Fill the rest with empty lines}
  for y:=yh-count+1 to yh do
   DoEmptyLine(y,xl,xh);
{Restore current position}
  ttyGotoXY(OldX,OldY);
  ttySetFlush(oldflush);
End;



Procedure ScrollScrnRegionDown(xl,yl,xh,yh, count: longint);
{
  Scroll the indicated region count lines down. The empty lines are filled
  with blanks in the current color. The screen position is restored
  afterwards.
}
Var
  y,oldx,oldy : byte;
  oldflush    : boolean;
Begin
  oldflush:=ttySetFlush(Flushing);
  oldx:=CurrX;
  oldy:=CurrY;
{Scroll}
  for y:=yh downto yl+count do
   DoScrollLine(y-count,y,xl,xh);
{Restore TextAttr}
  ttySendStr(Attr2Ansi(TextAttr,$ff));
{Fill the rest with empty lines}
  for y:=yl to yl+count-1 do
   DoEmptyLine(y,xl,xh);
{Restore current position}
  ttyGotoXY(OldX,OldY);
  ttySetFlush(oldflush);
End;



{*************************************************************************
                            KeyBoard
*************************************************************************}
{$i keyscan.inc}

const
  kbAltCenter = kbCtrlCenter;  {there is no true DOS scancode for Alt+Center (Numpad "5") reusing Ctrl+Center}

Const
  KeyBufferSize = 20;
var
  KeyBuffer : Array[0..KeyBufferSize-1] of AnsiChar;
  KeyPut,
  KeySend   : longint;
  isKitty : boolean;

Procedure PushKey(Ch:AnsiChar);
Var
  Tmp : Longint;
Begin
  Tmp:=KeyPut;
  Inc(KeyPut);
  If KeyPut>=KeyBufferSize Then
   KeyPut:=0;
  If KeyPut<>KeySend Then
   KeyBuffer[Tmp]:=Ch
  Else
   KeyPut:=Tmp;
End;



Function PopKey:AnsiChar;
Begin
  If KeyPut<>KeySend Then
   Begin
     PopKey:=KeyBuffer[KeySend];
     Inc(KeySend);
     If KeySend>=KeyBufferSize Then
      KeySend:=0;
   End
  Else
   PopKey:=#0;
End;



Procedure PushExt(b:byte);
begin
  PushKey(#0);
  PushKey(chr(b));
end;



const
  AltKeyStr  : string[38]='qwertyuiopasdfghjklzxcvbnm1234567890-=';
  AltKeyStr2 : string[38]='QWERTYUIOPASDFGHJKLZXCVBNM1234567890-=';
  AltCodeStr : string[38]=#016#017#018#019#020#021#022#023#024#025#030#031#032#033#034#035#036#037#038+
                          #044#045#046#047#048#049#050#120#121#122#123#124#125#126#127#128#129#130#131;
Function FAltKey(ch:AnsiChar):byte;
var
  Idx : longint;
Begin
  Idx:=Pos(ch,AltKeyStr);
  if Idx=0 then
    Idx:=Pos(ch,AltKeyStr2);
  if Idx>0 then
   FAltKey:=byte(AltCodeStr[Idx])
  else
   FAltKey:=0;
End;

{ This one doesn't care about keypresses already processed by readkey  }
{ and waiting in the KeyBuffer, only about waiting keypresses at the   }
{ TTYLevel (including ones that are waiting in the TTYRecvChar buffer) }
function sysKeyPressed: boolean;
var
  fdsin : tfdSet;
begin
  if (InCnt>0) then
   sysKeyPressed:=true
  else
   begin
     fpFD_ZERO(fdsin);
     fpFD_SET(TTYin,fdsin);
     sysKeypressed:=(fpSelect(TTYIn+1,@fdsin,nil,nil,0)>0);
   end;
end;

Function KeyPressed:Boolean;
Begin
  Keypressed := (KeySend<>KeyPut) or sysKeyPressed;
End;

Function GetScanCodeValue(kMod:byte;ch : AnsiChar):byte;
const MaxIdx=20;
      ScanCodeValue : array [0..MaxIdx-1,0..3] of byte = (
         {A} (kbUp,kbUp,kbCtrlUp,kbAltUp),
         {B} (kbDown,kbDown,kbCtrlDown,kbAltDown),
         {C} (kbRight,kbRight,kbCtrlRight,kbAltRight),
         {D} (kbLeft,kbLeft,kbCtrlLeft,kbAltLeft),
         {E} (kbCenter,kbCenter,kbCtrlCenter,kbAltCenter),
         {F} (kbEnd,kbEnd,kbCtrlEnd,kbAltEnd),
         {G} (kbHome,kbHome,kbCtrlHome,kbAltHome),  {place holder }
         {H} (kbHome,kbHome,kbCtrlHome,kbAltHome),
         {I} (kbEnd,kbEnd,kbCtrlEnd,kbAltEnd),   {place holder }
         {J} (kbEnd,kbEnd,kbCtrlEnd,kbAltEnd),   {place holder }
         {K} (kbEnd,kbEnd,kbCtrlEnd,kbAltEnd),   {place holder }
         {L} (kbEnd,kbEnd,kbCtrlEnd,kbAltEnd),   {place holder }
         {M} (kbEnd,kbEnd,kbCtrlEnd,kbAltEnd),   {place holder }
         {N} (kbEnd,kbEnd,kbCtrlEnd,kbAltEnd),   {place holder }
         {O} (kbEnd,kbEnd,kbCtrlEnd,kbAltEnd),   {place holder }
         {P} (kbF1,kbShiftF1,kbCtrlF1,kbAltF1),
         {Q} (kbF2,kbShiftF2,kbCtrlF2,kbAltF2),
         {R} (kbF3,kbShiftF3,kbCtrlF3,kbAltF3),
         {S} (kbF4,kbShiftF4,kbCtrlF4,kbAltF4),
             (kbNoKey,kbNoKey,kbNoKey,kbNoKey)   {place holder }
      );
var idx,iMod : longword;
begin
  GetScanCodeValue:=0;
  idx:=ord(ch)-ord('A');
  iMod:=0;
  kMod:=kMod-1;
  if (kMod and 2)<>0 then
    iMod:=3
  else if (kMod and 4)<>0 then
    iMod:=2
  else if (kMod and 1)<>0 then
    iMod:=1;
  //writeln('kMod ',kMod,'  ch ',ch,'   iMod ',iMod,'   idx ',idx);
  if idx<MaxIdx then
    GetScanCodeValue:=ScanCodeValue[idx,iMod];
end;

const MaxNumScanCodeIdx=35;
      NumScanCodeValue : array [0..MaxNumScanCodeIdx-1,0..3] of byte = (
          {00} (kbEsc,kbEsc,kbEsc,kbEsc),   { place holder }
          {01} (kbHome,kbHome,kbCtrlHome,kbAltHome),
          {02} (kbIns,kbShiftIns,kbCtrlIns,kbAltIns),
          {03} (kbDel,kbShiftDel,kbCtrlDel,kbAltDel),
          {04} (kbEnd,kbEnd,kbCtrlEnd,kbAltEnd),
          {05} (kbPgUp,kbPgUp,kbCtrlPgUp,kbAltPgUp),
          {06} (kbPgDn,kbPgDn,kbCtrlPgDn,kbAltPgDn),
          {07} (kbHome,kbHome,kbCtrlHome,kbAltHome),
          {08} (kbEnd,kbEnd,kbCtrlEnd,kbAltEnd),
          {09} (kbAltEsc,kbAltEsc,kbAltEsc,kbAltEsc), { place holder }
          {10} (kbHome,kbHome,kbCtrlHome,kbAltHome),  { place holder }
          {11} (kbF1,kbShiftF1,kbCtrlF1,kbAltF1),
          {12} (kbF2,kbShiftF2,kbCtrlF2,kbAltF2),
          {13} (kbF3,kbShiftF3,kbCtrlF3,kbAltF3),
          {14} (kbF4,kbShiftF4,kbCtrlF4,kbAltF4),
          {15} (kbF5,kbShiftF5,kbCtrlF5,kbAltF5),
          {16} (kbF6,kbShiftF2,kbCtrlF6,kbAltF6),  { place holder }
          {17} (kbF6,kbShiftF6,kbCtrlF6,kbAltF6),
          {18} (kbF7,kbShiftF7,kbCtrlF7,kbAltF7),
          {19} (kbF8,kbShiftF8,kbCtrlF8,kbAltF8),
          {20} (kbF9,kbShiftF9,kbCtrlF9,kbAltF9),
          {21} (kbF10,kbShiftF10,kbCtrlF10,kbAltF10),
          {22} (kbF2,kbShiftF2,kbCtrlF2,kbAltF2),  { place holder }
          {23} (kbF11,kbShiftF11,kbCtrlF11,kbAltF11),
          {24} (kbF12,kbShiftF12,kbCtrlF12,kbAltF12),
          {25} (kbShiftF1,kbShiftF1,kbCtrlF1,kbAltF1),
          {26} (kbShiftF2,kbShiftF2,kbCtrlF2,kbAltF2),
          {27} (kbShiftF3,kbShiftF3,kbCtrlF3,kbAltF3),  { place holder }
          {28} (kbShiftF3,kbShiftF3,kbCtrlF3,kbAltF3),
          {29} (kbShiftF4,kbShiftF4,kbCtrlF4,kbAltF4),
          {30} (kbShiftF5,kbShiftF5,kbCtrlF5,kbAltF5),  { place holder }
          {31} (kbShiftF5,kbShiftF5,kbCtrlF5,kbAltF5),
          {32} (kbShiftF6,kbShiftF6,kbCtrlF6,kbAltF6),
          {33} (kbShiftF7,kbShiftF7,kbCtrlF7,kbAltF7),
          {34} (kbShiftF8,kbShiftF8,kbCtrlF8,kbAltF8)
          {
          (kb,kbShift,kbCtrl,kbAlt),
          (kb,kbShift,kbCtrl,kbAlt),
          (kb,kbShift,kbCtrl,kbAlt),
          }
      );

procedure AdjustmentForRxvtTerminal;
var term:string;
begin
  term:=fpgetenv('COLORTERM');
  if lowercase(copy(term,1,4))='rxvt' then
  begin
    NumScanCodeValue[25,0]:=kbShiftF3;
    NumScanCodeValue[26,0]:=kbShiftF4;
    NumScanCodeValue[28,0]:=kbShiftF5;
    NumScanCodeValue[29,0]:=kbShiftF6;
    NumScanCodeValue[31,0]:=kbShiftF7;
    NumScanCodeValue[32,0]:=kbShiftF8;
    NumScanCodeValue[33,0]:=kbShiftF9;
    NumScanCodeValue[34,0]:=kbShiftF10;
  end;
end;

Function GetScanCodeValue(kMod:byte; num1,num2 : ShortInt):byte;
var idx,iMod : longword;
begin
  GetScanCodeValue:=0;
  idx:=199;
  if (num1=-1) then
  begin
    idx:=num2;
  end else
  begin
    idx:=num1;
  end;
  iMod:=0;
  kMod:=kMod-1;
  if (kMod and 2)<>0 then
    iMod:=3
  else if (kMod and 4)<>0 then
    iMod:=2
  else if (kMod and 1)<>0 then
    iMod:=1;
  if idx<MaxNumScanCodeIdx then
    GetScanCodeValue:=NumScanCodeValue[idx,iMod];
end;


Function ReadKey:AnsiChar;
Var
  ch       : AnsiChar;
  OldState,
  State    : longint;
  FDS      : TFDSet;
  num1,num2,kMod : shortint;
  OPrefix : boolean;
  ScanCode:byte;
  SavedEscapeStr : ShortString;

  procedure ProcessNum(aCh : AnsiChar);
  var num : shortInt;
  begin
    num:=ord(aCh)-ord('0');
    if num2=-1 then
      num2:=0;
    num2:=num2*10+num;
    if (num1<>-1) or OPrefix then
      kMod:=num2;
  end;

  procedure PushBackSavedStr;
  var i : longword;
  begin
    for i:=1 to length(SavedEscapeStr) do
      PushKey(SavedEscapeStr[i]);
  end;

Begin
{Check Buffer first}
  if KeySend<>KeyPut then
   begin
     ReadKey:=PopKey;
     exit;
   end;
{Wait for Key}
{ Only if none are waiting! (JM) }
  if not sysKeyPressed then
    begin
      FpFD_ZERO (FDS);
      fpFD_SET (0,FDS);
      fpSelect (1,@FDS,nil,nil,nil);
    end;

  ch:=ttyRecvChar;
  SavedEscapeStr:=ch;
{Esc Found ?}
  CASE ch OF
  #27: begin
     State:=1;
     num1:=-1;
     num2:=-1;
     kMod:=1;
     OPrefix:=false;
     Delay(10);
     { This has to be sysKeyPressed and not "keyPressed", since after }
     { one iteration keyPressed will always be true because of the    }
     { pushKey commands (JM)                                          }
     while (State<>0) and (sysKeyPressed) do
      begin
        ch:=ttyRecvChar;
        SavedEscapeStr:=SavedEscapeStr+ch;
        OldState:=State;
        State:=0;
        case OldState of
        1 : begin {Esc}
              case ch of
          'a'..'z',
          'A'..'N',
          'P'..'Z',
          '0'..'9',
           '-','=' : PushExt(FAltKey(ch));
               #10 : PushKey(#10);
                #9 : PushExt(kbShiftTab);  {#27#9}
              #127 : PushExt(kbAltBack);  {#27#127}
               #27 : State:=4;
               '[' : State:=5;
{$IFDEF Unix}
               'O' : if not (sysKeyPressed) then
                       PushExt(kbAltO)  {#27'[O'}
                     else
                       begin
                         OPrefix:=true;
                         State:=6;
                       end;
{$ENDIF}
               else
                 PushBackSavedStr;
               end;
            end;
        2 : begin
              case ch of
               ';':  begin num1:=num2; num2:=-1; State:=2; end;
               '0'..'9':  begin ProcessNum(ch); State:=2; end;
               '^' : begin kMod:=(kMod-1) or 4+1; PushExt(GetScanCodeValue(kMod,num1,num2)); end; {ctrl}
               '~' : begin  { none }
                       ScanCode:=GetScanCodeValue(kMod,num1,num2);
                       if ScanCode <> 1 then
                         if ScanCode <> 0 then
                           PushExt(ScanCode)
                         else
                           PushBackSavedStr
                       else
                         PushKey(#27);
                     end;
               '@' : begin kMod:=(kMod-1) or (4 or 1)+1; PushExt(GetScanCodeValue(kMod,num1,num2)); end; {ctrl-shift}
               '$' : begin kMod:=(kMod-1) or 1+1; PushExt(GetScanCodeValue(kMod,num1,num2)); end; {Shift}
               'A'..'H',
               'P'..'S': PushExt(GetScanCodeValue(kMod,ch));
              else
                PushBackSavedStr;
              end;
            end;
        4 : begin {Esc}
              case ch of
               'O':  begin State:=7; kMod:=(kMod-1) or 2+1; end;
               '[':  begin State:=8; kMod:=(kMod-1) or 2+1; end;
              else
                PushBackSavedStr;
              end;
            end;
        5 : begin {Esc[}
              case ch of
               '0'..'8':  begin ProcessNum(ch); State:=2; end;
               'A':  PushExt(kbUp);  {#27'[A'}
               'B':  PushExt(kbDown);  {#27'[B'}
               'C':  PushExt(kbRight);  {#27'[C'}
               'D':  PushExt(kbLeft);  {#27'[D'}
               'E':  PushExt(kbCenter);  {#27'[E'}
               'F':  PushExt(kbEnd);  {#27'[F'}
{$ifdef FREEBSD}
               'G':  PushExt(kbPgDn);  {#27'[G'}
{$else FREEBSD}
               'G':  PushExt(kbCenter);  {#27'[G'}
{$endif  FREEBSD}
               'H':  PushExt(kbHome);  {#27'[H'}
               'I':  PushExt(kbPgUp);  {#27'[I'}
               'M':  PushExt(kbF1);  {#27'[M'}
               'N':  PushExt(kbF2);  {#27'[N'}
               'O':  if not (sysKeyPressed) then
                       PushExt(kbF3)  {#27'[O'}
                     else
                       State:=14;
               'P':  if not isKitty then PushExt(kbF4) else PushExt(kbF1);  {#27'[P'}
               'Q':  if not isKitty then PushExt(kbF5) else PushExt(kbF2);  {#27'[Q'}
               'R':  PushExt(kbF6);  {#27'[R'}
               'S':  if not isKitty then PushExt(kbF7) else PushExt(kbF4);  {#27'[S'}
               'T':  PushExt(kbF8);  {#27'[T'}
               'U':  PushExt(kbF9);  {#27'[U'}
               'V':  PushExt(kbF10);  {#27'[V'}
               'W':  PushExt(kbF11);  {#27'[W'}
               'X':  PushExt(kbF12);  {#27'[X'}
               'Z':  PushExt(kbShiftTab);  {#27'[Z'}
               '[':  State:=15;
               'a':  PushExt(kbUp);  {#27'[a'}
               'b':  PushExt(kbDown);  {#27'[b'}
               'c':  PushExt(kbRight);  {#27'[c'}
               'd':  PushExt(kbLeft);  {#27'[d'}
              else
                PushBackSavedStr;
              end;
            end;
        6 : begin {EscO}
              case ch of
               '2'..'3',
               '5':  begin ProcessNum(ch); State:=2; end;
               'A':  PushExt(kbAltUp);  {#27'OA'}
               'B':  PushExt(kbAltDown);  {#27'OB'}
               'C':  PushExt(kbAltRight);  {#27'OC'}
               'D':  PushExt(kbLeft);  {#27'OD'}
               'F':  PushExt(kbEnd);  {#27'OF'}
               'H':  PushExt(kbHome);  {#27'OH'}
               'P':  PushExt(kbF1);  {#27'OP'}
               'Q':  PushExt(kbF2);  {#27'OQ'}
               'R':  PushExt(kbF3);  {#27'OR'}
               'S':  PushExt(kbF4);  {#27'OS'}
               'T':  PushExt(kbF5);  {#27'OT'}
               'U':  PushExt(kbF6);  {#27'OU'}
               'V':  PushExt(kbF7);  {#27'OV'}
               'W':  PushExt(kbF8);  {#27'OW'}
               'X':  PushExt(kbF9);  {#27'OX'}
               'Y':  PushExt(kbF10);  {#27'OY'}
               'Z':  PushExt(kbF11);  {#27'OZ'}
               '[':  PushExt(kbF12);  {#27'O['}
               'a':  PushExt(kbCtrlUp);  {#27'Oa'}
               'b':  PushExt(kbCtrlDown);  {#27'Ob'}
               'c':  PushExt(kbCtrlRight);  {#27'Oc'}
               'd':  PushExt(kbCtrlLeft);  {#27'Od'}
               'l':  PushExt(kbF8);  {#27'Ol'}
               'n':  PushExt(kbShiftDel);  {#27'On'}
               'p':  PushExt(kbShiftIns);  {#27'Op'}
               't':  PushExt(kbF5);  {#27'Ot'}
               'u':  PushExt(kbF6);  {#27'Ou'}
               'v':  PushExt(kbF7);  {#27'Ov'}
               'w':  PushExt(kbF9);  {#27'Ow'}
               'x':  PushExt(kbF10);  {#27'Ox'}
               'y':  PushExt(kbF11);  {#27'Oy'}
               'z':  PushExt(kbF12);  {#27'Oz'}
              else
                PushBackSavedStr;
              end;
            end;
        7 : begin {O}
              case ch of
               'A':  PushExt(kbAltUp);  {#27#27'OA'}
               'B':  PushExt(kbAltDown);  {#27#27'OB'}
               'C':  PushExt(kbAltRight);  {#27#27'OC'}
               'D':  PushExt(kbAltLeft);  {#27#27'OD'}
               'P':  PushExt(kbAltF1);  {#27#27'OP'}
               'Q':  PushExt(kbAltF2);  {#27#27'OQ'}
               'R':  PushExt(kbAltF3);  {#27#27'OR'}
               'S':  PushExt(kbAltF4);  {#27#27'OS'}
               'T':  PushExt(kbAltF5);  {#27#27'OT'}
               'l':  PushExt(kbAltF8);  {#27#27'Ol'}
               't':  PushExt(kbAltF5);  {#27#27'Ot'}
               'u':  PushExt(kbAltF6);  {#27#27'Ou'}
               'v':  PushExt(kbAltF7);  {#27#27'Ov'}
               'w':  PushExt(kbAltF9);  {#27#27'Ow'}
               'x':  PushExt(kbAltF10);  {#27#27'Ox'}
               'y':  PushExt(kbAltF11);  {#27#27'Oy'}
               'z':  PushExt(kbAltF12);  {#27#27'Oz'}
              else
                PushBackSavedStr;
              end;
            end;
        8 : begin {[}
              case ch of
               '1'..'8':  begin ProcessNum(ch); State:=2; end;
               'A':  PushExt(kbAltUp);  {#27#27'[A'}
               'B':  PushExt(kbAltDown);  {#27#27'[B'}
               'C':  PushExt(kbAltRight);  {#27#27'[C'}
               'D':  PushExt(kbAltLeft);  {#27#27'[D'}
               '[':  State:=10;
               'a':  PushExt(kbAltUp);  {#27#27'[a'}
               'b':  PushExt(kbAltDown);  {#27#27'[b'}
               'c':  PushExt(kbAltRight);  {#27#27'[c'}
               'd':  PushExt(kbAltLeft);  {#27#27'[d'}
              else
                PushBackSavedStr;
              end;
            end;
        10 : begin {[}
              case ch of
               'A':  PushExt(kbAltF1);  {#27#27'[[A'}
               'B':  PushExt(kbAltF2);  {#27#27'[[B'}
               'C':  PushExt(kbAltF3);  {#27#27'[[C'}
               'D':  PushExt(kbAltF4);  {#27#27'[[D'}
               'E':  PushExt(kbAltF5);  {#27#27'[[E'}
              else
                PushBackSavedStr;
              end;
            end;
        14 : begin {[O}
              case ch of
               'a':  PushExt(kbCtrlUp);  {#27'[Oa'}
               'b':  PushExt(kbCtrlDown);  {#27'[Ob'}
               'c':  PushExt(kbCtrlRight);  {#27'[Oc'}
               'd':  PushExt(kbCtrlLeft);  {#27'[Od'}
              else
                PushBackSavedStr;
              end;
            end;
        15 : begin {[[}
              case ch of
               'A':  PushExt(kbF1);  {#27'[[A'}
               'B':  PushExt(kbF2);  {#27'[[B'}
               'C':  PushExt(kbF3);  {#27'[[C'}
               'D':  PushExt(kbF4);  {#27'[[D'}
               'E':  PushExt(kbF5);  {#27'[[E'}
              else
                PushBackSavedStr;
              end;
            end;
      255 : ;
        end;
        if State<>0 then
         Delay(10);
      end;
     if State=1 then
      PushKey(ch);
   end;
  #127: PushKey(#8);
  else PushKey(ch);
  End;
  ReadKey:=PopKey;
End;


Procedure Delay(MS: Word);
{
  Wait for DTime milliseconds.
}
Begin
  fpSelect(0,nil,nil,nil,MS);
End;


{****************************************************************************
                        Write(ln)/Read(ln) support
****************************************************************************}

procedure DoLn;
begin
  if CurrY=WindMaxY then
   begin
     if FullWin then
      begin
        ttySendStr(#10#13);
        CurrX:=WindMinX;
        CurrY:=WindMaxY;
      end
     else
      begin
        ScrollScrnRegionUp(WindMinX,WindMinY,WindMaxX,WindMaxY,1);
        ttyGotoXY(WindMinX,WindMaxY);
      end;
   end
  else
   ttyGotoXY(WindMinX,CurrY+1);
end;


var
  Lastansi  : boolean;
  AnsiCode  : shortstring;
Procedure DoWrite(const s:shortstring);
{
  Write string to screen, parse most common AnsiCodes
}
var
  found,
  OldFlush  : boolean;
  x,y,
  i,j,
  SendBytes : longint;

  function AnsiPara(var hstr:shortstring):byte;
  var
    k,j  : longint;
    code : word;
  begin
    j:=pos(';',hstr);
    if j=0 then
     j:=length(hstr);
    val(copy(hstr,3,j-3),k,code);
    Delete(hstr,3,j-2);
    if k=0 then
     k:=1;
    AnsiPara:=k;
  end;

  procedure SendText;
  var
    LeftX : longint;
  begin
    while (SendBytes>0) do
     begin
       LeftX:=WindMaxX-CurrX+1;
       if (SendBytes>=LeftX) then
        begin
          ttyWrite(Copy(s,i-SendBytes,LeftX));
          dec(SendBytes,LeftX);
          DoLn;
        end
       else
        begin
          ttyWrite(Copy(s,i-SendBytes,SendBytes));
          SendBytes:=0;
        end;
     end;
  end;

begin
  oldflush:=ttySetFlush(Flushing);
{ Support textattr:= changing }
  if OldTextAttr<>TextAttr then
   begin
     i:=TextAttr;
     TextAttr:=OldTextAttr;
     ttyColor(i);
   end;
{ write the stuff }
  SendBytes:=0;
  i:=1;
  while (i<=length(s)) do
   begin
     if (s[i]=#27) or (LastAnsi) then
      begin
        SendText;
        LastAnsi:=false;
        j:=i;
        found:=false;
        while (j<=length(s)) and (not found) do
         begin
           found:=not (s[j] in [#27,'[','0'..'9',';','?']);
           inc(j);
         end;
        Ansicode:=AnsiCode+Copy(s,i,j-i);
        if found then
         begin
           case AnsiCode[length(AnsiCode)] of
            'm' : ttyColor(Ansi2Attr(AnsiCode,TextAttr));
            'H' : begin {No other way :( Coz First Para=Y}
                    y:=AnsiPara(AnsiCode);
                    x:=AnsiPara(AnsiCode);
                    GotoXY(x,y);
                  end;
            'J' : if AnsiPara(AnsiCode)=2 then
                   ClrScr;
            'K' : ClrEol;
            'A' : GotoXY(CurrX,Max(CurrY-AnsiPara(AnsiCode),WindMinY));
            'B' : GotoXY(CurrX,Min(CurrY+AnsiPara(AnsiCode),WindMaxY));
            'C' : GotoXY(Min(CurrX+AnsiPara(AnsiCode),WindMaxX),CurrY);
            'D' : GotoXY(Max(CurrX-AnsiPara(AnsiCode),WindMinX),CurrY);
            'h' : ; {Stupid Thedraw [?7h Code}
           else
            found:=false;
           end;
         end
        else
         begin
           LastAnsi:=true;
           found:=true;
         end;
      {Clear AnsiCode?}
        if not LastAnsi then
         AnsiCode:='';
      {Increase Idx or SendBytes}
        if found then
         i:=j-1
        else
         inc(SendBytes);
      end
     else
      begin
        LastAnsi:=false;
        case s[i] of
         #13 : begin {CR}
                 SendText;
                 ttyGotoXY(WindMinX,CurrY);
               end;
         #10 : begin {NL}
                 SendText;
                 DoLn;
               end;
          #9 : begin {Tab}
                 SendText;
                 ttyWrite(Space(9-((CurrX-1) and $08)));
               end;
          #8 : begin {BackSpace}
                 SendText;
                 ttyWrite(#8);
               end;
        else
         inc(SendBytes);
        end;
      end;
     inc(i);
   end;
  if SendBytes>0 then
   SendText;
  ttySetFlush(oldFLush);
end;


Procedure CrtWrite(Var F: TextRec);
{
  Top level write function for CRT
}
Var
  Temp : shortstring;
  idx,i : Longint;
  oldflush : boolean;
Begin
  oldflush:=ttySetFlush(Flushing);
  idx:=0;
  while (F.BufPos>0) do
   begin
     i:=F.BufPos;
     if i>255 then
      i:=255;
     SetLength(Temp,i);
     Move(F.BufPTR^[idx],Temp[1],i);
     DoWrite(Temp);
     dec(F.BufPos,i);
     inc(idx,i);
   end;

  ttySetFlush(oldFLush);
End;


Procedure CrtRead(Var F: TextRec);
{
  Read from CRT associated file.
}
var
  c : AnsiChar;
  i : longint;
Begin
  if isATTY(F.Handle)=1 then
    begin
      F.BufPos := 0;
      i := 0;
      repeat
        c := readkey;
        case c of
          { ignore special keys }
          #0:
            c:= readkey;
          { Backspace }
          #8:
            if i > 0 then
              begin
                if not(OutputRedir or InputRedir) then
                  write(#8#32#8);
                dec(i);
              end;
          { Unhandled extended key }
          #27:;
          { CR }
          #13:
            begin
              F.BufPtr^[i] := #10;
              if not(OutputRedir or InputRedir) then
                write(#10);
              inc(i);
            end;
          else
            begin
              if not(OutputRedir or InputRedir) then
                write(c);
              F.BufPtr^[i] := c;
              inc(i);
            end;
        end;
      until (c in [#10,#13]) or (i >= F.BufSize);
      F.BufEnd := i;
      exit;
    end;
  F.BufEnd:=fpRead(F.Handle, F.BufPtr^, F.BufSize);
{ fix #13 only's -> #10 to overcome terminal setting }
  for i:=1to F.BufEnd do
   begin
     if (F.BufPtr^[i-1]=#13) and (F.BufPtr^[i]<>#10) then
      F.BufPtr^[i-1]:=#10;
   end;
  F.BufPos:=F.BufEnd;
  if not(OutputRedir or InputRedir) then
    CrtWrite(F)
  else F.BufPos := 0;
End;


Procedure CrtReturn(Var F:TextRec);
Begin
end;


Procedure CrtClose(Var F: TextRec);
{
  Close CRT associated file.
}
Begin
  F.Mode:=fmClosed;
End;


Procedure CrtOpen(Var F: TextRec);
{
  Open CRT associated file.
}
Begin
  If F.Mode=fmOutput Then
   begin
     TextRec(F).InOutFunc:=@CrtWrite;
     TextRec(F).FlushFunc:=@CrtWrite;
   end
  Else
   begin
     F.Mode:=fmInput;
     TextRec(F).InOutFunc:=@CrtRead;
     TextRec(F).FlushFunc:=@CrtReturn;
   end;
  TextRec(F).CloseFunc:=@CrtClose;
End;


procedure AssignCrt(var F: Text);
{
  Assign a file to the console. All output on file goes to console instead.
}
begin
  Assign(F,'');
  TextRec(F).OpenFunc:=@CrtOpen;
end;


{******************************************************************************
                            High Level Functions
******************************************************************************}

Procedure DelLine;
{
  Delete current line. Scroll subsequent lines up
}
Begin
  ScrollScrnRegionUp(WindMinX, CurrY, WindMaxX, WindMaxY, 1);
End;



Procedure InsLine;
{
  Insert line at current cursor position. Scroll subsequent lines down.
}
Begin
  ScrollScrnRegionDown(WindMinX, CurrY, WindMaxX, WindMaxY, 1);
End;

{$ifdef linux}
  {$define havekiocsound}
   const  KIOCSOUND = $4B2F;    // start sound generation (0 for off)
{$else}
 {$ifdef FreeBSD}
   const  KIOCSOUND =$20004b3f;
   {$define havekiocsound}
 {$endif}
{$endif}

// ioctl might fail e.g. in putty. A redirect check is not enough,
// needs check for physical console too.

Procedure Sound(Hz: Word);
begin
{$ifdef havekiocsound}
  if (not OutputRedir) and (hz>0) then
    fpIoctl(TextRec(Output).Handle, KIOCSOUND, Pointer(1193180 div Hz));
{$endif}
end;

Procedure NoSound;
begin
{$ifdef havekiocsound}
  if not OutputRedir then
    fpIoctl(TextRec(Output).Handle, KIOCSOUND, nil);
{$endif}
end;

Procedure TextMode (Mode: word);
{
  Only Clears Screen under linux}
begin
  ClrScr;
end;


{******************************************************************************
                                     Extra
******************************************************************************}

procedure CursorBig;
begin
  ttySendStr(#27'[?17;0;64c');
end;


procedure CursorOn;
begin
  ttySendStr(#27'[?2c');
end;


procedure CursorOff;
begin
  ttySendStr(#27'[?1c');
end;


{******************************************************************************
                               Initialization
******************************************************************************}

var
  OldIO : {$IFDEF FPC_DOTTEDUNITS}UnixApi.{$ENDIF}TermIo.TermIos;
  inputRaw, outputRaw: boolean;

procedure saveRawSettings(const tio: {$IFDEF FPC_DOTTEDUNITS}UnixApi.{$ENDIF}TermIo.termios);
Begin
  with tio do
   begin
     inputRaw :=
       ((c_iflag and (IGNBRK or BRKINT or PARMRK or ISTRIP or
                                INLCR or IGNCR or ICRNL or IXON)) = 0) and
       ((c_lflag and (ECHO or ECHONL or ICANON or ISIG or IEXTEN)) = 0);
     outPutRaw :=
       ((c_oflag and OPOST) = 0) and
       ((c_cflag and (CSIZE or PARENB)) = 0) and
       ((c_cflag and CS8) <> 0);
   end;
end;

procedure restoreRawSettings(tio: {$IFDEF FPC_DOTTEDUNITS}UnixApi.{$ENDIF}TermIo.termios);
begin
  with tio do
    begin
      if inputRaw then
        begin
          c_iflag := c_iflag and (not (IGNBRK or BRKINT or PARMRK or ISTRIP or
            INLCR or IGNCR or ICRNL or IXON));
          c_lflag := c_lflag and
            (not (ECHO or ECHONL or ICANON or ISIG or IEXTEN));
       end;
     if outPutRaw then
       begin
         c_oflag := c_oflag and not(OPOST);
         c_cflag := c_cflag and not(CSIZE or PARENB) or CS8;
       end;
   end;
end;


Procedure SetRawMode(b:boolean);
Var
  Tio : Termios;
Begin
  if b then
   begin
     TCGetAttr(1,Tio);
     SaveRawSettings(Tio);
     OldIO:=Tio;
     CFMakeRaw(Tio);
   end
  else
   begin
     RestoreRawSettings(OldIO);
     Tio:=OldIO;
   end;
  TCSetAttr(1,TCSANOW,Tio);
End;



procedure GetXY(var x,y:byte);
var
  fds    : tfdSet;
  i,j,
  readed : longint;
  buf    : array[0..255] of AnsiChar;
  s      : string[16];
begin
  x:=0;
  y:=0;
  s:=#27'[6n';
  fpWrite(0,s[1],length(s));
  fpFD_ZERO(fds);
  fpFD_SET(1,fds);
  readed:=0;
  repeat
    if (fpSelect(2,@fds,nil,nil,1000)>0) then
     begin
       readed:=readed+fpRead(1,buf[readed],sizeof(buf)-readed);
       i:=0;
       while (i+5<readed) and (buf[i]<>#27) and (buf[i+1]<>'[') do
        inc(i);
       if i+5<readed then
        begin
          s:=space(16);
          move(buf[i+2],s[1],16);
          j:=Pos('R',s);
          if j>0 then
           begin
             i:=Pos(';',s);
             Val(Copy(s,1,i-1),y);
             Val(Copy(s,i+1,j-(i+1)),x);
             break;
           end;
        end;
     end
    else
      break;
  until false;
end;


Procedure GetConsoleBuf;
var
  WinInfo : TWinSize;
begin
  if Assigned(ConsoleBuf) then
    FreeMem(ConsoleBuf,ScreenHeight*ScreenWidth*2);
  ScreenWidth:=0;
  ScreenHeight:=0;
  if (not OutputRedir) and (fpIOCtl(TextRec(Output).Handle,TIOCGWINSZ,@Wininfo)>=0) then
    begin
    ScreenWidth:=Wininfo.ws_col;
    ScreenHeight:=Wininfo.ws_row;
    end;
  // Set some arbitrary defaults which make some sense...
  If (ScreenWidth=0) then
     ScreenWidth:=80;
  If (ScreenHeight=0) then
     ScreenHeight:=25;
  GetMem(ConsoleBuf,ScreenHeight*ScreenWidth*2);
  FillChar(ConsoleBuf^,ScreenHeight*ScreenWidth*2,0);
end;


Initialization
{$ifdef debugcrt}
  Assign(DebugFile,'debug.txt');
  ReWrite(DebugFile);
{$endif}
{ Redirect the standard output }
  assigncrt(Output);
  Rewrite(Output);
  TextRec(Output).Handle:=StdOutputHandle;
  assigncrt(Input);
  Reset(Input);
  TextRec(Input).Handle:=StdInputHandle;
{ Are we redirected to a file ? }
  OutputRedir:= IsAtty(TextRec(Output).Handle)<>1;
{ does the input come from another console or from a file? }
  InputRedir :=
   (IsAtty(TextRec(Input).Handle)<>1) or
   (not OutputRedir and
    (TTYName(TextRec(Input).Handle) <> TTYName(TextRec(Output).Handle)));
{ Get Size of terminal and set WindMax to the window }
  GetConsoleBuf;
  WindMinX:=1;
  WindMinY:=1;
  WindMaxX:=ScreenWidth;
  WindMaxY:=ScreenHeight;
  WindMax:=((ScreenHeight-1) Shl 8)+(ScreenWidth-1);
{Get Current X&Y or Reset to Home}
  if OutputRedir then
   begin
     CurrX:=1;
     CurrY:=1;
   end
  else
   begin
   { Set default Terminal Settings }
     SetRawMode(True);
   { Get current X,Y if not set already }
     GetXY(CurrX,CurrY);
     if (CurrX=0) then
      begin
        CurrX:=1;
        CurrY:=1;
        ttySendStr(#27'[H');
      end;
   {Reset Attribute (TextAttr=7 at startup)}
      ttySendStr(#27'[m');
    end;
  AdjustmentForRxvtTerminal;
  isKitty:=lowercase(copy(fpgetenv('TERM'),1,11))='xterm-kitty';

Finalization
{$ifdef debugcrt}
  Close(DebugFile);
{$endif}
  ttyFlushOutput;
  if not OutputRedir then
    SetRawMode(False);
{ remove console buf }
  if Assigned(ConsoleBuf) then
   FreeMem(ConsoleBuf,ScreenHeight*ScreenWidth*2);

End.
