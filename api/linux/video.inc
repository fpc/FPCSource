{
  System independent low-level video interface for linux

  $Id$
}
uses
  Linux, Strings, FileCtrl, TermInfo;

var
  LastCursorType : byte;
  TtyFd: Longint;
  Console: Boolean;
  OldVideoBuf: PVideoBuf;

{$ASMMODE ATT}

procedure SendEscapeSeqNdx(Ndx: Word);
var
  P: PChar;
begin
  P:=cur_term^.ttype.Strings[Ndx];
  if assigned(p) then
   fdWrite(TTYFd, P^, StrLen(P));
end;


procedure SendEscapeSeq(const S: String);
begin
  fdWrite(TTYFd, S[1], Length(S));
end;


Function IntStr(l:longint):string;
var
  s : string;
begin
  Str(l,s);
  IntStr:=s;
end;


Function XY2Ansi(x,y,ox,oy:longint):String;
{
  Returns a string with the escape sequences to go to X,Y on the screen
}
Begin
  if y=oy then
   begin
     if x=ox then
      begin
        XY2Ansi:='';
        exit;
      end;
     if x=1 then
      begin
        XY2Ansi:=#13;
        exit;
      end;
     if x>ox then
      begin
        XY2Ansi:=#27'['+IntStr(x-ox)+'C';
        exit;
      end
     else
      begin
        XY2Ansi:=#27'['+IntStr(ox-x)+'D';
        exit;
      end;
   end;
  if x=ox then
   begin
     if y>oy then
      begin
        XY2Ansi:=#27'['+IntStr(y-oy)+'B';
        exit;
      end
     else
      begin
        XY2Ansi:=#27'['+IntStr(oy-y)+'A';
        exit;
      end;
   end;
  if (x=1) and (oy+1=y) then
   XY2Ansi:=#13#10
  else
   XY2Ansi:=#27'['+IntStr(y)+';'+IntStr(x)+'H';
End;



const
  AnsiTbl : string[8]='04261537';
Function Attr2Ansi(Attr,OAttr:longint):string;
{
  Convert Attr to an Ansi String, the Optimal code is calculate
  with use of the old OAttr
}
var
  hstr : string[16];
  OFg,OBg,Fg,Bg : longint;

  procedure AddSep(ch:char);
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
  OFg:=Attr and $f;
  OBg:=Attr shr 4;
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


procedure UpdateTTY(Force:boolean);
type
  tchattr=packed record
    ch : char;
    attr : byte;
  end;
var
  outbuf   : array[0..1023+255] of char;
  chattr   : tchattr;
  skipped  : boolean;
  outptr,
  spaces,
  eol,
  LastX,LastY,
  x,y,
  SpaceAttr,
  LastAttr : longint;
  p,pold   : pvideocell;

  procedure outdata(hstr:string);
  begin
    while (eol>0) do
     begin
       hstr:=#13#10+hstr;
       dec(eol);
     end;
    move(hstr[1],outbuf[outptr],length(hstr));
    inc(outptr,length(hstr));
    if outptr>1024 then
     begin
       fdWrite(TTYFd,outbuf,outptr);
       outptr:=0;
     end;
  end;

  procedure OutClr(c:byte);
  begin
    if c=LastAttr then
     exit;
    OutData(Attr2Ansi(c,LastAttr));
    LastAttr:=c;
  end;

  procedure OutSpaces;
  begin
    if (Spaces=0) then
     exit;
    OutClr(SpaceAttr);
    OutData(Space(Spaces));
    LastX:=x;
    LastY:=y;
    Spaces:=0;
  end;

begin
  OutPtr:=0;
  Eol:=0;
  skipped:=true;
  p:=PVideoCell(VideoBuf);
  pold:=PVideoCell(OldVideoBuf);
{ init Attr and X,Y }
  OutData(#27'[m'#27'[H');
  LastAttr:=7;
  LastX:=1;
  LastY:=1;
  for y:=1 to ScreenHeight do
   begin
     SpaceAttr:=0;
     Spaces:=0;
     for x:=1 to ScreenWidth do
      begin
        if (not force) and (p^=pold^) then
         begin
           if (Spaces>0) then
            OutSpaces;
           skipped:=true;
         end
        else
         begin
           if skipped then
            begin
              OutData(XY2Ansi(x,y,LastX,LastY));
              LastX:=x;
              LastY:=y;
              skipped:=false;
            end;
           chattr:=tchattr(p^);
           if chattr.ch in [#0,#255] then
            chattr.ch:=' ';
           if chattr.ch=' ' then
            begin
              if Spaces=0 then
               SpaceAttr:=chattr.Attr;
              if (chattr.attr and $f0)=(spaceattr and $f0) then
               chattr.Attr:=SpaceAttr
              else
               begin
                 OutSpaces;
                 SpaceAttr:=chattr.Attr;
               end;
              inc(Spaces);
            end
           else
            begin
              if (Spaces>0) then
               OutSpaces;
              if LastAttr<>chattr.Attr then
               OutClr(chattr.Attr);
              OutData(chattr.ch);
              LastX:=x+1;
              LastY:=y;
            end;
           p^:=tvideocell(chattr);
         end;
        inc(p);
        inc(pold);
      end;
     if (Spaces>0) then
      OutSpaces;
     if force then
      inc(eol);
   end;
  eol:=0;
  OutData(XY2Ansi(CursorX,CursorY,LastX,LastY));
  fdWrite(TTYFd,outbuf,outptr);
end;


procedure InitVideo;
const
  fontstr : string[3]=#27'(K';
var
  ThisTTY: String[30];
  FName: String;
  WS: packed record
    ws_row, ws_col, ws_xpixel, ws_ypixel: Word;
  end;
  Err: Longint;
begin
  LowAscii:=false;
  if VideoBufSize<>0 then
   DoneVideo;
  { check for tty }
  ThisTTY:=TTYName(stdin);
  if IsATTY(stdin) then
   begin
     { write code to set a correct font }
     fdWrite(stdout,fontstr[1],length(fontstr));
     { running on a tty, find out whether locally or remotely }
     if (Copy(ThisTTY, 1, 8) = '/dev/tty') and
        (ThisTTY[9] >= '0') and (ThisTTY[9] <= '9') then
      begin
        { running on the console }
        FName:='/dev/vcsa' + ThisTTY[9];
        TTYFd:=OpenFile(FName, filReadWrite); { open console }
      end
     else
      TTYFd:=-1;
     if TTYFd<>-1 then
      Console:=true
     else
      begin
        { running on a remote terminal, no error with /dev/vcsa }
        Console:=False;
        TTYFd:=stdout;
      end;
     ioctl(stdin, TIOCGWINSZ, @WS);
     ScreenWidth:=WS.ws_Col;
     ScreenHeight:=WS.ws_Row;
     if WS.ws_Col=0 then
      WS.ws_Col:=80;
     if WS.ws_Row=0 then
      WS.ws_Row:=25;
     CursorX:=1;
     CursorY:=1;
     ScreenColor:=True;
     { allocate pmode memory buffer }
     VideoBufSize:=ScreenWidth*ScreenHeight*2;
     GetMem(VideoBuf,VideoBufSize);
     GetMem(OldVideoBuf,VideoBufSize);
     { Start with a clear screen }
     if not Console then
      begin
        setupterm(nil, stdout, err);
        SendEscapeSeqNdx(cursor_home);
        SendEscapeSeqNdx(cursor_normal);
        SendEscapeSeqNdx(cursor_visible);
        SendEscapeSeqNdx(enter_ca_mode);
        SetCursorType(crUnderLine);
      end;
     ClearScreen;
   end
  else
   ErrorCode:=errVioInit; { not a TTY }
end;

procedure DoneVideo;
begin
  if VideoBufSize=0 then
   exit;
  ClearScreen;
  if Console then
   SetCursorPos(1,1)
  else
   begin
     SendEscapeSeqNdx(exit_ca_mode);
     SendEscapeSeqNdx(cursor_home);
     SendEscapeSeqNdx(cursor_normal);
     SendEscapeSeqNdx(cursor_visible);
     SetCursorType(crUnderLine);
     SendEscapeSeq(#27'[H');
   end;
  FreeMem(VideoBuf,VideoBufSize);
  FreeMem(OldVideoBuf,VideoBufSize);
  VideoBufSize:=0;
end;


procedure ClearScreen;
begin
  FillWord(VideoBuf^,VideoBufSize shr 1,$0720);
  if Console then
   UpdateScreen(true)
  else
   begin
     SendEscapeSeq(#27'[0m');
     SendEscapeSeqNdx(clear_screen);
   end;
end;


procedure UpdateScreen(Force: Boolean);
var
  DoUpdate : boolean;
begin
  if LockUpdateScreen<>0 then
   exit;
  if not force then
   begin
{$ifdef i386}
     asm
          movl    VideoBuf,%esi
          movl    OldVideoBuf,%edi
          movl    VideoBufSize,%ecx
          shrl    $2,%ecx
          repe
          cmpsl
          orl     %ecx,%ecx
          setne   DoUpdate
     end;
{$endif i386}
   end
  else
   DoUpdate:=true;
  if not DoUpdate then
   exit;
  if Console then
   begin
     fdSeek(TTYFd, 4, skBeg);
     fdWrite(TTYFd, VideoBuf^,VideoBufSize);
   end
  else
   begin
     UpdateTTY(force);
   end;
  Move(VideoBuf^, OldVideoBuf^, VideoBufSize);
end;


function GetCapabilities: Word;
begin
{ about cpColor... we should check the terminfo database... }
  GetCapabilities:=cpUnderLine + cpBlink + cpColor;
end;


procedure SetCursorPos(NewCursorX, NewCursorY: Word);
var
  Pos : array [1..2] of Byte;
begin
  if Console then
   begin
     fdSeek(TTYFd, 2, skBeg);
     Pos[1]:=NewCursorX;
     Pos[2]:=NewCursorY;
     fdWrite(TTYFd, Pos, 2);
   end
  else
   begin
     { newcursorx,y is 0 based ! }
     SendEscapeSeq(XY2Ansi(NewCursorX+1,NewCursorY+1,0,0));
   end;
  CursorX:=NewCursorX+1;
  CursorY:=NewCursorY+1;
end;


function GetCursorType: Word;
begin
  GetCursorType:=LastCursorType;
end;


procedure SetCursorType(NewType: Word);
begin
  LastCursorType:=NewType;
  case NewType of
   crBlock :
     SendEscapeSeq(#27'[?17;0;64c');
   crHidden :
     SendEscapeSeq(#27'[?1c');
  else
    SendEscapeSeq(#27'[?2c');
  end;
end;


function DefaultVideoModeSelector(const VideoMode: TVideoMode; Params: Longint): Boolean;
begin
  DefaultVideoModeSelector:=false;
end;


procedure RegisterVideoModes;
begin
end;

{
  $Log$
  Revision 1.2  2000-03-12 15:02:10  peter
    * removed unused var

  Revision 1.1  2000/01/06 01:20:31  peter
    * moved out of packages/ back to topdir

  Revision 1.1  1999/11/24 23:36:38  peter
    * moved to packages dir

  Revision 1.5  1999/07/05 21:38:19  peter
    * works now also on not /dev/tty* units
    * if col,row is 0,0 then take 80x25 by default

  Revision 1.4  1999/02/22 12:46:16  peter
    + lowascii boolean if ascii < #32 is handled correctly

  Revision 1.3  1999/02/08 10:34:26  peter
    * cursortype futher implemented

  Revision 1.2  1998/12/12 19:13:03  peter
    * keyboard updates
    * make test target, make all only makes units

  Revision 1.1  1998/12/04 12:48:30  peter
    * moved some dirs

  Revision 1.6  1998/12/03 10:18:07  peter
    * tty fixed

  Revision 1.5  1998/12/01 15:08:17  peter
    * fixes for linux

  Revision 1.4  1998/11/01 20:29:12  peter
    + lockupdatescreen counter to not let updatescreen() update

  Revision 1.3  1998/10/29 12:49:50  peter
    * more fixes

  Revision 1.1  1998/10/26 11:31:47  peter
    + inital include files

}
