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
{$ifdef logging}
  f: file;

const
  logstart: string = '';
  nl: char = #10;
  logend: string = #10#10;
{$endif logging}
{$ASMMODE ATT}
const
  can_delete_term : boolean = false;

procedure SendEscapeSeqNdx(Ndx: Word);
var
  P,pdelay: PChar;
begin
  if not assigned(cur_term_Strings) then
    RunError(219);
  P:=cur_term_Strings^[Ndx];
  if assigned(p) then
   begin { Do not transmit the delays }
     pdelay:=strpos(p,'$<');
     if assigned(pdelay) then
       pdelay^:=#0;
     fdWrite(TTYFd, P^, StrLen(P));
     if assigned(pdelay) then
       pdelay^:='$';
   end;
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
    if outptr>=1024 then
     begin
{$ifdef logging}
       blockwrite(f,logstart[1],length(logstart));
       blockwrite(f,nl,1);
       blockwrite(f,outptr,sizeof(outptr));
       blockwrite(f,nl,1);
       blockwrite(f,outbuf,outptr);
       blockwrite(f,nl,1);
{$endif logging}
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
              if ord(chattr.ch)<32 then
                begin
                  Chattr.Attr:= $ff xor Chattr.Attr;
                  ChAttr.ch:= chr(ord(chattr.ch)+$30);
                end;
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
{$ifdef logging}
  blockwrite(f,logstart[1],length(logstart));
  blockwrite(f,nl,1);
  blockwrite(f,outptr,sizeof(outptr));
  blockwrite(f,nl,1);
  blockwrite(f,outbuf,outptr);
  blockwrite(f,nl,1);
{$endif logging}
  fdWrite(TTYFd,outbuf,outptr);
end;

var
  InitialVideoTio, preInitVideoTio, postInitVideoTio: linux.termios;
  inputRaw, outputRaw: boolean;

procedure saveRawSettings(const tio: linux.termios);
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

procedure restoreRawSettings(tio: linux.termios);
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
  TCSetAttr(1,TCSANOW,tio);
end;

procedure TargetEntry;
begin
  TCGetAttr(1,InitialVideoTio);
end;

procedure TargetExit;
begin
  TCSetAttr(1,TCSANOW,InitialVideoTio);
end;

procedure prepareInitVideo;
begin
  TCGetAttr(1,preInitVideoTio);
  saveRawSettings(preInitVideoTio);
end;

procedure videoInitDone;
begin
  TCGetAttr(1,postInitVideoTio);
  restoreRawSettings(postInitVideoTio);
end;

procedure prepareDoneVideo;
var
  tio: linux.termios;
begin
  TCGetAttr(1,tio);
  saveRawSettings(tio);
  TCSetAttr(1,TCSANOW,postInitVideoTio);
end;

procedure doneVideoDone;
begin
  restoreRawSettings(preInitVideoTio);
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
  prev_term : TerminalCommon_ptr1;
begin
{$ifndef CPUI386}
  LowAscii:=false;
{$endif CPUI386}
  if VideoBufSize<>0 then
   begin
     clearscreen;
     if Console then
      SetCursorPos(1,1)
     else
      begin
        SendEscapeSeqNdx(cursor_home);
        SendEscapeSeq(#27'[H');
      end;
     exit;
   end;
  { check for tty }
  ThisTTY:=TTYName(stdin);
  if IsATTY(stdin) then
   begin
     { save current terminal characteristics and remove rawness }
     prepareInitVideo;
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
        LowAscii:=false;
        TTYFd:=stdout;
      end;
     ioctl(stdin, TIOCGWINSZ, @WS);
     if WS.ws_Col=0 then
      WS.ws_Col:=80;
     if WS.ws_Row=0 then
      WS.ws_Row:=25;
     ScreenWidth:=WS.ws_Col;
     { TDrawBuffer only has FVMaxWidth elements
       larger values lead to crashes }
     if ScreenWidth> FVMaxWidth then
       ScreenWidth:=FVMaxWidth;
     ScreenHeight:=WS.ws_Row;
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
        prev_term:=cur_term;
        setupterm(nil, stdout, err);
        can_delete_term:=assigned(prev_term) and (prev_term<>cur_term);
        SendEscapeSeqNdx(cursor_home);
        SendEscapeSeqNdx(cursor_normal);
        SendEscapeSeqNdx(cursor_visible);
        SendEscapeSeqNdx(enter_ca_mode);
        SetCursorType(crUnderLine);
      end
     else if not assigned(cur_term) then
       begin
         setupterm(nil, stdout, err);
         can_delete_term:=false;
       end;
     ClearScreen;
{$ifdef logging}
     assign(f,'video.log');
     rewrite(f,1);
{$endif logging}
     { save new terminal characteristics and possible restore rawness }
     videoInitDone;
   end
  else
   ErrorCode:=errVioInit; { not a TTY }
end;

procedure DoneVideo;
begin
  if VideoBufSize=0 then
   exit;
  prepareDoneVideo;
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
  doneVideoDone;
  if can_delete_term then
    begin
      del_curterm(cur_term);
      can_delete_term:=false;
    end;
{$ifdef logging}
  close(f);
{$endif logging}
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
  Revision 1.2  2000-10-26 23:08:48  peter
    * merged freebsd from fixes

  Revision 1.1.2.1  2000/10/25 12:23:20  marco
   * Linux dir split up

  Revision 1.1.2.11  2000/10/19 07:28:18  pierre
   * do not transmit the delay part in terminfo strings

  Revision 1.1.2.10  2000/10/13 15:09:40  pierre
   * Handle zero size for term correctly

  Revision 1.1.2.9  2000/10/10 16:39:44  pierre
   + transform low ascii chars by changing their colors and adding 48

  Revision 1.1.2.8  2000/10/10 15:34:58  pierre
   * fixe a bug in Attr2Ansi

  Revision 1.1.2.7  2000/10/10 10:52:56  pierre
   + FVMaxWidth to avoid too wide screens

  Revision 1.1.2.6  2000/10/09 21:57:42  pierre
   * Set LowAscii to false only if not on a local tty

  Revision 1.1.2.5  2000/10/09 16:29:15  pierre
   * more linux terminal fixes

  Revision 1.1.2.4  2000/10/04 11:44:33  pierre
   add TargetEntry and TargetExit procedures (needed for linux)

  Revision 1.1.2.3  2000/10/03 22:31:29  pierre
   * avoid invalid cur_term var

  Revision 1.1.2.2  2000/09/25 13:21:19  jonas
    + added preserving of rawness of terminal when going though
      init/donevideo
    * del_term() is now called in donevideo
    * if initvideo is called while the video is already iniialized, the
      screen is cleared and the cursor is set home, instead of going
      through the whole donevideo and then initvideo

  Revision 1.1.2.1  2000/08/02 12:29:06  jonas
    * fixed crashes under ncurses 4 by adding auto-detection for ncurses 4/5
    * cur_term is not directly usable anymore for the largest part because
      of a different record layout in ncurses 4/5, therefore the pointers
      cur_term_booleans, cur_term_numbers, cur_term_strings and
      cur_term_common are now available
    * adapted video.inc to use the new naming convention

  Revision 1.1  2000/07/13 06:29:39  michael
  + Initial import

  Revision 1.3  2000/06/30 12:28:57  jonas
    * fixed termtype structure

  Revision 1.2  2000/03/12 15:02:10  peter
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