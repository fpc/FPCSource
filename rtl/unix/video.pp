{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Video unit for linux

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Video;

{$I-}

{*****************************************************************************}
                                   interface
{*****************************************************************************}

{$i videoh.inc}

{*****************************************************************************}
                                implementation
{*****************************************************************************}

uses  baseunix,termio,strings
     {$ifdef linux},linuxvcs{$endif};

{$i video.inc}


Type TConsoleType = (ttyNetwork
                     {$ifdef linux},ttyLinux{$endif}
                     ,ttyFreeBSD
                     ,ttyNetBSD);

type  Ttermcode=(
        enter_alt_charset_mode,
        exit_alt_charset_mode,
        clear_screen,
        cursor_home,
        cursor_normal,
        cursor_visible,
        cursor_invisible,
        enter_ca_mode,
        exit_ca_mode,
        exit_am_mode,
        ena_acs
      );
      Ttermcodes=array[Ttermcode] of Pchar;
      Ptermcodes=^Ttermcodes;

const term_codes_ansi:Ttermcodes= {Linux escape sequences are equal to ansi sequences}
        (#$1B#$5B#$31#$31#$6D,                              {enter_alt_charset_mode}
         #$1B#$5B#$31#$30#$6D,                              {exit_alt_charset_mode}
         #$1B#$5B#$48#$1B#$5B#$4A,                          {clear_screen}
         #$1B#$5B#$48,                                      {cursor_home}
         nil,                                               {cursor_normal}
         nil,                                               {cursor_visible}
         nil,                                               {cursor_invisible}
         nil,                                               {enter_ca_mode}
         nil,                                               {exit_ca_mode}
         nil,                                               {exit_am_mode}
         nil);                                              {ena_acs}

      term_codes_freebsd:Ttermcodes=
        (nil,                                               {enter_alt_charset_mode}
         nil,                                               {exit_alt_charset_mode}
         #$1B#$5B#$48#$1B#$5B#$4A,                          {clear_screen}
         #$1B#$5B#$48,                                      {cursor_home}
         #$1B#$5B#$3D#$30#$43,                              {cursor_normal}
         #$1B#$5B#$3D#$31#$43,                              {cursor_visible}
         nil,                                               {cursor_invisible}
         nil,                                               {enter_ca_mode}
         nil,                                               {exit_ca_mode}
         nil,                                               {exit_am_mode}
         nil);                                              {ena_acs}

      term_codes_linux:Ttermcodes=
        (#$1B#$5B#$31#$31#$6D,                              {enter_alt_charset_mode}
         #$1B#$5B#$31#$30#$6D,                              {exit_alt_charset_mode}
         #$1B#$5B#$48#$1B#$5B#$4A,                          {clear_screen}
         #$1B#$5B#$48,                                      {cursor_home}
         #$1B#$5B#$3F#$32#$35#$68#$1B#$5B#$3F#$30#$63,      {cursor_normal}
         #$1B#$5B#$3F#$32#$35#$68#$1B#$5B#$3F#$30#$63,      {cursor_visible}
         #$1B#$5B#$3F#$32#$35#$6C,                          {cursor_invisible}
         nil,                                               {enter_ca_mode}
         nil,                                               {exit_ca_mode}
         nil,                                               {exit_am_mode}
         nil);                                              {ena_acs}

      term_codes_vt100:Ttermcodes=
        (#$0E,                                              {enter_alt_charset_mode}
         #$0F,                                              {exit_alt_charset_mode}
         #$1B#$5B#$48#$1B#$5B#$4A#$24#$3C#$35#$30#$3E,      {clear_screen}
         #$1B#$5B#$48,                                      {cursor_home}
         nil,                                               {cursor_normal}
         nil,                                               {cursor_visible}
         nil,                                               {cursor_invisible}
         nil,                                               {enter_ca_mode}
         nil,                                               {exit_ca_mode}
         #$1B#$5B#$3F#$37#$6C,                              {exit_am_mode}
         #$1B#$28#$42#$1B#$29#$30);                         {ena_acs}

      term_codes_vt220:Ttermcodes=
        (#$1B#$28#$30#$24#$3C#$32#$3E,                      {enter_alt_charset_mode}
         #$1B#$28#$42#$24#$3C#$34#$3E,                      {exit_alt_charset_mode}
         #$1B#$5B#$48#$1B#$5B#$4A,                          {clear_screen}
         #$1B#$5B#$48,                                      {cursor_home}
         nil,                                               {cursor_normal}
         nil,                                               {cursor_visible}
         nil,                                               {cursor_invisible}
         nil,                                               {enter_ca_mode}
         nil,                                               {exit_ca_mode}
         #$1B#$5B#$3F#$37#$6C,                              {exit_am_mode}
         #$1B#$29#$30);                                     {ena_acs}

      term_codes_xterm:Ttermcodes=
        (#$0E,                                              {enter_alt_charset_mode}
         #$0F,                                              {exit_alt_charset_mode}
         #$1B#$5B#$48#$1B#$5B#$32#$4A,                      {clear_screen}
         #$1B#$5B#$48,                                      {cursor_home}
         #$1B#$5B#$3F#$31#$32#$6C#$1B#$5B#$3F#$32#$35#$68,  {cursor_normal}
         #$1B#$5B#$3F#$31#$32#$3B#$32#$35#$68,              {cursor_visible}
         #$1B#$5B#$3F#$32#$35#$6C,                          {cursor_invisible}
         #$1B#$5B#$3F#$31#$30#$34#$39#$68,                  {enter_ca_mode}
         #$1B#$5B#$3F#$31#$30#$34#$39#$6C,                  {exit_ca_mode}
         #$1B#$5B#$3F#$37#$6C,                              {exit_am_mode}
         #$1B#$28#$42#$1B#$29#$30);                         {ena_acs}


const    terminal_names:array[0..8] of string[7]=(
                        'ansi',
                        'cons',
                        'eterm',
                        'gnome',
                        'konsole',
                        'linux',
                        'vt100',
                        'vt220',
                        'xterm');
         terminal_data:array[0..8] of Ptermcodes=(
                        @term_codes_ansi,
                        @term_codes_freebsd,
                        @term_codes_xterm,
                        @term_codes_xterm,
                        @term_codes_xterm,
                        @term_codes_linux,
                        @term_codes_vt100,
                        @term_codes_vt220,
                        @term_codes_xterm);

var
  LastCursorType : byte;
  TtyFd: Longint;
  Console: TConsoleType;
  cur_term_strings:Ptermcodes;
{$ifdef logging}
  f: file;

const
  logstart: string = '';
  nl: char = #10;
  logend: string = #10#10;
{$endif logging}

{$ifdef cpui386}
{$ASMMODE ATT}
{$endif cpui386}

const

{  can_delete_term : boolean = false;}
  ACSIn : string = '';
  ACSOut : string = '';
  InACS : boolean =false;

function IsACS(var ch,ACSchar : char): boolean;
begin
  IsACS:=false;
  case ch of
    #24, #30: {}
      ch:='^';
    #25, #31: {}
      ch:='v';
    #26, #16: {Never introduce a ctrl-Z ... }
      ch:='>';
    {#27,needed in Escape sequences} #17: {}
      ch:='<';
    #176, #177, #178: {°±²}
      begin
        IsACS:=true;
        ACSChar:='a';
      end;
    #180, #181, #182, #185: {´µ¶¹}
      begin
        IsACS:=true;
        ACSChar:='u';
      end;
    #183, #184, #187, #191: {·¸»¿}
      begin
        IsACS:=true;
        ACSChar:='k';
      end;
    #188, #189, #190, #217: {¼½¾Ù}
      begin
        IsACS:=true;
        ACSChar:='j';
      end;
    #192, #200, #211, #212: {ÀÈÓÔ}
      begin
        IsACS:=true;
        ACSChar:='m';
      end;
    #193, #202, #207, #208: {ÁÊÏÐ}
      begin
        IsACS:=true;
        ACSChar:='v';
      end;
    #194, #203, #209, #210: {ÂËÑÒ}
      begin
        IsACS:=true;
        ACSChar:='w';
      end;
    #195, #198, #199, #204: {ÃÆÇÌ}
      begin
        IsACS:=true;
        ACSChar:='t';
      end;
    #196, #205: {ÄÍ}
      begin
        IsACS:=true;
        ACSChar:='q';
      end;
    #179, #186: {³º}
      begin
        IsACS:=true;
        ACSChar:='x';
      end;
    #197, #206, #215, #216: {ÅÎ×Ø}
      begin
        IsACS:=true;
        ACSChar:='n';
      end;
    #201, #213, #214, #218: {ÉÕÖÚ}
      begin
        IsACS:=true;
        ACSChar:='l';
      end;
    #254: { þ }
      begin
        ch:='*';
      end;
    { Shadows for Buttons }
    #220: { Ü }
      begin
        IsACS:=true;
        ACSChar:='a';
      end;
    #223: { ß }
      begin
        IsACS:=true;
        ACSChar:='a';
      end;
  end;
end;


function SendEscapeSeqNdx(Ndx:Ttermcode) : boolean;
var
  P,pdelay: PChar;
begin
  SendEscapeSeqNdx:=false;
  if not assigned(cur_term_Strings) then
    exit{RunError(219)};
  P:=cur_term_Strings^[Ndx];
  if assigned(p) then
   begin { Do not transmit the delays }
     pdelay:=strpos(p,'$<');
     if assigned(pdelay) then
       pdelay^:=#0;
     fpWrite(stdoutputhandle, P^, StrLen(P));
     SendEscapeSeqNdx:=true;
     if assigned(pdelay) then
       pdelay^:='$';
   end;
end;


procedure SendEscapeSeq(const S: String);
begin
  fpWrite(stdoutputhandle, S[1], Length(S));
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
  Returns a string with the escape sequences to go to X,Y on the screen.

  Note that x, y, ox, oy are 1-based (i.e. top-left corner of the screen
  is (1, 1)), while SetCursorPos parameters and CursorX and CursorY
  are 0-based (top-left corner of the screen is (0, 0)).
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
  if ((x=1) and (oy+1=y)) and (console<>ttyfreebsd) then
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
{$ifdef ENDIAN_LITTLE}
    ch : char;
    attr : byte;
{$else}
    attr : byte;
    ch : char;
{$endif}
  end;
var
  outbuf   : array[0..1023+255] of char;
  chattr   : tchattr;
  skipped  : boolean;
  outptr,
  spaces,
  eol,
  x,y,
  LastX,LastY,
  SpaceAttr,
  LastAttr : longint;
  p,pold   : pvideocell;
  LastLineWidth : Longint;

procedure TransformUsingACS(var st : string);
var
  res : string;
  i : longint;
  ch,ACSch : char;
begin
  res:='';
  for i:=1 to length(st) do
    begin
      ch:=st[i];
      if IsACS(ch,ACSch) then
        begin
          if not InACS then
            begin
              res:=res+ACSIn;
              InACS:=true;
            end;
          res:=res+ACSch;
        end
      else
        begin
          if InACS then
            begin
              res:=res+ACSOut+Attr2Ansi(LastAttr,0);
              InACS:=false;
            end;
          res:=res+ch;
        end;
    end;
  st:=res;
end;



  procedure outdata(hstr:string);
  begin
   If Length(HStr)>0 Then
   Begin
    while (eol>0) do
     begin
       hstr:=#13#10+hstr;
       dec(eol);
     end;
    if NoExtendedFrame and (ACSIn<>'') and (ACSOut<>'') then
      TransformUsingACS(Hstr);
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
       fpWrite(stdoutputhandle,outbuf,outptr);
       outptr:=0;
     end;
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

function GetTermString(ndx:Ttermcode):String;
var
   P,pdelay: PChar;
begin
  GetTermString:='';
  if not assigned(cur_term_Strings) then
    exit{RunError(219)};
  P:=cur_term_Strings^[Ndx];
  if assigned(p) then
   begin { Do not transmit the delays }
     pdelay:=strpos(p,'$<');
     if assigned(pdelay) then
       pdelay^:=#0;
     GetTermString:=StrPas(p);
     if assigned(pdelay) then
       pdelay^:='$';
   end;
end;

begin
  OutPtr:=0;
  Eol:=0;
  skipped:=true;
  p:=PVideoCell(VideoBuf);
  pold:=PVideoCell(OldVideoBuf);
{ init Attr, X,Y and set autowrap off }
  SendEscapeSeq(#27'[m'#27'[?7l'{#27'[H'} );
//  1.0.x: SendEscapeSeq(#27'[m'{#27'[H'});
  LastAttr:=7;
  LastX:=-1;
  LastY:=-1;
  for y:=1 to ScreenHeight do
   begin
     SpaceAttr:=0;
     Spaces:=0;
     LastLineWidth:=ScreenWidth;
     If (y=ScreenHeight) And (Console=ttyFreeBSD) {And :am: is on} Then
      LastLineWidth:=ScreenWidth-2;
     for x:=1 to LastLineWidth do
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
                  ChAttr.ch:= chr(ord(chattr.ch)+ord('A')-1);
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
      inc(eol)
     else
      skipped:=true;
   end;
  eol:=0;
 {if am in capabilities? Then}
  If (Console=ttyFreeBSD) and (Plongint(p)^<>plongint(pold)^) Then
   Begin
    OutData(XY2Ansi(ScreenWidth,ScreenHeight,LastX,LastY));
    OutData(#8);
    {Output last char}
    chattr:=tchattr(p[1]);
    if LastAttr<>chattr.Attr then
     OutClr(chattr.Attr);
    OutData(chattr.ch);
    inc(LastX);
//    OutData(XY2Ansi(ScreenWidth-1,ScreenHeight,LastX,LastY));
//   OutData(GetTermString(Insert_character));
    OutData(#8+#27+'[1@');

    chattr:=tchattr(p^);
    if LastAttr<>chattr.Attr then
     OutClr(chattr.Attr);
    OutData(chattr.ch);
    inc(LastX);
   end;
  OutData(XY2Ansi(CursorX+1,CursorY+1,LastX,LastY));
{$ifdef logging}
  blockwrite(f,logstart[1],length(logstart));
  blockwrite(f,nl,1);
  blockwrite(f,outptr,sizeof(outptr));
  blockwrite(f,nl,1);
  blockwrite(f,outbuf,outptr);
  blockwrite(f,nl,1);
{$endif logging}
  fpWrite(stdoutputhandle,outbuf,outptr);
  if InACS then
    SendEscapeSeqNdx(exit_alt_charset_mode);
 {turn autowrap on}
//  SendEscapeSeq(#27'[?7h');
end;

var
  preInitVideoTio, postInitVideoTio: termio.termios;
  inputRaw, outputRaw: boolean;

procedure saveRawSettings(const tio: termio.termios);
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

procedure restoreRawSettings(tio: termio.termios);
begin
  with tio do
    begin
      if inputRaw then
        begin
          c_iflag := c_iflag and (not (IGNBRK or BRKINT or PARMRK or ISTRIP or
            INLCR or IGNCR or ICRNL or IXON));
          c_lflag := c_lflag and
            (not (ECHO or ECHONL or ICANON or ISIG or IEXTEN));
          c_cc[VMIN]:=1;
          c_cc[VTIME]:=0;
        end;
     if outPutRaw then
       begin
         c_oflag := c_oflag and not(OPOST);
         c_cflag := c_cflag and not(CSIZE or PARENB) or CS8;
       end;
   end;
  TCSetAttr(1,TCSANOW,tio);
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
  tio: termio.termios;
begin
  TCGetAttr(1,tio);
  saveRawSettings(tio);
  TCSetAttr(1,TCSANOW,postInitVideoTio);
end;

procedure doneVideoDone;
begin
  restoreRawSettings(preInitVideoTio);
end;

procedure SysInitVideo;
var
  FName: String;
  WS: packed record
    ws_row, ws_col, ws_xpixel, ws_ypixel: Word;
  end;
{  Err: Longint;}
{  prev_term : TerminalCommon_ptr1;}
  term:string;
  i:word;
{$ifdef Linux}
  s:string[15];
{$endif}
{$ifdef freebsd}
  ThisTTY: String[30];
{$endif}

const font_vga:array[0..6] of char=#15#27'%@'#27'(U';
      font_custom:array[0..2] of char=#27'(K';

begin
{$ifndef CPUI386}
  LowAscii:=false;
{$endif CPUI386}
  { check for tty }
  if (IsATTY(stdinputhandle)=1) then
   begin
     { save current terminal characteristics and remove rawness }
     prepareInitVideo;
     { running on a tty, find out whether locally or remotely }
     TTyfd:=-1;
     Console:=TTyNetwork;                 {Default: Network or other vtxxx tty}
     cur_term_strings:=@term_codes_vt100; {Default: vt100}
   {$ifdef linux}
     if vcs_device>=0 then
       begin
         str(vcs_device,s);
         fname:='/dev/vcsa'+s;
         { open console, $1b6=rw-rw-rw- }
         ttyfd:=fpopen(fname,$1b6,O_RDWR);
         if ttyfd<>-1 then
           console:=ttylinux
         else
           if try_grab_vcsa then
             begin
               ttyfd:=fpopen(fname,$1b6,O_RDWR);
               if ttyfd<>-1 then
                 console:=ttylinux;
             end;
       end;
   {$endif}
   {$ifdef freebsd}
     ThisTTY:=TTYName(stdinputhandle);
     if copy(ThisTTY, 1, 9) = '/dev/ttyv' then  {FreeBSD has these}
       begin
         { check for (Free?)BSD native}
         if (ThisTTY[10]>='0') and (ThisTTY[10]<='9') Then
            Console:=ttyFreeBSD;   {TTYFd ?}
       end;
   {$endif}
     term:=fpgetenv('TERM');
     for i:=low(terminal_names) to high(terminal_names) do
       if copy(term,1,length(terminal_names[i]))=terminal_names[i] then
         cur_term_strings:=terminal_data[i];
    if cur_term_strings=@term_codes_freebsd then
      console:=ttyFreeBSD;
{$ifdef linux}
    if (console<>ttylinux) then
      begin
{$endif}
        if cur_term_strings=@term_codes_linux then
          begin
            {Executed in case ttylinux is false (i.e. no vcsa), but
             TERM=linux.}
            {Enable the VGA character set (codepage 437,850,....)}
            fpwrite(stdoutputhandle,@font_vga,7);
          end
        else
          {No VGA font :( }
          fpwrite(stdoutputhandle,@font_custom,3);
        { running on a remote terminal, no error with /dev/vcsa }
        LowAscii:=false;
   {$ifdef linux}
      end;
   {$endif}
     fpioctl(stdinputhandle, TIOCGWINSZ, @WS);
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
     CursorX:=0;
     CursorY:=0;
     LastCursorType:=$ff;
     ScreenColor:=True;
     { Start with a clear screen }
   {$ifdef linux}
     if Console<>ttylinux then
      begin
   {$endif}
        SendEscapeSeqNdx(cursor_home);
        SendEscapeSeqNdx(cursor_normal);
        SendEscapeSeqNdx(cursor_visible);
        SendEscapeSeqNdx(enter_ca_mode);
        SetCursorType(crUnderLine);
        If Console=ttyFreeBSD Then
          SendEscapeSeqNdx(exit_am_mode);
   {$ifdef linux}
      end;
   {$endif}
     if assigned(cur_term_Strings) then
       begin
         ACSIn:=StrPas(cur_term_strings^[enter_alt_charset_mode]);
         ACSOut:=StrPas(cur_term_strings^[exit_alt_charset_mode]);
         if (ACSIn<>'') and (ACSOut<>'') then
           SendEscapeSeqNdx(ena_acs);
         if pos('$<',ACSIn)>0 then
           ACSIn:=Copy(ACSIn,1,Pos('$<',ACSIn)-1);
         if pos('$<',ACSOut)>0 then
           ACSOut:=Copy(ACSOut,1,Pos('$<',ACSOut)-1);
         If fpGetEnv('TERM')='xterm' then
           NoExtendedFrame := true;  {use of acs for xterm is ok}
       end
     else
       begin
         ACSIn:='';
         ACSOut:='';
       end;
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

procedure SysDoneVideo;
begin
  prepareDoneVideo;
{$ifdef linux}
  if Console=ttylinux then
   SetCursorPos(0,0)
  else
   begin
{$endif}
     SendEscapeSeqNdx(exit_ca_mode);
     SendEscapeSeqNdx(cursor_home);
     SendEscapeSeqNdx(cursor_normal);
     SendEscapeSeqNdx(cursor_visible);
     SetCursorType(crUnderLine);
     SendEscapeSeq(#27'[H');
     if cur_term_strings=@term_codes_linux then
       begin
         {Executed in case ttylinux is false (i.e. no vcsa), but
          TERM=linux.}
         {Enable the character set set through setfont}
         fpwrite(stdoutputhandle,#27'(K',3);
       end;
{$ifdef linux}
   end;
{$endif}
  ACSIn:='';
  ACSOut:='';
  doneVideoDone;
{$ifdef logging}
  close(f);
{$endif logging}
end;


procedure SysClearScreen;
begin
{$ifdef linux}
  if Console=ttylinux then
    UpdateScreen(true)
  else
    begin
{$endif}
      SendEscapeSeq(#27'[0m');
      SendEscapeSeqNdx(clear_screen);
{$ifdef linux}
    end;
{$endif}
end;


procedure SysUpdateScreen(Force: Boolean);
var
  DoUpdate : boolean;
  i : longint;
  p1,p2 : plongint;
begin
  if not force then
   begin
{$ifdef cpui386}
     asm
          pushl   %esi
          pushl   %edi
          movl    VideoBuf,%esi
          movl    OldVideoBuf,%edi
          movl    VideoBufSize,%ecx
          shrl    $2,%ecx
          repe
          cmpsl
          setne   DoUpdate
          popl    %edi
          popl    %esi
     end;
{$else not cpui386}
     p1:=plongint(VideoBuf);
     p2:=plongint(OldVideoBuf);
     for i:=0 to VideoBufSize div 2 do
       if (p1^<>p2^) then
         begin
           DoUpdate:=true;
           break;
         end
       else
         begin
           { Inc does add sizeof(longint) to both pointer values }
           inc(p1);
           inc(p2);
         end;
{$endif not cpui386}
   end
  else
   DoUpdate:=true;
  if not DoUpdate then
   exit;
{$ifdef linux}
  if Console=ttylinux then
   begin
     fplSeek(TTYFd, 4, Seek_Set);
     fpWrite(TTYFd, VideoBuf^,VideoBufSize);
   end
  else
   begin
{$endif}
     UpdateTTY(force);
{$ifdef linux}
   end;
{$endif}
  Move(VideoBuf^, OldVideoBuf^, VideoBufSize);
end;


function SysGetCapabilities: Word;
begin
{ about cpColor... we should check the terminfo database... }
  SysGetCapabilities:=cpUnderLine + cpBlink + cpColor;
end;


procedure SysSetCursorPos(NewCursorX, NewCursorY: Word);
var
  Pos : array [1..2] of Byte;
begin
 if (CursorX=NewCursorX) and (CursorY=NewCursorY) then
    exit;
{$ifdef linux}
  if Console=ttylinux then
   begin
     fplSeek(TTYFd, 2, Seek_Set);
     Pos[1]:=NewCursorX;
     Pos[2]:=NewCursorY;
     fpWrite(TTYFd, Pos, 2);
   end
  else
   begin
{$endif}
     { newcursorx,y and CursorX,Y are 0 based ! }
     SendEscapeSeq(XY2Ansi(NewCursorX+1,NewCursorY+1,CursorX+1,CursorY+1));
{$ifdef linux}
   end;
{$endif}
  CursorX:=NewCursorX;
  CursorY:=NewCursorY;
end;


function SysGetCursorType: Word;
begin
  SysGetCursorType:=LastCursorType;
end;


procedure SysSetCursorType(NewType: Word);
begin
  If LastCursorType=NewType then
   exit;
  LastCursorType:=NewType;
  case NewType of
   crBlock :
     Begin
       If not SendEscapeSeqNdx(cursor_visible) then
        If Console<>ttyFreeBSD Then     // should be done only for linux?
         SendEscapeSeq(#27'[?17;0;64c');
     End;
   crHidden :
     Begin
       If not SendEscapeSeqNdx(cursor_invisible) then
        If Console<>ttyFreeBSD Then
         SendEscapeSeq(#27'[?1c');
     End;
  else
    begin
      If not SendEscapeSeqNdx(cursor_normal) then
        If Console<>ttyFreeBSD Then
         SendEscapeSeq(#27'[?2c');
    end;
  end;
end;

Const
  SysVideoDriver : TVideoDriver = (
    InitDriver : @SysInitVideo;
    DoneDriver : @SysDoneVideo;
    UpdateScreen : @SysUpdateScreen;
    ClearScreen : @SysClearScreen;
    SetVideoMode : Nil;
    GetVideoModeCount : Nil;
    GetVideoModeData : Nil;
    SetCursorPos : @SysSetCursorPos;
    GetCursorType : @SysGetCursorType;
    SetCursorType : @SysSetCursorType;
    GetCapabilities : @SysGetCapabilities;
  );

initialization
  SetVideoDriver(SysVideoDriver);
end.
