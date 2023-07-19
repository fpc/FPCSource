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
unit video;

{$I-}
{$GOTO on}

{*****************************************************************************}
                                   interface
{*****************************************************************************}

{$i videoh.inc}

{*****************************************************************************}
                                implementation
{*****************************************************************************}

uses  baseunix,termio,strings,unixkvmbase,graphemebreakproperty,eastasianwidth
     ,charset
     {$ifdef linux},linuxvcs{$endif};

const
  CP_ISO01 = 28591;  {ISO 8859-1}
  CP_ISO02 = 28592;  {ISO 8859-2}
  CP_ISO05 = 28595;  {ISO 8859-5}

var external_codepage:TSystemCodePage;

{$i video.inc}

type  Tconsole_type=(ttyNetwork
                     {$ifdef linux},ttyLinux{$endif}
                     ,ttyFreeBSD
                     ,ttyNetBSD);

      Ttermcode=(
        enter_alt_charset_mode,
        exit_alt_charset_mode,
        clear_screen,
        cursor_home,
        cursor_normal,
        cursor_visible_underline,
        cursor_visible_block,
        cursor_invisible,
        enter_ca_mode,
        exit_ca_mode,
        exit_am_mode,
        ena_acs
      );
      Ttermcodes=array[Ttermcode] of PAnsiChar;
      Ptermcodes=^Ttermcodes;

const term_codes_ansi:Ttermcodes=
        (#$1B#$5B#$31#$31#$6D,                              {enter_alt_charset_mode}
         #$1B#$5B#$31#$30#$6D,                              {exit_alt_charset_mode}
         #$1B#$5B#$48#$1B#$5B#$4A,                          {clear_screen}
         #$1B#$5B#$48,                                      {cursor_home}
         nil,                                               {cursor_normal}
         nil,                                               {cursor visible, underline}
         nil,                                               {cursor visible, block}
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
         #$1B#$5B#$3D#$31#$43,                              {cursor visible, underline}
         #$1B#$5B#$3D#$31#$43,                              {cursor visible, block}
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
         #$1B'[?25h'#$1B'[?0c',                             {cursor_normal}
         #$1B'[?0c',                                        {cursor visible, underline}
         #$1B'[?6c',                                        {cursor visible, block}
         #$1B'[?1c',                                        {cursor_invisible}
         nil,                                               {enter_ca_mode}
         nil,                                               {exit_ca_mode}
         nil,                                               {exit_am_mode}
         nil);                                              {ena_acs}

      term_codes_vt100:Ttermcodes=
        (#$0E,                                              {enter_alt_charset_mode}
         #$0F,                                              {exit_alt_charset_mode}
         #$1B#$5B#$48#$1B#$5B#$4A{#$24#$3C#$35#$30#$3E},    {clear_screen}
         #$1B#$5B#$48,                                      {cursor_home}
         nil,                                               {cursor_normal}
         nil,                                               {cursor visible, underline}
         nil,                                               {cursor visible, block}
         nil,                                               {cursor_invisible}
         nil,                                               {enter_ca_mode}
         nil,                                               {exit_ca_mode}
         #$1B#$5B#$3F#$37#$6C,                              {exit_am_mode}
         #$1B#$28#$42#$1B#$29#$30);                         {ena_acs}

      term_codes_vt220:Ttermcodes=
        (#$1B#$28#$30{#$24#$3C#$32#$3E},                    {enter_alt_charset_mode}
         #$1B#$28#$42{#$24#$3C#$34#$3E},                    {exit_alt_charset_mode}
         #$1B#$5B#$48#$1B#$5B#$4A,                          {clear_screen}
         #$1B#$5B#$48,                                      {cursor_home}
         nil,                                               {cursor_normal}
         nil,                                               {cursor visible, underline}
         nil,                                               {cursor visible, block}
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
         #$1B#$5B#$3F#$31#$32#$3B#$32#$35#$68,              {cursor visible, underline}
         #$1B#$5B#$3F#$31#$32#$3B#$32#$35#$68,              {cursor visible, block}
         #$1B#$5B#$3F#$32#$35#$6C,                          {cursor_invisible}
         #$1B#$5B#$3F#$31#$30#$34#$39#$68,                  {enter_ca_mode}
         #$1B#$5B#$3F#$31#$30#$34#$39#$6C,                  {exit_ca_mode}
         #$1B#$5B#$3F#$37#$6C,                              {exit_am_mode}
         #$1B#$28#$42#$1B#$29#$30);                         {ena_acs}

      term_codes_beos:Ttermcodes=
        (nil,//#$0E,                                              {enter_alt_charset_mode}
         nil,//#$0F,                                              {exit_alt_charset_mode}
         #$1B#$5B#$48#$1B#$5B#$4A,                                {clear_screen}
         #$1B#$5B#$48,                                            {cursor_home}
         #$1B'[?25h',// nil,//#$1B#$5B#$3F#$31#$32#$6C#$1B#$5B#$3F#$32#$35#$68,  {cursor_normal}
         nil,//#$1B#$5B#$3F#$31#$32#$3B#$32#$35#$68,              {cursor visible, underline}
         nil,//#$1B#$5B#$3F#$31#$32#$3B#$32#$35#$68,              {cursor visible, block}
         #$1B'[?25l',//nil,//#$1B#$5B#$3F#$32#$35#$6C,                          {cursor_invisible}
         nil,//#$1B#$5B#$3F#$31#$30#$34#$39#$68,                  {enter_ca_mode}
         nil,//#$1B#$5B#$3F#$31#$30#$34#$39#$6C,                  {exit_ca_mode}
         nil,//#$1B#$5B#$3F#$37#$6C,                              {exit_am_mode}
         nil);//#$1B#$28#$42#$1B#$29#$30);                         {ena_acs}

const    terminal_names:array[0..11] of string[7]=(
                        'ansi',
                        'cons',
                        'eterm',
                        'gnome',
                        'konsole',
                        'linux',
                        'rxvt',
                        'screen',
                        'vt100',
                        'vt220',
                        'xterm',
                        'beterm');
         terminal_data:array[0..11] of Ptermcodes=(
                        @term_codes_ansi,
                        @term_codes_freebsd,
                        @term_codes_xterm,
                        @term_codes_xterm,
                        @term_codes_xterm,
                        @term_codes_linux,
                        @term_codes_xterm,
                        @term_codes_xterm,
                        @term_codes_vt100,
                        @term_codes_vt220,
                        @term_codes_xterm,
                        @term_codes_beos);

var
  LastCursorType : byte;
{$ifdef linux}
  TtyFd: Longint;
{$endif linux}
  Console: Tconsole_type;
  cur_term_strings:Ptermcodes;
{$ifdef logging}
  f: file;

const
  logstart: shortstring = '';
  nl: AnsiChar = #10;
  logend: shortstring = #10#10;
{$endif logging}

{$ifdef cpui386}
{$ASMMODE ATT}
{$endif cpui386}

const

{  can_delete_term : boolean = false;}
  ACSIn : shortstring = '';
  ACSOut : shortstring = '';
  in_ACS : boolean =false;

  TerminalSupportsHighIntensityColors: boolean = false;
  TerminalSupportsBold: boolean = true;

{Contains all code pages that can be considered a normal vga font.
    Note: KOI8-R has line drawing characters in wrong place. Support
          can perhaps be added, for now we'll let it rest.}
function is_vga_code_page(CP: TSystemCodePage): Boolean;
begin
  case CP of
    437,850,852,866:
      result:=true;
    else
      result:=false;
  end;
end;

function Unicode2DecSpecialGraphics(Ch: WideChar): AnsiChar;
begin
  case Ch of
    #$25C6:
      Result := #$60;
    #$2592,#$2591,#$2593,#$2584,#$2580:
      Result := #$61;
    #$2409:
      Result := #$62;
    #$240C:
      Result := #$63;
    #$240D:
      Result := #$64;
    #$240A:
      Result := #$65;
    #$00B0:
      Result := #$66;
    #$00B1:
      Result := #$67;
    #$2424:
      Result := #$68;
    #$240B:
      Result := #$69;
    #$2518,#$255B,#$255C,#$255D:
      Result := #$6A;
    #$2510,#$2556,#$2555,#$2557:
      Result := #$6B;
    #$250C,#$2553,#$2552,#$2554:
      Result := #$6C;
    #$2514,#$2558,#$2559,#$255A:
      Result := #$6D;
    #$253C,#$256C,#$256B,#$256A:
      Result := #$6E;
    #$23BA:
      Result := #$6F;
    #$23BB:
      Result := #$70;
    #$2500,#$2550:
      Result := #$71;
    #$23BC:
      Result := #$72;
    #$23BD:
      Result := #$73;
    #$251C,#$255E,#$255F,#$2560:
      Result := #$74;
    #$2524,#$2561,#$2562,#$2563:
      Result := #$75;
    #$2534,#$2569,#$2567,#$2568:
      Result := #$76;
    #$252C,#$2566,#$2564,#$2565:
      Result := #$77;
    #$2502,#$2551:
      Result := #$78;
    #$2264:
      Result := #$79;
    #$2265:
      Result := #$7A;
    #$03A0:
      Result := #$7B;
    #$2260:
      Result := #$7C;
    #$00A3:
      Result := #$7D;
    #$00B7:
      Result := #$7E;
    else
      Result := #0;
  end;
end;

function convert_vga_to_acs(ch:AnsiChar):word;

{Ch contains a character in the VGA character set (i.e. codepage 437).
 This routine tries to convert some VGA symbols as well as possible to the
 xterm alternate character set.

 Return type is word to allow expanding to UCS-2 characters in the
 future.}

begin
  case ch of
    #18:
      convert_vga_to_acs:=word('|');
    #24, #30: {↑▲}
      convert_vga_to_acs:=word('^');
    #25, #31: {↓▼}
      convert_vga_to_acs:=word('v');
    #26, #16: {Never introduce a ctrl-Z ... →►}
      convert_vga_to_acs:=word('>');
    {#27,} #17: {←◄}
      convert_vga_to_acs:=word('<');
    #176, #177, #178: {░▒▓}
      convert_vga_to_acs:=$f800+word('a');
    #180, #181, #182, #185: {┤╡╢╣}
      convert_vga_to_acs:=$f800+word('u');
    #183, #184, #187, #191: {╖╕╗┐}
      convert_vga_to_acs:=$f800+word('k');
    #188, #189, #190, #217: {╝╜╛┘}
      convert_vga_to_acs:=$f800+word('j');
    #192, #200, #211, #212: {└╚╙╘}
      convert_vga_to_acs:=$f800+word('m');
    #193, #202, #207, #208: {┴╩╧╨}
      convert_vga_to_acs:=$f800+word('v');
    #194, #203, #209, #210: {┬╦╤╥}
      convert_vga_to_acs:=$f800+word('w');
    #195, #198, #199, #204: {├╞╟╠}
      convert_vga_to_acs:=$f800+word('t');
    #196, #205: {─═}
      convert_vga_to_acs:=$f800+word('q');
    #179, #186: {│║}
      convert_vga_to_acs:=$f800+word('x');
    #197, #206, #215, #216: {┼╬╫╪}
      convert_vga_to_acs:=$f800+word('n');
    #201, #213, #214, #218: {╔╒╓┌}
      convert_vga_to_acs:=$f800+word('l');
    #254: { ■ }
      convert_vga_to_acs:=word('*');
    { Shadows for Buttons }
    #220  { ▄ },
    #223: { ▀ }
      convert_vga_to_acs:=$f800+word('a');
    else
      convert_vga_to_acs:=word(ch);
  end;
end;

procedure SendEscapeSeqNdx(ndx:Ttermcode);

var p:PAnsiChar;

begin
{ Always true because of vt100 default.
  if not assigned(cur_term_Strings) then
    exit}{RunError(219)};
  p:=cur_term_strings^[ndx];
  if p<>nil then
    fpwrite(stdoutputhandle,p^,strlen(p));
end;


procedure SendEscapeSeq(const S: shortstring);
begin
  fpWrite(stdoutputhandle, S[1], Length(S));
end;


function IntStr(l:longint):shortstring;

begin
  Str(l,intstr);
end;


Function XY2Ansi(x,y,ox,oy:longint):shortstring;
{
  Returns a string with the escape sequences to go to X,Y on the screen.

  Note that x, y, ox, oy are 1-based (i.e. top-left corner of the screen
  is (1, 1)), while SetCursorPos parameters and CursorX and CursorY
  are 0-based (top-left corner of the screen is (0, 0)).
}

var delta:longint;
    direction:AnsiChar;
    movement:string[32];

begin
  if ((x=1) and (oy+1=y)) and (console<>ttyfreebsd) then
    begin
      XY2Ansi:=#13#10;
      exit;
    end;
  direction:='H';
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
     delta:=ox-x;
     direction:=AnsiChar(byte('C')+byte(x<=ox));
   end;
  if x=ox then
   begin
     delta:=oy-y;
     direction:=AnsiChar(byte('A')+byte(y>oy));
   end;

  if direction='H' then
    movement:=intstr(y)+';'+intstr(x)
  else
    movement:=intstr(abs(delta));

  xy2ansi:=#27'['+movement+direction;
end;

const  ansitbl:array[0..7] of AnsiChar='04261537';

function attr2ansi(Fg,Bg:byte;Attr:TEnhancedVideoAttributes;OFg,OBg:byte;OAttr:TEnhancedVideoAttributes):shortstring;
const
  AttrOnOffStr: array [TEnhancedVideoAttribute, Boolean] of shortstring = (
    ('22;','1;'),
    ('22;','2;'),
    ('23;','3;'),
    ('24;','4;'),
    ('25;','5;'),
    ('25;','6;'),
    ('27;','7;'),
    ('28;','8;'),
    ('29;','9;'),
    ('24;','21;'));
var
  tmpS: shortstring;
  A: TEnhancedVideoAttribute;
begin
  attr2ansi:=#27'[';

  if Attr<>OAttr then
  begin
    { turn off old attributes first }
    for A := Low(TEnhancedVideoAttribute) to High(TEnhancedVideoAttribute) do
      if (not (A in Attr)) and (A in OAttr) then
        attr2ansi:=attr2ansi+AttrOnOffStr[A,False];
    { then, turn on new attributes }
    for A := Low(TEnhancedVideoAttribute) to High(TEnhancedVideoAttribute) do
      if (A in Attr) and (not (A in OAttr)) then
        attr2ansi:=attr2ansi+AttrOnOffStr[A,True];
  end;

  if (Fg > 15) or (Bg > 15) then
    begin
      if Fg<>OFg then
        begin
          if TerminalSupportsBold and (ofg and 8<>0) then
            attr2ansi:=attr2ansi+'22;';
          Str(Fg,tmpS);
          attr2ansi:=attr2ansi+'38;5;'+tmpS+';';
        end;
      if Bg<>OBg then
        begin
          Str(Bg,tmpS);
          attr2ansi:=attr2ansi+'48;5;'+tmpS+';';
        end;
    end
  else
    begin
      if TerminalSupportsBold then
        if fg and 8<>0 then
          begin
            {Enable bold if not yet on.}
            if ofg and 8=0 then
              attr2ansi:=attr2ansi+'1;';
          end
        else
          {Disable bold if on.}
          if ofg and 8<>0 then
            attr2ansi:=attr2ansi+'22;';
      if bg and 8<>0 then
        begin
          {Enable bold if not yet on.}
          if obg and 8=0 then
            attr2ansi:=attr2ansi+'5;';
        end
      else
        {Disable bold if on.}
        if obg and 8<>0 then
          attr2ansi:=attr2ansi+'25;';

      if TerminalSupportsHighIntensityColors then
      begin
        if fg and 15<>ofg and 15 then
          if fg and 8<>0 then
            attr2ansi:=attr2ansi+'9'+ansitbl[fg and 7]+';'
          else
            attr2ansi:=attr2ansi+'3'+ansitbl[fg and 7]+';';
      end
      else
      begin
        if fg and 7<>ofg and 7 then
          attr2ansi:=attr2ansi+'3'+ansitbl[fg and 7]+';';
      end;
      if bg and 7<>obg and 7 then
         attr2ansi:=attr2ansi+'4'+ansitbl[bg and 7]+';';
    end;

  if attr2ansi[length(attr2ansi)]=';' then
    attr2ansi[length(attr2ansi)]:='m'
  else
   attr2ansi:='';
end;


procedure UpdateTTY(Force:boolean);
var
  outbuf   : array[0..1023+255] of AnsiChar;
  chattr   : tenhancedvideocell;
  skipped  : boolean;
  outptr,
  spaces,
  eol,
  x,y,
  LastX,LastY : longint;
  SpaceFg, SpaceBg : byte;
  SpaceAttr: TEnhancedVideoAttributes;
  LastFg, LastBg : byte;
  LastAttr: TEnhancedVideoAttributes;
  LastLineWidth : Longint;
  p,pold   : penhancedvideocell;
  LastCharWasDoubleWidth: Boolean;
  CurCharWidth: Integer;

  function transform(const hstr:UnicodeString):RawByteString;
  var
    DecSpecialGraphicsCharacter: AnsiChar;
  begin
    if external_codepage=CP_UTF8 then
      result:=Utf8Encode(hstr)
    else
      begin
        DecSpecialGraphicsCharacter:=#0;
        if (Length(hstr)=1) and (ACSIn<>'') and (ACSOut<>'') then
          DecSpecialGraphicsCharacter:=Unicode2DecSpecialGraphics(hstr[1]);
        if DecSpecialGraphicsCharacter<>#0 then
        begin
          result:=ACSIn+DecSpecialGraphicsCharacter+ACSOut;
          SetCodePage(result,external_codepage,False);
        end
        else
        begin
          result:=Utf8Encode(hstr);
          SetCodePage(result,external_codepage,True);
          if (result='?') and (hstr<>'?') then
          begin
            { Character is missing in the external codepage. }
            { Try some replacements. }
            if Length(hstr)=1 then
              begin
                case hstr[1] of
                  #$2195:
                    result:='|';
                  #$2191,#$25B2:
                    result:='^';
                  #$2193,#$25BC:
                    result:='v';
                  #$2192,#$25BA:
                    result:='>';
                  #$2190,#$25C4:
                    result:='<';
                  #$25A0:
                    result:='*';
                end;
                SetCodePage(result,external_codepage,False);
              end;
          end;
        end;
      end;
  end;

  procedure outdata(hstr:rawbytestring);

  begin
   If Length(HStr)>0 Then
   Begin
    while (eol>0) do
     begin
       outbuf[outptr]:=#13;
       outbuf[outptr+1]:=#10;
       inc(outptr,2);
       dec(eol);
     end;
{    if (convert=cv_vga_to_acs) and (ACSIn<>'') and (ACSOut<>'') then
      transform_using_acs(Hstr);}
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

  procedure OutClr(Fg,Bg:byte;Attr:TEnhancedVideoAttributes);
  begin
    if (Fg=LastFg) and (Bg=LastBg) and (Attr=LastAttr) then
     exit;
    OutData(Attr2Ansi(Fg,Bg,Attr,LastFg,LastBg,LastAttr));
    LastFg:=Fg;
    LastBg:=Bg;
    LastAttr:=Attr;
  end;

  procedure OutSpaces;
  begin
    if (Spaces=0) then
     exit;
    OutClr(SpaceFg,SpaceBg,SpaceAttr);
    OutData(Space(Spaces));
    LastX:=x;
    LastY:=y;
    Spaces:=0;
  end;

(*
function GetTermString(ndx:Ttermcode):shortstring;
var
   P{,pdelay}: PAnsiChar;
begin
  GetTermString:='';
  if not assigned(cur_term_Strings) then
    exit{RunError(219)};
  P:=cur_term_Strings^[Ndx];
  if assigned(p) then
   begin { Do not transmit the delays }
{     pdelay:=strpos(p,'$<');
     if assigned(pdelay) then
       pdelay^:=#0;}
     GetTermString:=StrPas(p);
{     if assigned(pdelay) then
       pdelay^:='$';}
   end;
end;
*)

begin
  OutPtr:=0;
  Eol:=0;
  skipped:=true;
  p:=PEnhancedVideoCell(@EnhancedVideoBuf[0]);
  pold:=PEnhancedVideoCell(@OldEnhancedVideoBuf[0]);
{ init Attr, X,Y and set autowrap off }
  SendEscapeSeq(#27'[0;40;37m'#27'[?7l'{#27'[H'} );
//  1.0.x: SendEscapeSeq(#27'[m'{#27'[H'});
  LastFg:=7;
  LastBg:=0;
  LastAttr:=[];
  LastX:=-1;
  LastY:=-1;
  for y:=1 to ScreenHeight do
   begin
     SpaceFg:=0;
     SpaceBg:=0;
     SpaceAttr:=[];
     Spaces:=0;
     LastLineWidth:=ScreenWidth;
     If (y=ScreenHeight) And (Console=ttyFreeBSD) {And :am: is on} Then
      LastLineWidth:=ScreenWidth-2;
     LastCharWasDoubleWidth:=False;
     for x:=1 to LastLineWidth do
      begin
        if LastCharWasDoubleWidth then
         LastCharWasDoubleWidth:=false
        else
          begin
            CurCharWidth := ExtendedGraphemeClusterDisplayWidth(p^.ExtendedGraphemeCluster);
            if (not force) and (p^=pold^) and
              ((CurCharWidth <= 1) or (x=LastLineWidth) or (p[1]=pold[1])) then
             begin
               if (Spaces>0) then
                OutSpaces;
               skipped:=true;
               if CurCharWidth = 2 then
                 LastCharWasDoubleWidth:=true;
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
               chattr:=p^;
    {           if chattr.ch in [#0,#255] then
                chattr.ch:=' ';}
               if chattr.ExtendedGraphemeCluster=' ' then
                begin
                  if Spaces=0 then
                   begin
                     SpaceFg:=chattr.ForegroundColor;
                     SpaceBg:=chattr.BackgroundColor;
                     SpaceAttr:=chattr.EnhancedVideoAttributes;
                   end;
                  if (chattr.BackgroundColor=SpaceBg) and (chattr.EnhancedVideoAttributes=SpaceAttr) then
                   chattr.ForegroundColor:=SpaceFg
                  else
                   begin
                     OutSpaces;
                     SpaceFg:=chattr.ForegroundColor;
                     SpaceBg:=chattr.BackgroundColor;
                     SpaceAttr:=chattr.EnhancedVideoAttributes;
                   end;
                  inc(Spaces);
                end
               else
                begin
                  if (Spaces>0) then
                   OutSpaces;
    {              if ord(chattr.ch)<32 then
                    begin
                      Chattr.Attr:= $ff xor Chattr.Attr;
                      ChAttr.ch:=chr(ord(chattr.ch)+ord('A')-1);
                    end;}
                  if (LastFg<>chattr.ForegroundColor) or (LastBg<>chattr.BackgroundColor) or (LastAttr<>chattr.EnhancedVideoAttributes) then
                   OutClr(chattr.ForegroundColor,chattr.BackgroundColor,chattr.EnhancedVideoAttributes);
                  OutData(transform(chattr.ExtendedGraphemeCluster));
                  if CurCharWidth=2 then
                   begin
                    LastX:=x+2;
                    LastCharWasDoubleWidth:=True;
                   end
                  else
                   begin
                     LastX:=x+1;
                     LastCharWasDoubleWidth:=False;
                   end;
                  LastY:=y;
                end;
               //p^:=chattr;
             end;
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
  if (Console=ttyFreeBSD) and (p^<>pold^) Then
   begin
    OutData(XY2Ansi(ScreenWidth,ScreenHeight,LastX,LastY));
    OutData(#8);
    {Output last AnsiChar}
    chattr:=p[1];
    if (LastFg<>chattr.ForegroundColor) or (LastBg<>chattr.BackgroundColor) or (LastAttr<>chattr.EnhancedVideoAttributes) then
     OutClr(chattr.ForegroundColor,chattr.BackgroundColor,chattr.EnhancedVideoAttributes);
    OutData(transform(chattr.ExtendedGraphemeCluster));
    inc(LastX);
//    OutData(XY2Ansi(ScreenWidth-1,ScreenHeight,LastX,LastY));
//   OutData(GetTermString(Insert_character));
    OutData(#8+#27+'[1@');

    chattr:=p^;
    if (LastFg<>chattr.ForegroundColor) or (LastBg<>chattr.BackgroundColor) or (LastAttr<>chattr.EnhancedVideoAttributes) then
     OutClr(chattr.ForegroundColor,chattr.BackgroundColor,chattr.EnhancedVideoAttributes);
    OutData(transform(chattr.ExtendedGraphemeCluster));
    inc(LastX);
   end;
  OutData(XY2Ansi(CursorX+1,CursorY+1,LastX,LastY));
  if in_ACS then
    begin
      {If the program crashes and the ACS is still enabled, the user's
       keyboard will output strange characters. Therefore we disable the
       acs after each screen update, so the risk that it happens is greatly
       reduced.}
{      SendEscapeSeqNdx(exit_alt_charset_mode);}
      outdata(acsout);
      in_acs:=false;
    end;
{$ifdef logging}
  blockwrite(f,logstart[1],length(logstart));
  blockwrite(f,nl,1);
  blockwrite(f,outptr,sizeof(outptr));
  blockwrite(f,nl,1);
  blockwrite(f,outbuf,outptr);
  blockwrite(f,nl,1);
{$endif logging}
  fpWrite(stdoutputhandle,outbuf,outptr);
 {turn autowrap on}
//  SendEscapeSeq(#27'[?7h');
end;

{$ifdef linux}
procedure update_vcsa(force:boolean);

const max_updates=64;

label update,update_all,equal_loop,unequal_loop;

var position,update_count,i:word;
    update_positions:array[0..max_updates-1] of word;
    update_lengths:array[0..max_updates-1] of word;

begin
  if force then
    goto update_all;

  update_count:=0;
  i:=0;

equal_loop:
  repeat
    if videobuf^[i]<>oldvideobuf^[i] then
      goto unequal_loop;
    inc(i);
  until i>videobufsize div 2;
  goto update;

unequal_loop:
  if update_count>=max_updates then
    goto update_all;
  update_positions[update_count]:=i;
  update_lengths[update_count]:=0;
  inc(update_count);
  repeat
    if videobuf^[i]=oldvideobuf^[i] then
      goto equal_loop;
    inc(i);
    inc(update_lengths[update_count-1]);
  until i>videobufsize div 2;

update:
  for i:=1 to update_count do
    begin
      position:=update_positions[i-1];
      fppwrite(ttyfd,videobuf^[position],update_lengths[i-1]*2,4+position*2);
    end;
  exit;
update_all:
  fppwrite(ttyfd,videobuf^,videobufsize,4);
end;
{$endif}

var
  preInitVideoTio, postInitVideoTio: termio.termios;
  inputRaw, outputRaw: boolean;

procedure saveRawSettings(const tio: termio.termios);

begin
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

procedure decide_codepages;

var s:shortstring;

begin
  if is_vga_code_page(external_codepage) then
    begin
      {Possible override...}
      s:=upcase(fpgetenv('CONSOLEFONT_CP'));
      if s='CP437' then
        external_codepage:=437
      else if s='CP850' then
        external_codepage:=850;
    end;
  {A non-vcsa Linux console can display most control characters, but not all.}
  case external_codepage of
    CP_ISO01:            {West Europe}
      CurrentLegacy2EnhancedTranslationCodePage:=850;
    CP_ISO02:            {East Europe}
      CurrentLegacy2EnhancedTranslationCodePage:=852;
    CP_ISO05:            {Cyrillic}
      CurrentLegacy2EnhancedTranslationCodePage:=866;
    CP_UTF8:
      CurrentLegacy2EnhancedTranslationCodePage:=437;
    else
      if is_vga_code_page(external_codepage) then
        CurrentLegacy2EnhancedTranslationCodePage:=external_codepage
      else
        {We don't know how to convert to the external codepage. Use codepage
         437 in the hope that the actual font has similarity to codepage 437.}
        CurrentLegacy2EnhancedTranslationCodePage:=437;
  end;
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
{$ifdef linux}
  FName: shortstring;
{$endif linux}
  WS: packed record
    ws_row, ws_col, ws_xpixel, ws_ypixel: Word;
  end;
{  Err: Longint;}
{  prev_term : TerminalCommon_ptr1;}
  term:shortstring;
  i:word;
{$ifdef Linux}
  s:string[15];
{$endif}
{$ifdef freebsd}
  ThisTTY: String[30];
{$endif}

const font_vga:array[0..11] of AnsiChar=#15#27'%@'#27'(U'#27'[3h';
      font_lat1:array[0..5] of AnsiChar=#27'%@'#27'(B';

begin
  { check for tty }
  if (IsATTY(stdinputhandle)=1) then
   begin
     { save current terminal characteristics and remove rawness }
     prepareInitVideo;
{$ifdef linux}
     { running on a tty, find out whether locally or remotely }
     TTyfd:=-1;
{$endif linux}
     Console:=TTyNetwork;                 {Default: Network or other vtxxx tty}
     cur_term_strings:=@term_codes_vt100; {Default: vt100}
     external_codepage:=CP_ISO01;         {Default: ISO-8859-1}
     if UTF8Enabled then
       external_codepage:=CP_UTF8;
   {$ifdef linux}
     if (vcs_device>=0) and (external_codepage<>CP_UTF8) then
       begin
         str(vcs_device,s);
         fname:='/dev/vcsa'+s;
         { open console, $1b6=rw-rw-rw- }
         ttyfd:=fpopen(fname,$1b6,O_RDWR);
         if ttyfd<>-1 then
           begin
             console:=ttylinux;
             external_codepage:=437;  {VCSA defaults to codepage 437.}
           end
         else
           if try_grab_vcsa then
             begin
               ttyfd:=fpopen(fname,$1b6,O_RDWR);
               if ttyfd<>-1 then
                 begin
                   console:=ttylinux;
                   external_codepage:=437;  {VCSA defaults to codepage 437.}
                 end;
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
    if cur_term_strings=@term_codes_xterm then
    begin
      {$ifdef haiku}
      TerminalSupportsBold := true;
      TerminalSupportsHighIntensityColors := false;
      {$else}
      TerminalSupportsBold := false;
      TerminalSupportsHighIntensityColors := true;
      {$endif}
    end
    else
    begin
      TerminalSupportsBold := true;
      TerminalSupportsHighIntensityColors := false;
    end;
    if cur_term_strings=@term_codes_beos then
    begin
      TerminalSupportsBold := false;
      TerminalSupportsHighIntensityColors := false;
    end;
    if cur_term_strings=@term_codes_freebsd then
      console:=ttyFreeBSD;
{$ifdef linux}
    if (console<>ttylinux) then
      begin
{$endif}
        if cur_term_strings=@term_codes_linux then
          begin
            if external_codepage<>CP_UTF8 then
              begin
                {Enable the VGA character set (codepage 437,850,....)}
                fpwrite(stdoutputhandle,font_vga,sizeof(font_vga));
                external_codepage:=437;  {Now default to codepage 437.}
              end;
          end
        else
          begin
            if external_codepage<>CP_UTF8 then
              begin
                {No VGA font  :(  }
                fpwrite(stdoutputhandle,font_lat1,sizeof(font_lat1));
              end;
            { running on a remote terminal, no error with /dev/vcsa }
          end;
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
        SendEscapeSeqNdx(enter_ca_mode);
        SendEscapeSeqNdx(cursor_home);
        SendEscapeSeqNdx(cursor_normal);
        SendEscapeSeqNdx(cursor_visible_underline);
        SetCursorType(crUnderLine);
        If Console=ttyFreeBSD Then
          SendEscapeSeqNdx(exit_am_mode);
   {$ifdef linux}
      end;
   {$endif}
{   Always true because of vt100 default...
      if assigned(cur_term_Strings) then
       begin}
         ACSIn:=StrPas(cur_term_strings^[enter_alt_charset_mode]);
         ACSOut:=StrPas(cur_term_strings^[exit_alt_charset_mode]);
         if (ACSIn<>'') and (ACSOut<>'') then
           SendEscapeSeqNdx(ena_acs);
(*         If fpGetEnv('TERM')='xterm' then
           convert:=cv_vga_to_acs;  {use of acs for xterm is ok}*)
{       end
     else
       begin
         ACSIn:='';
         ACSOut:='';
       end;}
{$ifdef logging}
     assign(f,'video.log');
     rewrite(f,1);
{$endif logging}
     { save new terminal characteristics and possible restore rawness }
     videoInitDone;

     decide_codepages;
   end
  else
   ErrorCode:=errVioInit; { not a TTY }
end;

procedure SysDoneVideo;

var font_custom:array[0..2] of AnsiChar=#27'(K';

begin
  prepareDoneVideo;
  SetCursorType(crUnderLine);
{$ifdef linux}
  if Console=ttylinux then
   SetCursorPos(0,0)
  else
   begin
{$endif}
     SendEscapeSeqNdx(cursor_home);
     SendEscapeSeqNdx(cursor_normal);
     SendEscapeSeqNdx(cursor_visible_underline);
     SendEscapeSeq(#27'[H');
     SendEscapeSeqNdx(exit_ca_mode);
     if cur_term_strings=@term_codes_linux then
       begin
         {Executed in case ttylinux is false (i.e. no vcsa), but
          TERM=linux.}

         { if we're in utf8 mode, we didn't change the font, so
           no need to restore anything }
         if external_codepage<>CP_UTF8 then
         begin
           {Enable the character set set through setfont}
           fpwrite(stdoutputhandle,font_custom,3);
         end;
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
  I: Integer;
begin
{$ifdef linux}
  if console=ttylinux then
    update_vcsa(force)
  else
{$endif}
    updateTTY(force);
  for I := Low(EnhancedVideoBuf) to High(EnhancedVideoBuf) do
    OldEnhancedVideoBuf[I] := EnhancedVideoBuf[I];
end;


function SysGetCapabilities: Word;
begin
{ about cpColor... we should check the terminfo database... }
  SysGetCapabilities:=cpUnderLine + cpBlink + cpColor;
end;


procedure SysSetCursorPos(NewCursorX, NewCursorY: Word);
{$ifdef linux}
var
  Pos : array [1..2] of Byte;
{$endif linux}
begin
 if (CursorX=NewCursorX) and (CursorY=NewCursorY) then
    exit;
{$ifdef linux}
  if Console=ttylinux then
   begin
     Pos[1]:=NewCursorX;
     Pos[2]:=NewCursorY;
     fppwrite(ttyfd,pos,2,2);
   end
  else
{$endif}
    { newcursorx,y and CursorX,Y are 0 based ! }
    SendEscapeSeq(XY2Ansi(NewCursorX+1,NewCursorY+1,CursorX+1,CursorY+1));
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
    crBlock:
      SendEscapeSeqNdx(cursor_visible_block);
    crHidden:
      SendEscapeSeqNdx(cursor_invisible);
  else
    SendEscapeSeqNdx(cursor_normal);
  end;
end;

function SysSetVideoMode(const mode:Tvideomode):boolean;

var winsize:Twinsize;

begin
  {Due to xterm resize this procedure might get called with the new xterm
   size. Approve the video mode change if the new size equals that of
   the terminal window size.}
  SysSetVideoMode:=false;
  fpioctl(stdinputhandle,TIOCGWINSZ,@winsize);
  if (mode.row=winsize.ws_row) and
     (mode.col=winsize.ws_col) then
    begin
      screenwidth:=mode.col;
      screenheight:=mode.row;
      screencolor:=true;
      SysSetVideoMode:=true;
    end;
end;

Const
  SysVideoDriver : TVideoDriver = (
    InitDriver : nil;
    InitEnhancedDriver: @SysInitVideo;
    DoneDriver : @SysDoneVideo;
    UpdateScreen : @SysUpdateScreen;
    UpdateScreenArea : Nil;
    ClearScreen : @SysClearScreen;
    SetVideoMode : @SysSetVideoMode;
    GetVideoModeCount : Nil;
    GetVideoModeData : Nil;
    SetCursorPos : @SysSetCursorPos;
    GetCursorType : @SysGetCursorType;
    SetCursorType : @SysSetCursorType;
    GetCapabilities : @SysGetCapabilities;
    GetActiveCodePage : Nil;
    ActivateCodePage : Nil;
    GetSupportedCodePageCount : Nil;
    GetSupportedCodePage : Nil;
  );

initialization
  SetVideoDriver(SysVideoDriver);
end.
