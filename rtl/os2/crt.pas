{****************************************************************************

    $Id$

                            Standard CRT unit.
                    Free Pascal runtime library for OS/2.
                    Copyright (c) 1997 Daniel Mantione.

      This file may be reproduced and modified under the same conditions
                      as all other Free Pascal source code.

****************************************************************************}

unit crt;


interface

uses dos;

const
  _40cols=0;
  _80cols=1;
  _132cols=2;
  _25rows=0;
  _28rows=16;
  _43rows=32;
  _50rows=48;
  font8x8=_50rows;

  black         =0;
  blue          =1;
  green         =2;
  cyan          =3;
  red           =4;
  magenta       =5;
  brown         =6;
  lightgray     =7;
  darkgray      =8;
  lightblue     =9;
  lightgreen    =10;
  lightcyan     =11;
  lightred      =12;
  lightmagenta  =13;
  yellow        =14;
  white         =15;
  blink         =128;

{cemodeset means that the procedure textmode has failed to set up a mode.}

type    cexxxx=(cenoerror,cemodeset);

var textattr:byte;                      {Text attribute.        RW}
    windmin,windmax:word;               {Window coordinates.    R-}
    lastmode:word;                      {Last videomode.        R-}
    crt_error:cexxxx;                   {Crt-status.            RW}

function keypressed:boolean;
function readkey:char;

procedure clrscr;
procedure clreol;
function whereX:byte;
function whereY:byte;
procedure gotoXY(x,y:byte);
procedure window(left,top,right,bottom : byte);
procedure textmode(mode:integer);
procedure textcolor(colour:byte);
procedure textbackground(colour:byte);
procedure insline;
procedure delline;
procedure lowvideo;
procedure normvideo;
procedure highvideo;
procedure assigncrt(var f:text);
procedure delay(ms:word);
procedure sound(hz:word);
procedure nosound;

{***************************************************************************}

{***************************************************************************}

implementation

const   extkeycode:char=#0;

var maxrows,maxcols:word;

type    Tkbdkeyinfo=record
            charcode,scancode:char;
            fbstatus,bnlsshift:byte;
            fsstate:word;
            time:longint;
        end;

        {if you have information on the folowing datastructure, please
         send them to me at d.s.p.mantione@twi.tudelft.nl}

        {This datastructure is needed when we ask in what video mode we are,
         or we want to set up a new mode.}

        viomodeinfo=record
            cb:word;                         { length of the entire data
                                               structure }
            fbtype,                          { bit mask of mode being set}
            color: byte;                     { number of colors (power of 2) }
            col,                             { number of text columns }
            row,                             { number of text rows }
            hres,                            { horizontal resolution }
            vres: word;                      { vertical resolution }
            fmt_ID,                          { attribute format
                                               ! more info wanted !}
            attrib: byte;                    { number of attributes }
            buf_addr,                        { physical address of
                                               videobuffer, e.g. $0b800}
            buf_length,                      { length of a videopage (bytes)}
            full_length,                     { total video-memory on video-
                                               card (bytes)}
            partial_length:longint;          { ????? info wanted !}
            ext_data_addr:pointer;           { ????? info wanted !}
        end;

{EMXWRAP.DLL has strange calling conventions: All parameters must have
 a 4 byte size.}

function kbdcharin(var Akeyrec:Tkbdkeyinfo;wait,kbdhandle:longint):word; cdecl;
                   external 'EMXWRAP' index 204;
function kbdpeek(var Akeyrec:TkbdkeyInfo;kbdhandle:word):word; cdecl;
                 external 'EMXWRAP' index 222;

function dossleep(time:longint):word; cdecl;
                  external 'DOSCALLS' index 229;
function vioscrollup(top,left,bottom,right,lines:longint;
                     var screl:word;viohandle:longint):word; cdecl;
                     external 'EMXWRAP' index 107;
function vioscrolldn(top,left,bottom,right,lines:longint;
                     var screl:word;viohandle:longint):word; cdecl;
                     external 'EMXWRAP' index 147;
function viogetcurpos(var row,column:word;viohandle:longint):word; cdecl;
                      external 'EMXWRAP' index 109;
function viosetcurpos(row,column,viohandle:longint):word; cdecl;
                      external 'EMXWRAP' index 115;
function viowrtcharstratt(s:Pchar;len,row,col:longint;var attr:byte;
                          viohandle:longint):word; cdecl;
                          external 'EMXWRAP' index 148;
function viogetmode(var Amodeinfo:viomodeinfo;viohandle:longint):word; cdecl;
                    external 'EMXWRAP' index 121;
function viosetmode(var Amodeinfo:viomodeinfo;viohandle:longint):word; cdecl;
                    external 'EMXWRAP' index 122;

procedure setscreenmode(mode:word);

{ This procedure sets a new videomode. Note that the constants passes to
  this procedure are different than in the dos mode.}

const   modecols:array[0..2] of word=(40,80,132);
        moderows:array[0..3] of word=(25,28,43,50);

var newmode:viomodeinfo;

begin
  newmode.cb:=8;
  newmode.fbtype:=1;          {Non graphics colour mode.}
  newmode.color:=4;           {We want 16 colours, 2^4=16.}
  newmode.col:=modecols[mode and 15];
  newmode.row:=moderows[mode shr 4];
  if viosetmode(newmode,0)=0 then
    crt_error:=cenoerror
  else
    crt_error:=cemodeset;
  maxcols:=newmode.col;
  maxrows:=newmode.row;
end;

procedure getcursor(var y,x:word);
{Get the cursor position.}
begin
  viogetcurpos(y,x,0)
end;

procedure setcursor(y,x:word);
{Set the cursor position.}
begin
  viosetcurpos(y,x,0)
end;

procedure scroll_up(top,left,bottom,right,lines:word;var screl:word);
begin
  vioscrollup(top,left,bottom,right,lines,screl,0)
end;

procedure scroll_dn(top,left,bottom,right,lines:word;var screl:word);
begin
  vioscrolldn(top,left,bottom,right,lines,screl,0)
end;

function keypressed:boolean;
{Checks if a key is pressed.}
var Akeyrec:Tkbdkeyinfo;

begin
  kbdpeek(Akeyrec,0);
  keypressed:=(extkeycode<>#0) or ((Akeyrec.fbstatus and $40)<>0);
end;

function readkey:char;
{Reads the next character from the keyboard.}
var Akeyrec:Tkbdkeyinfo;
    c,s:char;

begin
    if extkeycode<>#0 then
        begin
            readkey:=extkeycode;
            extkeycode:=#0
        end
    else
        begin
          kbdcharin(Akeyrec,0,0);
          c:=Akeyrec.charcode;
          s:=Akeyrec.scancode;
          if (c=#224) and (s<>#0) then
            c:=#0;
          if c=#0 then
            extkeycode:=s;
          readkey:=c;
        end;
end;

procedure clrscr;
{Clears the current window.}
var screl:word;

begin
    screl:=$20+textattr shl 8;
    scroll_up(hi(windmin),lo(windmin),
              hi(windmax),lo(windmax),
              hi(windmax)-hi(windmin)+1,
              screl);
    gotoXY(1,1);
end;

procedure gotoXY(x,y:byte);

{Positions the cursor on (x,y) relative to the window origin.}

begin
    if x<1 then
        x:=1;
    if y<1 then
        y:=1;
    if y+hi(windmin)-2>=hi(windmax) then
        y:=hi(windmax)-hi(windmin)+1;
    if x+lo(windmin)-2>=lo(windmax) then
        x:=lo(windmax)-lo(windmin)+1;
    setcursor(y+hi(windmin)-1,x+lo(windmin)-1);
end;

function whereX:byte;

{Returns the x position of the cursor.}

var x,y:word;

begin
    getcursor(y,x);
    whereX:=x-lo(windmin)+1;
end;

function whereY:byte;

{Returns the y position of the cursor.}

var x,y:word;

begin
    getcursor(y,x);
    whereY:=y-hi(windmin)+1;
end;

procedure clreol;
{Clear from current position to end of line.
Contributed by Michail A. Baikov}

var i:byte;

begin
    {not fastest, but compatible}
    for i:=wherex to lo(windmax) do write(' ');
        gotoxy(1,wherey); {may be not}
end;


procedure delline;

{Deletes the line at the cursor.}

var row,left,right,bot:longint;
    fil:word;

begin
    row:=whereY;
    left:=lo(windmin)+1;
    right:=lo(windmax)+1;
    bot:=hi(windmax)+1;
    fil:=$20 or (textattr shl 8);
    scroll_up(row+1,left,bot,right,1,fil);
end;

procedure insline;

{Inserts a line at the cursor position.}

var row,left,right,bot:longint;
    fil:word;

begin
    row:=whereY;
    left:=lo(windmin)+1;
    right:=lo(windmax)+1;
    bot:=hi(windmax);
    fil:=$20 or (textattr shl 8);
    scroll_dn(row,left,bot-1,right,1,fil);
end;

procedure textmode(mode:integer);

{ Use this procedure to set-up a specific text-mode.}

begin
    textattr:=$07;
    lastmode:=mode;
    mode:=mode and $ff;
    setscreenmode(mode);
    windmin:=0;
    windmax:=(maxcols-1) or ((maxrows-1) shl 8);
    clrscr;
end;

procedure textcolor(colour:byte);

{All text written after calling this will have color as foreground colour.}

begin
    textattr:=(textattr and $70) or (colour and $f)+colour and 128;
end;

procedure textbackground(colour:byte);

{All text written after calling this will have colour as background colour.}

begin
    textattr:=(textattr and $8f) or ((colour and $7) shl 4);
end;

procedure normvideo;

{Changes the text-background to black and the foreground to white.}

begin
    textattr:=$7;
end;

procedure lowvideo;

{All text written after this will have low intensity.}

begin
    textattr:=textattr and $f7;
end;

procedure highvideo;

{All text written after this will have high intensity.}

begin
    textattr:=textattr or $8;
end;

procedure delay(ms:word);
{Waits ms microseconds.}
begin
  dossleep(ms)
end;

procedure window(left,top,right,bottom:byte);
{Change the write window to the given coordinates.}
begin
    if (left<1) or
     (top<1) or
     (right>maxcols) or
     (bottom>maxrows) or
     (left>right) or
     (top>bottom) then
        exit;
    windmin:=(left-1) or ((top-1) shl 8);
    windmax:=(right-1) or ((bottom-1) shl 8);
    gotoXY(1,1);
end;

procedure writePchar(s:Pchar;len:word);
{Write a series of characters to the screen.
 Not very fast, but is just text-mode isn't it?}
var
  x,y:word;
  i,n:integer;
  screl:word;
  ca:Pchar;

begin
  i:=0;
  getcursor(y,x);
  while i<=len-1 do
  begin
    case s[i] of
      #8: x:=x-1;
      #9: x:=(x-lo(windmin)) and $fff8+8+lo(windmin);
      #10: ;
      #13: begin
              x:=lo(windmin);
              inc(y);
        end;
      else
      begin
        ca:=@s[i];
        n:=1;
        while not(s[i+1] in [#8,#9,#10,#13]) and
              (x+n<=lo(windmax)) and (i<len-1) do
        begin
          inc(n);
          inc(i);
        end;
        viowrtcharstratt(ca,n,y,x,textattr,0);
        x:=x+n;
      end;
    end;
    if x>lo(windmax) then
        begin
            x:=lo(windmin);
            inc(y);
        end;
    if y>hi(windmax) then
        begin
            screl:=$20+textattr shl 8;
            scroll_up(hi(windmin),lo(windmin),
                      hi(windmax),lo(windmax),
                      1,screl);
            y:=hi(windmax);
        end;
    inc(i);
  end;
  setcursor(y,x);
end;

function crtread(var f:textrec):word;
{Read a series of characters from the console.}
var max,curpos:integer;
    c:char;
    clist:array[0..2] of char;

begin
    max:=f.bufsize-2;
    curpos:=0;
    repeat
        c:=readkey;
        case c of
            #0:
                readkey;
            #8:
                if curpos>0 then
                    begin
                        clist:=#8' '#8;
                        writePchar(@clist,3);
                        dec(curpos);
                    end;
            #13:
                begin
                    f.bufptr^[curpos]:=#13;
                    inc(curpos);
                    f.bufptr^[curpos]:=#10;
                    inc(curpos);
                    f.bufpos:=0;
                    f.bufend:=curpos;
                    clist[0]:=#13;
                    writePchar(@clist,1);
                    break;
                end;
            #32..#255:
                if curpos<max then
                    begin
                        f.bufptr^[curpos]:=c;
                        inc(curpos);
                        writePchar(@c,1);
                    end;
        end;
    until false;
    crtread:=0;
end;

function crtwrite(var f:textrec):word;

{Write a series of characters to the console.}

begin
    writePchar(Pchar(f.bufptr),f.bufpos);
    f.bufpos:=0;
    crtwrite:=0;
end;


function crtopen(var f:textrec):integer;

begin
    if f.mode=fmoutput then
        crtopen:=0
    else
        crtopen:=5;
end;

function crtinout(var f:textrec):integer;

begin
    case f.mode of
        fminput:
            crtinout:=crtread(f);
        fmoutput:
            crtinout:=crtwrite(f);
    end;
end;

function crtclose(var f:textrec):integer;

begin
    f.mode:=fmclosed;
    crtclose:=0;
end;

procedure assigncrt(var f:text);

{Assigns a file to the crt console.}

begin
    textrec(f).mode:=fmclosed;
    textrec(f).bufsize:=128;
    textrec(f).bufptr:=@textrec(f).buffer;
    textrec(f).bufpos:=0;
    textrec(f).openfunc:=@crtopen;
    textrec(f).inoutfunc:=@crtinout;
    textrec(f).flushfunc:=@crtinout;
    textrec(f).closefunc:=@crtclose;
    textrec(f).name[0]:='.';
    textrec(f).name[0]:=#0;
end;

procedure sound(hz:word);
{sound and nosound are not implemented because the OS/2 API supports a freq/
 duration procedure instead of start/stop procedures.}
begin
end;

procedure nosound;
begin
end;

{Initialization.}

var
  curmode: viomodeinfo;
begin
  textattr:=lightgray;
  curmode.cb:=sizeof(curmode);
  viogetmode(curmode,0);
  maxcols:=curmode.col;
  maxrows:=curmode.row;
  lastmode:=0;
  case maxcols of
    40: lastmode:=0;
    80: lastmode:=1;
    132: lastmode:=2;
  end;
  case maxrows of
    25:;
    28: lastmode:=lastmode+16;
    43: lastmode:=lastmode+32;
    50: lastmode:=lastmode+48;
  end;
  windmin:=0;
  windmax:=((maxrows-1) shl 8) or (maxcols-1);
  crt_error:=cenoerror;
  assigncrt(input);
  textrec(input).mode:=fminput;
  assigncrt(output);
  textrec(output).mode:=fmoutput;
end.

{
  $Log$
  Revision 1.4  2003-09-24 12:30:08  yuri
  * Removed emx code from crt.pas
  - Removed doscalls.imp (not full and obsolete)

  Revision 1.3  2002/08/04 19:37:55  hajny
    * fix for bug 1998 (write in window) + removed warnings


}
