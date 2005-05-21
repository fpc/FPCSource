{****************************************************************************


                            Standard CRT unit.
                    Free Pascal runtime library for EMX.
                    Copyright (c) 1997 Daniel Mantione.

      This file may be reproduced and modified under the same conditions
                      as all other Free Pascal source code.

****************************************************************************}

unit crt;

{$ASMMODE ATT}

interface

{$i crth.inc}

{cemodeset means that the procedure textmode has failed to set up a mode.}

type
  cexxxx=(cenoerror,cemodeset);

var
 crt_error:cexxxx;                   {Crt-status.            RW}

{***************************************************************************}

implementation

{$i textrec.inc}

const   extkeycode:char=#0;

var maxrows,maxcols:word;
    calibration:longint;

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
        Pviomodeinfo=^viomodeinfo;

    TVioCursorInfo=record
        case boolean of
        false:(
        yStart:word;    {Cursor start (top) scan line (0-based)}
        cEnd:word;      {Cursor end (bottom) scan line}
        cx:word;        {Cursor width (0=default width)}
        Attr:word);     {Cursor colour attribute (-1=hidden)}
        true:(
        yStartInt: integer; {integer variants can be used to specify negative}
        cEndInt:integer; {negative values (interpreted as percentage by OS/2)}
        cxInt:integer;
        AttrInt:integer);
    end;
    PVioCursorInfo=^TVioCursorInfo;

{EMXWRAP.DLL has strange calling conventions: All parameters must have
 a 4 byte size.}

function kbdcharin(var Akeyrec:Tkbdkeyinfo;wait,kbdhandle:longint):word; cdecl;
                   external 'EMXWRAP' index 204;
function kbdpeek(var Akeyrec:TkbdkeyInfo;kbdhandle:word):word; cdecl;
                 external 'EMXWRAP' index 222;

function dossleep(time:cardinal):cardinal; cdecl;
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
function viowrtTTY(s:Pchar;len,viohandle:longint):word; cdecl;
                      external 'EMXWRAP' index 119;
function viowrtcharstratt(s:Pchar;len,row,col:longint;var attr:byte;
                          viohandle:longint):word; cdecl;
                          external 'EMXWRAP' index 148;
function viogetmode(var Amodeinfo:viomodeinfo;viohandle:longint):word; cdecl;
                    external 'EMXWRAP' index 121;
function viosetmode(var Amodeinfo:viomodeinfo;viohandle:longint):word; cdecl;
                    external 'EMXWRAP' index 122;
function VioSetCurType(var CurData:TVioCursorInfo;VioHandle:word):word; cdecl;
external 'EMXWRAP' index 132;
{external 'VIOCALLS' index 32;}
function VioGetCurType(var CurData:TVioCursorInfo;VioHandle:word):word; cdecl;
external 'EMXWRAP' index 127;
{external 'VIOCALLS' index 27;}

procedure syscall;external name '___SYSCALL';


procedure setscreenmode(mode:word);

{ This procedure sets a new videomode. Note that the constants passes to
  this procedure are different than in the dos mode.}

const   modecols:array[0..2] of word=(40,80,132);
        moderows:array[0..3] of word=(25,28,43,50);

var newmode:viomodeinfo;

begin
    if os_mode=osOS2 then
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
        end
    else
        begin
            maxcols:=modecols[mode and 15];
            maxrows:=moderows[mode shr 4];
            crt_error:=cenoerror;
            {Set correct vertical resolution.}
            asm
                movw $0x1202,%ax
                movw 8(%ebp),%bx
                shrw $4,%bx
                cmpb $2,%bl
                jne .L_crtsetmode_a1
                decw %ax
            .L_crtsetmode_a1:
                mov $0x30,%bl
                int $0x10
            end;
            {132 column mode in DOS is videocard dependend.}
            if mode and 15=2 then
                begin
                    crt_error:=cemodeset;
                    exit;
                end;
            {Switch to correct mode.}
            asm
                mov 8(%ebp),%bx
                and $15,%bl
                mov $1,%ax
                cmp $1,%bl
                jne .L_crtsetmode_b1
                mov $3,%al
            .L_crtsetmode_b1:
                int $0x10
            {Use alternate print-screen function.}
                mov $0x12,%ah
                mov $0x20,%bl
                int $0x10
            end;
            {Set correct font.}
            case mode shr 4 of
                1:
                    {Set 8x14 font.}
                    asm
                        mov $0x1111,%ax
                        mov $0,%bl
                        int $0x10
                    end;
                2,3:
                    {Set 8x8 font.}
                    asm
                        mov $0x1112,%ax
                        mov $0,%bl
                        int $0x10
                    end;
            end;
        end;
end;

procedure getcursor(var y,x:word);

{Get the cursor position.}

begin
    if os_mode=osOS2 then
        viogetcurpos(y,x,0)
    else
        asm
            movb $3,%ah
            movb $0,%bh
            int $0x10
            movl y,%eax
            movl x,%ebx
            movzbl %dh,%edi
            andw $255,%dx
            movw %di,(%eax)
            movw %dx,(%ebx)
        end;
end;

{$ASMMODE INTEL}
procedure setcursor(y,x:word);

{Set the cursor position.}

begin
    if os_mode=osOS2 then
        viosetcurpos(y,x,0)
    else
        asm
            mov ah, 2
            mov bh, 0
            mov dh, byte ptr y
            mov dl, byte ptr x
            int 10h
        end;
end;

procedure scroll_up(top,left,bottom,right,lines:word;var screl:word);

begin
    if os_mode=osOS2 then
        vioscrollup(top,left,bottom,right,lines,screl,0)
    else
        asm
            mov ah, 6
            mov al, byte ptr lines
            mov edi, screl
            mov bh, [edi + 1]
            mov ch, byte ptr top
            mov cl, byte ptr left
            mov dh, byte ptr bottom
            mov dl, byte ptr right
            int 10h
        end;
end;

procedure scroll_dn(top,left,bottom,right,lines:word;var screl:word);

begin
    if os_mode=osOS2 then
        vioscrolldn(top,left,bottom,right,lines,screl,0)
    else
        asm
            mov ah, 7
            mov al, byte ptr lines
            mov edi, screl
            mov bh, [edi + 1]
            mov ch, byte ptr top
            mov cl, byte ptr left
            mov dh, byte ptr bottom
            mov dl, byte ptr right
            int 10h
        end;
end;

{$ASMMODE ATT}
function keypressed:boolean;

{Checks if a key is pressed.}

var Akeyrec:Tkbdkeyinfo;

begin
    if os_mode=osOS2 then
        begin
            kbdpeek(Akeyrec,0);
            keypressed:=(extkeycode<>#0) or ((Akeyrec.fbstatus and $40)<>0);
        end
    else
        begin
            if extkeycode<>#0 then
                begin
                    keypressed:=true;
                    exit
                end
            else
                asm
                    movb $1,%ah
                    int $0x16
                    setnz %al
                    movb %al,__RESULT
                end;
        end;
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
            if os_mode=osOS2 then
                begin
                    kbdcharin(Akeyrec,0,0);
                    c:=Akeyrec.charcode;
                    s:=Akeyrec.scancode;
                    if (c=#224) and (s<>#0) then
                        c:=#0;
                end
            else
                begin
                    asm
                        movb $0,%ah
                        int $0x16
                        movb %al,c
                        movb %ah,s
                    end;
                end;
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
    left:=lo(windmin);
    right:=lo(windmax);
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
    left:=lo(windmin);
    right:=lo(windmax);
    bot:=hi(windmax);
    fil:=$20 or (textattr shl 8);
    scroll_dn(row,left,bot,right,1,fil);
end;

procedure TextMode (Mode: word);

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

procedure textcolor(color:byte);

{All text written after calling this will have color as foreground colour.}

begin
    textattr:=(textattr and $70) or (color and $f)+color and 128;
end;

procedure textbackground(color:byte);

{All text written after calling this will have colour as background colour.}

begin
    textattr:=(textattr and $8f) or ((color and $7) shl 4);
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

var i,j:longint;

{Waits ms microseconds. The DOS code is copied from the DOS rtl.}

begin
    {Under OS/2 we could also calibrate like under DOS. But this is
     unreliable, because OS/2 can hold our programs while calibrating,
     if it needs the processor for other things.}
    if os_mode=osOS2 then
        dossleep(ms)
    else
        begin
            for i:=1 to ms do
                for j:=1 to calibration do
                    begin
                    end;
        end;
end;

procedure window(X1,Y1,X2,Y2:byte);

{Change the write window to the given coordinates.}

begin
    if (X1<1) or
     (Y1<1) or
     (X2>maxcols) or
     (Y2>maxrows) or
     (X1>X2) or
     (Y1>Y2) then
        exit;
    windmin:=(X1-1) or ((Y1-1) shl 8);
    windmax:=(X2-1) or ((Y2-1) shl 8);
    gotoXY(1,1);
end;

{$ASMMODE INTEL}
procedure writePchar(s:Pchar;len:word);

{Write a series of characters to the screen.

 Not very fast, but is just text-mode isn't it?}

var x,y:word;
    c:char;
    i,n:integer;
    screl:word;
    ca:Pchar;

begin
    i:=0;
    getcursor(y,x);
    while i<=len-1 do
        begin
            case s[i] of
                #7: asm
                     mov dl, 7
                     mov ah, 2
                     call syscall
                    end;
                #8: if X > Succ (Lo (WindMin)) then Dec (X);
         {      #9: x:=(x-lo(windmin)) and $fff8+8+lo(windmin);}
                #10: inc(y);
                #13: x:=lo(windmin);
                else
                    begin
                        ca:=@s[i];
                        n:=1;
                        while not(s[i+1] in [#7,#8,#10,#13]) and
{                         (x+n<=lo(windmax)+1) and (i<len-1) do}
                         (x+n<=lo(windmax)) and (i<len-1) do
                            begin
                                inc(n);
                                inc(i);
                            end;
                        if os_mode=osOS2 then
                            viowrtcharstratt(ca,n,y,x,textattr,0)
                        else
                            asm
                                mov ax, 1300h
                                mov bh, 0
                                mov bl, TEXTATTR
                                mov dh, byte ptr y
                                mov dl, byte ptr x
                                mov cx, n
                                push ebp
                                mov ebp, ca
                                int 10h
                                pop ebp
                            end;
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
{           writeln(stderr,x,'  ',y);}
            inc(i);
        end;
    setcursor(y,x);
end;

{$ASMMODE ATT}
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

function get_ticks:word;

type    Pword=^word;

begin
    get_ticks:=Pword(longint(first_meg)+$46c)^;
end;

procedure initdelay;

{Calibrate the delay procedure. Copied from DOS rtl.}

var first:word;

begin
    calibration:=0;

    { wait for new tick }
    first:=get_ticks;
    while get_ticks=first do
        begin
        end;
    first:=get_ticks;

    { this estimates calibration }
    while get_ticks=first do
        inc(calibration);

    { calculate this to ms }
    calibration:=calibration div 70;
    while true do
        begin
            first:=get_ticks;
            while get_ticks=first do
                begin
                end;
            first:=get_ticks;
            delay(55);
            if first=get_ticks then
                exit
            else
                begin
                    { decrement calibration two percent }
                    calibration:=calibration-calibration div 50;
                    dec(calibration);
                end;
        end;
end;



{****************************************************************************
                             Extra Crt Functions
****************************************************************************}


{$ASMMODE INTEL}
procedure CursorOn;
var
 I: TVioCursorInfo;
begin
 if Os_Mode = osOS2 then
  begin
   VioGetCurType (I, 0);
   with I do
    begin
     yStartInt := -90;
     cEndInt := -100;
     Attr := 15;
    end;
   VioSetCurType (I, 0);
  end
 else
  asm
   push es
   push bp
   mov ax, 1130h
   mov bh, 0
   mov ecx, 0
   int 10h
   pop bp
   pop es
   or ecx, ecx
   jnz @COnOld
   mov cx, 0707h
   jmp @COnAll
@COnOld:
   dec cx
   mov ch, cl
   dec ch
@COnAll:
   mov ah, 1
   int 10h
  end;
end;


procedure CursorOff;
var
 I: TVioCursorInfo;
begin
 if Os_Mode = osOS2 then
  begin
   VioGetCurType (I, 0);
   I.AttrInt := -1;
   VioSetCurType (I, 0);
  end
 else
  asm
   mov ah, 1
   mov cx, 0FFFFh
   int 10h
  end;
end;


procedure CursorBig;
var
 I: TVioCursorInfo;
begin
 if Os_Mode = osOS2 then
  begin
   VioGetCurType (I, 0);
   with I do
    begin
     yStart := 0;
     cEndInt := -100;
     Attr := 15;
    end;
   VioSetCurType (I, 0);
  end
 else
  asm
   mov ah, 1
   mov cx, 1Fh
   int 10h
  end;
end;

{$ASMMODE DEFAULT}



{Initialization.}

type    Pbyte=^byte;

var curmode:viomodeinfo;
    mode:byte;

begin
    textattr:=lightgray;
    if os_mode=osOS2 then
        begin
            curmode.cb:=sizeof(curmode);
            viogetmode(curmode,0);
            maxcols:=curmode.col;
            maxrows:=curmode.row;
            lastmode:=0;
            case maxcols of
                40:
                    lastmode:=0;
                80:
                    lastmode:=1;
                132:
                    lastmode:=2;
            end;
            case maxrows of
                25:;
                28:
                    lastmode:=lastmode+16;
                43:
                    lastmode:=lastmode+32;
                50:
                    lastmode:=lastmode+48;
            end
        end
    else
        begin
            {Request video mode to determine columns.}
            asm
                mov $0x0f,%ah
                int $0x10
{                mov %al,_MODE }
                mov %al,MODE
            end;
            case mode of
                0,1:
                    begin
                        lastmode:=0;
                        maxcols:=40;
                    end;
                else
                    begin
                        lastmode:=1;
                        maxcols:=80;
                    end;
            end;
            {Get number of rows from realmode $0040:$0084.}
            maxrows:=Pbyte(longint(first_meg)+$484)^;
            case maxrows of
                25:;
                28:
                    lastmode:=lastmode+16;
                43:
                    lastmode:=lastmode+32;
                50:
                    lastmode:=lastmode+48;
            end
        end;
    windmin:=0;
    windmax:=((maxrows-1) shl 8) or (maxcols-1);
    if os_mode <> osOS2 then
        initdelay;
    crt_error:=cenoerror;
    assigncrt(input);
    textrec(input).mode:=fminput;
    assigncrt(output);
    textrec(output).mode:=fmoutput;
end.
