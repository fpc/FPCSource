{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by Florian Klaempfl,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit crt;
interface

{$I os.inc}


const
{ CRT modes }
  BW40          = 0;            { 40x25 B/W on Color Adapter }
  CO40          = 1;            { 40x25 Color on Color Adapter }
  BW80          = 2;            { 80x25 B/W on Color Adapter }
  CO80          = 3;            { 80x25 Color on Color Adapter }
  Mono          = 7;            { 80x25 on Monochrome Adapter }
  Font8x8       = 256;          { Add-in for ROM font }

{ Mode constants for 3.0 compatibility }
  C40           = CO40;
  C80           = CO80;

{ Foreground and background color constants }
  Black         = 0;
  Blue          = 1;
  Green         = 2;
  Cyan          = 3;
  Red           = 4;
  Magenta       = 5;
  Brown         = 6;
  LightGray     = 7;

{ Foreground color constants }
  DarkGray      = 8;
  LightBlue     = 9;
  LightGreen    = 10;
  LightCyan     = 11;
  LightRed      = 12;
  LightMagenta  = 13;
  Yellow        = 14;
  White         = 15;

{ Add-in for blinking }
  Blink         = 128;

var

{ Interface variables }
  CheckBreak: Boolean;    { Enable Ctrl-Break }
  CheckEOF: Boolean;      { Enable Ctrl-Z }
  DirectVideo: Boolean;   { Enable direct video addressing }
  CheckSnow: Boolean;     { Enable snow filtering }
  LastMode: Word;         { Current text mode }
  TextAttr: Byte;         { Current text attribute }
  WindMin: Word;          { Window upper left coordinates }
  WindMax: Word;          { Window lower right coordinates }

{ Interface procedures }
procedure AssignCrt(var F: Text);
function KeyPressed: Boolean;
function ReadKey: Char;
procedure TextMode(Mode: Integer);
procedure Window(X1,Y1,X2,Y2: Byte);
procedure GotoXY(X,Y: Byte);
function WhereX: Byte;
function WhereY: Byte;
procedure ClrScr;
procedure ClrEol;
procedure InsLine;
procedure DelLine;
procedure TextColor(Color: Byte);
procedure TextBackground(Color: Byte);
procedure LowVideo;
procedure HighVideo;
procedure NormVideo;
procedure Delay(MS: Word);
procedure Sound(Hz: Word);
procedure NoSound;

{Extra Functions}
procedure cursoron;
procedure cursoroff;
procedure cursorbig;


implementation

uses
  go32;

var
  startattrib     : byte;
  col,row,
  maxcols,maxrows : longint;

{
  definition of textrec is in textrec.inc
}
{$i textrec.inc}

{****************************************************************************
                           Low level Routines
****************************************************************************}

    function getscreenmode : byte;
      begin
         dosmemget($40,$49,getscreenmode,1);
      end;

    procedure setscreenmode(mode : byte);

     var regs : trealregs;

      begin
{$ifdef GO32V2}
         regs.realeax:=mode;
         realintr($10,regs);
{$else GO32V2}
         asm
            movb 8(%ebp),%al
            xorb %ah,%ah
            pushl %ebp
            int $0x10
            popl %ebp
         end;
{$endif GO32V2}
      end;

    function screenrows : byte;

      begin
         dosmemget($40,$84,screenrows,1);
         { don't forget this: }
         inc(screenrows);
      end;

    function screencols : byte;

      begin
         dosmemget($40,$4a,screencols,1);
      end;

    function get_addr(row,col : byte) : word;

      begin
         get_addr:=((row-1)*maxcols+(col-1))*2;
      end;

    procedure screensetcursor(row,col : longint);

      var
         cols : byte;
         pos : word;

{$ifdef GO32V2}
         regs : trealregs;
{$endif GO32V2}
      begin
         if directvideo then
           begin
              { set new position for the BIOS }
              dosmemput($40,$51,row,1);
              dosmemput($40,$50,col,1);

              { calculates screen position }
              dosmemget($40,$4a,cols,1);
              { FPKPascal calculates with 32 bit }
              pos:=row*cols+col;

              { direct access to the graphics card registers }
              outportb($3d4,$0e);
              outportb($3d5,hi(pos));
              outportb($3d4,$0f);
              outportb($3d5,lo(pos));
           end
         else
{$ifndef GO32V2}
            asm
               movb     $0x02,%ah
               movb     $0,%bh
               movb     row,%dh
               movb     col,%dl
               pushl    %ebp
               int      $0x10
               popl     %ebp
            end;
{$else GO32V2}
            regs.realeax:=$0200;
            regs.realebx:=0;
            regs.realedx:=row*$100+col;
            realintr($10,regs);
{$endif GO32V2}
       end;

    procedure screengetcursor(var row,col : longint);

      begin
         col:=0;
         row:=0;
         dosmemget($40,$50,col,1);
         dosmemget($40,$51,row,1);
         inc(col);
         inc(row);
      end;

    { exported routines }

    procedure cursoron;

{$ifdef GO32V2}
    var     regs : trealregs;
{$endif GO32V2}
      begin
{$ifndef GO32V2}
         asm
            movb   $1,%ah
            movb   $10,%cl
            movb   $9,%ch
            pushl %ebp
            int   $0x10
            popl %ebp
         end;
{$else GO32V2}
            regs.realeax:=$0100;
            regs.realecx:=$90A;
            realintr($10,regs);
{$endif GO32V2}
      end;

    procedure cursoroff;

{$ifdef GO32V2}
    var     regs : trealregs;
{$endif GO32V2}
      begin
{$ifndef GO32V2}
         asm
            movb   $1,%ah
            movb   $-1,%cl
            movb   $-1,%ch
            pushl %ebp
            int   $0x10
            popl %ebp
         end;
{$else GO32V2}
            regs.realeax:=$0100;
            regs.realecx:=$ffff;
            realintr($10,regs);
{$endif GO32V2}
      end;

    procedure cursorbig;

{$ifdef GO32V2}
    var     regs : trealregs;
{$endif GO32V2}
      begin
{$ifdef GO32V2}
            regs.realeax:=$0100;
            regs.realecx:=$10A;
            realintr($10,regs);
{$else GO32V2}
         asm
            movb   $1,%ah
            movb   $10,%cl
            movb   $1,%ch
            pushl %ebp
            int   $0x10
            popl %ebp
         end;
{$endif GO32V2}
      end;

    var
       is_last : boolean;
       last : char;

    function readkey : char;

      var
         char2 : char;
         char1 : char;
{$ifdef GO32V2}
    var     regs : trealregs;
{$endif GO32V2}

      begin
         if is_last then
           begin
              is_last:=false;
              readkey:=last;
           end
         else
           begin
{$ifdef GO32V2}
            regs.realeax:=$0000;
            realintr($16,regs);
            byte(char1):=regs.realeax and $ff;
            byte(char2):=(regs.realeax and $ff00) div $100;
{$else GO32V2}
              asm
                 movb $0,%ah
                 pushl %ebp
                 int $0x16
                 popl %ebp
                 movw %ax,-2(%ebp)
              end;
{$endif GO32V2}
              if char1=#0 then
                begin
                   is_last:=true;
                   last:=char2;
                end;
              readkey:=char1;
           end;
      end;

    function keypressed : boolean;

{$ifdef GO32V2}
   var regs : trealregs;
{$endif GO32V2}
      begin
         if is_last then
           begin
              keypressed:=true;
              exit;
           end
         else
{$ifdef GO32V2}
         begin
            regs.realeax:=$0100;
            realintr($16,regs);
            if (regs.realflags and zeroflag) = 0 then
              keypressed:=true
              else keypressed:=false;
         end;
{$else GO32V2}
           asm
              movb $1,%ah
              pushl %ebp
              int $0x16
              popl %ebp
              setnz %al
              movb %al,__RESULT
           end;
{$endif GO32V2}
      end;

   procedure gotoxy(x,y : byte);

     begin
        if (x<1) then
          x:=1;
        if (y<1) then
          y:=1;
        if y+hi(windmin)-2>=hi(windmax) then
          y:=hi(windmax)-hi(windmin)+1;
        if x+lo(windmin)-2>=lo(windmax) then
          x:=lo(windmax)-lo(windmin)+1;
        screensetcursor(y+hi(windmin)-1,x+lo(windmin)-1);
     end;

   function wherex : byte;

     var
        row,col : longint;

     begin
        screengetcursor(row,col);
        wherex:=col-lo(windmin);
     end;

   function wherey : byte;

     var
        row,col : longint;

     begin
        screengetcursor(row,col);
        wherey:=row-hi(windmin);
     end;

   procedure Window(X1,Y1,X2,Y2: Byte);
     begin
        if (x1<1) or (x2>screencols) or (y2>screenrows) or
           (x1>x2) or (y1>y2) then
          exit;
        windmin:=(x1-1) or ((x1-1) shl 8);
        windmax:=(x2-1) or ((y2-1) shl 8);
        gotoxy(1,1);
     end;

   procedure clrscr;
     var
        fil : word;
        row : longint;
     begin
        fil:=32 or (textattr shl 8);
        for row:=hi(windmin) to hi(windmax) do
          dosmemfillword($b800,get_addr(row+1,lo(windmin)+1),lo(windmax)-lo(windmin)+1,fil);
        gotoxy(1,1);
     end;


   procedure textcolor(color : Byte);
     begin
        textattr:=(textattr and $70) or color;
     end;


   procedure lowvideo;
     begin
        textattr:=textattr and $f7;
     end;


   procedure highvideo;
     begin
        textattr:=textattr or $08;
     end;


   procedure textbackground(color : Byte);
     begin
        textattr:=(textattr and $8f) or ((color and $7) shl 4);
     end;


   procedure normvideo;
     begin
        textattr:=startattrib;
     end;


   procedure removeline(line : byte);
     var
        row,left,right,bot : longint;
        fil : word;
     begin
        row:=line+hi(windmin);
        left:=lo(windmin)+1;
        right:=lo(windmax)+1;
        bot:=hi(windmax)+1;
        fil:=32 or (textattr shl 8);
        while (row<bot) do
          begin
             dosmemmove($b800,get_addr(row+1,left),$b800,get_addr(row,left),(right-left+1)*2);
             inc(row);
          end;
        dosmemfillword($b800,get_addr(bot,left),right-left+1,fil);
     end;


   procedure delline;
     begin
        removeline(wherey);
     end;

   procedure insline;

     var
        row,col,left,right,bot : longint;
        fil : word;

     begin
        screengetcursor(row,col);
        inc(row);
        left:=lo(windmin)+1;
        right:=lo(windmax)+1;
        bot:=hi(windmax);
        fil:=32 or (textattr shl 8);
        while (bot>row) do
          begin
             dosmemmove($b800,get_addr(bot-1,left),$b800,get_addr(bot,left),(right-left+1)*2);
             dec(bot);
          end;
        dosmemfillword($b800,get_addr(row,left),right-left+1,fil);
     end;

   procedure clreol;
     var
        row,col : longint;
        fil : word;
     begin
        screengetcursor(row,col);
        inc(row);
        inc(col);
        fil:=32 or (textattr shl 8);
        dosmemfillword($b800,get_addr(row,col),lo(windmax)-col+2,fil);
     end;


   Procedure WriteChar(c:char);
     var
       sa   : longint;
       regs : trealregs;
     begin
       case c of
        #10 : inc(row);
        #13 : col:=lo(windmin)+1;
         #8 : begin
                if col>lo(windmin)+1 then
                 dec(col);
              end;
         #7 : begin { beep }
                regs.dl:=7;
                regs.ah:=2;
                realintr($21,regs);
              end;
       else
        begin
          sa:=(textattr shl 8) or byte(c);
          dosmemput($b800,get_addr(row,col),sa,sizeof(sa));
          inc(col);
        end;
       end;
       if col>lo(windmax)+1 then
        begin
          col:=lo(windmin)+1;
          inc(row);
        end;
       while row>hi(windmax)+1 do
        begin
          removeline(1);
          dec(row);
        end;
     end;


   Function CrtWrite(var f : textrec):integer;
      var
         i : longint;
      begin
         screengetcursor(row,col);
         inc(row);
         inc(col);
         for i:=0 to f.bufpos-1 do
          WriteChar(f.buffer[i]);
         f.bufpos:=0;
         screensetcursor(row-1,col-1);
         CrtWrite:=0;
      end;

   Function CrtClose(Var F: TextRec): Integer;
     Begin
       F.Mode:=fmClosed;
       CrtClose:=0;
     End;

   Function CrtOpen(Var F: TextRec): Integer;
     Begin
       If F.Mode = fmOutput Then
        CrtOpen:=0
       Else
        CrtOpen:=5;
     End;

   Function CrtRead(Var F: TextRec): Integer;
     Begin
       f.bufend:=do_read(f.handle,longint(f.bufptr),f.bufsize);
       f.bufpos:=0;
       CrtRead:=0;
     End;

   Function CrtInOut(Var F: TextRec): Integer;
     Begin
       Case F.Mode of
        fmInput: CrtInOut:=CrtRead(F);
        fmOutput: CrtInOut:=CrtWrite(F);
       End;
     End;

   procedure AssignCrt(var F: Text);
     begin
       Assign(F,'.');
       TextRec(F).OpenFunc:=@CrtOpen;
       TextRec(F).InOutFunc:=@CrtInOut;
       TextRec(F).FlushFunc:=@CrtInOut;
       TextRec(F).CloseFunc:=@CrtClose;
     end;

   procedure sound(hz : word);

     begin
        if hz=0 then
          begin
             nosound;
             exit;
          end;
        asm
           movzwl hz,%ecx
           movl $1193046,%eax
           cdq
           divl %ecx
           movl %eax,%ecx
           movb $0xb6,%al
           outb %al,$0x43
           movb %cl,%al
           outb %al,$0x42
           movb %ch,%al
           outb %al,$0x42
           inb $0x61,%al
           orb $0x3,%al
           outb %al,$0x61
        end ['EAX','ECX','EDX'];
     end;

   procedure nosound;

     begin
        asm
           inb $0x61,%al
           andb $0xfc,%al
           outb %al,$0x61
        end ['EAX'];
     end;

   var
      calibration : longint;

   procedure Delay(MS: Word);
      var
         i,j : longint;
     begin
        for i:=1 to ms do
          for j:=1 to calibration do
             begin
             end;
     end;

  function get_ticks:longint;

    begin
       dosmemget($40,$6c,get_ticks,4);
    end;

  procedure initdelay;


       { From the mailling list,

         by Jonathan Anderson (sarlok@geocities.com) }

    const
       threshold=3;
       { Raise this to increase speed but decrease accuracy        }
       { currently the calibration will be no more than 7 off      }
       { and shave a few ticks off the most accurate setting of 0  }
       { The best values to pick are powers of 2-1 (0,1,3,7,15...) }
       { but any non-negative value will work.                     }

    var
       too_small : boolean;
       first,
       incval    : longint;

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
       { calibration:=calibration div 70; }
       { this is a very bad estimation because }
       { the loop above calls a function       }
       { and the dealy loop does not           }
       calibration:=calibration div 3;

       { The ideal guess value is about half of the real value      }
       { although a value lower than that take a large performance  }
       { hit compared to a value higher than that because it has to }
       { go through the loop a few times.                           }

       if calibration<(threshold+1)*2 then
          calibration:=(threshold+1)*2;


       { If calibration is not at least this value, an }
       { infinite loop will result.                    }

       repeat
          incval:=calibration;
          if calibration<0 then
            begin
               calibration:=$7FFFFFFF;
               exit;
            end;
          { If calibration becomes less than 0, then    }
          { the maximum value was not long enough, so   }
          { assign it the maximum value and exit.       }
          { Without this code, an infinite loop would   }
          { result on superfast computers about 315800  }
          { times faster (oh yeah!) than my Pentium 75. }
          { If you don't think that will happen, take   }
          { out the if and save a few clock cycles.     }

          too_small:=true;     { Assumed true at beginning }

          while incval>threshold do
            begin
               incval:=incval div 2;
               first:=get_ticks;
               while get_ticks=first do
                 begin
                 end;
               first:=get_ticks;
               delay(55);
               if first=get_ticks then
                 begin
                    calibration:=calibration+incval;
                 end
               else
                 begin
                    calibration:=calibration-incval;
                    too_small:=false;
                    { If you have to decrement calibration,  }
                    { the initial value was not too small to }
                    { result in an accurate measurement.     }
                 end;
            end;
       until not too_small;
    end;


  procedure textmode(mode : integer);
    var
       set_font8x8 : boolean;
    begin
       lastmode:=mode;
       set_font8x8:=(mode and font8x8)<>0;
       mode:=mode and $ff;
       setscreenmode(mode);
       windmin:=0;
       windmax:=(screencols-1) or ((screenrows-1) shl 8);
       maxcols:=screencols;
       maxrows:=screenrows;
    end;


begin
   is_last:=false;

   { load system variables to temporary variables to save time }
   maxcols:=screencols;
   maxrows:=screenrows;

   { set output window }
   windmax:=(maxcols-1) or ((maxrows-1) shl 8);

   { save the current settings to restore the old state after the exit }
   screengetcursor(row,col);
   dosmemget($b800,get_addr(row+1,col+1)+1,startattrib,1);
   lastmode:=getscreenmode;
   textattr:=startattrib;

   { redirect the standard output }
   assigncrt(Output);
   assigncrt(Input);
   TextRec(Output).mode:=fmOutput;
   TextRec(Input).mode:=fmInput;

   { calculates delay calibration }
   initdelay;
end.

{
  $Log$
  Revision 1.2  1998-05-21 19:30:46  peter
    * objects compiles for linux
    + assign(pchar), assign(char), rename(pchar), rename(char)
    * fixed read_text_as_array
    + read_text_as_pchar which was not yet in the rtl

}


