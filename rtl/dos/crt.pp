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
{
  history:
  29th may 1994: version 1.0
             unit is completed
  14th june 1994: version 1.01
             the address from which startaddr was read wasn't right; fixed
  18th august 1994: version 1.1
             the upper left corner of winmin is now 0,0
  19th september 1994: version 1.11
             keypressed handles extended keycodes false; fixed
  27th february 1995: version 1.12
             * crtinoutfunc didn't the line wrap in the right way;
               fixed
  20th january 1996: version 1.13
             - unused variables removed
  21th august 1996: version 1.14
             * adapted to newer FPKPascal versions
             * make the comments english
   6th november 1996: version 1.49
             * some stuff for DPMI adapted
  15th november 1996: version 1.5
             * bug in screenrows fixed
  13th november 1997: removed textrec definition, is now included from 
               textrec.inc
}

unit crt;

{$I os.inc}

  interface
  
    uses
       go32;

    const
       { screen modes }
       bw40 = 0;
       co40 = 1;
       bw80 = 2;
       co80 = 3;
       mono = 7;
       font8x8 = 256;

       { screen color, fore- and background }
       black = 0;
       blue = 1;
       green = 2;
       cyan = 3;
       red = 4;
       magenta = 5;
       brown = 6;
       lightgray = 7;

       { only foreground }
       darkgray = 8;
       lightblue = 9;
       lightgreen = 10;
       lightcyan = 11;
       lightred = 12;
       lightmagenta = 13;
       yellow = 14;
       white = 15;

       { blink flag }
       blink = $80;

    const
    {$ifndef GO32V2}
       directvideo:boolean=true;
    {$else GO32V2}
       { direct video generates a GPF in DPMI of setcursor }
       directvideo:boolean=false;
    {$endif GO32V2}

    var
       { for compatibility }
       checkbreak,checkeof,checksnow : boolean;

       lastmode : word; { screen mode}
       textattr : byte; { current text attribute }
       windmin  : word; { upper right corner of the CRT window }
       windmax  : word; { lower left corner of the CRT window }

    function keypressed : boolean;
    function readkey : char;
    procedure gotoxy(x,y : byte);
    procedure window(left,top,right,bottom : byte);
    procedure clrscr;
    procedure textcolor(color : byte);
    procedure textbackground(color : byte);
    procedure assigncrt(var f : text);
    function wherex : byte;
    function wherey : byte;
    procedure delline;
    procedure delline(line : byte);
    procedure clreol;
    procedure insline;
    procedure cursoron;
    procedure cursoroff;
    procedure cursorbig;
    procedure lowvideo;
    procedure highvideo;
    procedure nosound;
    procedure sound(hz : word);
    procedure delay(ms : longint);
    procedure textmode(mode : integer);
    procedure normvideo;
    
  implementation
  
    var
       maxcols,maxrows : longint;

    { definition of textrec is in textrec.inc}

    {$i textrec.inc}

    { low level routines }

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
        wherex:=col-lo(windmin)+1;
     end;

   function wherey : byte;

     var
        row,col : longint;

     begin
        screengetcursor(row,col);
        wherey:=row-hi(windmin)+1;
     end;

   procedure window(left,top,right,bottom : byte);

     begin
        if (left<1) or
           (right>screencols) or
           (bottom>screenrows) or
           (left>right) or
           (top>bottom) then
           exit;
        windmin:=(left-1) or ((top-1) shl 8);
        windmax:=(right-1) or ((bottom-1) shl 8);
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

   var
      startattrib : byte;

   procedure normvideo;

     begin
        textattr:=startattrib;
     end;

   procedure delline(line : byte);

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
        delline(wherey);
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


   Function CrtWrite(var f : textrec):integer;

      var
         i,col,row : longint;
         c : char;
         va,sa : word;

      begin
         screengetcursor(row,col);
         inc(row);
         inc(col);
         va:=get_addr(row,col);
         for i:=0 to f.bufpos-1 do
           begin
              c:=f.buffer[i];
              case ord(c) of
                 10 : begin
                         inc(row);
                         va:=va+maxcols*2;
                      end;
                 13 : begin
                         col:=lo(windmin)+1;
                         va:=get_addr(row,col);
                     end;
                 8 : if col>lo(windmin)+1 then
                       begin
                          dec(col);
                          va:=va-2;
                       end;
                 7 : begin
                         { beep }
                      end;
              else
                 begin
                    sa:=textattr shl 8 or ord(c);
                    dosmemput($b800,va,sa,sizeof(sa));
                    inc(col);
                    va:=va+2;
                 end;
              end;
              if col>lo(windmax)+1 then
                begin
                   col:=lo(windmin)+1;
                   inc(row);
                   { it's easier to calculate the new address }
                   { it don't spend much time                 }
                   va:=get_addr(row,col);
                end;
              while row>hi(windmax)+1 do
                begin
                   delline(1);
                   dec(row);
                   va:=va-maxcols*2;
                end;
           end;
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
     {$IFDEF GO32V2}
       f.bufend:=do_read(f.handle,longint(f.bufptr),f.bufsize);
     {$ENDIF}
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

   procedure assigncrt(var f : text);
     begin
        TextRec(F).Mode:=fmClosed;
        TextRec(F).BufSize:=SizeOf(TextBuf);
        TextRec(F).BufPtr:=@TextRec(F).Buffer;
        TextRec(F).BufPos:=0;
        TextRec(F).OpenFunc:=@CrtOpen;
        TextRec(F).InOutFunc:=@CrtInOut;
        TextRec(F).FlushFunc:=@CrtInOut;
        TextRec(F).CloseFunc:=@CrtClose;
        TextRec(F).Name[0]:='.';
        TextRec(F).Name[1]:=#0;
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

   procedure delay(ms : longint);

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

var
   col,row : longint;

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
   TextRec(Output).mode:=fmOutput;
{$IFDEF GO32V2}
   assigncrt(Input);
   TextRec(Input).mode:=fmInput;
{$ENDIF GO32V2}

   { calculates delay calibration }
   initdelay;
end.

{
  $Log$
  Revision 1.1  1998-03-25 11:18:41  root
  Initial revision

  Revision 1.8  1998/01/26 11:56:39  michael
  + Added log at the end


  
  Working file: rtl/dos/crt.pp
  description:
  ----------------------------
  revision 1.7
  date: 1998/01/07 09:24:18;  author: michael;  state: Exp;  lines: +7 -2
  * Bug fixed in initdelay, avoiding possible infiniteloop.
  ----------------------------
  revision 1.6
  date: 1998/01/06 00:29:28;  author: michael;  state: Exp;  lines: +2 -2
  Implemented a system independent sequence of reset/rewrite/append fileopenfunc etc system \n (from Peter Vreman)
  ----------------------------
  revision 1.5
  date: 1998/01/05 16:52:15;  author: michael;  state: Exp;  lines: +7 -3
  + Minor change making use of new GO32V2 feature (From Peter Vreman)
  ----------------------------
  revision 1.4
  date: 1998/01/05 13:47:01;  author: michael;  state: Exp;  lines: +199 -127
  * Bug fixes by Peter Vreman (pfv@worldonline.nl), discovered
    when writing CRT examples.
    Bug fix from mailing list also applied.
  ----------------------------
  revision 1.3
  date: 1997/12/12 13:14:36;  author: pierre;  state: Exp;  lines: +33 -12
     + added handling of swap_vectors if under exceptions
       i.e. swapvector is not dummy under go32v2
     * bug in output, exceptions where not allways reset correctly
       now the code in dpmiexcp is called from v2prt0.as exit routine
     * in crt.pp corrected init_delay calibration loop
       and added it for go32v2 also (was disabled before due to crashes !!)
       the previous code did a wrong assumption on the time need to call
       get_ticks compared to an internal loop without call
  ----------------------------
  revision 1.2
  date: 1997/12/01 12:15:44;  author: michael;  state: Exp;  lines: +11 -5
  + added copyright reference in header.
  ----------------------------
  revision 1.1
  date: 1997/11/27 08:33:49;  author: michael;  state: Exp;
  Initial revision
  ----------------------------
  revision 1.1.1.1
  date: 1997/11/27 08:33:49;  author: michael;  state: Exp;  lines: +0 -0
  FPC RTL CVS start
  =============================================================================
}
