{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,98 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{***************************************************************************}
{                             Text output routines                          }
{***************************************************************************}

    const
       maxfonts    = 16;   { maximum possible fonts              }
       MaxChars    = 255;  { Maximum nr. of characters in a file }
       Prefix_Size = $80;  { prefix size to skip                 }
       SIGNATURE   = '+';  { Signature of CHR file               }

    type
(*       pbyte = ^byte;
       pword = ^word;
*)
      { Prefix header of Font file }
{      PFHeader = ^TFHeader;}
      TFHeader = packed record
         header_size: word;    {* Version 2.0 Header Format        *}
         font_name: array[1..4] of char;
         font_size: word;      {* Size in byte of file        *}
         font_major: byte;     {* Driver Version Information    *}
         font_minor: byte;
         min_major: byte;      {* BGI Revision Information         *}
         min_minor: byte;
      end;


      { Font record information }
{      PHeader = ^THeader;}
      THeader = packed record
        Signature:  char;     { signature byte                        }
        Nr_chars:   smallint;  { number of characters in file          }
        Reserved:   byte;
        First_char: byte;     { first character in file               }
        cdefs :     smallint;  { offset to character definitions       }
        scan_flag:  byte;     { TRUE if char is scanable              }
        org_to_cap: shortint;     { Height from origin to top of capitol  }
        org_to_base:shortint;     { Height from origin to baseline        }
        org_to_dec: shortint;     { Height from origin to bot of decender }
        _reserved: array[1..4] of char;
        Unused: byte;
      end;


      TOffsetTable =array[0..MaxChars] of smallint;
      TWidthTable  =array[0..MaxChars] of byte;

      tfontrec = packed record
        name : string[8];
        header : THeader;        { font header   }
        pheader : TFHeader;      { prefix header }
        offsets : TOffsetTable;
        widths : TWidthTable;
        instrlength: longint;    { length of instr, because instr can }
        instr : pchar;           { contain null characters            }
      end;



{      pStroke = ^TStroke;}
      TStroke = packed record
        opcode: byte;
        x: smallint;  { relative x offset character }
        y: smallint;  { relative y offset character }
      end;


      TStrokes = Array[0..1000] of TStroke;

      opcodes = (_END_OF_CHAR, _DO_SCAN, _MOVE, _DRAW);


    var
       fonts : array[1..maxfonts] of tfontrec;
       Strokes: TStrokes; {* Stroke Data Base           *}
{       Stroke_count: Array[0..MaxChars] of smallint;} {* Stroke Count Table *}

{***************************************************************************}
{                         Internal support routines                         }
{***************************************************************************}

    function testfont(p : pchar) : boolean;

      begin
         testfont:=(p[0]='P') and
          (p[1]='K') and
          (p[2]=#8) and
          (p[3]=#8);
      end;


    function InstallUserFont(const FontFileName : string) : smallint;

      begin
         _graphresult:=grOk;
         { first check if we do not allocate too many fonts! }
         if installedfonts=maxfonts then
           begin
              _graphresult:=grError;
              InstallUserFont := DefaultFont;
              exit;
           end;
         inc(installedfonts);
         fonts[installedfonts].name:=FontFileName;
         fonts[installedfonts].instr := nil;
         fonts[installedfonts].instrlength := 0;
         InstallUserFont:=installedfonts;
      end;


    function Decode(byte1,byte2: char; var x,y: smallint): smallint;
    { This routines decoes a signle word in a font opcode section  }
    { to a stroke record.                                          }
      var
       b1,b2: shortint;
     Begin
       b1:=shortint(byte1);
       b2:=shortint(byte2);
       { Decode the CHR OPCODE }
       Decode:=smallint(((b1 and $80) shr 6)+((b2 and $80) shr 7));
       { Now get the X,Y coordinates        }
       { bit 0..7 only which are considered }
       { signed values.                     }
{$R-}
       b1:=b1 and $7f;
       b2:=b2 and $7f;
       { Now if the MSB of these values are set }
       { then the value is signed, therefore we }
       { sign extend it...                      }
       if (b1 and $40)<>0 then b1:=b1 or $80;
       if (b2 and $40)<>0 then b2:=b2 or $80;
       x:=smallint(b1);
       y:=smallint(b2);
{$ifdef debug}
{$R+}
{$endif debug}
     end;


    function unpack(buf: pchar; index: smallint; var Stroke: TStrokes): smallint;

     var
      po: TStrokes;
      num_ops: smallint;
      opcode, i, opc: word;
      counter: smallint;
      lindex: smallint;
      jx, jy: smallint;
     begin
       num_ops := 0;
       counter := index;
       lindex :=0;


       while TRUE do    {* For each byte in buffer      *}
         Begin
           Inc(num_ops);  {* Count the operation                *}
           opcode := decode( buf[counter], buf[counter+1] ,jx, jy );
           Inc(counter,2);
           if( opcode = ord(_END_OF_CHAR) ) then break; {* Exit loop at end of char     *}
         end;

       counter:=index;

       for i:=0 to num_ops-1 do    {    /* For each opcode in buffer    */ }
         Begin
           opc := decode(buf[counter], buf[counter+1], po[lindex].x, po[lindex].y);  {* Decode the data field   *}
           inc(counter,2);
           po[lindex].opcode := opc;      {* Save the opcode            *}
           Inc(lindex);
         end;
       Stroke:=po;
       unpack := num_ops;       {* return OPS count             *}
     end;



    procedure GetTextPosition(var xpos,ypos: longint; const TextString: string);
     begin
         if CurrentTextInfo.Font = DefaultFont then
          begin
           if Currenttextinfo.direction=horizdir then
            begin
              case Currenttextinfo.horiz of
                   centertext : XPos:=(textwidth(textstring) shr 1);
                   lefttext   : XPos:=0;
                   righttext  : XPos:=textwidth(textstring);
              end;
              case Currenttextinfo.vert of
                  centertext : YPos:=-(textheight(textstring) shr 1);
                  bottomtext : YPos:=-textheight(textstring);
                  toptext    : YPos:=0;
              end;
            end else
            begin
              case Currenttextinfo.horiz of
                   centertext : XPos:=(textheight(textstring) shr 1);
                   lefttext   : XPos:=textheight(textstring);
                   righttext  : XPos:=textheight(textstring);
              end;
              case Currenttextinfo.vert of
                  centertext : YPos:=(textwidth(textstring) shr 1);
                  bottomtext : YPos:=0;
                  toptext    : YPos:=textwidth(textstring);
              end;
            end;
          end
         else
          begin
            if Currenttextinfo.direction=horizdir then
            begin
              case CurrentTextInfo.horiz of
                   centertext : XPos:=(textwidth(textstring) shr 1);
                   lefttext   : XPos:=0;
                   righttext  : XPos:=textwidth(textstring);
              end;
              case CurrentTextInfo.vert of
                  centertext : YPos:=(textheight(textstring) shr 1);
                  bottomtext : YPos:=0;
                  toptext    : YPos:=textheight(textstring);
              end;
            end else
            begin
              case CurrentTextInfo.horiz of
                   centertext : XPos:=(textheight(textstring) shr 1);
                   lefttext   : XPos:=0;
                   righttext  : XPos:=textheight(textstring);
              end;
              case CurrentTextInfo.vert of
                  centertext : YPos:=(textwidth(textstring) shr 1);
                  bottomtext : YPos:=0;
                  toptext    : YPos:=textwidth(textstring);
              end;
           end;
          end;
     end;

{***************************************************************************}
{                         Exported routines                                 }
{***************************************************************************}


    function RegisterBGIfont(font : pointer) : smallint;

      var
         hp : pchar;
         b : word;
         i: longint;
         Header: THeader;
         counter: longint;
         FontData: pchar;
         FHeader: TFHeader;
      begin
         RegisterBGIfont:=grInvalidFontNum;
         i:=0;
         { Check if the font header is valid first of all }
         if testfont(font) then
           begin
              hp:=pchar(font);
              { Move to EOF in prefix header }
              while (hp[i] <> chr($1a)) do Inc(i);
              move(hp[i+1],FHeader,sizeof(FHeader));
              move(hp[Prefix_Size],header,sizeof(Header));
              { check if the font name is already allocated? }
              i:=Prefix_Size+sizeof(Header)+1;
              for b:=1 to installedfonts do
                begin
                   if fonts[b].name=FHeader.Font_name then
                     begin
                        move(FHeader,fonts[b].PHeader,sizeof(FHeader));
                        move(Header,fonts[b].Header,sizeof(Header));
                        move(hp[i],Fonts[b].Offsets[Fonts[b].Header.First_Char],Fonts[b].Header.Nr_chars*sizeof(smallint));
                        Inc(i,Fonts[b].Header.Nr_chars*sizeof(smallint));
                        move(hp[i],Fonts[b].Widths[Fonts[b].Header.First_Char],Fonts[b].Header.Nr_chars*sizeof(byte));
                        Inc(i,Fonts[b].Header.Nr_chars*sizeof(byte));
                        counter:=Fonts[b].PHeader.font_size+PREFIX_SIZE-i;
                        { allocate also space for null }
                        GetMem(FontData,Counter+1);
                        move(hp[i],FontData^,Counter);
                        { Null terminate the string }
                        FontData[counter+1] := #0;
                        if fonts[b].header.Signature<> SIGNATURE then
                          begin
                            _graphResult:=grInvalidFont;
                            Freemem(FontData, Counter+1);
                            exit;
                          end;
                        fonts[b].instr:=FontData;
                        fonts[b].instrlength:=Counter+1;
                        RegisterBGIfont:=b;
                     end;
                end;
           end
         else
           RegisterBGIFont:=grInvalidFont;
      end;



    procedure GetTextSettings(var TextInfo : TextSettingsType);

      begin
         textinfo:=currenttextinfo;
      end;



    function TextHeight(const TextString : string) : word;

      begin
         if Currenttextinfo.font=DefaultFont
            then TextHeight:=8*CurrentTextInfo.CharSize
            else
              TextHeight:=Trunc((fonts[Currenttextinfo.font].header.org_to_cap-
                fonts[Currenttextinfo.font].header.org_to_dec) * CurrentYRatio);
      end;

    function TextWidth(const TextString : string) : word;
      var i,x : smallint;
          c   : byte;
      begin
         x := 0;
         { if this is the default font ... }
         if Currenttextinfo.font = Defaultfont then
            TextWidth:=length(TextString)*8*CurrentTextInfo.CharSize
         { This is a stroked font ... }
            else begin
               for i:=1 to length(TextString) do
                begin
                   c:=byte(textstring[i]);
{                   dec(c,fonts[Currenttextinfo.font].header.first_char);}
                   if (c-fonts[Currenttextinfo.font].header.first_char>=
                       fonts[Currenttextinfo.font].header.nr_chars) then
                     continue;
                   x:=x+byte(fonts[Currenttextinfo.font].widths[c]);
               end;
             TextWidth:=round(x * CurrentXRatio) ;
            end;
      end;


    procedure OutTextXY(x,y : smallint;const TextString : string);

      type
       Tpoint = record
         X,Y: smallint;
       end;
      var
         i,j,k,c       : longint;
         xpos,ypos     : longint;
         counter       : longint;
         FontBitmap    : TBitmapChar;
         cnt1,cnt2     : smallint;
         cnt3,cnt4     : smallint;
         charsize      : word;
         WriteMode     : word;
         CurX, CurY    : smallint;
         oldvalues     : linesettingstype;
         chr           : char;

      begin
         { save current write mode }
         WriteMode := CurrentWriteMode;
         CurrentWriteMode := NormalPut;
         GetTextPosition(xpos,ypos,textstring);
         X:=X-XPos; Y:=Y+YPos;
         XPos:=X; YPos:=Y;

         CharSize := CurrentTextInfo.Charsize;
         if Currenttextinfo.font=DefaultFont then
         begin
           c:=length(textstring);
           { We must a length strength which is ZERO based }
           { if c is a byte and length is zero, this is    }
           { dangerous, fixed                              }
           Dec(c);
           if CurrentTextInfo.direction=HorizDir then
           { Horizontal direction }
            begin
              for i:=0 to c do
              begin
                chr := TextString[i+1];
                xpos:=x+(i shl 3)*Charsize;
                { we copy the character bitmap before accessing it }
                { this improves speed on non optimizing compilers  }
                { since it is one less address calculation.        }
                Fontbitmap:=TBitmapChar(DefaultFontData[chr]);
                { no scaling }
                if CharSize = 1 then
                 Begin
                   for j:=0 to 7 do
                      for k:=0 to 7 do
                        if Fontbitmap[j,k] <> 0 then PutPixel(xpos+k,j+y,CurrentColor);
                 end
                else
                 { perform scaling of bitmap font }
                 Begin
                   j:=0;
                   cnt3:=0;

                   while j <= 7 do
                   begin
                     { X-axis scaling }
                     for cnt4 := 0 to charsize-1 do
                      begin
                        k:=0;
                        cnt2 := 0;
                        while k <= 7  do
                           begin
                             for cnt1 := 0 to charsize-1 do
                               begin
                                  If FontBitmap[j,k] <> 0 then
                                      PutPixel(xpos+cnt1+cnt2,y+cnt3+cnt4,CurrentColor);
                               end;
                               Inc(k);
                               Inc(cnt2,charsize);
                           end;
                      end;
                     Inc(j);
                     Inc(cnt3,charsize);
                   end;
                 end;
              end;
            end
           else
           { Vertical direction }
            begin
              for i:=0 to c do
              begin

                chr := TextString[i+1];
                Fontbitmap:=TBitmapChar(DefaultFontData[chr]);
                ypos := y-(i shl 3)*CharSize;

                { no scaling }
                if CharSize = 1 then
                 Begin
                   for j:=0 to 7 do
                      for k:=0 to 7 do
                        if Fontbitmap[j,k] <> 0 then PutPixel(xpos+j,ypos-k,
                           CurrentColor);
                 end
                else
                 { perform scaling of bitmap font }
                 Begin
                   j:=0;
                   cnt3:=0;

                   while j<=7 do
                   begin
                     { X-axis scaling }
                     for cnt4 := 0 to charsize-1 do
                      begin
                        k:=0;
                        cnt2 := 0;
                        while k<=7  do
                           begin
                             for cnt1 := 0 to charsize-1 do
                               begin
                                  If FontBitmap[j,k] <> 0 then
                                      PutPixel(xpos+cnt3-cnt4,ypos+cnt1-cnt2,
                                        CurrentColor);
                               end;
                               Inc(k);
                               Inc(cnt2,charsize);
                           end;
                      end;
                     Inc(j);
                     Inc(cnt3,charsize);
                   end;
                 end;
              end;
            end;
         end else
         { This is a stroked font which is already loaded into memory }
           begin
              getlinesettings(oldvalues);
              { reset line style to defaults }
              setlinestyle(solidln,oldvalues.pattern,normwidth);
              if Currenttextinfo.direction=vertdir then
                 xpos:=xpos + Textheight(textstring);
              CurX:=xpos; CurY:=ypos; x:=xpos; y:=ypos;
              for i:=1 to length(textstring) do
                begin
                   c:=byte(textstring[i]);
{                   Stroke_Count[c] := }
                   unpack( fonts[CurrentTextInfo.font].instr,
                     fonts[CurrentTextInfo.font].Offsets[c], Strokes );
                   counter:=0;
                   while true do
                     begin

                         if CurrentTextInfo.direction=VertDir then
                           begin
                             xpos:=x-round(Strokes[counter].Y*CurrentXRatio);
                             ypos:=y-round(Strokes[counter].X*CurrentYRatio);
                           end
                         else
                           begin
                             xpos:=x+round(Strokes[counter].X*CurrentXRatio) ;
                             ypos:=y-round(Strokes[counter].Y*CurrentYRatio) ;
                           end;
                         case opcodes(Strokes[counter].opcode) of
                           _END_OF_CHAR: break;
                           _DO_SCAN: begin
                                    { Currently unsupported };
                                    end;
                           _MOVE : Begin
                                     CurX := XPos;
                                     CurY := YPos;
                                   end;
                           _DRAW: Begin
                                    Line(CurX,CurY,xpos,ypos);
                                    CurX:=xpos;
                                    CurY:=ypos;
                                  end;
                             else
                               Begin
                               end;
                            end;
                        Inc(counter);
                     end; { end while }
                   if Currenttextinfo.direction=VertDir then
                     y:=y-round(byte(fonts[CurrenttextInfo.font].widths[c])*CurrentXRatio)
                   else
                     x:=x+round(byte(fonts[Currenttextinfo.font].widths[c])*CurrentXRatio) ;
                end;
              setlinestyle( oldvalues.linestyle, oldvalues.pattern, oldvalues.thickness);
           end;
        { restore write mode }
        CurrentWriteMode := WriteMode;
      end;


    procedure OutText(const TextString : string);
      var x,y:smallint;
      begin
         { Save CP }
         x:=CurrentX;
         y:=CurrentY;
         OutTextXY(CurrentX,CurrentY,TextString);
         { If the direction is Horizontal and the justification left }
         { then and only then do we update the CP                    }
         if (Currenttextinfo.direction=HorizDir) and
           (Currenttextinfo.horiz=LeftText) then
               inc(x,textwidth(TextString));
         { Update the CP }
         CurrentX := X;
         CurrentY := Y;
      end;





    procedure SetTextJustify(horiz,vert : word);

      begin
         if (horiz<0) or (horiz>2) or
            (vert<0) or (vert>2) then
           begin
              _graphresult:=grError;
              exit;
           end;
         Currenttextinfo.horiz:=horiz;
         Currenttextinfo.vert:=vert;
      end;


    procedure SetTextStyle(font,direction : word;charsize : word);

      var
         f : file;
         Prefix: array[0..Prefix_Size-1] of char; {* File Prefix Holder         *}
         Length, Current: longint;
         FontData: Pchar;
         Base: longint;
         hp  : pchar;
         i   : longint;
      begin
         if font>installedfonts then
           begin
              _graphresult:=grInvalidFontNum;
              exit;
           end;

         Currenttextinfo.font:=font;
         if (direction<>HorizDir) and (direction<>VertDir) then
           direction:=HorizDir;
         Currenttextinfo.direction:=direction;
         { According to the Turbo Pascal programmer's reference }
         { maximum charsize for bitmapped font is 10            }
         if (CurrentTextInfo.Font = DefaultFont) and (Charsize > 10) then
            Currenttextinfo.charsize:=10
         else if charsize<1 then
            Currenttextinfo.charsize:=1
         else
            Currenttextinfo.charsize:=charsize;

         { This is only valid for stroked fonts }
{$ifdef logging}
         LogLn('(org_to_cap - org_to_dec): ' + strf(
                fonts[Currenttextinfo.font].header.org_to_cap-
                fonts[Currenttextinfo.font].header.org_to_dec));
{$endif logging}
         if (charsize <> usercharsize) then
           Case CharSize of
             1: Begin
                  CurrentXRatio := 0.55;
                  CurrentYRatio := 0.55;
                End;
             2: Begin
                  CurrentXRatio := 0.65;
                  CurrentYRatio := 0.65;
                End;
             3: Begin
                  CurrentXRatio := 0.75;
                  CurrentYRatio := 0.75;
                End;
             4: Begin
                  CurrentXRatio := 1.0;
                  CurrentYRatio := 1.0;
                End;
             5: Begin
                  CurrentXRatio := 1.3;
                  CurrentYRatio := 1.3;
                End;
             6: Begin
                  CurrentXRatio := 1.65;
                  CurrentYRatio := 1.65
                End;
             7: Begin
                  CurrentXRatio := 2.0;
                  CurrentYRatio := 2.0;
                End;
             8: Begin
                  CurrentXRatio := 2.5;
                  CurrentYRatio := 2.5;
                End;
             9: Begin
                  CurrentXRatio := 3.0;
                  CurrentYRatio := 3.0;
                End;
             10: Begin
                   CurrentXRatio := 4.0;
                   CurrentYRatio := 4.0;
                 End
           End;
         { if this is a stroked font then load it if not already loaded }
         { into memory...                                               }
         if (font>DefaultFont) and not assigned(fonts[font].instr) then
           begin
              assign(f,bgipath+fonts[font].name+'.CHR');
{$i-}
              reset(f,1);
{$i+}
              if ioresult<>0 then
                begin
                   _graphresult:=grFontNotFound;
                   Currenttextinfo.font:=DefaultFont;
                   exit;
                end;
              {* Read in the file prefix        *}
              BlockRead(F, Prefix, Prefix_Size);
              hp:=Prefix;
              i:=0;
              while (hp[i] <> chr($1a)) do Inc(i);
              move(hp[i+1],fonts[font].PHeader,sizeof(TFHeader));
              (* Read in the Header file  *)
              BlockRead(F,fonts[font].Header,Sizeof(THeader));
              Base := FilePos(F);     {* Remember the address of table*}

              BlockRead(F,Fonts[font].Offsets[Fonts[font].Header.First_Char],Fonts[font].Header.Nr_chars*sizeof(smallint));

              {*        Load the character width table into memory.                     *}

              base := filePos( f );
              BlockRead(F,Fonts[font].Widths[Fonts[font].Header.First_Char],Fonts[font].Header.Nr_chars*sizeof(byte));

              {*        Determine the length of the stroke database.                    *}

              current := FilePos( f );          {* Current file location        *}
              Seek( f, FileSize(F));            {* Go to the end of the file    *}
              length := FilePos( f );           {* Get the file length          *}
              Seek( f, current);        {* Restore old file location    *}

              {*        Load the stroke database.                                       *}
              { also allocate space for Null character   }
              Getmem(FontData, Length+1);          {* Create space for font data        *}

              BlockRead(F, FontData^, length-current);        {* Load the stroke data   *}
              FontData[length-current+1] := #0;

             if fonts[font].header.Signature<> SIGNATURE then
             begin
                _graphResult:=grInvalidFont;
                Currenttextinfo.font:=DefaultFont;
                Freemem(FontData, Length+1);
                exit;
             end;
             fonts[font].instr:=FontData;
             fonts[font].instrLength:=Length+1;


              if not testfont(Prefix) then
                begin
                   _graphresult:=grInvalidFont;
                   Currenttextinfo.font:=DefaultFont;
                   Freemem(FontData, Length+1);
                end;
              close(f);
           end;
      end;

    procedure SetUserCharSize(Multx,Divx,Multy,Divy : word);
      begin
         CurrentXRatio := MultX / DivX;
         CurrentYRatio := MultY / DivY;
      end;

{
$Log$
Revision 1.11  2000-01-02 19:02:39  jonas
  * removed/commented out (inited but) unused vars and unused types

Revision 1.10  1999/12/23 16:48:13  jonas
  * turn off IO checking when attempting to open a font file (to avoid RTE)

Revision 1.9  1999/12/20 11:22:36  peter
  * integer -> smallint to overcome -S2 switch needed for ggi version

Revision 1.8  1999/11/11 22:29:21  florian
  * the writing of the default font was wrong when doing scaling:
    the last colunm/row wasn't drawn

Revision 1.7  1999/09/28 15:07:47  jonas
  * fix for disposing font data because it can contain #0 chars

Revision 1.6  1999/09/28 13:56:29  jonas
  * reordered some local variables (first 4 byte vars, then 2 byte vars
    etc)
  * font data is now disposed in exitproc, exitproc is now called
    GraphExitProc (was CleanModes) and resides in graph.pp instead of in
    modes.inc

Revision 1.5  1999/09/27 23:34:42  peter
  * new graph unit is default for go32v2
  * removed warnings/notes

Revision 1.4  1999/09/26 13:31:07  jonas
  * changed name of modeinfo variable to vesamodeinfo and fixed
    associated errors (fillchar(modeinfo,sizeof(tmodeinfo),#0) instead
    of sizeof(TVesamodeinfo) etc)
  * changed several sizeof(type) to sizeof(varname) to avoid similar
    errors in the future

Revision 1.3  1999/09/22 14:54:11  jonas
  * changed ratios so font sizes on screen are the same as with TP
  * SetUserCharSize must also use / instead of DIV

Revision 1.2  1999/09/22 13:30:52  jonas
  * changed org_to_cap, org_to_dec and org_to_base to shortint (from
    Michael Knapp's gxtext unit, part of the GraphiX package)
  * in settextstyle, the calculation of the ratios must be done
    with /, not DIV!!

Revision 1.1  1999/09/22 13:13:36  jonas
  * renamed text.inc -> gtext.inc to avoid conflict with system unit
  * fixed textwidth
  * isgraphmode now gets properly updated, so mode restoring works
    again

Revision 1.7 1999/09/12 17:29:00 jonas
  * several changes to internalellipse to make it faster
    and to make sure it updates the ArcCall correctly
    (not yet done for width = 3)
  * Arc mostly works now, only sometimes an endless loop, don't know
    why

Revision 1.6  1999/09/12 08:02:22  florian
  * fixed outtext(''), c was a byte, this leads to an underflow and
    garbage was written

Revision 1.5  1999/07/26 09:38:43  florian
  * bar: y2 can be less y1, fixed
  * settextstyle: charsize can be 0, must be changed into 1

Revision 1.4  1999/07/12 13:27:16  jonas
  + added Log and Id tags
  * added first FPC support, only VGA works to some extend for now
  * use -dasmgraph to use assembler routines, otherwise Pascal
    equivalents are used
  * use -dsupportVESA to support VESA (crashes under FPC for now)
  * only dispose vesainfo at closegrph if a vesa card was detected
  * changed int32 to longint (int32 is not declared under FPC)
  * changed the declaration of almost every procedure in graph.inc to
    "far;" becquse otherwise you can't assign them to procvars under TP
    real mode (but unexplainable "data segnment too large" errors prevent
    it from working under real mode anyway)

}
