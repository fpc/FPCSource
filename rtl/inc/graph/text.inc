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
       pbyte = ^byte;
       pword = ^word;

      { Prefix header of Font file }
      PFHeader = ^TFHeader;
      TFHeader = packed record
         header_size: word;    {* Version 2.0 Header Format	   *}
         font_name: array[1..4] of char;
         font_size: word;      {* Size in byte of file 	      *}
         font_major: byte;     {* Driver Version Information	*}
         font_minor: byte;
         min_major: byte;      {* BGI Revision Information	   *}
         min_minor: byte;
      end;


      { Font record information }
      PHeader = ^THeader;
      THeader = packed record
        Signature:  char;     { signature byte                        }
        Nr_chars:   integer;  { number of characters in file          }
        Reserved:   byte;
        First_char: byte;     { first character in file               }
        cdefs :     integer;  { offset to character definitions       }
        scan_flag:  byte;     { TRUE if char is scanable              }
        org_to_cap: byte;     { Height from origin to top of capitol  }
        org_to_base:byte;     { Height from origin to baseline	      }
        org_to_dec: byte;     { Height from origin to bot of decender }
        _reserved: array[1..4] of char;
        Unused: byte;
      end;


      TOffsetTable =array[0..MaxChars] of Integer;
      TWidthTable  =array[0..MaxChars] of byte;

      tfontrec = packed record
        name : string[8];
        header : THeader;        { font header   }
        pheader : TFHeader;      { prefix header }
        offsets : TOffsetTable;
        widths : TWidthTable;
        instr : pchar;
      end;



      pStroke = ^TStroke;
      TStroke = packed record
        opcode: byte;
        x: integer;  { relative x offset character }
        y: integer;  { relative y offset character }
      end;


      TStrokes = Array[0..1000] of TStroke;

      opcodes = (_END_OF_CHAR, _DO_SCAN, _MOVE, _DRAW);


    var
       fonts : array[1..maxfonts] of tfontrec;
       Strokes: TStrokes; {* Stroke Data Base		*}
       Stroke_count: Array[0..MaxChars] of integer; {* Stroke Count Table *}

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


    function InstallUserFont(const FontFileName : string) : integer;

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
         InstallUserFont:=installedfonts;
      end;


    function Decode(byte1,byte2: char; var x,y: integer): integer;
    { This routines decoes a signle word in a font opcode section  }
    { to a stroke record.                                          }
      var
       b1,b2: shortint;
     Begin
       b1:=shortint(byte1);
       b2:=shortint(byte2);
       { Decode the CHR OPCODE }
       Decode:=integer(((b1 and $80) shr 6)+((b2 and $80) shr 7));
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
       x:=integer(b1);
       y:=integer(b2);
{$R+}
     end;


    function unpack(buf: pchar; index: integer; var Stroke: TStrokes): integer;

     var
      pb: pword;
      po: TStrokes;
      num_ops: integer;
      opcode, i, opc: word;
      counter: integer;
      lindex: integer;
      jx, jy: integer;
     begin
       num_ops := 0;
       counter := index;
       lindex :=0;


       while TRUE do	{* For each byte in buffer	*}
         Begin
           Inc(num_ops);  {* Count the operation		*}
           opcode := decode( buf[counter], buf[counter+1] ,jx, jy );
           Inc(counter,2);
           if( opcode = ord(_END_OF_CHAR) ) then break;	{* Exit loop at end of char	*}
         end;

       counter:=index;

       for i:=0 to num_ops-1 do    { 	/* For each opcode in buffer	*/ }
         Begin
           opc := decode(buf[counter], buf[counter+1], po[lindex].x, po[lindex].y);  {* Decode the data field	*}
           inc(counter,2);
           po[lindex].opcode := opc;      {* Save the opcode		*}
           Inc(lindex);
         end;
       Stroke:=po;
       unpack := num_ops;       {* return OPS count		*}
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


    function RegisterBGIfont(font : pointer) : integer;

      var
         hp : pchar;
         b : word;
         i,j: longint;
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
              move(hp[i+1],FHeader,sizeof(TFHeader));
              move(hp[Prefix_Size],header,sizeof(THeader));
              { check if the font name is already allocated? }
              i:=Prefix_Size+sizeof(THeader);
              for b:=1 to installedfonts do
                begin
                   if fonts[b].name=FHeader.Font_name then
                     begin
                        move(FHeader,fonts[b].PHeader,sizeof(TFHeader));
                        move(Header,fonts[b].Header,sizeof(THeader));
                        move(hp[i],Fonts[b].Offsets[Fonts[b].Header.First_Char],Fonts[b].Header.Nr_chars*sizeof(integer));
                        Inc(i,Fonts[b].Header.Nr_chars*sizeof(integer));
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
                        RegisterBGIfont:=grOK;
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
              TextHeight:=fonts[Currenttextinfo.font].header.org_to_cap-
                round(fonts[Currenttextinfo.font].header.org_to_base * CurrentYRatio) ;
      end;

    function TextWidth(const TextString : string) : word;
      var i,x : Integer;
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
                   dec(c,fonts[Currenttextinfo.font].header.first_char);
                   if (c<0) or (c>=fonts[Currenttextinfo.font].header.nr_chars) then
                     continue;
                   x:=x+byte(fonts[Currenttextinfo.font].widths[c]);
               end;
             TextWidth:=round(x * CurrentXRatio) ;
            end;
      end;


    procedure OutTextXY(x,y : integer;const TextString : string);

      type
       Tpoint = record
         X,Y: Integer;
       end;
      var
         ch: char;
         b1,b2         : shortint;
         b3            : byte;
         c             : byte;
         i,j,k         : longint;
         oldvalues     : linesettingstype;
         nextpos       : word;
         xpos,ypos,offs: longint;
         counter       : longint;
         FontBitmap    : TBitmapChar;
         chr: char;
         cnt1,cnt2     : integer;
         cnt3,cnt4     : integer;
         charsize      : word;

         TextBuffer    : array[1..sizeof(string)*2] of Tpoint;
         WriteMode     : word;
         CurX, CurY    : integer;

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
                        if Fontbitmap[j,k] <> 0 then DirectPutPixel(xpos+k,j+y);
                 end
                else
                 { perform scaling of bitmap font }
                 Begin
                   j:=0;
                   cnt3:=0;

                   while j < 7 do
                   begin
                     { X-axis scaling }
                     for cnt4 := 0 to charsize-1 do
                      begin
                        k:=0;
                        cnt2 := 0;
                        while k < 7  do
                           begin
                             for cnt1 := 0 to charsize-1 do
                               begin
                                  If FontBitmap[j,k] <> 0 then
                                      DirectPutPixel(xpos+cnt1+cnt2,y+cnt3+cnt4);
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
                        if Fontbitmap[j,k] <> 0 then DirectPutPixel(xpos+j,ypos-k);
                 end
                else
                 { perform scaling of bitmap font }
                 Begin
                   j:=0;
                   cnt3:=0;

                   while j < 7 do
                   begin
                     { X-axis scaling }
                     for cnt4 := 0 to charsize-1 do
                      begin
                        k:=0;
                        cnt2 := 0;
                        while k < 7  do
                           begin
                             for cnt1 := 0 to charsize-1 do
                               begin
                                  If FontBitmap[j,k] <> 0 then
                                      DirectPutPixel(xpos+cnt3-cnt4,ypos+cnt1-cnt2);
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
                   Stroke_Count[c] := unpack( fonts[CurrentTextInfo.font].instr,
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
      var x,y:integer;
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
         Prefix: array[0..Prefix_Size-1] of char; {* File Prefix Holder		*}
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
         else
            Currenttextinfo.charsize:=charsize;

         { This is only valid for stroked fonts }
         if (charsize <> usercharsize) then
         begin
            CurrentXRatio := charsize / 4;
            CurrentYRatio := charsize / 4;
         end;
         { if this is a stroked font then load it if not already loaded }
         { into memory...                                               }
         if (font>DefaultFont) and not assigned(fonts[font].instr) then
           begin
              assign(f,bgipath+fonts[font].name+'.CHR');
              reset(f,1);
              if ioresult<>0 then
                begin
                   _graphresult:=grFontNotFound;
                   Currenttextinfo.font:=DefaultFont;
                   exit;
                end;
              {* Read in the file prefix	*}
              BlockRead(F, Prefix, Prefix_Size);
              hp:=Prefix;
              i:=0;
              while (hp[i] <> chr($1a)) do Inc(i);
              move(hp[i+1],fonts[font].PHeader,sizeof(TFHeader));
              (* Read in the Header file  *)
              BlockRead(F,fonts[font].Header,Sizeof(THeader));
              Base := FilePos(F);     {* Remember the address of table*}

              BlockRead(F,Fonts[font].Offsets[Fonts[font].Header.First_Char],Fonts[font].Header.Nr_chars*sizeof(integer));

              {*	Load the character width table into memory.			*}

              base := filePos( f );
              BlockRead(F,Fonts[font].Widths[Fonts[font].Header.First_Char],Fonts[font].Header.Nr_chars*sizeof(byte));

              {*	Determine the length of the stroke database.			*}

              current := FilePos( f );		{* Current file location	*}
              Seek( f, FileSize(F));		{* Go to the end of the file	*}
              length := FilePos( f );		{* Get the file length		*}
              Seek( f, current);	{* Restore old file location	*}

              {*	Load the stroke database.					*}
              { also allocate space for Null character   }
              Getmem(FontData, Length+1);          {* Create space for font data	*}

              BlockRead(F, FontData^, length-current);        {* Load the stroke data	*}
              FontData[length-current+1] := #0;

             if fonts[font].header.Signature<> SIGNATURE then
             begin
                _graphResult:=grInvalidFont;
                Currenttextinfo.font:=DefaultFont;
                Freemem(FontData, Length+1);
                exit;
             end;
             fonts[font].instr:=FontData;


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


