{

    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    This include implements the different palette manipulation
    routines.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

CONST
 { This is the standard VGA palette for the first 256 colors }
 { at 6 bpp for each RG and B component.                     }
 DefaultColors: Array[0..255] of RGBRec = (
  (Red:   0;Green:   0;Blue:   0),
  (Red:   0;Green:   0;Blue: 168),
  (Red:   0;Green: 168;Blue:   0),
  (Red:   0;Green: 168;Blue: 168),
  (Red: 168;Green:   0;Blue:   0),
  (Red: 168;Green:   0;Blue: 168),
  (Red: 168;Green:  84;Blue:   0),
  (Red: 168;Green: 168;Blue: 168),
  (Red:  84;Green:  84;Blue:  84),
  (Red:  84;Green:  84;Blue: 252),
  (Red:  84;Green: 252;Blue:  84),
  (Red:  84;Green: 252;Blue: 252),
  (Red: 252;Green:  84;Blue:  84),
  (Red: 252;Green:  84;Blue: 252),
  (Red: 252;Green: 252;Blue:  84),
  (Red: 252;Green: 252;Blue: 252),
  (Red:   0;Green:   0;Blue:   0),
  (Red:  20;Green:  20;Blue:  20),
  (Red:  32;Green:  32;Blue:  32),
  (Red:  44;Green:  44;Blue:  44),
  (Red:  56;Green:  56;Blue:  56),
  (Red:  68;Green:  68;Blue:  68),
  (Red:  80;Green:  80;Blue:  80),
  (Red:  96;Green:  96;Blue:  96),
  (Red: 112;Green: 112;Blue: 112),
  (Red: 128;Green: 128;Blue: 128),
  (Red: 144;Green: 144;Blue: 144),
  (Red: 160;Green: 160;Blue: 160),
  (Red: 180;Green: 180;Blue: 180),
  (Red: 200;Green: 200;Blue: 200),
  (Red: 224;Green: 224;Blue: 224),
  (Red: 252;Green: 252;Blue: 252),
  (Red:   0;Green:   0;Blue: 252),
  (Red:  64;Green:   0;Blue: 252),
  (Red: 124;Green:   0;Blue: 252),
  (Red: 188;Green:   0;Blue: 252),
  (Red: 252;Green:   0;Blue: 252),
  (Red: 252;Green:   0;Blue: 188),
  (Red: 252;Green:   0;Blue: 124),
  (Red: 252;Green:   0;Blue:  64),
  (Red: 252;Green:   0;Blue:   0),
  (Red: 252;Green:  64;Blue:   0),
  (Red: 252;Green: 124;Blue:   0),
  (Red: 252;Green: 188;Blue:   0),
  (Red: 252;Green: 252;Blue:   0),
  (Red: 188;Green: 252;Blue:   0),
  (Red: 124;Green: 252;Blue:   0),
  (Red:  64;Green: 252;Blue:   0),
  (Red:   0;Green: 252;Blue:   0),
  (Red:   0;Green: 252;Blue:  64),
  (Red:   0;Green: 252;Blue: 124),
  (Red:   0;Green: 252;Blue: 188),
  (Red:   0;Green: 252;Blue: 252),
  (Red:   0;Green: 188;Blue: 252),
  (Red:   0;Green: 124;Blue: 252),
  (Red:   0;Green:  64;Blue: 252),
  (Red: 124;Green: 124;Blue: 252),
  (Red: 156;Green: 124;Blue: 252),
  (Red: 188;Green: 124;Blue: 252),
  (Red: 220;Green: 124;Blue: 252),
  (Red: 252;Green: 124;Blue: 252),
  (Red: 252;Green: 124;Blue: 220),
  (Red: 252;Green: 124;Blue: 188),
  (Red: 252;Green: 124;Blue: 156),
  (Red: 252;Green: 124;Blue: 124),
  (Red: 252;Green: 156;Blue: 124),
  (Red: 252;Green: 188;Blue: 124),
  (Red: 252;Green: 220;Blue: 124),
  (Red: 252;Green: 252;Blue: 124),
  (Red: 220;Green: 252;Blue: 124),
  (Red: 188;Green: 252;Blue: 124),
  (Red: 156;Green: 252;Blue: 124),
  (Red: 124;Green: 252;Blue: 124),
  (Red: 124;Green: 252;Blue: 156),
  (Red: 124;Green: 252;Blue: 188),
  (Red: 124;Green: 252;Blue: 220),
  (Red: 124;Green: 252;Blue: 252),
  (Red: 124;Green: 220;Blue: 252),
  (Red: 124;Green: 188;Blue: 252),
  (Red: 124;Green: 156;Blue: 252),
  (Red: 180;Green: 180;Blue: 252),
  (Red: 196;Green: 180;Blue: 252),
  (Red: 216;Green: 180;Blue: 252),
  (Red: 232;Green: 180;Blue: 252),
  (Red: 252;Green: 180;Blue: 252),
  (Red: 252;Green: 180;Blue: 232),
  (Red: 252;Green: 180;Blue: 216),
  (Red: 252;Green: 180;Blue: 196),
  (Red: 252;Green: 180;Blue: 180),
  (Red: 252;Green: 196;Blue: 180),
  (Red: 252;Green: 216;Blue: 180),
  (Red: 252;Green: 232;Blue: 180),
  (Red: 252;Green: 252;Blue: 180),
  (Red: 232;Green: 252;Blue: 180),
  (Red: 216;Green: 252;Blue: 180),
  (Red: 196;Green: 252;Blue: 180),
  (Red: 180;Green: 252;Blue: 180),
  (Red: 180;Green: 252;Blue: 196),
  (Red: 180;Green: 252;Blue: 216),
  (Red: 180;Green: 252;Blue: 232),
  (Red: 180;Green: 252;Blue: 252),
  (Red: 180;Green: 232;Blue: 252),
  (Red: 180;Green: 216;Blue: 252),
  (Red: 180;Green: 196;Blue: 252),
  (Red:   0;Green:   0;Blue: 112),
  (Red:  28;Green:   0;Blue: 112),
  (Red:  56;Green:   0;Blue: 112),
  (Red:  84;Green:   0;Blue: 112),
  (Red: 112;Green:   0;Blue: 112),
  (Red: 112;Green:   0;Blue:  84),
  (Red: 112;Green:   0;Blue:  56),
  (Red: 112;Green:   0;Blue:  28),
  (Red: 112;Green:   0;Blue:   0),
  (Red: 112;Green:  28;Blue:   0),
  (Red: 112;Green:  56;Blue:   0),
  (Red: 112;Green:  84;Blue:   0),
  (Red: 112;Green: 112;Blue:   0),
  (Red:  84;Green: 112;Blue:   0),
  (Red:  56;Green: 112;Blue:   0),
  (Red:  28;Green: 112;Blue:   0),
  (Red:   0;Green: 112;Blue:   0),
  (Red:   0;Green: 112;Blue:  28),
  (Red:   0;Green: 112;Blue:  56),
  (Red:   0;Green: 112;Blue:  84),
  (Red:   0;Green: 112;Blue: 112),
  (Red:   0;Green:  84;Blue: 112),
  (Red:   0;Green:  56;Blue: 112),
  (Red:   0;Green:  28;Blue: 112),
  (Red:  56;Green:  56;Blue: 112),
  (Red:  68;Green:  56;Blue: 112),
  (Red:  84;Green:  56;Blue: 112),
  (Red:  96;Green:  56;Blue: 112),
  (Red: 112;Green:  56;Blue: 112),
  (Red: 112;Green:  56;Blue:  96),
  (Red: 112;Green:  56;Blue:  84),
  (Red: 112;Green:  56;Blue:  68),
  (Red: 112;Green:  56;Blue:  56),
  (Red: 112;Green:  68;Blue:  56),
  (Red: 112;Green:  84;Blue:  56),
  (Red: 112;Green:  96;Blue:  56),
  (Red: 112;Green: 112;Blue:  56),
  (Red:  96;Green: 112;Blue:  56),
  (Red:  84;Green: 112;Blue:  56),
  (Red:  68;Green: 112;Blue:  56),
  (Red:  56;Green: 112;Blue:  56),
  (Red:  56;Green: 112;Blue:  68),
  (Red:  56;Green: 112;Blue:  84),
  (Red:  56;Green: 112;Blue:  96),
  (Red:  56;Green: 112;Blue: 112),
  (Red:  56;Green:  96;Blue: 112),
  (Red:  56;Green:  84;Blue: 112),
  (Red:  56;Green:  68;Blue: 112),
  (Red:  80;Green:  80;Blue: 112),
  (Red:  88;Green:  80;Blue: 112),
  (Red:  96;Green:  80;Blue: 112),
  (Red: 104;Green:  80;Blue: 112),
  (Red: 112;Green:  80;Blue: 112),
  (Red: 112;Green:  80;Blue: 104),
  (Red: 112;Green:  80;Blue:  96),
  (Red: 112;Green:  80;Blue:  88),
  (Red: 112;Green:  80;Blue:  80),
  (Red: 112;Green:  88;Blue:  80),
  (Red: 112;Green:  96;Blue:  80),
  (Red: 112;Green: 104;Blue:  80),
  (Red: 112;Green: 112;Blue:  80),
  (Red: 104;Green: 112;Blue:  80),
  (Red:  96;Green: 112;Blue:  80),
  (Red:  88;Green: 112;Blue:  80),
  (Red:  80;Green: 112;Blue:  80),
  (Red:  80;Green: 112;Blue:  88),
  (Red:  80;Green: 112;Blue:  96),
  (Red:  80;Green: 112;Blue: 104),
  (Red:  80;Green: 112;Blue: 112),
  (Red:  80;Green: 104;Blue: 112),
  (Red:  80;Green:  96;Blue: 112),
  (Red:  80;Green:  88;Blue: 112),
  (Red:   0;Green:   0;Blue:  64),
  (Red:  16;Green:   0;Blue:  64),
  (Red:  32;Green:   0;Blue:  64),
  (Red:  48;Green:   0;Blue:  64),
  (Red:  64;Green:   0;Blue:  64),
  (Red:  64;Green:   0;Blue:  48),
  (Red:  64;Green:   0;Blue:  32),
  (Red:  64;Green:   0;Blue:  16),
  (Red:  64;Green:   0;Blue:   0),
  (Red:  64;Green:  16;Blue:   0),
  (Red:  64;Green:  32;Blue:   0),
  (Red:  64;Green:  48;Blue:   0),
  (Red:  64;Green:  64;Blue:   0),
  (Red:  48;Green:  64;Blue:   0),
  (Red:  32;Green:  64;Blue:   0),
  (Red:  16;Green:  64;Blue:   0),
  (Red:   0;Green:  64;Blue:   0),
  (Red:   0;Green:  64;Blue:  16),
  (Red:   0;Green:  64;Blue:  32),
  (Red:   0;Green:  64;Blue:  48),
  (Red:   0;Green:  64;Blue:  64),
  (Red:   0;Green:  48;Blue:  64),
  (Red:   0;Green:  32;Blue:  64),
  (Red:   0;Green:  16;Blue:  64),
  (Red:  32;Green:  32;Blue:  64),
  (Red:  40;Green:  32;Blue:  64),
  (Red:  48;Green:  32;Blue:  64),
  (Red:  56;Green:  32;Blue:  64),
  (Red:  64;Green:  32;Blue:  64),
  (Red:  64;Green:  32;Blue:  56),
  (Red:  64;Green:  32;Blue:  48),
  (Red:  64;Green:  32;Blue:  40),
  (Red:  64;Green:  32;Blue:  32),
  (Red:  64;Green:  40;Blue:  32),
  (Red:  64;Green:  48;Blue:  32),
  (Red:  64;Green:  56;Blue:  32),
  (Red:  64;Green:  64;Blue:  32),
  (Red:  56;Green:  64;Blue:  32),
  (Red:  48;Green:  64;Blue:  32),
  (Red:  40;Green:  64;Blue:  32),
  (Red:  32;Green:  64;Blue:  32),
  (Red:  32;Green:  64;Blue:  40),
  (Red:  32;Green:  64;Blue:  48),
  (Red:  32;Green:  64;Blue:  56),
  (Red:  32;Green:  64;Blue:  64),
  (Red:  32;Green:  56;Blue:  64),
  (Red:  32;Green:  48;Blue:  64),
  (Red:  32;Green:  40;Blue:  64),
  (Red:  44;Green:  44;Blue:  64),
  (Red:  48;Green:  44;Blue:  64),
  (Red:  52;Green:  44;Blue:  64),
  (Red:  60;Green:  44;Blue:  64),
  (Red:  64;Green:  44;Blue:  64),
  (Red:  64;Green:  44;Blue:  60),
  (Red:  64;Green:  44;Blue:  52),
  (Red:  64;Green:  44;Blue:  48),
  (Red:  64;Green:  44;Blue:  44),
  (Red:  64;Green:  48;Blue:  44),
  (Red:  64;Green:  52;Blue:  44),
  (Red:  64;Green:  60;Blue:  44),
  (Red:  64;Green:  64;Blue:  44),
  (Red:  60;Green:  64;Blue:  44),
  (Red:  52;Green:  64;Blue:  44),
  (Red:  48;Green:  64;Blue:  44),
  (Red:  44;Green:  64;Blue:  44),
  (Red:  44;Green:  64;Blue:  48),
  (Red:  44;Green:  64;Blue:  52),
  (Red:  44;Green:  64;Blue:  60),
  (Red:  44;Green:  64;Blue:  64),
  (Red:  44;Green:  60;Blue:  64),
  (Red:  44;Green:  52;Blue:  64),
  (Red:  44;Green:  48;Blue:  64),
  (Red:   0;Green:   0;Blue:   0),
  (Red:   0;Green:   0;Blue:   0),
  (Red:   0;Green:   0;Blue:   0),
  (Red:   0;Green:   0;Blue:   0),
  (Red:   0;Green:   0;Blue:   0),
  (Red:   0;Green:   0;Blue:   0),
  (Red:   0;Green:   0;Blue:   0),
  (Red:   0;Green:   0;Blue:   0));

  procedure SetAllPaletteDefault(const Palette:PaletteType);
   var
    i: longint;
    Size: longint;
   begin
     { palette routines do not work in DirectColor mode }
     if DirectColor then
       begin
         _GraphResult := grError;
         exit;
       end;
     Size:=Palette.Size;  { number of entries...}
     { first determine if we are not trying to }
     { change too much colors...               }
     if Palette.Size > PaletteSize then
      begin
        _GraphResult := grError;
        exit;
      end;
     Dec(Size); { Color arrays are indexed according to zero }
     for i:=0 to Size do
      begin
        { skip if RGB values are -1 , as stated in the TP manual }
        if (Palette.Colors[i].Red <> -1) and (Palette.Colors[i].Green <> -1)
           and (Palette.Colors[i].Blue <> -1) then
              SetRGBPalette(i,
                Palette.Colors[i].Red,
                Palette.Colors[i].Green,
                Palette.Colors[i].Blue);
      end;
   end;

  {********************************************************}
  { Procedure SetPalette()                                 }
  {--------------------------------------------------------}
  { This routine changes the colorNum to the default       }
  { palette entry specified in the second parameter.       }
  { For example, SetPalette(0, Lightcyan) makes the        }
  { 0th palette entry to the default Light Cyan Color .    }
  {********************************************************}
   Procedure SetPalette(ColorNum: word; Color: shortint);
    begin
     { palette routines do not work in DirectColor mode }
     if DirectColor then
       begin
         _GraphResult := grError;
         exit;
       end;
      { Check if we can actually change that palette color }
      if ColorNum > PaletteSize then
        Begin
          _GraphResult := grError;
          exit;
        end
      else
      { Check if the max. default color is reached...}
      { no, this disables palette setting for 256 color modes! (JM) }
{      if Color > EGAWhite then
        begin
          _GraphResult := grError;
          exit;
        end;}
      SetRGBPalette(ColorNum,
          DefaultColors[Color].Red,
          DefaultColors[Color].Green,
          DefaultColors[Color].Blue);
    end;


    procedure GetPalette(var Palette: PaletteType);
      var
        i: longint;
        size : longint;
      begin
        { palette routines do not work in DirectColor mode }
        if DirectColor then
         begin
           _GraphResult := grError;
           exit;
         end;
        Palette.Size := PaletteSize;
        { index at zero }
        size := PaletteSize - 1;
        for i:=0 to size do
          GetRGBPalette(i,
             Palette.Colors[i].Red,
             Palette.Colors[i].Green,
             Palette.Colors[i].Blue);
      end;

    function GetPaletteSize: smallint;
     begin
       GetPaletteSize := PaletteSize;
     end;

    procedure GetDefaultPalette(var Palette: PaletteType);
      begin
        System.move(DefaultColors, Palette.Colors, sizeof(DefaultColors));
        { The default palette always has 256 entries, but in reality }
        { it depends on the number of colors possible.               }
        Palette.Size := PaletteSize;
        if PaletteSize > 256 then Palette.Size := 256;
      end;

