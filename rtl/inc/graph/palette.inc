{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,99 by the Free Pascal development team

    This include implements the different palette manipulation
    routines.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{CONST
  DefaultPalette : PaletteType =
  (255;                         }
var
  DefaultColors : PaletteType;

  procedure SetAllPalette(var Palette:PaletteType);
   var
    i: longint;
    Size: longint;
   begin
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
      { Check if we can actually change that palette color }
      if ColorNum > PaletteSize then
        Begin
          _GraphResult := grError;
          exit;
        end
      else
      { Check if the max. default color is reached...}
      if Color > EGAWhite then
        begin
          _GraphResult := grError;
          exit;
        end;
      SetRGBPalette(ColorNum,
          DefaultColors.Colors[Color].Red,
          DefaultColors.Colors[Color].Green,
          DefaultColors.Colors[Color].Blue);
    end;


    procedure GetPalette(var Palette: PaletteType);
      var
        i: longint;
        size : longint;
      begin
        Palette.Size := PaletteSize;
        { index at zero }
        size := PaletteSize - 1;
        for i:=0 to size do
          GetRGBPalette(i,
             Palette.Colors[i].Red,
             Palette.Colors[i].Green,
             Palette.Colors[i].Blue);
      end;

    function GetPaletteSize: integer;
     begin
       GetPaletteSize := PaletteSize;
     end;

    procedure GetDefaultPalette(var Palette: PaletteType);
      begin
        move(DefaultColors, Palette, sizeof(DefaultColors));
        { The default palette always has 256 entries, but in reality }
        { it depends on the number of colors possible.               }
        Palette.Size := PaletteSize;
      end;

{
procedure SetRGBPalette(ColorNum,RedValue,GreenValue,BlueValue:integer);
begin
  asm
    movw  $0x3c8,%DX
    movb  ColorNum,%al
    outb  %AL,%DX
    incw  %DX
    movb  RedValue,%al
    shrb  $2,%al
    outb  %AL,%DX
    movb  GreenValue,%al
    shrb  $2,%al
    outb  %AL,%DX
    movb  BlueValue,%al
    shrb  $2,%al
    outb  %AL,%DX
  end;
end;

procedure GetRGBPalette(ColorNum:byte; var RedValue,GreenValue,BlueValue:byte);
begin
  asm
    movw  $0x3c7,%DX
    movb  ColorNum,%ax
    outb  %AL,%DX
    addw  $2,%DX
    xorl  %eax,%eax
    inb   %DX,%AL
    shlb  $2,%al
    movb  %al,RedValue
    inb   %DX,%AL
    shlb  $2,%al
    movb  %al,GreenValue
    inb   %DX,%AL
    shlb  $2,%al
    movb  %al,BlueValue
  end;
end; }


