{
    This file is part of text IDE
    Copyright (c) 2000 by Pierre Muller

    Unit to export current screen buffer to an ansi file

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpdpansi;

interface

uses
  objects,
  video;

function ExportBufferToAnsiFile(var Buffer : TVideoBuf;xmin,xmax,ymin,ymax,linesize : sw_integer;
           SaveAsText : boolean;var f : text) : boolean;

implementation

uses
  wutils;

const
{ Foreground and background color constants }
  Black         = 0;
  Blue          = 1;
  Green         = 2;
  Cyan          = 3;
  Red           = 4;
  Magenta       = 5;
  Brown         = 6;
  LightGray     = 7;

const ColorTab : array[0..7] of byte =
      (Black,Red,Green,Brown,Blue,Magenta,Cyan,LightGray);

{$i-}
function ExportBufferToAnsiFile(var Buffer  : TVideoBuf;xmin,xmax,ymin,ymax,linesize : sw_integer;
           SaveAsText : boolean;var f : text) : boolean;
var
  CurColor : byte;
  CurForColor, CurBackColor : byte;
  CurIsBold, CurIsBlinking : boolean;

  procedure ChangeColor(NewColor : byte);
    var
      ForColor, BackColor : byte;
      IsBold, IsBlinking : boolean;
    begin
      ForColor:=NewColor and 7;
      BackColor:=(NewColor and $70) shr 4;
      IsBold:=(NewColor and 8) <> 0;
      IsBlinking:=(NewColor and $80) <> 0;
      if IsBlinking<>CurIsBlinking then
        begin
          if IsBlinking then
            Write(f,#27'[5m')
          else
            Write(f,#27'[25m');
          CurIsBlinking:=IsBlinking;
        end;
      if IsBold<>CurIsBold then
        begin
          if IsBold then
            Write(f,#27'[1m')
          else
            Write(f,#27'[21m');
          CurIsBold:=IsBold;
        end;
      if CurForColor<>ForColor then
        begin
          Write(f,#27'['+inttostr(ColorTab[ForColor]+30)+'m');
          CurForColor:=ForColor;
        end;
      if CurBackColor<>BackColor then
        begin
          Write(f,#27'['+inttostr(ColorTab[BackColor]+40)+'m');
          CurBackColor:=BackColor;
        end;
      CurColor:=NewColor;
    end;
var
  Ch : char;
  textAttr : byte;
  i, j : sw_integer;
begin
  CurColor:=0;
  for i:=ymin to ymax do
    begin
      for j:=xmin to xmax do
        begin
          ch:=chr(Buffer[i*linesize+j] and $ff);
          textattr:=Buffer[i*linesize+j] shr 8;
          if (textattr<>CurColor) and not SaveAsText then
            ChangeColor(textattr);
          { Escape escape, by printing two #27 PM }
          if (ch=#27) or (ord(ch)<=16) then
            Write(f,#27);
          Write(f,ch);
        end;
      writeln(f);
    end;
  ExportBufferToAnsiFile:=(IOResult=0);
end;

end.
