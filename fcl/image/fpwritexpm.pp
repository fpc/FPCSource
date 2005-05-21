{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    XPM writer implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
unit FPWriteXPM;

interface

uses FPImage, classes, sysutils;

type

  TFPWriterXPM = class (TFPCustomImageWriter)
    private
      FPalChars : string;
      FColorFormat : string;
      FColorShift : word;
      FColorSize : byte;
      procedure SetColorSize (AValue : byte);
      function ColorToHex (c:TFPColor) : string;
    protected
      procedure InternalWrite (Str:TStream; Img:TFPCustomImage); override;
    public
      constructor Create; override;
      property PalChars : string read FPalChars write FPalChars;
      property ColorCharSize : byte read FColorSize write SetColorSize;
      // number of characters to use for 1 colorcomponent
  end;


implementation

const
  DefPalChars = '.,-*abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789@#;:=+%$()[]';

constructor TFPWriterXPM.create;
begin
  inherited create;
  PalChars := DefPalChars;
  FColorSize := 4;
end;

procedure TFPWriterXPM.SetColorSize (AValue : byte);
begin
  if AValue > 3 then
    FColorSize := 4
  else if AValue = 0 then
    FColorSize := 1
  else
    FColorSize := AValue;
end;

function TFPWriterXPM.ColorToHex (c:TFPColor) : string;
var r,g,b : word;
begin
  with c do
    begin
    r := red shr FColorShift;
    g := green shr FColorShift;
    b := blue shr FColorShift;
    end;
  result := format(FColorFormat,[r,g,b]);
end;

procedure TFPWriterXPM.InternalWrite (Str:TStream; Img:TFPCustomImage);
var p, l : TStringList;
    c, len, r, t : integer;
  procedure BuildPaletteStrings;
  var r,c,e : integer;
    procedure MakeCodes (const head:string; charplace:integer);
    var r : integer;
    begin
      r := 1;
      dec (charplace);
      while (r <= e) and (c >= 0) do
        begin
        if Charplace = 1 then
          MakeCodes (head+PalChars[r],charplace)
        else
          p.Add (head+PalChars[r]);
        inc (r);
        dec(c);
        end;
    end;
  begin
    // Calculate length of codes
    len := 1;
    e := length(PalChars);
    r := e;
    c := img.palette.count;
    while (r <= c) do
      begin
      inc (len);
      r := r * e;
      end;
    MakeCodes ('',len);
  end;
  procedure InitConsts;
  var fmt : string;
  begin
    fmt := inttostr(FColorSize);
    fmt := '%'+fmt+'.'+fmt+'x';
    FColorFormat := fmt+fmt+fmt;
    case FColorSize of
      1 : FColorShift := 12;
      2 : FColorShift := 8;
      3 : FColorShift := 4;
      else FColorShift := 0;
    end;
  end;
var s : string;
begin
  l := TStringList.Create;
  p := TStringList.Create;
  try
    l.Add ('/* XPM */');
    l.Add ('static char *graphic[] = {');
    c := img.palette.count;
    BuildPaletteStrings;
    l.add (format('"%d %d %d %d",',[img.width,img.height,c,len]));
    InitConsts;
    for r := 0 to c-1 do
      begin
      if img.palette[r] <> colTransparent then
        l.Add (format('"%s c #%s",',[p[r],ColorToHex(img.palette.color[r])]))
      else
        l.Add (format('"%s c None",',[p[r]]));
      end;
    for r := 0 to img.Height-1 do
      begin
      s := p[img.pixels[0,r]];
      for t := 1 to img.Width-1 do
        s := s + p[img.pixels[t,r]];
      s := '"'+s+'"';
      if r < img.Height-1 then
        s := s + ',';
      l.Add (s);
      end;
    l.Add ('};');
  finally
    l.SaveToStream (Str);
    p.Free;
    l.Free;
  end;
end;

initialization
  ImageHandlers.RegisterImageWriter ('XPM Format', 'xpm', TFPWriterXPM);
end.
