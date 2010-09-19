{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2001-2010 Nikolay Nikolov (nickysn@users.sourceforge.net)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version
    with the following modification:

    As a special exception, the copyright holders of this library give you
    permission to link this library with independent modules to produce an
    executable, regardless of the license terms of these independent modules,and
    to copy and distribute the resulting executable under terms of your choice,
    provided that you also meet, for each linked independent module, the terms
    and conditions of the license of that module. An independent module is a
    module which is not derived from or based on this library. If you modify
    this library, you may extend this exception to your version of the library,
    but you are not obligated to do so. If you do not wish to do so, delete this
    exception statement from your version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit CGA;

{$MODE objfpc}
{$ASMMODE intel}
{$INLINE on}

interface

procedure CGAText;
procedure CGA320;
procedure CGA640;
procedure CGADump(q: PByte);
procedure CGASetPalette(palette, border: Integer);
procedure CGAPrecalc;
procedure CGAFree;

implementation

uses
  go32fix, crt;

const
  palette: array[0..15, 0..2] of Byte = (
    ( 0, 0, 0), ( 0, 0,42), ( 0,42, 0), ( 0,42,42), (42, 0, 0), (42, 0,42), (42,21, 0), (42,42,42),
    (21,21,21), (21,21,63), (21,63,21), (21,63,63), (63,21,21), (63,21,63), (63,63,21), (63,63,63));
  cgaback: array[0..3, 0..12] of Integer = (
    (  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 11, 13, 15),
    (  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 12, 14),
    (  0,  1,  3,  5,  7,  8,  9, 10, 11, 12, 13, 14, 15),
    (  0,  1,  2,  4,  6,  8,  9, 10, 11, 12, 13, 14, 15));

type
  Float = Extended;
  TCGAVideoBuffer = array[0..16383] of Byte;
  PCGAPrecalc = ^TCGAPrecalc;
  TCGAPrecalc = array[0..15{r}, 0..15{g}, 0..15{b}, 0..3{y}, 0..3{x}] of Byte;
  PCGAPrecalcError = ^TCGAPrecalcError;
  TCGAPrecalcError = array[0..15{r}, 0..15{g}, 0..15{b}] of Integer;

var
  cgapal: array[0..3] of Integer;
  videobuf: TCGAVideoBuffer;
  precalcbuf: array[0..12, 0..3] of PCGAPrecalc; {3.25mb}
  precalcerror: array[0..12, 0..3] of PCGAPrecalcError; {0.8125mb}
  error: Integer;
  lastpalette, lastback: Integer;

procedure CGA320;

var
  regs: TRealRegs;

begin
  regs.ax := $0004;
  RealIntr($10, regs);
  lastpalette := -1;
  lastback := -1;
end;

procedure CGA640;

var
  regs: TRealRegs;

begin
  regs.ax := $0006;
  RealIntr($10, regs);
end;

procedure CGAText;

var
  regs: TRealRegs;

begin
  regs.ax := $0003;
  RealIntr($10, regs);
end;

procedure CGASetPalette(palette, border: Integer);

var
  regs: TRealRegs;

begin
  if (palette = lastpalette) and (border = lastback) then
    exit;
  lastpalette := palette;
  lastback := border;
  regs.ah := $0B;
  regs.bh := 1;
  regs.bl := palette and 1;
  RealIntr($10, regs);
  if (palette and 2) = 0 then
    Inc(border, 16);
  regs.ah := $0B;
  regs.bh := 0;
  regs.bl := border;
  RealIntr($10, regs);
end;

procedure CGABlitToScreen(p: Pointer); assembler; register;

asm
  mov esi, p
  mov edi, $B8000
  push es
  mov ax, fs
  mov es, ax
  mov ecx, 16192/4
  rep movsd
  pop es
end;

function ColorDistance(r1, g1, b1, r2, g2, b2: Integer): Integer;

var
  RMean: Integer;

begin
//  Result := Sqr(r1 - r2) + Sqr(g1 - g2) + Sqr(b1 - b2);

  { formula taken from: http://www.compuphase.com/cmetric.htm }
  RMean := (r1 + r2) div 2;
  Result := ((512 + RMean)*Sqr(r1 - r2) shr 8) + 4*Sqr(g1 - g2) + ((767 - RMean)*Sqr(b1 - b2) shr 8);
end;

function CGACalc2(r, g, b: Integer; dx, dy: Integer; back, pal: Integer): Integer; inline;

begin
  Result := precalcbuf[back, pal]^[r shr 4, g shr 4, b shr 4, dy, dx];
end;

procedure CGACalc(r, g, b: Integer; var dither, best1, best2: Integer);

var
  I, J: Integer;
  mindist: Float;
  dist: Float;
  r1, g1, b1: Integer;
  tmp: Integer;
{  dither: Integer;} {0-none; 1-50%; 2-25%; 3-12.5%; 4-37.5%}

begin
  r := Round(r*63 / 15);
  g := Round(g*63 / 15);
  b := Round(b*63 / 15);
  mindist := $7FFFFFFF;
  for I := 0 to 3 do
  begin
    dist := ColorDistance(r, g, b, palette[cgapal[I], 0], palette[cgapal[I], 1], palette[cgapal[I], 2]);
    if dist < mindist then
    begin
      mindist := dist;
      best1 := I;
      dither := 0;
    end;
  end;

  for J := 0 to 3 do
  begin
    r1 := palette[cgapal[J], 0];
    g1 := palette[cgapal[J], 1];
    b1 := palette[cgapal[J], 2];
    for I := 0 to 3 do
    begin
      if I = J then
        continue;
      dist := ColorDistance(r, g, b, (palette[cgapal[I], 0] + r1) div 2, (palette[cgapal[I], 1] + g1) div 2, (palette[cgapal[I], 2] + b1) div 2);
      if dist < mindist then
      begin
        mindist := dist;
        best1 := J;
        best2 := I;
        dither := 1;
      end;
      dist := ColorDistance(r, g, b, (palette[cgapal[I], 0] + 3*r1) div 4, (palette[cgapal[I], 1] + 3*g1) div 4, (palette[cgapal[I], 2] + 3*b1) div 4);
      if dist < mindist then
      begin
        mindist := dist;
        best1 := J;
        best2 := I;
        dither := 2;
      end;
      dist := ColorDistance(r, g, b, (palette[cgapal[I], 0] + 7*r1) div 8, (palette[cgapal[I], 1] + 7*g1) div 8, (palette[cgapal[I], 2] + 7*b1) div 8);
      if dist < mindist then
      begin
        mindist := dist;
        best1 := J;
        best2 := I;
        dither := 3;
      end;
      dist := ColorDistance(r, g, b, (3*palette[cgapal[I], 0] + 5*r1) div 8, (3*palette[cgapal[I], 1] + 5*g1) div 8, (3*palette[cgapal[I], 2] + 5*b1) div 8);
      if dist < mindist then
      begin
        mindist := dist;
        best1 := J;
        best2 := I;
        dither := 4;
      end;
    end;
  end;

  error := error + Round(Sqrt(mindist) * {290}40);
  case dither of
    0: best2 := best1;
    1: begin
      if best1 > best2 then
      begin
        tmp := best1;
        best1 := best2;
        best2 := tmp;
      end;
    end;
  end;
end;

function CGACalcError(s: PByte; back, pal: Integer): Integer;

var
  X, Y: Integer;
  r, g, b: Integer;

begin
  Result := 0;
  for Y := 0 to 199 {div 4} do
  begin
    for X := 0 to 319 {div 4} do
    begin
      b := s[0];
      g := s[1];
      r := s[2];
      Inc(Result, precalcerror[back, pal]^[b shr 4, g shr 4, r shr 4]);
      Inc(s, 4 {+ 4 + 4 + 4});
    end;
    {Inc(s, 320*4*3);}
  end;
end;

procedure CGADump2(s, d: PByte; back, pal: Integer);

var
  I: Integer;
  src, dest: PByte;
  X, Y: Integer;
  r1, g1, b1: Integer;
  r2, g2, b2: Integer;
  r3, g3, b3: Integer;
  r4, g4, b4: Integer;

begin
  error := 0;
  src := s;
  dest := d;
  for Y := 0 to 99 do
  begin
    for X := 0 to 79 do
    begin
      b1 := src[0];
      g1 := src[1];
      r1 := src[2];
      b2 := src[4];
      g2 := src[5];
      r2 := src[6];
      b3 := src[8];
      g3 := src[9];
      r3 := src[10];
      b4 := src[12];
      g4 := src[13];
      r4 := src[14];
      dest^ := (CGACalc2(r1, g1, b1, 0, (Y and 1) shl 1, back, pal) shl 6) or
               (CGACalc2(r2, g2, b2, 1, (Y and 1) shl 1, back, pal) shl 4) or
               (CGACalc2(r3, g3, b3, 2, (Y and 1) shl 1, back, pal) shl 2) or
               (CGACalc2(r4, g4, b4, 3, (Y and 1) shl 1, back, pal));

      Inc(src, 4*4);
      Inc(dest);
    end;
    Inc(src, 320*4);
  end;
  src := s + 320*4;
  dest := d + 8192;
  for Y := 0 to 99 do
  begin
    for X := 0 to 79 do
    begin
      b1 := src[0];
      g1 := src[1];
      r1 := src[2];
      b2 := src[4];
      g2 := src[5];
      r2 := src[6];
      b3 := src[8];
      g3 := src[9];
      r3 := src[10];
      b4 := src[12];
      g4 := src[13];
      r4 := src[14];
      dest^ := (CGACalc2(r1, g1, b1, 0, ((Y and 1) shl 1) + 1, back, pal) shl 6) or
               (CGACalc2(r2, g2, b2, 1, ((Y and 1) shl 1) + 1, back, pal) shl 4) or
               (CGACalc2(r3, g3, b3, 2, ((Y and 1) shl 1) + 1, back, pal) shl 2) or
               (CGACalc2(r4, g4, b4, 3, ((Y and 1) shl 1) + 1, back, pal));

      Inc(src, 4*4);
      Inc(dest);
    end;
    Inc(src, 320*4);
  end;
end;

procedure CGADump(q: PByte);

var
  pal, back: Integer;
  bestpal, bestback: Integer;
  besterror: Integer;

begin
  besterror := $7FFFFFFF;
  for pal := 0 to 3 do
  begin
    for back := 0 to 12 do
    begin
      error := CGACalcError(q, back, pal);
      if error < besterror then
      begin
        besterror := error;
        bestpal := pal;
        bestback := back;
      end;
    end;
  end;

  CGADump2(q, videobuf, bestback, bestpal);

  CGASetPalette(bestpal, cgaback[bestpal, bestback]);
  CGABlitToScreen(@videobuf);
end;

procedure CGAPrecalc;

var
  pal, back: Integer;
  r, g, b: Integer;
  x, y: Integer;
  dither: Integer;
  best1, best2: Integer;
  res: Integer;

begin
  Writeln('Precalculating CGA lookup tables, please wait...');
  for pal := 0 to 3 do
  begin
    case pal of
      0: begin
        cgapal[1] := 10;
        cgapal[2] := 12;
        cgapal[3] := 14;
      end;
      1: begin
        cgapal[1] := 11;
        cgapal[2] := 13;
        cgapal[3] := 15;
      end;
      2: begin
        cgapal[1] := 2;
        cgapal[2] := 4;
        cgapal[3] := 6;
      end;
      3: begin
        cgapal[1] := 3;
        cgapal[2] := 5;
        cgapal[3] := 7;
      end;
    end;
    for back := 0 to 12 do
    begin
      if (precalcbuf[back, pal] = nil) and (precalcerror[back, pal] = nil) then
      begin
        new(precalcbuf[back, pal]);
        new(precalcerror[back, pal]);
      end
      else
        continue;

      cgapal[0] := cgaback[pal, back];
      error := 0;
      Write('  (');
      TextAttr := cgapal[0];
      Write('*');
      TextAttr := cgapal[1];
      Write('*');
      TextAttr := cgapal[2];
      Write('*');
      TextAttr := cgapal[3];
      Write('*');
      TextAttr := 7;
      Write(')');
      for r := 0 to 15 do
        for g := 0 to 15 do
          for b := 0 to 15 do
          begin
            error := 0;
            CGACalc(r, g, b, dither, best1, best2);
            precalcerror[back, pal]^[r, g, b] := error;
            for y := 0 to 3 do
              for x := 0 to 3 do
              begin
                case dither of
                  0: res := best1;
                  1: begin
                    if ((x + y) and 1) <> 0 then
                      res := best1
                    else
                      res := best2;
                  end;
                  2: begin
                    if ((x and 1) = 0) and ((y and 1) = 0) then
                      res := best2
                    else
                      res := best1;
                  end;
                  3: begin
                    if (x = y) and ((x and 1) = 0) then
                      res := best2
                    else
                      res := best1;
                  end;
                  4: begin
                    if (((x and 1) = 0) and ((y and 1) = 0)) or (x = y) then
                      res := best2
                    else
                      res := best1;
                  end;
                end;
                precalcbuf[back, pal]^[r, g, b, y, x] := res;
              end;
          end;
    end;
  end;
end;

procedure CGAFree;

var
  pal, back: Integer;

begin
  for pal := 0 to 3 do
  begin
    for back := 0 to 12 do
    begin
      if precalcbuf[back, pal] <> nil then
      begin
        dispose(precalcbuf[back, pal]);
	precalcbuf[back, pal] := nil;
      end;
      
      if precalcerror[back, pal] <> nil then
      begin
        dispose(precalcerror[back, pal]);
	precalcerror[back, pal] := nil;
      end;
    end;
  end;
end;

initialization
  FillChar(precalcbuf, SizeOf(precalcbuf), 0);
  FillChar(precalcerror, SizeOf(precalcerror), 0);

finalization
  CGAFree;

end.
