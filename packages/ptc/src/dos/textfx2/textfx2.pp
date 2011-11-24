{*
 * TextFX2 Copyright (c) 1998 Jari Komppa aka Sol/Trauma
 * <mailto:solar@compart.fi>
 *
 * Textmode low-level functions
 *
 * This sourcefile is kinda long-ish, and should be split into several
 * sources, but I have wanted to keep it in one file since everything
 * here is kinda small and.. well, I wanted to keep it as a single .obj
 * file.
 *
 * if you make improvements, send me a copy!
 * if you use this for something, let me know!
 *}

{$MODE objfpc}
{$INLINE on}

unit textfx2;

interface

const
{*
 * Charsets in 'lightness' order. First byte = num of chars
 *
 * Please note that these don't work with the current calcpal
 * strategy :)
 *
 *}
  charset_b8ibm: array [0..254] of Byte = { all imbscii characters }
( 254, 32, 96, 39, 250, 95, 126, 46, 94, 34, 249, 248, 44, 58, 45,
196, 59, 253, 167, 61, 166, 252, 47, 28, 217, 192, 169, 205, 246, 7,
170, 27, 190, 43, 212, 62, 60, 124, 26, 226, 193, 40, 243, 242, 240,
63, 41, 37, 139, 191, 55, 91, 207, 218, 200, 176, 9, 105, 241, 92,
141, 33, 125, 238, 102, 161, 231, 123, 211, 202, 247, 108, 99, 168,
188, 73, 93, 67, 29, 175, 174, 106, 114, 189, 140, 24, 76, 116, 194,
208, 49, 115, 50, 70, 228, 13, 84, 80, 156, 51, 120, 122, 179, 173,
184, 53, 89, 155, 244, 213, 90, 25, 135, 223, 57, 42, 83, 118, 128,
101, 245, 127, 171, 74, 19, 159, 4, 1, 180, 110, 137, 230, 195, 209,
18, 97, 111, 117, 86, 229, 31, 138, 69, 144, 22, 148, 130, 16, 132,
181, 129, 36, 157, 198, 136, 12, 214, 71, 239, 133, 160, 162, 149,
163, 151, 52, 54, 98, 107, 251, 104, 224, 197, 183, 154, 235, 164,
112, 131, 85, 147, 121, 236, 150, 232, 134, 153, 11, 210, 225, 79,
172, 145, 227, 152, 100, 23, 21, 88, 17, 48, 119, 75, 68, 113, 30, 72,
15, 233, 56, 103, 65, 142, 234, 5, 82, 109, 216, 201, 254, 66, 38,
158, 143, 237, 203, 187, 77, 221, 146, 14, 78, 35, 81, 64, 20, 177,
87, 6, 165, 3, 204, 186, 222, 199, 206, 185, 182, 215, 220, 2, 178,
10, 8, 219);

  charset_b7asc: array [0..94] of Byte = { 7b ascii (chars 32 - 126) }
( 94, 32, 96, 39, 95, 126, 46, 94, 34, 44, 58, 45, 59, 61, 47, 43, 62,
60, 40, 63, 41, 37, 55, 91, 105, 92, 33, 125, 102, 123, 108, 99, 73,
93, 67, 106, 114, 76, 116, 49, 115, 50, 70, 84, 80, 51, 120, 122, 53,
89, 90, 57, 42, 83, 118, 101, 74, 110, 97, 111, 117, 86, 69, 36, 71,
52, 54, 98, 107, 104, 112, 85, 121, 79, 100, 88, 48, 119, 75, 68, 113,
72, 56, 103, 65, 82, 109, 66, 38, 77, 78, 35, 81, 64, 87);

  charset_b7sml: array [0..14] of Byte = { " crsxzvenaouwm" dark->light. }
( 14, 32, 99, 114, 115, 120, 122, 118, 101, 110, 97, 111, 117, 119,
109 );

  charset_b8gry: array [0..5] of Byte = { 8b ibm grayscale characters }
( 5, 32, 176, 177, 178, 219 );

  charset_b7nws: array [0..6] of Byte = { 7b grayscale 'newschool' askee chars}
( 6, 32{' '}, 46{'.'}, 111{'o'}, 109{'m'}, 87{'W'}, 77{'M'} );

  use_charset: Pbyte = @charset_b7asc;
  { Character set to use. Can be changed run-time. }

  colmap: PWord = nil;

procedure set80x43; { Sets up 80x43, no blink, no cursor. }
procedure set80x50; { Sets up 80x50, no blink, no cursor. }
procedure set80x25; { Resets 80x25, blink, cursor. }
procedure border(color: Byte); { _ONLY_ for debugging! }
procedure vrc; { Although all should be timer-synced instead.. }

{*
 * calc_ functions are pretty *S*L*O*W* so use them to precalculate
 * color tables and then use those tables instead.
 *}

function calcpal_colorbase(red, green, blue: Real): Word;
function calcpal_lightbase(red, green, blue: Real): Word;
function calcpal_lightbase_g(red, green, blue: Real): Word;
{Function (*calcpal)(float red, float green, float blue): Word;}
const calcpal: Function(red, green, blue: Real): Word = @calcpal_colorbase;
    {* Finds the closest color/char combo for any 0:63,0:63,0:63 value.
     *
     * calcpal_colorbase is the 'old' calcpal, only "a bit" optimized.
     * calcpal is now function pointer so calcpal function can be changed
     * run-time. Use the functions directly if you need speed (and
     * compile with -oe256 or something to force inlining)
     *}

function calc_gscale(light: Real): Word;
function calc_gscale2(light: Real): Word;
    {* Finds the closes gscale color/char combo for 0..1 range
     * gscale2 uses colors 8,7,15, normal just uses 7.
     *}

procedure build_colormap(dots: Integer);
    {* Used to calculate colormap for dump_nnx() -functions.
     * if dots=0, will output nothing.
     *         1, will cprintf .:s as process.
     *         2, will cprintf rolling wheel as process.
     *}

procedure dispose_colormap;

procedure dump_80x(y0, y1: Integer; buffer: PInteger);
    {* Dumps 80-pixel wide 0bgr-truecolor buffer from y0 to y1.
     * (For fullscreen dump in 80x43 use dump_80x(0,43,buf);
     *}

procedure dump_160x(y0, y1: Integer; buffer: PInteger);
    {* Dumps 160-pixel wide 0bgr-truecolor buffer from y0 to y1
     * with 4-to-1 pixel averaging.
     *}

procedure dump_320x(y0, y1: Integer; buffer: PInteger);
    {* Dumps 160-pixel wide 0bgr-truecolor buffer from y0 to y1
     * with 16-to-1 pixel averaging. (this is tad bit slow :)
     *}

implementation

uses
  go32fix;

{ $define __USE_178NOT176}
 { uncomment to use 75% char instead of 25% char }

{$DEFINE __USE_REALIBMPAL}
 { comment out to use 'clean' truecolor palette for calculations }

const
  COLORMAP_DEPTH = 4;
 {* Normally, build 1<<4, ie. 16x16x16 colormap.
  * if you require bigger map, increase the value.
  * (5 will mean 32x32x32 etc).
  * 8 is max for dump_80x and _320x, 6 is max for _160x.
  * if you make your own routines, well, nothing is too much :)
  *}
{ Don't touch the rest of the defines. }
  COLMAPDIM = 1 shl COLORMAP_DEPTH;
  TRUCOLBITS = 8 - COLORMAP_DEPTH;

{$IFDEF __USE_REALIBMPAL}
  palette: array [0..16*3-1] of Byte = ( {IBM basic palette, 16c}
     0, 0, 0,  0, 0,42,  0,42, 0,  0,42,42, 42, 0, 0, 42, 0,42, 42,21, 0, 42,42,42,
    21,21,21, 21,21,63, 21,63,21, 21,63,63, 63,21,21, 63,21,63, 63,63,21, 63,63,63);
{$ELSE}
  palette: array [0..16*3-1] of Byte = ( { 'clean' RGB palette }
     0, 0, 0,  0, 0,32,  0,32, 0,  0,32,32, 32, 0, 0, 32, 0,32, 32,32, 0, 32,32,32,
    32,32,32,  0, 0,63,  0,63, 0,  0,63,63, 63, 0, 0, 63, 0,63, 63,63, 0, 63,63,63);
{$ENDIF}

procedure set80x43; { Sets up 80x43, no blink, no cursor. }

var
  regs: TRealRegs;

begin
  regs.ax := $1201; { Set 350 scanlines }
  regs.bl := $30;
  realintr($10, regs);
  regs.ax := $3; { Set text mode }
  realintr($10, regs);
  regs.ax := $1112; { Set font }
  regs.bx := 0;
  realintr($10, regs);
  regs.bh := 0; { Kill cursor - doesn't seem to work.. }
  regs.ah := 3;
  realintr($10, regs);
  regs.cx := $2000;
  regs.ah := 1;
  realintr($10, regs);
  regs.ax := $1003; { Kill blink }
  regs.bl := 0;
  realintr($10, regs);
  regs.ax := $0200; { Position cursor to 51,80 - better way to kill. }
  regs.bx := $0033;
  regs.dx := $004f;
  realintr($10, regs);
end;

procedure set80x50; { Sets up 80x50, no blink, no cursor. }

var
  regs: TRealRegs;

begin
  regs.ax := $1202; { Set 400 scanlines }
  regs.bl := $30;
  realintr($10, regs);
  regs.ax := $3; { Set text mode }
  realintr($10, regs);
  regs.ax := $1112; { Set font }
  regs.bx := 0;
  realintr($10, regs);
  regs.bh := 0; { Kill cursor - doesn't seem to work.. }
  regs.ah := 3;
  realintr($10, regs);
  regs.cx := $2000;
  regs.ah := 1;
  realintr($10, regs);
  regs.ax := $1003; { Kill blink }
  regs.bl := 0;
  realintr($10, regs);
  regs.ax := $0200; { Position cursor to 51,80 - better way to kill. }
  regs.bx := $0033;
  regs.dx := $004f;
  realintr($10, regs);
end;

procedure set80x25; { Resets 80x25, blink, cursor. }

var
  regs: TRealRegs;

begin
  regs.ax := $1202; { Set 400 scanlines }
  regs.bl := $30;
  realintr($10, regs);
  regs.ax := $3;    { Set text mode }
  realintr($10, regs);
  regs.ax := $1114; { Set font }
  regs.bx := 0;
  realintr($10, regs);
  regs.bh := 0;      { Ressurrect cursor }
  regs.ah := 3;
  realintr($10, regs);
  regs.cx := regs.cx and $dfff;
  regs.ah := 1;
  realintr($10, regs);
  regs.ax := $1003; { Enable blink }
  regs.bl := 1;
  realintr($10, regs);
end;

procedure border(color: Byte); { _ONLY_ for debugging! }

begin
  inportb($3da);
  outportb($3c0, 17+32);
  outportb($3c0, color);
end;

procedure vrc; { Although all should be timer-synced instead.. }

begin
  while (inportb($3da) and 8) = 0 do ;
  while (inportb($3da) and 8) <> 0 do ;
end;

function GetCOLMAP(const r, g, b: Integer): Integer; Inline;

begin
  Result := (colmap + ((r shl (COLORMAP_DEPTH*2)) + (g shl COLORMAP_DEPTH) + b))^;
end;

procedure SetCOLMAP(const r, g, b, v: Integer); Inline;

begin
  (colmap + ((r shl (COLORMAP_DEPTH*2)) + (g shl COLORMAP_DEPTH) + b))^ := v;
end;

function calcpal_colorbase(red, green, blue: Real): Word;

var
  a, b, c, d, ch, co: Integer;
  lastdist, dist: Double;

begin
  red := red * 1.2;
  green := green * 1.2;
  blue := blue * 1.2;
  lastdist := 1e242;
  d := 0;
  for c := 0 to 15 do
  begin
    dist := sqr(palette[d + 0] - red) +
            sqr(palette[d + 1] - green) +
            sqr(palette[d + 2] - blue);
    if dist < lastdist then
    begin
      lastdist := dist;
      co := c;
      ch := 219; { 100% block in IBMSCII }
    end;
    Inc(d, 3);
  end;
  c := co;
  d := c*3;
  a := 0;
  for b := 0 to 15 do
  begin
    dist := sqr(((palette[a+0]+palette[d+0]) / 2.0) - red) +
            sqr(((palette[a+1]+palette[d+1]) / 2.0) - green) +
            sqr(((palette[a+2]+palette[d+2]) / 2.0) - blue);
    if dist < lastdist then
    begin
      lastdist := dist;
      co := b + (c shl 4);
      ch := 177; { 50% block in IBMSCII }
    end;
    {$IFDEF __USE_178NOT176}
      dist := sqr((palette[a+0]*0.75+palette[d+0]*0.25) - red) +
              sqr((palette[a+1]*0.75+palette[d+1]*0.25) - green) +
              sqr((palette[a+2]*0.75+palette[d+2]*0.25) - blue);
      if dist < lastdist then
      begin
        lastdist := dist;
        co := b + (c shl 4);
        ch := 178; { 75% block in IBMSCII }
      end;
      dist := sqr((palette[a+0]*0.25+palette[d+0]*0.75) - red) +
              sqr((palette[a+1]*0.25+palette[d+1]*0.75) - green) +
              sqr((palette[a+2]*0.25+palette[d+2]*0.75) - blue);
      if dist < lastdist then
      begin
        lastdist := dist;
        co := c + (b shl 4);
        ch := 178; { 75% block in IBMSCII }
      end;
    {$ELSE}
      dist := sqr((palette[a+0]*0.25+palette[d+0]*0.75) - red) +
              sqr((palette[a+1]*0.25+palette[d+1]*0.75) - green) +
              sqr((palette[a+2]*0.25+palette[d+2]*0.75) - blue);
      if dist < lastdist then
      begin
        lastdist := dist;
        co := b + (c shl 4);
        ch := 176; { 25% block in IBMSCII }
      end;
      dist := sqr((palette[a+0]*0.75+palette[d+0]*0.25) - red) +
              sqr((palette[a+1]*0.75+palette[d+1]*0.25) - green) +
              sqr((palette[a+2]*0.75+palette[d+2]*0.25) - blue);
      if dist < lastdist then
      begin
        lastdist := dist;
        co := c + (b shl 4);
        ch := 176; { 25% block in IBMSCII }
      end;
    {$ENDIF}
    Inc(a, 3);
  end;
  calcpal_colorbase := (co shl 8) + ch;
end;

{*
 * Unlike _colorbase, _lightbase and _gscale calculations are
 * based on some trivial assumptions, such as that the character
 * tables have linear grayscale ramps and stuff like that.
 *
 * ie: they are *not* accurate!
 *
 * The tables were generated by calculating the dot distance from
 * center of character, ((xdistmax-xdist)^2)+((ydistmax-ydist)^2),
 * and sorting by this value. (HOW are you supposed to calculate
 * random pattern lightness value anyway?! =)
 *
 * Bright and dark color values are just thrown in without any
 * math background. (How could there be some? At this point you
 * should realize we have thrown all accurancy out the window).
 *
 * So. They work - kinda. They don't work correctly, but there
 * you go.
 *
 * color ramp= (dark color) [0 .. 1] + (light color) [0.3 .. 1]
 *
 * (didn't bother to rip AAlib :)
 *}

function calcpal_lightbase(red, green, blue: Real): Word;

var
  light, col, a, a3: Integer;
  lastdist, dist: Real;

begin
  lastdist := 1e24;
  a3 := 3;
  for a := 1 to 15 do
  begin
    dist := Sqr(palette[a * 3 + 0] - red) +
            Sqr(palette[a * 3 + 1] - green) +
            Sqr(palette[a * 3 + 2] - blue);
    if dist < lastdist then
    begin
      lastdist := dist;
      col := a;
    end;
    Inc(a3, 3);
  end;
  light := Trunc(((0.2990 * red + 0.5870 * green + 0.1140 * blue) / 63) * 64);
  if light < 32 then
    light := Trunc(((0.2990 * red + 0.5870 * green + 0.1140 * blue) / 63) * 1.5 * use_charset^)
  else
    light := Trunc(((0.2990 * red + 0.5870 * green + 0.1140 * blue) / 63) * use_charset^);
  calcpal_lightbase := (col shl 8) + (use_charset + light + 1)^;
end;

function calcpal_lightbase_g(red, green, blue: Real): Word;

var
  light: Integer;

begin
  light := Trunc(((0.2990 * red + 0.5870 * green + 0.1140 * blue) / 63) * use_charset^);
  calcpal_lightbase_g := (7 shl 8) + (use_charset + light + 1)^;
end;

function calc_gscale(light: Real): Word;

begin
  calc_gscale := (7 shl 8) + (use_charset + Trunc(light * (use_charset^ + 1)))^;
end;

function calc_gscale2(light: Real): Word;

begin
  if light < 0.3 then
    calc_gscale2 := (8 shl 8) + (use_charset + Trunc(light * 3 * (use_charset^ + 1)))^
  else
    if light < 0.6 then
      calc_gscale2 := (7 shl 8) + (use_charset + Trunc((light + 0.3) * (use_charset^ + 1)))^
    else
      calc_gscale2 := (15 shl 8) + (use_charset + Trunc(light * (use_charset^ + 1)))^;
end;

procedure build_colormap(dots: Integer);

const
  wheel: array [0..3] of Char = ('-', '\', '|', '/');

var
  r, g, b: Integer;
  f: Double;

begin
  if dots = 2 then
    Write(' ');
  if colmap <> nil then
    FreeMem(colmap);
  f := 64.0 / COLMAPDIM;
  colmap := GetMem(SizeOf(Word) * COLMAPDIM * COLMAPDIM * COLMAPDIM);
  for r := 0 to COLMAPDIM - 1 do
  begin
    for g := 0 to COLMAPDIM - 1 do
      for b := 0 to COLMAPDIM - 1 do
        SetCOLMAP(r, g, b, calcpal(r * f, g * f, b * f));
    if dots = 1 then
      Write('.');
    if dots = 2 then
      Write({#127}#8, wheel[r and 3]);
  end;
end;

procedure dispose_colormap;

begin
  if colmap <> nil then
    FreeMem(colmap);
  colmap := nil;
end;

procedure dump_80x(y0, y1: Integer; buffer: PInteger);

var
  x, y, yd: Integer;
  scr: DWord;
  buf: PByte;

begin
  buf := PByte(buffer);
  scr := $b8000 + (y0 * 160);
  yd := y1 - y0;
  for y := 0 to yd - 1 do
    for x := 0 to 79 do
    begin
      MemW[scr] := GetCOLMAP((buf + 0)^ shr TRUCOLBITS,
                             (buf + 1)^ shr TRUCOLBITS,
                             (buf + 2)^ shr TRUCOLBITS);
      Inc(scr, 2);
      Inc(buf, 4);
    end;
end;

procedure dump_160x(y0, y1: Integer; buffer: PInteger);

var
  x, y, yd: Integer;
  i: DWord;
  scr: DWord;
  buf: PByte;

begin
  buf := @i;
  scr := $b8000 + (y0 * 160);
  yd := y1 - y0;
  for y := 0 to yd - 1 do
  begin
    for x := 0 to 79 do
    begin
      i := ((buffer+0)^ and $fcfcfcfc)+
           ((buffer+1)^ and $fcfcfcfc)+
           ((buffer+160)^ and $fcfcfcfc)+
           ((buffer+161)^ and $fcfcfcfc);
      i := i shr 2;
      i := i and $fcfcfcfc;
      MemW[scr] := GetCOLMAP((buf + 0)^ shr TRUCOLBITS,
                             (buf + 1)^ shr TRUCOLBITS,
                             (buf + 2)^ shr TRUCOLBITS);
      Inc(scr, 2);
      Inc(buffer, 2);
    end;
    Inc(buffer, 160);
  end;
end;

procedure dump_320x(y0, y1: Integer; buffer: PInteger);

var
  x, y, yd, r, g, b, xx, yy: Integer;
  buf: PByte;
  scr: DWord;

begin
  buf := PByte(buffer);
  scr := $b8000 + (y0 * 160);
  yd := y1 - y0;
  for y := 0 to yd - 1 do
  begin
    for x := 0 to 79 do
    begin
      r := 0; g := 0; b:= 0;
      xx := 0;
      while xx < 4 * 4 do
      begin
        yy := 0;
        while yy < 4 * 4 * 320 do
        begin
          Inc(r, (buf + xx + yy + 0)^);
          Inc(g, (buf + xx + yy + 1)^);
          Inc(b, (buf + xx + yy + 2)^);
          Inc(yy, 320 * 4);
        end;
        Inc(xx, 4);
      end;
      MemW[scr] := GetCOLMAP(r shr (TRUCOLBITS + 4),
                             g shr (TRUCOLBITS + 4),
                             b shr (TRUCOLBITS + 4));
      Inc(scr, 2);
      Inc(buf, 4 * 4);
    end;
    Inc(buf, 80 * 4 * 4 * 3);
  end;
end;

end.
