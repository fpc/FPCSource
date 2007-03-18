{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 Land demo for OpenPTC 1.0 C++ API

 Based on Heightmap example from Hornet (RIP)
 PTC version Copyright (c) 1998 Marcus Fletcher (cus@commsat.demon.co.uk)

 Updated to OpenPTC 1.0 by Glenn Fiedler (ptc@gaffer.org)

 Cursor keys to move, <Pause> to brake and <Esc> to quit
}

Program Land;

{$MODE objfpc}

Uses
  ptc;

Const
  SCREENWIDTH = 320;
  SCREENHEIGHT = 200;

  FOV : Integer = 256; { half of the xy field of view (This is based on the 0-2048 convention) }

Var
  HMap : Array[0..256*256 - 1] Of Uint8; { Height field }
  CMap : Array[0..256*256 - 1] Of Uint8; { Color map }

  lasty, { Last pixel drawn on a given column }
  lastc : Array[0..SCREENWIDTH - 1] Of Integer; { Color of last pixel on a column }
  CosT, SinT : Array[0..2047] Of Integer; { Cosine and Sine tables }

{ Reduces a value to 0..255 (used in height field computation) }
Function Clamp(x : Integer) : Integer;

Begin
  If x < 0 Then
    Clamp := 0
  Else
    If x > 255 Then
      Clamp := 255
    Else
      Clamp := x;
End;

{ Heightfield and colormap computation }
Procedure ComputeMap;

Var
  p, i, j, k, k2, p2, a, b, c, d : Integer;

Begin
  { Start from a plasma clouds fractal }
  HMap[0] := 128;
  p := 256;
  While p > 1 Do
  Begin
    p2 := p Shr 1;
    k := p * 8 + 20;
    k2 := k Shr 1;
    i := 0;
    While i < 256 Do
    Begin
      j := 0;
      While j < 256 Do
      Begin
	a := HMap[(i Shl 8) + j];
	b := HMap[(((i + p) And 255) Shl 8) + j];
	c := HMap[(i Shl 8) + ((j + p) And 255)];
	d := HMap[(((i + p) And 255) Shl 8) + ((j + p) And 255)];

	HMap[(i Shl 8) + ((j + p2) And 255)] :=
	  Clamp(((a + c) Shr 1) + (Random(k) - k2));
	HMap[(((i + p2) And 255) Shl 8) + ((j + p2) And 255)] :=
	  Clamp(((a + b + c + d) Shr 2) + (Random(k) - k2));
	HMap[(((i + p2) And 255) Shl 8) + j] :=
	  Clamp(((a + b) Shr 1) + (Random(k) - k2));
	Inc(j, p);
      End;
      Inc(i, p);
    End;
    p := p2;
  End;

  { Smoothing }
  For k := 0 To 2 Do
  Begin
    i := 0;
    While i < 256*256 Do
    Begin
      For j := 0 To 255 Do
	HMap[i + j] := (HMap[((i + 256) And $FF00) + j] +
			HMap[i + ((j + 1) And $FF)] +
			HMap[((i - 256) And $FF00) + j] +
			HMap[i + ((j - 1) And $FF)]) Shr 2;
      Inc(i, 256);
    End;
  End;

  { Color computation (derivative of the height field) }
  i := 0;
  While i < 256*256 Do
  Begin
    For j := 0 To 255 Do
    Begin
      k := 128 + (HMap[((i + 256) And $FF00) + ((j + 1) And 255)] - HMap[i + j])*4;
      If k < 0 Then
	k := 0;
      If k > 255 Then
	k := 255;
      CMap[i + j] := k;
    End;
    Inc(i, 256);
  End;
End;

{ Calculate the lookup tables }
Procedure InitTables;

Var
  a : Integer;
  result : Single;

Begin
  For a := 0 To 2047 Do
  Begin
    { Precalculate cosine }
    result := cos(a * PI / 1024) * 256;
    CosT[a] := Trunc(result);

    { and sine }
    result := sin(a * PI / 1024) * 256;
    SinT[a] := Trunc(result);
  End;
End;

{
 Draw a "section" of the landscape; x0,y0 and x1,y1 and the xy coordinates
 on the height field, hy is the viewpoint height, s is the scaling factor
 for the distance. x0,y0,x1,y1 are 16.16 fixed point numbers and the
 scaling factor is a 16.8 fixed point value.
}
Procedure Line(x0, y0, x1, y1, hy, s : Integer; surface_buffer : PUint32; fadeout : Integer);

Var
  sx, sy, i, a, b, u0, u1, v0, v1, h0, h1, h2, h3, h, c, y : Integer;
  coord_x, coord_y, sc, cc, currentColor : Integer;
  pixel : PUint32;

Begin
  { Compute xy speed }
  sx := (x1 - x0) Div SCREENWIDTH;
  sy := (y1 - y0) Div SCREENWIDTH;

  For i := 0 To SCREENWIDTH - 1 Do
  Begin
    { Compute the xy coordinates; a and b will be the position inside the }
    { single map cell (0..255). }
    a := (x0 Shr 8) And $FF;
    b := (y0 Shr 8) And $FF;

    u0 := (x0 Shr 16) And $FF;
    u1 := (u0 + 1) And $FF;
    v0 := (y0 Shr 8) And $FF00;
    v1 := (v0 + 256) And $FF00;

    { Fetch the height at the four corners of the square the point is in }
    h0 := HMap[u0 + v0];
    h1 := HMap[u1 + v0];
    h2 := HMap[u0 + v1];
    h3 := HMap[u1 + v1];

    { Compute the height using bilinear interpolation }
    h0 := (h0 Shl 8) + a * (h1 - h0);
    h2 := (h2 Shl 8) + a * (h3 - h2);
    h := ((h0 Shl 8) + b * (h2 - h0)) Shr 16;

    { Fetch the color at the centre of the square the point is in }
    h0 := CMap[u0 + v0];
    h1 := CMap[u1 + v0];
    h2 := CMap[u0 + v1];
    h3 := CMap[u1 + v1];

    { Compute the color using bilinear interpolation (in 16.16) }
    h0 := (h0 Shl 8) + a * (h1 - h0);
    h2 := (h2 Shl 8) + a * (h3 - h2);
    c := ((h0 Shl 8) + b * (h2 - h0));

    { Compute screen height using the scaling factor }
    y := (((h - hy) * s) Shr 11) + (SCREENHEIGHT Shr 1);

    { Draw the column }
    a := lasty[i];
    If y < a Then
    Begin
      coord_x := i;
      coord_y := a;
      If lastc[i] = -1 Then
	lastc[i] := c;

      sc := (c - lastc[i]) Div (a - y);
      cc := lastc[i];

      If a > (SCREENHEIGHT - 1) Then
      Begin
	Dec(coord_y, a - (SCREENHEIGHT - 1));
	a := SCREENHEIGHT - 1;
      End;
      If y < 0 Then
	y := 0;

      While y < a Do
      Begin
	currentColor := cc Shr 18;
	pixel := surface_buffer + (coord_y * SCREENWIDTH) + coord_x;
	pixel^ := ((currentColor Shl 2) * (150 - fadeout) Div 150) Shl 8;
	Inc(cc, sc);
	Dec(coord_y);
	Dec(a);
      End;
      lasty[i] := y;
    End;
    lastc[i] := c;

    { Advance to next xy position }
    Inc(x0, sx); Inc(y0, sy);
  End;
End;

{ Draw the view from the point x0,y0 (16.16) looking at angle a }
Procedure View(x0, y0, angle, height : Integer; surface_buffer : PUint32);

Var
  d, u0, a, v0, u1, v1, h0, h1, h2, h3 : Integer;

Begin
  { Initialize last-y and last-color arrays }
  For d := 0 To SCREENWIDTH - 1 Do
  Begin
    lasty[d] := SCREENHEIGHT;
    lastc[d] := -1;
  End;

  { Compute the xy coordinates; a and b will be the position inside the }
  { single map cell (0..255). }
  u0 := (x0 Shr 16) And $FF;
  a := (x0 Shr 8) And $FF;
  v0 := (y0 Shr 8) And $FF00;
  u1 := (u0 + 1) And $FF;
  v1 := (v0 + 256) And $FF00;

  { Fetch the height at the four corners of the square the point is in }
  h0 := HMap[u0 + v0];
  h1 := HMap[u1 + v0];
  h2 := HMap[u0 + v1];
  h3 := HMap[u1 + v1];

  { Compute the height using bilinear interpolation }
  h0 := (h0 Shl 8) + a * (h1 - h0);
  h2 := (h2 Shl 8) + a * (h3 - h2);

  { Draw the landscape from near to far without overdraw }
  d := 0;
  While d < 150 Do
  Begin
    Line(x0 + (d Shl 8)*CosT[(angle - FOV) And $7FF],
	 y0 + (d Shl 8)*SinT[(angle - FOV) And $7FF],
	 x0 + (d Shl 8)*CosT[(angle + FOV) And $7FF],
	 y0 + (d Shl 8)*SinT[(angle + FOV) And $7FF],
	 height, (100 Shl 8) Div (d + 1),
	 surface_buffer,
	 d);
    Inc(d, 1 + (d Shr 6));
  End;
End;

Var
  format : TPTCFormat;
  console : TPTCConsole;
  surface : TPTCSurface;
  timer : TPTCTimer;
  key : TPTCKeyEvent;
  pixels : PUint32;
  Done : Boolean;

  x0, y0 : Integer;
  height : Integer;
  angle, deltaAngle, deltaSpeed, CurrentSpeed, scale, delta : Double;
  index : Integer;

Begin
  Done := False;
  format := Nil;
  console := Nil;
  surface := Nil;
  timer := Nil;
  key := Nil;
  Try
    Try
      key := TPTCKeyEvent.Create;
      format := TPTCFormat.Create(32, $00FF0000, $0000FF00, $000000FF);
      console := TPTCConsole.Create;
      console.open('Land demo', SCREENWIDTH, SCREENHEIGHT, format);
      surface := TPTCSurface.Create(SCREENWIDTH, SCREENHEIGHT, format);

      { Compute the height map }
      ComputeMap;
      InitTables;

      x0 := 0;
      y0 := 0;

      height := -200;
      angle := 0;
      deltaAngle := 0;
      deltaSpeed := 4096;
      CurrentSpeed := deltaSpeed * 10;

      { time scaling constant }
      scale := 20;

      { create timer }
      timer := TPTCTimer.Create;

      { start timer }
      timer.start;

      { main loop }
      Repeat
        { get time delta between frames }
        delta := timer.delta;

        { clear surface }
        surface.clear;

        { lock surface pixels }
        pixels := surface.lock;
        Try
          { draw current landscape view }
          View(x0, y0, Trunc(angle), height, pixels);
	Finally
          { unlock surface }
          surface.unlock;
	End;

        { copy surface to console }
        surface.copy(console);

        { update console }
        console.update;

        { check key press }
        While console.KeyPressed Do
        Begin
          { read key press }
	  console.ReadKey(key);

          { handle key press }
	  Case key.code Of
                        { increase speed }
	    PTCKEY_UP : CurrentSpeed += deltaSpeed * delta * scale;
                        { decrease speed }
	    PTCKEY_DOWN : CurrentSpeed -= deltaSpeed * delta * scale;
                        { turn to the left }
	    PTCKEY_LEFT : deltaAngle -= 1;
                        { turn to the right }
	    PTCKEY_RIGHT : deltaAngle += 1;
	    PTCKEY_SPACE : Begin
              { stop moving }
	      CurrentSpeed := 0;
	      deltaAngle := 0;
	    End;
                           { exit }
	    PTCKEY_ESCAPE : Done := True;
	  End;
        End;

        { Update position/angle }
        angle += deltaAngle * delta * scale;

        index := Trunc(angle) And $7FF;
        Inc(x0, Trunc(CurrentSpeed * CosT[index]) Div 256);
        Inc(y0, Trunc(CurrentSpeed * SinT[index]) Div 256);
      Until Done;
    Finally
      console.close;
      console.Free;
      surface.Free;
      timer.Free;
      format.Free;
      key.Free;
    End;
  Except
    On error : TPTCError Do
      { report error }
      error.report;
  End;
End.
