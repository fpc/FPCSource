{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 Lights demo for OpenPTC 1.0 C++ API
 Copyright (c) Glenn Fiedler (ptc@gaffer.org)
 This source code is licensed under the GNU GPL
}

Program Lights;

{$MODE objfpc}

Uses
  ptc;

Var
  { distance lookup table }
  distance_table : Array[0..299, 0..511] Of DWord; { note: 16.16 fixed }

{ intensity calculation }
Function CalcIntensity(dx, dy : Integer; i : DWord) : DWord;{ Inline;}

Begin
  { lookup intensity at [dx,dy] }
  CalcIntensity := i * distance_table[dy, dx];
End;

Var
  console : TPTCConsole;
  surface : TPTCSurface;
  format : TPTCFormat;
  palette : TPTCPalette;
  dx, dy : Integer;
  divisor : Single;
  data : PUint32;
  pixels, line : PUint8;
  width : Integer;
  i : Integer;
  x, y, x1, y1, x2, y2, x3, y3, x4, y4 : Integer;
  cx1, cy1, cx2, cy2, cx3, cy3, cx4, cy4 : Single;
  dx1, dy1, dx2, dy2, dx3, dy3, dx4, dy4 : Single;
  _dx1, _dx2, _dx3, _dx4 : Integer;
  _dy1, _dy2, _dy3, _dy4 : Integer;
  ix1, ix2, ix3, ix4 : Integer;
  i1, i2, i3, i4 : DWord;
  length : Integer;
  move_t, move_dt, move_ddt : Single;
  flash_t, flash_dt, flash_ddt : Single;
  intensity : DWord;
  max_intensity, max_intensity_inc : Single;

Begin
  console := Nil;
  format := Nil;
  surface := Nil;
  palette := Nil;
  Try
    Try
      { create console }
      console := TPTCConsole.Create;

      format := TPTCFormat.Create(8);

      { open console }
      console.open('Lights demo', 320, 200, format);

      { create surface }
      surface := TPTCSurface.Create(320, 200, format);

      { setup intensity table }
      For dy := 0 To 199 Do
        For dx := 0 To 511 Do
        Begin
          divisor := sqrt((dx * dx) + (dy * dy));
          If divisor < 0.3 Then
            divisor := 0.3;
          distance_table[dy, dx] := Trunc(65535 / divisor);
        End;

      { create palette }
      palette := TPTCPalette.Create;

      { generate greyscale palette }
      data := palette.lock;
      Try
        For i := 0 To 255 Do
          data[i] := (i Shl 16) Or (i Shl 8) Or i;
      Finally
        palette.unlock;
      End;

      { set console palette }
      console.palette(palette);

      { set surface palette }
      surface.palette(palette);

      { data }
      cx1 := 60;
      cy1 := 110;
      cx2 := 100;
      cy2 := 80;
      cx3 := 250;
      cy3 := 110;
      cx4 := 200;
      cy4 := 90;
      dx1 := 0;
      dy1 := 0;
      dx2 := 0;
      dy2 := 0;
      dx3 := 0;
      dy3 := 0;
      dx4 := 0;
      dy4 := 0;
      i1 := 0;
      i2 := 0;
      i3 := 0;
      i4 := 0;

      { time data }
      move_t := 0.3;
      move_dt := 0.1;
      move_ddt := 0.0006;
      flash_t := 0.1;
      flash_dt := 0.0;
      flash_ddt := 0.0004;

      { control data }
      max_intensity := 30;
      max_intensity_inc := 0.2;

      { main loop }
      While Not console.KeyPressed Do
      Begin
        { source positions }
        x1 := Trunc(cx1 + dx1);
        y1 := Trunc(cy1 + dy1);
        x2 := Trunc(cx2 + dx2);
        y2 := Trunc(cy2 + dy2);
        x3 := Trunc(cx3 + dx3);
        y3 := Trunc(cy3 + dy3);
        x4 := Trunc(cx4 + dx4);
        y4 := Trunc(cy4 + dy4);

        { lock surface }
        pixels := surface.lock;
        Try
          { get surface dimensions }
          width := surface.width;

          { line loop }
          For y := 0 To 199 Do
          Begin
            { calcalate pointer to start of line }
            line := pixels + y * width;

            { get y deltas }
            _dy1 := abs(y - y1);
            _dy2 := abs(y - y2);
            _dy3 := abs(y - y3);
            _dy4 := abs(y - y4);

            { setup x }
            x := 0;

            { line loop }
            While x < width Do
            Begin
              { get x deltas }
              _dx1 := abs(x1 - x);
              _dx2 := abs(x2 - x);
              _dx3 := abs(x3 - x);
              _dx4 := abs(x4 - x);

              { get increments }
              ix1 := 1;
              ix2 := 1;
              ix3 := 1;
              ix4 := 1;
              If x1 > x Then
                ix1 := -1;
              If x2 > x Then
                ix2 := -1;
              If x3 > x Then
                ix3 := -1;
              If x4 > x Then
                ix4 := -1;

              { set span length to min delta }
              length := width - x;
              If (x1 > x) And (_dx1 < length) Then
                length := _dx1;
              If (x2 > x) And (_dx2 < length) Then
                length := _dx2;
              If (x3 > x) And (_dx3 < length) Then
                length := _dx3;
              If (x4 > x) And (_dx4 < length) Then
                length := _dx4;

              { pixel loop }
              While length > 0 Do
              Begin
                Dec(length);
                { calc intensities }
                intensity := CalcIntensity(_dx1, _dy1, i1);
                Inc(intensity, CalcIntensity(_dx2, _dy2, i2));
                Inc(intensity, CalcIntensity(_dx3, _dy3, i3));
                Inc(intensity, CalcIntensity(_dx4, _dy4, i4));
                intensity := intensity Shr 16;
                If intensity > 255 Then
                  intensity := 255;

                { update deltas }
                Inc(_dx1, ix1);
                Inc(_dx2, ix2);
                Inc(_dx3, ix3);
                Inc(_dx4, ix4);

                { store the pixel }
                line[x] := intensity;
                Inc(x);
              End;
            End;
          End;
	Finally
          { unlock surface }
          surface.unlock;
	End;

        { move the lights around }
        dx1 := 50  * sin((move_t + 0.0) * 0.10);
        dy1 := 80  * sin((move_t + 0.6) * 0.14);
        dx2 := 100 * sin((move_t + 0.1) * 0.10);
        dy2 := 30  * sin((move_t - 0.4) * 0.30);
        dx3 := 39  * sin((move_t + 9.9) * 0.20);
        dy3 := 50  * sin((move_t + 0.4) * 0.30);
        dx4 := 70  * sin((move_t - 0.3) * 0.25);
        dy4 := 40  * sin((move_t - 0.1) * 0.31);

        { flash intensity }
        i1 := Trunc(max_intensity * (sin((flash_t + 0.000) * 1.000) + 1));
        i2 := Trunc(max_intensity * (sin((flash_t + 2.199) * 0.781) + 1));
        i3 := Trunc(max_intensity * (sin((flash_t - 1.450) * 1.123) + 1));
        i4 := Trunc(max_intensity * (sin((flash_t + 0.000) * 0.500) + 1));

        { update time }
        move_t := move_t + move_dt;
        move_dt := move_dt + move_ddt;
        flash_t := flash_t + flash_dt;
        flash_dt := flash_dt + flash_ddt;

        { reset on big flash... }
        If (move_t > 600) And (i1 > 10000) And (i2 > 10000) And
           (i3 > 10000) And (i4 > 10000) Then
        Begin
          move_t := 0.3;
          move_dt := 0.1;
          move_ddt := 0.0006;
          flash_t := 0.1;
          flash_dt := 0.0;
          flash_ddt := 0.0004;
          max_intensity := 0.0;
          max_intensity_inc := 0.2;
        End;

        { update intensity }
        max_intensity := max_intensity + max_intensity_inc;
        max_intensity_inc := max_intensity_inc + 0.008;

        { copy surface to console }
        surface.copy(console);

        { update console }
        console.update;
      End;
    Finally
      console.close;
      surface.Free;
      console.Free;
      palette.Free;
      format.Free;
    End;
  Except
    On error : TPTCError Do
      { report error }
      error.report;
  End;
End.
