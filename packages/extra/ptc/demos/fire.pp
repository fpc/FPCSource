{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 Fire demo for OpenPTC 1.0 C++ API
 Copyright (c) Glenn Fiedler (ptc@gaffer.org)
 This source code is licensed under the GNU GPL
}

Program Fire;

{$MODE objfpc}

Uses
  ptc;

Function pack(r, g, b : Uint32) : Uint32;

Begin
  { pack color integer }
  pack := (r Shl 16) Or (g Shl 8) Or b;
End;

Procedure generate(palette : TPTCPalette);

Var
  data : PUint32;
  i, c : Integer;

Begin
  { lock palette data }
  data := palette.lock;

  Try
    { black to red }
    i := 0;
    c := 0;
    While i < 64 Do
    Begin
      data[i] := pack(c, 0, 0);
      Inc(c, 4);
      Inc(i);
    End;

    { red to yellow }
    c := 0;
    While i < 128 Do
    Begin
      data[i] := pack(255, c, 0);
      Inc(c, 4);
      Inc(i);
    End;

    { yellow to white }
    c := 0;
    While i < {192}128 Do
    Begin
      data[i] := pack(255, 255, c);
      Inc(c, 4);
      Inc(i);
    End;

    { white }
    While i < 256 Do
    Begin
      data[i] := pack(255, 255, 255);
      Inc(i);
    End;

  Finally
    { unlock palette }
    palette.unlock;
  End;
End;

Var
  format : TPTCFormat;
  console : TPTCConsole;
  surface : TPTCSurface;
  palette : TPTCPalette;
  state : Integer;
  intensity : Single;
  pixels, pixel, p : PUint8;
  width, height : Integer;
  x, y : Integer;
  top, bottom, c1, c2 : Uint32;
  generator : PUint8;
  color : Integer;
  area : TPTCArea;

Begin
  format := Nil;
  console := Nil;
  surface := Nil;
  palette := Nil;
  area := Nil;
  Try
    Try
      { create format }
      format := TPTCFormat.Create(8);

      { create console }
      console := TPTCConsole.Create;

      { open console }
      console.open('Fire demo', 320, 200, format);

      { create surface }
      surface := TPTCSurface.Create(320, 208, format);

      { create palette }
      palette := TPTCPalette.Create;

      { generate palette }
      generate(palette);

      { set console palette }
      console.palette(palette);

      { set surface palette }
      surface.palette(palette);

      { flame data }
      state := 0;
      intensity := 0;

      { setup copy area }
      area := TPTCArea.Create(0, 0, 320, 200);

      { main loop }
      Repeat
        { lower flame on keypress }
        If console.KeyPressed Then
          state := 2;

        { state machine }
        Case state Of
          0 : Begin
            { raise flame }
            intensity += 0.007;

            { maximum flame height }
            If intensity > 0.8 Then
              state := 1;
          End;
          1 : Begin
            { constant flame }
          End;
          2 : Begin
            { lower flame }
            intensity := intensity - 0.005;

            { exit program when flame is out }
            If intensity < 0.01 Then
            Begin
              console.close;
	      Exit;
            End;
          End;
        End;

        { lock surface pixels }
        pixels := surface.lock;
	
	Try
          { get surface dimensions }
          width := surface.width;
          height := surface.height;

          { flame vertical loop }
          y := 1;
          While y < height - 4 Do
          Begin
            { current pixel pointer }
            pixel := pixels + y * width;

            { flame horizontal loop }
            For x := 0 To width - 1 Do
            Begin
              { sum top pixels }
              p := pixel + (width Shl 1);
              top := p^;
              Inc(top, (p - 1)^);
              Inc(top, (p + 1)^);

              { bottom pixel }
              bottom := (pixel + (width Shl 2))^;

              { combine pixels }
              c1 := (top + bottom) Shr 2;
              If c1 > 1 Then
                Dec(c1);

              { interpolate }
              c2 := (c1 + bottom) Shr 1;

              { store pixels }
              pixel^ := c1;
              (pixel + width)^ := c2;

              { next pixel }
              Inc(pixel);
            End;
            Inc(y, 2);
          End;

          { setup flame generator pointer }
          generator := pixels + width * (height - 4);

          { update flame generator bar }
          x := 0;
          While x < width Do
          Begin
            { random block color taking intensity into account }
            color := random(Integer(Trunc(255 * intensity)));

            { write 4x4 color blocks }
            (generator + 0)^             := color;
            (generator + 1)^             := color;
            (generator + 2)^             := color;
            (generator + 3)^             := color;
            (generator + width + 0)^     := color;
            (generator + width + 1)^     := color;
            (generator + width + 2)^     := color;
            (generator + width + 3)^     := color;
            (generator + width * 2 + 0)^ := color;
            (generator + width * 2 + 1)^ := color;
            (generator + width * 2 + 2)^ := color;
            (generator + width * 2 + 3)^ := color;
            (generator + width * 3 + 0)^ := color;
            (generator + width * 3 + 1)^ := color;
            (generator + width * 3 + 2)^ := color;
            (generator + width * 3 + 3)^ := color;

            { next block }
            Inc(generator, 4);
            Inc(x, 4);
          End;

        Finally
          { unlock surface }
          surface.unlock;
	End;

        { copy surface to console }
        surface.copy(console, area, area);

        { update console }
        console.update;
      Until False;
      
    Finally
      console.Free;
      surface.Free;
      format.Free;
      palette.Free;
      area.Free;
    End;
  Except
    On error : TPTCError Do
      { report error }
      error.report;
  End;
End.
