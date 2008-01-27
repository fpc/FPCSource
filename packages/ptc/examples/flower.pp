{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 Flower demo for OpenPTC 1.0 C++ API
 Copyright (c) Scott Buchanan (aka Goblin)
 This source code is licensed under the GNU GPL
}

Program Flower;

{$MODE objfpc}

Uses
  ptc, Math;

Function pack(r, g, b : Uint32) : Uint32;

Begin
  { pack color integer }
  pack := (r Shl 16) Or (g Shl 8) Or b;
End;

Procedure generate_flower(flower : TPTCSurface);

Var
  data : PUint8;
  x, y, fx, fy, fx2, fy2 : Integer;
  TWO_PI : Single;

Begin
  { lock surface }
  data := flower.lock;
  
  Try
    { surface width and height constants for cleaner code }
    fx := flower.width;
    fy := flower.height;
    fx2 := fx Div 2;
    fy2 := fy Div 2;

    { useful 2*pi constant }
    TWO_PI := 2 * PI;

    { generate flower image }
    For y := 0 To fy - 1 Do
      For x := 0 To fx - 1 Do
        data[x + y * fx] := Trunc(1.0 * Cos(18*ArcTan2((y - fy2),(x - fx2))) * 255 / TWO_PI +
		                  0.3 * Sin(15*ArcTan2((y - fy2),(x - fx2))) * 255 / TWO_PI +
                                  Sqrt((y - fy2) * (y - fy2) + (x - fx2) * (x - fx2))) And $FF;

    { You might want to move the 1.0 and 0.3 and the 18 and the 15
      to parameters passed to the generate function...
      the 1.0 and the 0.3 define the 'height' of the flower, while the
      18 and 15 control the number of 'petals' }
  Finally
    flower.unlock;
  End;
End;

Procedure generate(palette : TPTCPalette);

Var
  data : PUint32;
  i, c : Integer;

Begin
  { lock palette data }
  data := palette.lock;
  
  Try
    { black to yellow }
    i := 0;
    c := 0;
    While i < 64 Do
    Begin
      data[i] := pack(c, c, 0);
      Inc(c, 4);
      Inc(i);
    End;

    { yellow to red }
    c := 0;
    While i < 128 Do
    Begin
      data[i] := pack(255, 255 - c, 0);
      Inc(c, 4);
      Inc(i);
    End;

    { red to white }
    c := 0;
    While i < 192 Do
    Begin
      data[i] := pack(255, c, c);
      Inc(c, 4);
      Inc(i);
    End;

    { white to black }
    c := 0;
    While i < 256 Do
    Begin
      data[i] := pack(255 - c, 255 - c, 255 - c);
      Inc(c, 4);
      Inc(i);
    End;
  Finally
    { unlock palette }
    palette.unlock;
  End;
End;

Var
  console : TPTCConsole;
  format : TPTCFormat;
  flower_surface : TPTCSurface;
  surface : TPTCSurface;
  palette : TPTCPalette;
  area : TPTCArea;
  time, delta : Single;
  scr, map : PUint8;
  width, height, mapWidth : Integer;
  xo, yo, xo2, yo2, xo3, yo3 : Single;
  offset1, offset2, offset3 : Integer;
  x, y : Integer;

Begin
  area := Nil;
  format := Nil;
  palette := Nil;
  surface := Nil;
  flower_surface := Nil;
  console := Nil;
  Try
    Try
      { create format }
      format := TPTCFormat.Create(8);

      { create console }
      console := TPTCConsole.Create;

      { create flower surface }
      flower_surface := TPTCSurface.Create(640, 400, format);

      { generate flower }
      generate_flower(flower_surface);

      { open console }
      console.open('Flower demo', 320, 200, format);

      { create surface }
      surface := TPTCSurface.Create(320, 200, format);

      { create palette }
      palette := TPTCPalette.Create;

      { generate palette }
      generate(palette);

      { set console palette }
      console.palette(palette);

      { set surface palette }
      surface.palette(palette);

      { setup copy area }
      area := TPTCArea.Create(0, 0, 320, 200);

      { time data }
      time := 0;
      delta := 0.04;

      { main loop }
      While Not console.KeyPressed Do
      Begin
        { lock surface pixels }
        scr := surface.lock;
	Try
          map := flower_surface.lock;
	  Try
            { get surface dimensions }
            width := surface.width;
            height := surface.height;
            mapWidth := flower_surface.width;

            xo := (width / 2) + 120 * sin(time * 1.1 + 1.5);
            yo := (height / 2) + 90 * cos(time * 0.8 + 1.1);
            offset1 := Trunc(xo) + Trunc(yo) * mapWidth;

            xo2 := (width / 2) + 120 * sin(time * 0.9 + 4.2);
            yo2 := (height / 2) + 90 * cos(time * 0.7 + 6.9);
            offset2 := Trunc(xo2) + Trunc(yo2) * mapWidth;

            xo3 := (width / 2) + 120 * sin(time * 0.9 + 3.1);
            yo3 := (height / 2) + 90 * cos(time * 1.1 + 1.2);
            offset3 := Trunc(xo3) + Trunc(yo3) * mapWidth;

            { vertical loop }
            For y := 0 To height - 1 Do
              { horizontal loop }
	      For x := 0 To width - 1 Do
	        scr[x + y * width] := (map[x + y * mapWidth + offset1] +
				       map[x + y * mapWidth + offset2] +
				       map[x + y * mapWidth + offset3]) And $FF;
	  Finally
            { unlock surface }
            flower_surface.unlock;
	  End;
	Finally
          { unlock surface }
          surface.unlock;
	End;

        { copy surface to console }
        surface.copy(console, area, area);

        { update console }
        console.update;

        { update time }
        time := time + delta;
      End;
    Finally
      If Assigned(console) Then
        console.close;
      area.Free;
      format.Free;
      palette.Free;
      surface.Free;
      flower_surface.Free;
      console.Free;
    End;
  Except
    On error : TPTCError Do
      { report error }
      error.report;
  End;
End.
