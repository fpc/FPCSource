{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
  Texture warp demo for OpenPTC 1.0 C++ API
  Copyright (c) 1998 Jonathan Matthew
  This source code is licensed under the GNU GPL
}

Program TexWarp;

{$MODE objfpc}

Uses
  ptc;

Const
{ colour balance values.  change these if you don't like the colouring }
{ of the texture. }
  red_balance : Uint32 = 2;
  green_balance : Uint32 = 3;
  blue_balance : Uint32 = 1;

Procedure blur(s : TPTCSurface);

Var
  d : PUint8;
  pitch : Integer;
  spack, r : Integer;

Begin
  { lock surface }
  d := s.lock;
  
  Try
    pitch := s.pitch;
    spack := (s.height - 1) * pitch;

    { first pixel }
    For r := 0 To 3 Do
      d[r] := (d[pitch + r] + d[r + 4] + d[spack + r] + d[pitch - 4 + r]) Div 4;

    { rest of first line }
    For r := 4 To pitch - 1 Do
      d[r] := (d[r + pitch] + d[r + 4] + d[r - 4] + d[spack + r]) Div 4;

    { rest of surface except last line }
    For r := pitch To ((s.height - 1) * pitch) - 1 Do
      d[r] := (d[r - pitch] + d[r + pitch] + d[r + 4] + d[r - 4]) Div 4;

    { last line except last pixel }
    For r := (s.height - 1) * pitch To (s.height * s.pitch) - 5 Do
      d[r] := (d[r - pitch] + d[r + 4] + d[r - 4] + d[r - spack]) Div 4;

    { last pixel }
    For r := (s.height * s.pitch) - 4 To s.height * s.pitch Do
      d[r] := (d[r - pitch] + d[r - 4] + d[r - spack] + d[r + 4 - pitch]) Div 4;

  Finally
    s.unlock;
  End;
End;

Procedure generate(surface : TPTCSurface);

Var
  dest : PUint32;
  i : Integer;
  x, y : Integer;
  d : PUint32;
  cv : Uint32;
  r, g, b : Uint8;

Begin
  { draw random dots all over the surface }
  dest := surface.lock;
  Try
    For i := 0 To surface.width * surface.height - 1 Do
    Begin
      x := Random(surface.width);
      y := Random(surface.height);
      d := dest + (y * surface.width) + x;
      cv := (Random(100) Shl 16) Or (Random(100) Shl 8) Or Random(100);
      d^ := cv;
    End;
  Finally
    surface.unlock;
  End;
  
  { blur the surface }
  For i := 1 To 5 Do
    blur(surface);
  
  { multiply the color values }
  dest := surface.lock;
  Try
    For i := 0 To surface.width * surface.height - 1 Do
    Begin
      cv := dest^;
      r := (cv Shr 16) And 255;
      g := (cv Shr 8) And 255;
      b := cv And 255;
      r *= red_balance;
      g *= green_balance;
      b *= blue_balance;
      If r > 255 Then
        r := 255;
      If g > 255 Then
        g := 255;
      If b > 255 Then
        b := 255;
      dest^ := (r Shl 16) Or (g Shl 8) Or b;
      Inc(dest);
    End;
  Finally
    surface.unlock;
  End;
End;

Procedure grid_map(grid : PUint32; xbase, ybase, xmove, ymove, amp : Single);

Var
  x, y : Integer;
  a, b, id : Single;

Begin
  a := 0;
  For y := 0 To 25 Do
  Begin
    b := 0;
    For x := 0 To 40 Do
    Begin
      { it should be noted that there is no scientific basis for }
      { the following three lines :) }
      grid[0] := Uint32(Trunc((xbase * 14 + x*4 + xmove*sin(b)+sin(cos(a)*sin(amp))*15) * 65536));
      grid[1] := Uint32(Trunc((ybase * 31 + y*3 + ymove*cos(b)*sin(sin(a)*cos(amp))*30) * 65536));
      id := (cos(xbase) + sin(ybase) + cos(a*xmove*0.17) + sin(b*ymove*0.11)) * amp * 23;
      If id < -127 Then
        grid[2] := 0
      Else
        If id > 127 Then
	  grid[2] := 255 Shl 16
	Else
	  grid[2] := (128 Shl 16) + Trunc(id * 65536.0);
      grid += 3;
      b += pi / 30;
    End;
    a += pi / 34;
  End;
End;

Procedure make_light_table(lighttable : PUint8);

Var
  i, j : Integer;
  tv : Integer;

Begin
  For i := 0 To 255 Do
    For j := 0 To 255 Do
    Begin
      { light table goes from 0 to i*2. }
      tv := (i * j) Div 128;
      If tv > 255 Then
        tv := 255;
      lighttable[(j * 256) + i] := tv;
    End;
End;

{ if you want to see how to do this properly, look at the tunnel3d demo. }
{ (not included in this distribution :) }
Procedure texture_warp(dest, grid, texture : PUint32; lighttable : PUint8);

Var
  utl, utr, ubl, ubr : Integer;
  vtl, vtr, vbl, vbr : Integer;
  itl, itr, ibl, ibr : Integer;
  dudx, dvdx, didx, dudy, dvdy, didy, ddudy, ddvdy, ddidy : Integer;
  dudx2, dvdx2, didx2 : Integer;
  bx, by, px, py : Integer;
  uc, vc, ic, ucx, vcx, icx : Integer;
  
  edi : Uint32;
  texel : Uint32;
  
  cbp, dp : PUint32;
  dpix : Uint32;
  
  ltp : PUint8;

Begin
  cbp := grid;
  For by := 0 To 24 Do
  Begin
    For bx := 0 To 39 Do
    Begin
      utl := Integer(cbp^);
      vtl := Integer((cbp + 1)^);
      itl := Integer((cbp + 2)^);
      utr := Integer((cbp + (1 * 3))^);
      vtr := Integer((cbp + (1 * 3) + 1)^);
      itr := Integer((cbp + (1 * 3) + 2)^);
      ubl := Integer((cbp + (41 * 3))^);
      vbl := Integer((cbp + (41 * 3) + 1)^);
      ibl := Integer((cbp + (41 * 3) + 2)^);
      ubr := Integer((cbp + (42 * 3))^);
      vbr := Integer((cbp + (42 * 3) + 1)^);
      ibr := Integer((cbp + (42 * 3) + 2)^);
      dudx := (utr - utl) Div 8;
      dvdx := (vtr - vtl) Div 8;
      didx := (itr - itl) Div 8;
      dudx2 := (ubr - ubl) Div 8;
      dvdx2 := (vbr - vbl) Div 8;
      didx2 := (ibr - ibl) Div 8;
      dudy := (ubl - utl) Div 8;
      dvdy := (vbl - vtl) Div 8;
      didy := (ibl - itl) Div 8;
      ddudy := (dudx2 - dudx) Div 8;
      ddvdy := (dvdx2 - dvdx) Div 8;
      ddidy := (didx2 - didx) Div 8;
      uc := utl;
      vc := vtl;
      ic := itl;
      For py := 0 To 7 Do
      Begin
        ucx := uc;
	vcx := vc;
	icx := ic;
	dp := dest + (((by * 8 + py)*320) + (bx * 8));
	For px := 0 To 7 Do
	Begin

          { get light table pointer for current intensity }
	  ltp := lighttable + ((icx And $FF0000) Shr 8);

          { get texel }
	  edi := ((ucx And $FF0000) Shr 16) + ((vcx And $FF0000) Shr 8);
	  texel := texture[edi];
	  
          { calculate actual colour }
	  dpix := ltp[(texel Shr 16) And 255];
	  dpix := dpix Shl 8;
	  dpix := dpix Or ltp[(texel Shr 8) And 255];
	  dpix := dpix Shl 8;
	  dpix := dpix Or ltp[texel And 255];
	  
	  dp^ := dpix;
	  Inc(dp);
	  
	  ucx += dudx;
	  vcx += dvdx;
	  icx += didx;
	End;
	uc += dudy;
	vc += dvdy;
	ic += didy;
	dudx += ddudy;
	dvdx += ddvdy;
	didx += ddidy;
      End;
      cbp += 3;
    End;
    cbp += 3;
  End;
End;

Var
  format : TPTCFormat;
  texture : TPTCSurface;
  surface : TPTCSurface;
  console : TPTCConsole;
  lighttable : PUint8;
  { texture grid }
  grid : Array[0..41*26*3-1] Of Uint32;
  xbase, ybase, xmove, ymove, amp, dct, dxb, dyb, dxm, dym, sa : Single;
  
  p1, p2 : PUint32;

Begin
  format := Nil;
  texture := Nil;
  surface := Nil;
  console := Nil;
  lighttable := Nil;
  Try
    Try
      { create format }
      format := TPTCFormat.Create(32, $00FF0000, $0000FF00, $000000FF);
    
      { create texture surface }
      texture := TPTCSurface.Create(256, 256, format);

      { create texture }
      generate(texture);

      { create lighttable }
      lighttable := GetMem(256 * 256);
      make_light_table(lighttable);

      { create console }
      console := TPTCConsole.Create;

      { open console }
      console.open('Warp demo', 320, 200, format);

      { create drawing surface }
      surface := TPTCSurface.Create(320, 200, format);

      { control values }
      xbase := 0;
      ybase := 0;
      xmove := 0;
      ymove := 0;
      amp := 0;
      dct := 0.024;
      dxb := 0.031;
      dyb := -0.019;
      dxm := 0.015;
      dym := -0.0083;
    
      { main loop }
      While Not console.KeyPressed Do
      Begin
    
        { create texture mapping grid }
        grid_map(grid, xbase, ybase, xmove, ymove*3, amp);

        p1 := surface.lock;
	Try
	  p2 := texture.lock;
	  Try
            { map texture to drawing surface }
            texture_warp(p1, grid, p2, lighttable);
	  Finally
            texture.unlock;
	  End;
	Finally
          surface.unlock;
	End;

        { copy surface to console }
        surface.copy(console);

        { update console }
        console.update;
      
        { move control values (limit them so it doesn't go too far) }
        xbase += dxb;
        If xbase > pi Then
          dxb := -dxb;
        If xbase < (-pi) Then
          dxb := -dxb;
      
        ybase += dyb;
        If ybase > pi Then
          dyb := -dyb;
        If ybase < (-pi) Then
          dyb := -dyb;
      
        xmove += dxm;
        If xmove > pi Then
          dxm := -dxm;
        If xmove < (-pi) Then
          dxm := -dxm;
      
        ymove += dym;
        If ymove > pi Then
          dym := -dym;
        If ymove < (-pi) Then
          dym := -dym;
      
        amp += dct;
        sa := sin(amp);
        If (sa > -0.0001) And (sa < 0.0001) Then
        Begin
          If amp > 8.457547 Then
	    dct := -dct;
	  If amp < -5.365735 Then
	    dct := -dct;
        End;
      End;
    Finally
      console.close;
      console.Free;
      surface.Free;
      texture.Free;
      format.Free;
      If assigned(lighttable) Then
        FreeMem(lighttable);
    End;
  Except
    On e : TPTCError Do
      e.report;
  End;
End.
