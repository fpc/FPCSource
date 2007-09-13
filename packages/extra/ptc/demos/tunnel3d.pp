{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 Tunnel3D demo for OpenPTC 1.0 C++ API

 Realtime raytraced tunnel
 Copyright (c) 1998 Christian Nentwich (brn@eleet.mcb.at)
 This source code is licensed under the GNU LGPL

 And do not just blatantly cut&paste this into your demo :)
}

Program Tunnel3D;

{$MODE objfpc}

Uses
  ptc, Math;

Type
  PVector = ^TVector;
  TVector = Array[0..2] Of Single;      { X,Y,Z }
  TMatrix = Array[0..3, 0..3] Of Single;{ FIRST  = COLUMN
                                          SECOND = ROW

                                          [0, 0]  [1, 0]  [2, 0]
                                          [0, 1]  [1, 1]  [2, 1]
                                          [0, 2]  [1, 2]  [2, 2]
  (I know the matrices are the wrong way round, so what, the code is quite
  old :) }

  TRayTunnel = Class(TObject)
  Private
    tunneltex : PUint8;                      { Texture }
    pal : PUint8;                            { Original palette }
    lookup : PUint32;                         { Lookup table for lighting }

    sintab, costab : PSingle;                { Take a guess }

    u_array, v_array, l_array : PInteger;    { Raytraced coordinates and light }
    norms : PVector;

    radius, radius_sqr : Single;
    rot : TMatrix;

    pos, light : TVector;                    { Position in the tunnel, pos of }
    xa, ya, za : Integer;                    { lightsource, angles }

    lightstatus : Boolean;                   { Following the viewer ? }

  Public
    Constructor Create(rad : Single);        { Constructor takes the radius }
    Destructor Destroy; Override;

    Procedure load_texture;

    Procedure tilt(x, y, z : Integer);              { Rotate relative }
    Procedure tilt(x, y, z : Integer; abs : Uint8); { Absolute }

    Procedure move(dx, dy, dz : Single);            { Relative move }
    Procedure move(x, y, z : Single; abs : Uint8);  { Absolute }

    Procedure movelight(dx, dy, dz : Single);
    Procedure movelight(x, y, z : Single; abs : Uint8);

    Procedure locklight(lock : Boolean);    { Make the light follow the viewer }

    Procedure interpolate;                  { Raytracing }

    Procedure draw(dest : PUint32);          { Draw the finished tunnel }
  End;

{ VECTOR ROUTINES }
Procedure vector_normalize(Var v : TVector);

Var
  length : Single;

Begin
  length := v[0] * v[0] + v[1] * v[1] + v[2] * v[2];
  length := sqrt(length);
  If length <> 0 Then
  Begin
    v[0] := v[0] / length;
    v[1] := v[1] / length;
    v[2] := v[2] / length;
  End
  Else
  Begin
    v[0] := 0;
    v[1] := 0;
    v[2] := 0;
  End;
End;

Procedure vector_times_matrix(Const v : TVector; Const m : TMatrix;
                              Var res : TVector);

Var
  i, j : Integer;

Begin
  For j := 0 To 2 Do
  Begin
    res[j] := 0;
    For i := 0 To 2 Do
      res[j] := res[j] + (m[j, i] * v[i]);
  End;
End;

Procedure matrix_idle(Var m : TMatrix);

Begin
  FillChar(m, SizeOf(TMatrix), 0);
  m[0, 0] := 1;
  m[1, 1] := 1;
  m[2, 2] := 1;
  m[3, 3] := 1;
End;

Procedure matrix_times_matrix(Const m1, m2 : TMatrix; Var res : TMatrix);

Var
  i, j, k : Integer;

Begin
  For j := 0 To 3 Do
    For i := 0 To 3 Do
    Begin
      res[i, j] := 0;
      For k := 0 To 3 Do
        res[i, j] := res[i, j] + (m1[k, j] * m2[i, k]);
    End;
End;

Procedure matrix_rotate_x(Var m : TMatrix; angle : Integer; sintab, costab : PSingle);

Var
  tmp, tmp2 : TMatrix;

Begin
  matrix_idle(tmp);
  tmp[1, 1] := costab[angle];
  tmp[2, 1] := sintab[angle];
  tmp[1, 2] := -sintab[angle];
  tmp[2, 2] := costab[angle];
  matrix_times_matrix(tmp, m, tmp2);
  Move(tmp2, m, SizeOf(TMatrix));
End;

Procedure matrix_rotate_y(Var m : TMatrix; angle : Integer; sintab, costab : PSingle);

Var
  tmp, tmp2 : TMatrix;

Begin
  matrix_idle(tmp);
  tmp[0, 0] := costab[angle];
  tmp[2, 0] := -sintab[angle];
  tmp[0, 2] := sintab[angle];
  tmp[2, 2] := costab[angle];
  matrix_times_matrix(tmp, m, tmp2);
  Move(tmp2, m, SizeOf(TMatrix));
End;

Procedure matrix_rotate_z(Var m : TMatrix; angle : Integer; sintab, costab : PSingle);

Var
  tmp, tmp2 : TMatrix;

Begin
  matrix_idle(tmp);
  tmp[0, 0] := costab[angle];
  tmp[1, 0] := sintab[angle];
  tmp[0, 1] := -sintab[angle];
  tmp[1, 1] := costab[angle];
  matrix_times_matrix(tmp, m, tmp2);
  Move(tmp2, m, SizeOf(TMatrix));
End;

Constructor TRayTunnel.Create(rad : Single);

Var
  x, y : Single;
  i, j : Integer;
  tmp : TVector;

Begin
  tunneltex := Nil;
  sintab := Nil;
  costab := Nil;
  u_array := Nil;
  v_array := Nil;
  norms := Nil;
  lookup := Nil;
  pal := Nil;

  radius := rad;
  radius_sqr := rad * rad;

  sintab := GetMem(1024 * SizeOf(Single)); { Set trigonometry and lookups }
  costab := GetMem(1024 * SizeOf(Single));
  u_array := GetMem(64 * 26 * SizeOf(Integer));
  v_array := GetMem(64 * 26 * SizeOf(Integer));
  l_array := GetMem(64 * 26 * SizeOf(Integer));
  norms := GetMem(64 * 26 * 3 * SizeOf(Single));

  lookup := GetMem(65 * 256 * SizeOf(Uint32));
  pal := GetMem(768 * SizeOf(Uint8));

  For i := 0 To 1023 Do
  Begin
    sintab[i] := sin(i * pi / 512);
    costab[i] := cos(i * pi / 512);
  End;

  { Generate normal vectors }
  y := -100;
  For j := 0 To 25 Do
  Begin
    x := -160;
    For i := 0 To 40 Do
    Begin
      tmp[0] := x;
      tmp[1] := y;
      tmp[2] := 128;
      vector_normalize(tmp);
      norms[j * 64 + i] := tmp;
      x := x + 8;
    End;
    y := y + 8;
  End;

  { Reset tunnel and light position and all angles }
  pos[0] := 0; pos[1] := 0; pos[2] := 0;
  light[0] := 1; light[1] := 1; light[2] := 0;

  xa := 0; ya := 0; za := 0;

  lightstatus := False;

  { Normalize light vector to length 1.0 }
  vector_normalize(light);
End;

Destructor TRayTunnel.Destroy;

Begin
  If Assigned(tunneltex) Then
    FreeMem(tunneltex);
  If Assigned(pal) Then
    FreeMem(pal);
  If Assigned(lookup) Then
    FreeMem(lookup);
  If Assigned(norms) Then
    FreeMem(norms);
  If Assigned(l_array) Then
    FreeMem(l_array);
  If Assigned(v_array) Then
    FreeMem(v_array);
  If Assigned(u_array) Then
    FreeMem(u_array);
  If Assigned(costab) Then
    FreeMem(costab);
  If Assigned(sintab) Then
    FreeMem(sintab);
End;

Procedure TRayTunnel.load_texture;

Var
  texfile : File;
  tmp : PUint8;
  i, j : Uint32;
  r, g, b : Uint32;
  newoffs : Integer;

Begin
  { Allocate tunnel texture 65536+33 bytes too big }

  If tunneltex <> Nil Then
  Begin
    FreeMem(tunneltex);
    tunneltex := Nil;
  End;
  tunneltex := GetMem(2*65536 + 33);
  tmp := GetMem(65536);

  { Align the texture on a 64k boundary }
  While (PtrUInt(tunneltex) And $FFFF) <> 0 Do
    Inc(tunneltex);

  ASSign(texfile, 'tunnel3d.raw');
  Reset(texfile, 1);
  BlockRead(texfile, pal^, 768);
  BlockRead(texfile, tmp^, 65536);
  Close(texfile);

  { Generate lookup table for lighting (65 because of possible inaccuracies) }

  For j := 0 To 64 Do
    For i := 0 To 255 Do
    Begin
      r := pal[i * 3] Shl 2;
      g := pal[i * 3 + 1] Shl 2;
      b := pal[i * 3 + 2] Shl 2;
      r := (r * j) Shr 6;
      g := (g * j) Shr 6;
      b := (b * j) Shr 6;
      If r > 255 Then
        r := 255;
      If g > 255 Then
        g := 255;
      If b > 255 Then
        b := 255;
      lookup[j * 256 + i] := (r Shl 16) Or (g Shl 8) Or b;
    End;

  { Arrange texture for cache optimised mapping }

  For j := 0 To 255 Do
    For i := 0 To 255 Do
    Begin
      newoffs := ((i Shl 8) And $F800) + (i And $0007) + ((j Shl 3) And $7F8);
      (tunneltex + newoffs)^ := (tmp + j * 256 + i)^;
    End;

  FreeMem(tmp);
End;

Procedure TRayTunnel.interpolate;

Var
  ray, intsc, norm, lvec : TVector;
  x, y, a, b, c, discr, t, res : Single;
  i, j : Integer;

Begin
  If lightstatus Then { Lightsource locked to viewpoint }
    light := pos;

  matrix_idle(rot);
  matrix_rotate_x(rot, xa And $3FF, sintab, costab);
  matrix_rotate_y(rot, ya And $3FF, sintab, costab);
  matrix_rotate_z(rot, za And $3FF, sintab, costab);

  { Constant factor }
  c := 2 * (pos[0] * pos[0] + pos[1] * pos[1] - radius_sqr);

  { Start raytracing }
  y := -100;
  For j := 0 To 25 Do
  Begin
    x := -160;
    For i := 0 To 40 Do
    Begin
      vector_times_matrix(norms[(j Shl 6) + i], rot, ray);

      a := 2 * (ray[0] * ray[0] + ray[1] * ray[1]);
      b := 2 * (pos[0] * ray[0] + pos[1] * ray[1]);

      discr := b * b - a * c;
      If discr > 0 Then
      Begin
        discr := sqrt(discr);
        t := (- b + discr) / a;

        { Calculate intersection point }
        intsc[0] := pos[0] + t * ray[0];
        intsc[1] := pos[1] + t * ray[1];
        intsc[2] := pos[2] + t * ray[2];

        { Calculate texture index at intersection point (cylindrical mapping) }
        { Try and adjust the 0.2 to stretch/shrink the texture }
        u_array[(j Shl 6) + i] := Trunc(intsc[2] * 0.2) Shl 16;
        v_array[(j Shl 6) + i] := Trunc(abs(arctan2(intsc[1], intsc[0]) * 256 / pi)) Shl 16;

        { Calculate the dotproduct between the normal vector and the vector }
        { from the intersection point to the lightsource }
        norm[0] := intsc[0] / radius;
        norm[1] := intsc[1] / radius;
        norm[2] := 0;

        lvec[0] := intsc[0] - light[0];
        lvec[1] := intsc[1] - light[1];
        lvec[2] := intsc[2] - light[2];
        vector_normalize(lvec);

        res := lvec[0] * norm[0] + lvec[1] * norm[1] + lvec[2] * norm[2];

        { Scale the light a bit }
        res *= res;
        If res < 0 Then
          res := 0;
        If res > 1 Then
          res := 1;
        res *= 63;

        { Put it into the light array }
        l_array[(j Shl 6) + i] := Trunc(res) Shl 16;
      End
      Else
      Begin
        u_array[(j Shl 6) + i] := 0;
        v_array[(j Shl 6) + i] := 0;
        l_array[(j Shl 6) + i] := 0;
      End;
      x := x + 8;
    End;
    y := y + 8;
  End;
End;

Procedure TRayTunnel.draw(dest : PUint32);

Var
  x, y, lu, lv, ru, rv, liu, liv, riu, riv : Integer;
  iu, iv, i, j, ll, rl, lil, ril, l, il : Integer;
  iadr, adr, til_u, til_v, til_iu, til_iv : DWord;
  bla : Uint8;

Begin
  For j := 0 To 24 Do
    For i := 0 To 39 Do
    Begin
      iadr := (j Shl 6) + i;

      { Set up gradients }
      lu := u_array[iadr]; ru := u_array[iadr + 1];
      liu := (u_array[iadr + 64] - lu) Shr 3;
      riu := (u_array[iadr + 65] - ru) Shr 3;

      lv := v_array[iadr]; rv := v_array[iadr + 1];
      liv := (v_array[iadr + 64] - lv) Shr 3;
      riv := (v_array[iadr + 65] - rv) Shr 3;

      ll := l_array[iadr]; rl := l_array[iadr + 1];
      lil := (l_array[iadr + 64] - ll) Shr 3;
      ril := (l_array[iadr + 65] - rl) Shr 3;

      For y := 0 To 7 Do
      Begin
        iu := (ru - lu) Shr 3;
        iv := (rv - lv) Shr 3;
        l := ll;
        il := (rl - ll) Shr 3;

        { Mess up everything for the sake of cache optimised mapping :) }
        til_u := DWord(((lu Shl 8) And $F8000000) Or ((lu Shr 1) And $00007FFF) Or (lu And $00070000));
        til_v := DWord(((lv Shl 3) And $07F80000) Or ((lv Shr 1) And $00007FFF));
        til_iu := DWord((((iu Shl 8) And $F8000000) Or ((iu Shr 1) And $00007FFF) Or
                          (iu And $00070000)) Or $07F88000);
        til_iv := DWord((((iv Shl 3) And $07F80000) Or ((iv Shr 1) And $00007FFF)) Or $F8078000);

        adr := til_u + til_v;

        For x := 0 To 7 Do
        Begin
          { Interpolate texture u,v and light }
	  Inc(til_u, til_iu);
	  Inc(til_v, til_iv);
          Inc(l, il);

          adr := adr Shr 16;

          til_u := til_u And DWord($F8077FFF);
          til_v := til_v And $07F87FFF;

          bla := (tunneltex + adr)^;

          adr := til_u + til_v;

          { Look up the light and write to buffer }
          (dest + ((j Shl 3) + y) * 320 + (I Shl 3) + x)^ := lookup[((l And $3F0000) Shr 8) + bla];
        End;

        Inc(lu, liu); Inc(ru, riu);
        Inc(lv, liv); Inc(rv, riv);
        Inc(ll, lil); Inc(rl, ril);
      End;
    End;
End;

{ tilt rotates the viewer in the tunnel in a relative / absolute way }
Procedure TRayTunnel.tilt(x, y, z : Integer);

Begin
  xa := (xa + x) And $3FF;
  ya := (ya + y) And $3FF;
  za := (za + z) And $3FF;
End;

Procedure TRayTunnel.tilt(x, y, z : Integer; abs : Uint8);

Begin
  xa := x And $3FF;
  ya := y And $3FF;
  za := z And $3FF;
End;

{ Relative / absolute move }
Procedure TRayTunnel.move(dx, dy, dz : Single);

Begin
  pos[0] := pos[0] + dx;
  pos[1] := pos[1] + dy;
  pos[2] := pos[2] + dz;
End;

Procedure TRayTunnel.move(x, y, z : Single; abs : Uint8);

Begin
  pos[0] := x;
  pos[1] := y;
  pos[2] := z;
End;

{ Relative / absolute move for the lightsource }
Procedure TRayTunnel.movelight(dx, dy, dz : Single);

Begin
  light[0] := light[0] + dx;
  light[1] := light[1] + dy;
  light[2] := light[2] + dz;
End;

Procedure TRayTunnel.movelight(x, y, z : Single; abs : Uint8);

Begin
  light[0] := x;
  light[1] := y;
  light[2] := z;
End;

{ Lock lightsource to the viewer }
Procedure TRayTunnel.locklight(lock : Boolean);

Begin
  lightstatus := lock;
End;

Var
  console : TPTCConsole;
  surface : TPTCSurface;
  format : TPTCFormat;
  tunnel : TRayTunnel;
  posz, phase_x, phase_y : Single;
  angle_x, angle_y : Integer;
  buffer : PUint32;

Begin
  format := Nil;
  surface := Nil;
  console := Nil;
  tunnel := Nil;
  Try
    Try
      format := TPTCFormat.Create(32, $00FF0000, $0000FF00, $000000FF);

      console := TPTCConsole.create;
      console.open('Tunnel3D demo', 320, 200, format);

      surface := TPTCSurface.create(320, 200, format);

      { Create a tunnel, radius=700 }
      tunnel := TRayTunnel.Create(700);

      tunnel.load_texture;

      { Light follows the viewer }
      tunnel.locklight(True);

      posz := 80; phase_x := 0; phase_y := 0;
      angle_x := 6; angle_y := 2;

      While Not console.KeyPressed Do
      Begin
        buffer := surface.lock;
	Try
          tunnel.interpolate;

          { Draw to offscreen buffer }
          tunnel.draw(buffer);
	Finally
          surface.unlock;
	End;

        { And copy to screen }
        surface.copy(console);

        console.update;

        tunnel.tilt(angle_x, angle_y, 0);
        tunnel.move(sin(phase_x), cos(phase_y), posz);

        phase_x := phase_x + 0.2;
        phase_y := phase_y + 0.1;
      End;
    Finally
      console.close;
      console.Free;
      surface.Free;
      tunnel.Free;
      format.Free;
    End;
  Except
    On error : TPTCError Do
      error.report;
  End;
End.
