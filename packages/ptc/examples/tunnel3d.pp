{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 Tunnel3D demo for OpenPTC 1.0 C++ API

 Realtime raytraced tunnel
 Copyright (c) 1998 Christian Nentwich (brn@eleet.mcb.at)
 This source code is licensed under the GNU LGPL

 and do not just blatantly cut&paste this into your demo :)
}

program Tunnel3D;

{$MODE objfpc}

uses
  ptc, Math;

type
  PVector = ^TVector;
  TVector = array [0..2] of Single;      { X,Y,Z }
  TMatrix = array [0..3, 0..3] of Single;{ FIRST  = COLUMN
                                          SECOND = ROW

                                          [0, 0]  [1, 0]  [2, 0]
                                          [0, 1]  [1, 1]  [2, 1]
                                          [0, 2]  [1, 2]  [2, 2]
  (I know the matrices are the wrong way round, so what, the code is quite
  old :) }

  TRayTunnel = class
  private
    tunneltex: PUint8;                      { Texture }
    tunneltex_orig: PUint8;                 { Original start of texture memory block }
    pal: PUint8;                            { Original palette }
    lookup: PUint32;                         { Lookup table for lighting }

    sintab, costab: PSingle;                { Take a guess }

    u_array, v_array, l_array: PInteger;    { Raytraced coordinates and light }
    norms: PVector;

    radius, radius_sqr: Single;
    rot: TMatrix;

    pos, light: TVector;                    { Position in the tunnel, pos of }
    xa, ya, za: Integer;                    { lightsource, angles }

    lightstatus: Boolean;                   { Following the viewer ? }

  public
    constructor Create(rad: Single);        { constructor takes the radius }
    destructor Destroy; override;

    procedure load_texture;

    procedure tilt(x, y, z: Integer);              { Rotate relative }
    procedure tilt(x, y, z: Integer; abs: Uint8); { Absolute }

    procedure move(dx, dy, dz: Single);            { Relative move }
    procedure move(x, y, z: Single; abs: Uint8);  { Absolute }

    procedure movelight(dx, dy, dz: Single);
    procedure movelight(x, y, z: Single; abs: Uint8);

    procedure locklight(lock: Boolean);    { Make the light follow the viewer }

    procedure interpolate;                  { Raytracing }

    procedure draw(dest: PUint32);          { Draw the finished tunnel }
  end;

{ VECTOR ROUTINES }
procedure vector_normalize(var v: TVector);
var
  length: Single;
begin
  length := v[0] * v[0] + v[1] * v[1] + v[2] * v[2];
  length := sqrt(length);
  if length <> 0 then
  begin
    v[0] := v[0] / length;
    v[1] := v[1] / length;
    v[2] := v[2] / length;
  end
  else
  begin
    v[0] := 0;
    v[1] := 0;
    v[2] := 0;
  end;
end;

procedure vector_times_matrix(const v: TVector; const m: TMatrix;
                              var res: TVector);
var
  i, j: Integer;
begin
  for j := 0 to 2 do
  begin
    res[j] := 0;
    for i := 0 to 2 do
      res[j] := res[j] + (m[j, i] * v[i]);
  end;
end;

procedure matrix_idle(var m: TMatrix);
begin
  FillChar(m, SizeOf(TMatrix), 0);
  m[0, 0] := 1;
  m[1, 1] := 1;
  m[2, 2] := 1;
  m[3, 3] := 1;
end;

procedure matrix_times_matrix(const m1, m2: TMatrix; var res: TMatrix);
var
  i, j, k: Integer;
begin
  for j := 0 to 3 do
    for i := 0 to 3 do
    begin
      res[i, j] := 0;
      for k := 0 to 3 do
        res[i, j] := res[i, j] + (m1[k, j] * m2[i, k]);
    end;
end;

procedure matrix_rotate_x(var m: TMatrix; angle: Integer; sintab, costab: PSingle);
var
  tmp, tmp2: TMatrix;
begin
  matrix_idle(tmp);
  tmp[1, 1] := costab[angle];
  tmp[2, 1] := sintab[angle];
  tmp[1, 2] := -sintab[angle];
  tmp[2, 2] := costab[angle];
  matrix_times_matrix(tmp, m, tmp2);
  Move(tmp2, m, SizeOf(TMatrix));
end;

procedure matrix_rotate_y(var m: TMatrix; angle: Integer; sintab, costab: PSingle);
var
  tmp, tmp2: TMatrix;
begin
  matrix_idle(tmp);
  tmp[0, 0] := costab[angle];
  tmp[2, 0] := -sintab[angle];
  tmp[0, 2] := sintab[angle];
  tmp[2, 2] := costab[angle];
  matrix_times_matrix(tmp, m, tmp2);
  Move(tmp2, m, SizeOf(TMatrix));
end;

procedure matrix_rotate_z(var m: TMatrix; angle: Integer; sintab, costab: PSingle);
var
  tmp, tmp2: TMatrix;
begin
  matrix_idle(tmp);
  tmp[0, 0] := costab[angle];
  tmp[1, 0] := sintab[angle];
  tmp[0, 1] := -sintab[angle];
  tmp[1, 1] := costab[angle];
  matrix_times_matrix(tmp, m, tmp2);
  Move(tmp2, m, SizeOf(TMatrix));
end;

constructor TRayTunnel.Create(rad: Single);
var
  x, y: Single;
  i, j: Integer;
  tmp: TVector;
begin
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

  for i := 0 to 1023 do
  begin
    sintab[i] := sin(i * pi / 512);
    costab[i] := cos(i * pi / 512);
  end;

  { Generate normal vectors }
  y := -100;
  for j := 0 to 25 do
  begin
    x := -160;
    for i := 0 to 40 do
    begin
      tmp[0] := x;
      tmp[1] := y;
      tmp[2] := 128;
      vector_normalize(tmp);
      norms[j * 64 + i] := tmp;
      x := x + 8;
    end;
    y := y + 8;
  end;

  { Reset tunnel and light position and all angles }
  pos[0] := 0; pos[1] := 0; pos[2] := 0;
  light[0] := 1; light[1] := 1; light[2] := 0;

  xa := 0; ya := 0; za := 0;

  lightstatus := False;

  { Normalize light vector to length 1.0 }
  vector_normalize(light);
end;

destructor TRayTunnel.Destroy;
begin
  FreeMem(tunneltex_orig);
  FreeMem(pal);
  FreeMem(lookup);
  FreeMem(norms);
  FreeMem(l_array);
  FreeMem(v_array);
  FreeMem(u_array);
  FreeMem(costab);
  FreeMem(sintab);
end;

procedure TRayTunnel.load_texture;
var
  texfile: File;
  tmp: PUint8 = nil;
  i, j: Uint32;
  r, g, b: Uint32;
  newoffs: Integer;
begin
  try
    { Allocate tunnel texture 65536+33 bytes too big }

    if tunneltex_orig <> nil then
    begin
      FreeMem(tunneltex_orig);
      tunneltex_orig := nil;
    end;
    tunneltex_orig := GetMem(2*65536 + 33);
    tmp := GetMem(65536);

    { Align the texture on a 64k boundary }
    tunneltex := tunneltex_orig;
    while (PtrUInt(tunneltex) and $FFFF) <> 0 do
      Inc(tunneltex);

    AssignFile(texfile, 'tunnel3d.raw');
    Reset(texfile, 1);
    try
      BlockRead(texfile, pal^, 768);
      BlockRead(texfile, tmp^, 65536);
    finally
      CloseFile(texfile);
    end;

    { Generate lookup table for lighting (65 because of possible inaccuracies) }

    for j := 0 to 64 do
      for i := 0 to 255 do
      begin
        r := pal[i * 3] shl 2;
        g := pal[i * 3 + 1] shl 2;
        b := pal[i * 3 + 2] shl 2;
        r := (r * j) shr 6;
        g := (g * j) shr 6;
        b := (b * j) shr 6;
        if r > 255 then
          r := 255;
        if g > 255 then
          g := 255;
        if b > 255 then
          b := 255;
        lookup[j * 256 + i] := (r shl 16) or (g shl 8) or b;
      end;

    { Arrange texture for cache optimised mapping }

    for j := 0 to 255 do
      for i := 0 to 255 do
      begin
        newoffs := ((i shl 8) and $F800) + (i and $0007) + ((j shl 3) and $7F8);
        (tunneltex + newoffs)^ := (tmp + j * 256 + i)^;
      end;
  finally
    FreeMem(tmp);
  end;
end;

procedure TRayTunnel.interpolate;
var
  ray, intsc, norm, lvec: TVector;
  x, y, a, b, c, discr, t, res: Single;
  i, j: Integer;
begin
  if lightstatus then { Lightsource locked to viewpoint }
    light := pos;

  matrix_idle(rot);
  matrix_rotate_x(rot, xa and $3FF, sintab, costab);
  matrix_rotate_y(rot, ya and $3FF, sintab, costab);
  matrix_rotate_z(rot, za and $3FF, sintab, costab);

  { Constant factor }
  c := 2 * (pos[0] * pos[0] + pos[1] * pos[1] - radius_sqr);

  { Start raytracing }
  y := -100;
  for j := 0 to 25 do
  begin
    x := -160;
    for i := 0 to 40 do
    begin
      vector_times_matrix(norms[(j shl 6) + i], rot, ray);

      a := 2 * (ray[0] * ray[0] + ray[1] * ray[1]);
      b := 2 * (pos[0] * ray[0] + pos[1] * ray[1]);

      discr := b * b - a * c;
      if discr > 0 then
      begin
        discr := sqrt(discr);
        t := (- b + discr) / a;

        { Calculate intersection point }
        intsc[0] := pos[0] + t * ray[0];
        intsc[1] := pos[1] + t * ray[1];
        intsc[2] := pos[2] + t * ray[2];

        { Calculate texture index at intersection point (cylindrical mapping) }
        { try and adjust the 0.2 to stretch/shrink the texture }
        u_array[(j shl 6) + i] := Integer(Trunc(intsc[2] * 0.2) shl 16);
        v_array[(j shl 6) + i] := Trunc(abs(arctan2(intsc[1], intsc[0]) * 256 / pi)) shl 16;

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
        res := Sqr(res);
        if res < 0 then
          res := 0;
        if res > 1 then
          res := 1;
        res := res * 63;

        { Put it into the light array }
        l_array[(j shl 6) + i] := Trunc(res) shl 16;
      end
      else
      begin
        u_array[(j shl 6) + i] := 0;
        v_array[(j shl 6) + i] := 0;
        l_array[(j shl 6) + i] := 0;
      end;
      x := x + 8;
    end;
    y := y + 8;
  end;
end;

procedure TRayTunnel.draw(dest: PUint32);
var
  x, y, lu, lv, ru, rv, liu, liv, riu, riv: Integer;
  iu, iv, i, j, ll, rl, lil, ril, l, il: Integer;
  iadr, adr, til_u, til_v, til_iu, til_iv: DWord;
  bla: Uint8;
begin
  for j := 0 to 24 do
    for i := 0 to 39 do
    begin
      iadr := (j shl 6) + i;

      { Set up gradients }
      lu := u_array[iadr]; ru := u_array[iadr + 1];
      liu := (u_array[iadr + 64] - lu) div 8;
      riu := (u_array[iadr + 65] - ru) div 8;

      lv := v_array[iadr]; rv := v_array[iadr + 1];
      liv := (v_array[iadr + 64] - lv) div 8;
      riv := (v_array[iadr + 65] - rv) div 8;

      ll := l_array[iadr]; rl := l_array[iadr + 1];
      lil := (l_array[iadr + 64] - ll) div 8;
      ril := (l_array[iadr + 65] - rl) div 8;

      for y := 0 to 7 do
      begin
        iu := (ru - lu) div 8;
        iv := (rv - lv) div 8;
        l := ll;
        il := (rl - ll) div 8;

        { Mess up everything for the sake of cache optimised mapping :) }
        til_u := DWord(((lu shl 8) and $F8000000) or ((lu shr 1) and $00007FFF) or (lu and $00070000));
        til_v := DWord(((lv shl 3) and $07F80000) or ((lv shr 1) and $00007FFF));
        til_iu := DWord((((iu shl 8) and $F8000000) or ((iu shr 1) and $00007FFF) or
                          (iu and $00070000)) or $07F88000);
        til_iv := DWord((((iv shl 3) and $07F80000) or ((iv shr 1) and $00007FFF)) or $F8078000);

        adr := til_u + til_v;

        for x := 0 to 7 do
        begin
          { Interpolate texture u,v and light }
          til_u := DWord(til_u + til_iu);
          til_v := DWord(til_v + til_iv);
          Inc(l, il);

          adr := adr shr 16;

          til_u := til_u and DWord($F8077FFF);
          til_v := til_v and $07F87FFF;

          bla := (tunneltex + adr)^;

          adr := til_u + til_v;

          { Look up the light and write to buffer }
          (dest + ((j shl 3) + y) * 320 + (I shl 3) + x)^ := lookup[((l and $3F0000) shr 8) + bla];
        end;

        Inc(lu, liu); Inc(ru, riu);
        Inc(lv, liv); Inc(rv, riv);
        Inc(ll, lil); Inc(rl, ril);
      end;
    end;
end;

{ tilt rotates the viewer in the tunnel in a relative / absolute way }
procedure TRayTunnel.tilt(x, y, z: Integer);
begin
  xa := (xa + x) and $3FF;
  ya := (ya + y) and $3FF;
  za := (za + z) and $3FF;
end;

procedure TRayTunnel.tilt(x, y, z: Integer; abs: Uint8);
begin
  xa := x and $3FF;
  ya := y and $3FF;
  za := z and $3FF;
end;

{ Relative / absolute move }
procedure TRayTunnel.move(dx, dy, dz: Single);
begin
  pos[0] := pos[0] + dx;
  pos[1] := pos[1] + dy;
  pos[2] := pos[2] + dz;
end;

procedure TRayTunnel.move(x, y, z: Single; abs: Uint8);
begin
  pos[0] := x;
  pos[1] := y;
  pos[2] := z;
end;

{ Relative / absolute move for the lightsource }
procedure TRayTunnel.movelight(dx, dy, dz: Single);
begin
  light[0] := light[0] + dx;
  light[1] := light[1] + dy;
  light[2] := light[2] + dz;
end;

procedure TRayTunnel.movelight(x, y, z: Single; abs: Uint8);
begin
  light[0] := x;
  light[1] := y;
  light[2] := z;
end;

{ Lock lightsource to the viewer }
procedure TRayTunnel.locklight(lock: Boolean);
begin
  lightstatus := lock;
end;

var
  console: IPTCConsole;
  surface: IPTCSurface;
  format: IPTCFormat;
  tunnel: TRayTunnel = nil;
  posz, phase_x, phase_y: Single;
  angle_x, angle_y: Integer;
  buffer: PUint32;
begin
  try
    try
      format := TPTCFormatFactory.CreateNew(32, $00FF0000, $0000FF00, $000000FF);

      console := TPTCConsoleFactory.CreateNew;
      console.open('Tunnel3D demo', 320, 200, format);

      surface := TPTCSurfaceFactory.CreateNew(320, 200, format);

      { Create a tunnel, radius=700 }
      tunnel := TRayTunnel.Create(700);

      tunnel.load_texture;

      { Light follows the viewer }
      tunnel.locklight(True);

      posz := 80; phase_x := 0; phase_y := 0;
      angle_x := 6; angle_y := 2;

      while not console.KeyPressed do
      begin
        buffer := surface.lock;
        try
          tunnel.interpolate;

          { Draw to offscreen buffer }
          tunnel.draw(buffer);
        finally
          surface.unlock;
        end;

        { and copy to screen }
        surface.copy(console);

        console.update;

        tunnel.tilt(angle_x, angle_y, 0);
        tunnel.move(sin(phase_x), cos(phase_y), posz);

        phase_x := phase_x + 0.2;
        phase_y := phase_y + 0.1;
      end;
    finally
      if Assigned(console) then
        console.close;
      tunnel.Free;
    end;
  except
    on error: TPTCError do
      error.report;
  end;
end.
