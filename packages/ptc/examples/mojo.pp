{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
Mojo demo for OpenPTC 1.0 C++ API
Coded by Alex Evans and adapted to OpenPTC 1.0 by Glenn Fiedler

nasty code by alex "statix" evans for ptc. (c) copyright alex evans 1998
time... 02.00 am on 13/1/98.
have fun
it's my take on some classic light mask effect
it's raytracing through properly modelled fog with occlusion, multiple
shadow rays cast back to the light for each pixel ray, and erm, its
s l o w... but it looks nice don't it?

oh and fresnel fall off... or something

UNTESTED! ok?

define inv for interesting fx (not)
}

program Mojo;

{$MODE objfpc}
{$INLINE on}

uses
  ptc, SysUtils;

{ $DEFINE INV}

const
  SC = 12;
  MINSEGSIZE = 2.5;
  NSEG = 5;
  frandtab_seed: Uint16 = 54;

var
  MaskMap: PUint8;
  frandtab: array [0..65535] of Uint16;

type
  FVector = object
{    case Boolean of
      False: (X, Y, Z: Single);
      True: (R, G, B: Single);}
    X, Y, Z: Single;

    constructor Init;
    constructor Init(_x, _y, _z: Single);

    function Magnitude: Single; inline;
    function MagnitudeSq: Single; inline;
    procedure Normalise; inline;
  end;
  FMatrix = object
    Row: array [0..2] of FVector;
    constructor Init;
    constructor Init(a, b, c: FVector);
    function Column0: FVector; inline;
    function Column1: FVector; inline;
    function Column2: FVector; inline;
    procedure MakeXRot(theta: Single); inline;
    procedure MakeYRot(theta: Single); inline;
    procedure MakeZRot(theta: Single); inline;
    procedure MakeID; inline;
    function Transpose: FMatrix; inline;
    procedure TransposeInPlace; inline;
    procedure Normalise; inline;
  end;
  PRay = ^TRay;
  TRay = Object
    mPosn: FVector;
    mDir: FVector;
    constructor Init(const p, d: FVector);
  end;
  VLight = class
    mAng: Single;
    mPosn: FVector;
    mTarget: FVector;
    mAxis: FMatrix;
    mCol: FVector;

    p, p2, _d: FVector; { temp space }

    constructor Create(const col: FVector);
    procedure Move(const q: FVector);
    procedure MoveT(const q: FVector);
    procedure Update;
    function Light(const ray: TRay): FVector;
    function CalcLight(t: Single): Single;
  end;

constructor FVector.Init;
begin
end;

constructor FVector.Init(_x, _y, _z: Single);
begin
  X := _x;
  Y := _y;
  Z := _z;
end;

function FVector.Magnitude: Single; inline;
begin
  Result := Sqrt(Sqr(X) + Sqr(Y) + Sqr(Z));
end;

function FVector.MagnitudeSq: Single; inline;
begin
  Result := Sqr(X) + Sqr(Y) + Sqr(Z);
end;

procedure FVector.Normalise; inline;
var
  l: Single;
begin
  l := 1 / Magnitude;
  X := X * l;
  Y := Y * l;
  Z := Z * l;
end;

operator * (a, b: FVector): Single; inline;
begin
  Result := a.X * b.X + a.Y * b.Y + a.Z * b.Z;
end;

operator * (a: FVector; b: Single): FVector; inline;
begin
  Result.X := a.X * b;
  Result.Y := a.Y * b;
  Result.Z := a.Z * b;
end;

operator + (a, b: FVector): FVector; inline;
begin
  Result.X := a.X + b.X;
  Result.Y := a.Y + b.Y;
  Result.Z := a.Z + b.Z;
end;

operator - (a, b: FVector): FVector; inline;
begin
  Result.X := a.X - b.X;
  Result.Y := a.Y - b.Y;
  Result.Z := a.Z - b.Z;
end;

operator ** (a, b: FVector) res: FVector; inline;
begin
  Result.X := a.Y * b.Z - a.Z * b.Y;
  Result.Y := a.Z * b.X - a.X * b.Z;
  Result.Z := a.X * b.Y - a.Y * b.X;
end;

constructor FMatrix.Init;
begin
end;

constructor FMatrix.Init(a, b, c: FVector);
begin
  Row[0] := a;
  Row[1] := b;
  Row[2] := c;
end;

function FMatrix.Column0: FVector; inline;
begin
  Result.Init(Row[0].X, Row[1].X, Row[2].X);
end;

function FMatrix.Column1: FVector; inline;
begin
  Result.Init(Row[0].Y, Row[1].Y, Row[2].Y);
end;

function FMatrix.Column2: FVector; inline;
begin
  Result.Init(Row[0].Z, Row[1].Z, Row[2].Z);
end;

procedure FMatrix.MakeXRot(theta: Single); inline;
var
  c, s: Single;
begin
  c := cos(theta);
  s := sin(theta);
  Row[1].Y := c; Row[1].Z := s; Row[1].X := 0;
  Row[2].Y := -s; Row[2].Z := c; Row[2].X := 0;
  Row[0].Y := 0; Row[0].Z := 0; Row[0].X := 1;
end;

procedure FMatrix.MakeYRot(theta: Single); inline;
var
  c, s: Single;
begin
  c := cos(theta);
  s := sin(theta);
  Row[2].Z := c; Row[2].X := s; Row[2].Y := 0;
  Row[0].Z := -s; Row[0].X := c; Row[0].Y := 0;
  Row[1].Z := 0; Row[1].X := 0; Row[1].Y := 1;
end;

procedure FMatrix.MakeZRot(theta: Single); inline;
var
  c, s: Single;
begin
  c := cos(theta);
  s := sin(theta);
  Row[0].X := c; Row[0].Y := s; Row[0].Z := 0;
  Row[1].X := -s; Row[1].Y := c; Row[1].Z := 0;
  Row[2].X := 0; Row[2].Y := 0; Row[2].Z := 1;
end;

procedure FMatrix.MakeID; inline;
begin
  Row[0].Init(1, 0, 0);
  Row[1].Init(0, 1, 0);
  Row[2].Init(0, 0, 1);
end;

function FMatrix.Transpose: FMatrix; inline;
begin
  Result.Init(Column0, Column1, Column2);
end;

procedure FMatrix.TransposeInPlace; inline;
begin
  Init(Column0, Column1, Column2);
end;

procedure FMatrix.Normalise; inline;
begin
  Row[2].Normalise;
  Row[0] := Row[1]**Row[2];
  Row[0].Normalise;
  Row[1] := Row[2]**Row[0];
  Row[1].Normalise;
end;

operator * (const m: FMatrix; const a: Single): FMatrix; inline;
begin
  Result.Init(m.Row[0]*a, m.Row[1]*a, m.Row[2]*a);
end;

operator * (const m, a: FMatrix): FMatrix; inline;
var
  v1, v2, v3: FVector;
begin
  v1.Init(m.Row[0].X*a.Row[0].X+m.Row[0].Y*a.Row[1].X+m.Row[0].Z*a.Row[2].X,
          m.Row[0].X*a.Row[0].Y+m.Row[0].Y*a.Row[1].Y+m.Row[0].Z*a.Row[2].Y,
          m.Row[0].X*a.Row[0].Z+m.Row[0].Y*a.Row[1].Z+m.Row[0].Z*a.Row[2].Z);
  v2.Init(m.Row[1].X*a.Row[0].X+m.Row[1].Y*a.Row[1].X+m.Row[1].Z*a.Row[2].X,
          m.Row[1].X*a.Row[0].Y+m.Row[1].Y*a.Row[1].Y+m.Row[1].Z*a.Row[2].Y,
          m.Row[1].X*a.Row[0].Z+m.Row[1].Y*a.Row[1].Z+m.Row[1].Z*a.Row[2].Z);
  v3.Init(m.Row[2].X*a.Row[0].X+m.Row[2].Y*a.Row[1].X+m.Row[2].Z*a.Row[2].X,
          m.Row[2].X*a.Row[0].Y+m.Row[2].Y*a.Row[1].Y+m.Row[2].Z*a.Row[2].Y,
          m.Row[2].X*a.Row[0].Z+m.Row[2].Y*a.Row[1].Z+m.Row[2].Z*a.Row[2].Z);
  Result.Init(v1, v2, v3);
end;

operator * (const m: FMatrix; const a: FVector): FVector; inline;
begin
  Result.Init(a*m.Row[0], a*m.Row[1], a*m.Row[2]);
end;

operator + (const m, a: FMatrix): FMatrix; inline;
begin
  Result.Init(m.Row[0]+a.Row[0], m.Row[1]+a.Row[1], m.Row[2]+a.Row[2]);
end;

operator - (const m, a: FMatrix): FMatrix; inline;
begin
  Result.Init(m.Row[0]+a.Row[0], m.Row[1]+a.Row[1], m.Row[2]+a.Row[2]);
end;

constructor TRay.Init(const p, d: FVector);
begin
  mPosn := p;
  mDir := d;
  mDir.Normalise;
end;

constructor VLight.Create(const col: FVector);
begin
  mCol := col * 0.9;
  mAng := 2.8;
  mPosn.Init(0, 0, 20);
  mTarget.Init(0, 0, 0.1);
  mAxis.MakeID;
  Update;
end;

procedure VLight.Move(const q: FVector);
begin
  mPosn := q;
  Update;
end;

procedure VLight.MoveT(const q: FVector);
begin
  mTarget := q;
  Update;
end;

procedure VLight.Update;
begin
  mAxis.Row[2] := (mTarget - mPosn);
  mAxis.Normalise;
end;

function VLight.Light(const ray: TRay): FVector;
var
  f, A, B, C, D, t1, t2, t3, fr, l1, l2, t, h: Single;
  frc, x, y, q: Integer;
  pp: FVector;
begin
  f := 0;

  p2 := ray.mPosn;
  p := mAxis * (ray.mPosn - mPosn);
  _d := mAxis * ray.mDir;
  A := (_d.X*_d.X+_d.Y*_d.Y);
  B := 2*(_d.X*p.X+_d.Y*p.Y)-mAng*(_d.Z);
  C := (p.X*p.X+p.Y*p.Y)-mAng*(p.Z);
  D := B*B-4*A*C;
  if D <= 0 then
  begin
    Result.Init(0, 0, 0);
    exit;
  end;
  D := Sqrt(D);
  A := A * 2;
  t1 := (-B-D)/A;
  t2 := (-B+D)/A;
  frc := 255;
  t3 := -ray.mPosn.Z/ray.mDir.Z;
  if t2<=0 then
  begin
    Result.Init(0, 0, 0);
    exit;
  end;
  if t1<0 then
    t1 := 0;
  if t3>0 then
  begin
    { clip to bitmap plane }
    pp := ray.mPosn + ray.mDir*t3;
    x := 160+Trunc(SC*pp.X);
{$IFNDEF INV}
    if (x>=0) and (x<=319) then
    begin
      y := 100 + Trunc(SC*pp.Y);
      if (y>=0) and (y<=199) then
      begin
        {Result.Init(0, 0, 1);
        exit;}
        frc := MaskMap[y*320+x];
        if frc<1 then
        begin
          if t1>t3 then
            t1 := t3;
          if t2>t3 then
            t2 := t3;
        end;
      end
      else
        t3 := t2
    end
    else
      t3 := t2;
{$ELSE}
    if (x >= 0) and (x <= 319) then
    begin
      y := 100 + Trunc(SC*pp.Y);
      if (y >= 0) and (y <= 199) and (MaskMap[y*320 + x] < 128) then
        t3 := t2;
    end;
    if t1 > t3 then
      t1 := t3;
    if t2 > t3 then
      t2 := t3;
{$ENDIF}
  end;
  if t1>=t2 then
  begin
    Result.Init(0, 0, 0);
    exit;
  end;
  fr := frc/255;
  l1 := CalcLight(t1);
  if t1>t3 then
    l1 := l1 * fr;
  q := NSEG;
  t := t1;
  h := (t2-t1)/NSEG;
  if h<MINSEGSIZE then
    h := MINSEGSIZE;
  while (t<t3) and (q>0) and (t<t2) do
  begin
    t := t + h;
    if (t>t2) then
    begin
      h := h - (t2-t);
      t := t2;
      q := 0;
    end
    else
      Dec(q);
    h := (t-t1);
    p := p + _d*h;
    p2 := p2 + ray.mDir*h;
    l2 := CalcLight(t);
    f := f + (l1+l2)*h;
    l1 := l2;
    t1 := t;
  end;
  while (q>0) and (t<t2) do
  begin
    t := t + h;
    if t>t2 then
    begin
      h := h - (t2-t);
      t := t2;
      q := 0;
    end
    else
      Dec(q);
    p := p + _d*h;
    p2 := p2 + ray.mDir*h;
    l2 := CalcLight(t);
    if t>t3 then
      l2 := l2 * fr;
    f := f + (l1+l2)*h;
    l1 := l2;
    t1 := t;
  end;
  Result := mCol*f;
end;

function VLight.CalcLight(t: Single): Single;
var
  f: Single;
  x, y, c: Integer;
begin
  { trace line to bitmap from mPosn to p2 }
  if not ((mPosn.Z > 0) xor (p2.Z > 0)) then
  begin
    { fresnel fall off... }
    Result := p.Z / p.MagnitudeSq;
    exit;
  end;
  f := -(mPosn.Z)/(p2.Z - mPosn.Z);
  x := 160 + Trunc(SC*((p2.X-mPosn.X)*f+mPosn.X));
{$IFNDEF INV}
  if (x < 0) or (x > 319) then
  begin
    Result := p.Z / p.MagnitudeSq;
    exit;
  end;
  y := 100 + Trunc(SC*((p2.Y-mPosn.Y)*f+mPosn.Y));
  if (y < 0) or (y > 199) then
  begin
    Result := p.Z / p.MagnitudeSq;
    exit;
  end;
  c := MaskMap[y * 320 + x];
{$ELSE}
  if (x < 0) or (x > 319) then
  begin
    Result := 0;
    exit;
  end;
  y := 100 + Trunc(SC*((p2.Y-mPosn.Y)*f+mPosn.Y));
  if (y < 0) or (y > 199) then
  begin
    Result := 0;
    exit;
  end;
  c := 255 - MaskMap[y * 320 + x];
{$ENDIF}
  if c = 0 then
  begin
    Result := 0;
    exit;
  end;
  Result := (c*(1/255))*p.Z / p.MagnitudeSq;
end;

function CLIPC(f: Single): Integer; inline;
begin
  Result := Trunc(f * 255);
  if Result < 0 then
    Result := 0
  else
    if Result > 255 then
      Result := 255;
end;

procedure initfrand;
var
  s, c1: Integer;
begin
  FillChar(frandtab, SizeOf(frandtab), 0);
  s := 1;
  for c1 := 1 to 65535 do
  begin
    frandtab[c1] := s and $FFFF;
    s := (((s shr 4) xor (s shr 13) xor (s shr 15)) and 1) + (s shl 1);
  end;
end;

function frand: Integer; inline;
begin
  Result := frandtab[frandtab_seed];
  frandtab_seed := (frandtab_seed + 1) and $FFFF;
end;

procedure VLightPart(console: IPTCConsole; surface: IPTCSurface);
var
  vl: VLight = nil;
  vl2: VLight = nil;
  camposn: FVector;
  camaxis: FMatrix;
  c1, c2, c3, ti, xx, yy, zz, i, a, x, y: Integer;
  idx: array [0..(200 div 16) - 1, 0..(320 div 16) - 1] of Uint8;
  order: array [0..10*19 - 1, 0..1] of Integer;
  vlightt, t, cz, camf: Single;
  col: FVector;
  ray: TRay;
  oc, c, c2_: Uint32;
  time, delta: Single;
  pitch: Integer;
  screenbuf, pd: PUint8;
  tmp: FVector;
  F: File;
begin
  MaskMap := nil;

  try
    oc := 0;
    initfrand;
    tmp.Init(0.1, 0.4, 1);
    vl := VLight.Create(tmp);
    tmp.Init(1, 0.5, 0.2);
    vl2 := VLight.Create(tmp);
    tmp.Init(0, 0, 20);
    vl.Move(tmp);
    tmp.Init(0, 6, 30);
    vl2.Move(tmp);

    camposn.Init(7, 0.5, -10);
    camaxis.Init;
    camaxis.MakeID;
    tmp.Init(0, 0, 0);
    camaxis.Row[2] := tmp - camposn;
    camaxis.Normalise;
    camf := 100;

    MaskMap := GetMem(320 * 200);
    FillChar(MaskMap^, 320 * 200, 0);

    { load mojo.raw }
    AssignFile(F, 'mojo.raw');
    Reset(F, 1);
    try
      BlockRead(F, MaskMap^, 320*200);
    finally
      CloseFile(F);
    end;

    { build the order of the squares }
    for c1 := 0 to 10*19 - 1 do
    begin
      order[c1, 0] := c1 mod 19;
      order[c1, 1] := (c1 div 19) + 1;
    end;

    { swap them around }
    for c1 := 0 to 9999 do
    begin
      c2 := Random(190);
      c3 := Random(190);
      ti := order[c2, 0]; order[c2, 0] := order[c3, 0]; order[c3, 0] := ti;
      ti := order[c2, 1]; order[c2, 1] := order[c3, 1]; order[c3, 1] := ti;
    end;

    { time settings }
    time := 0;
    delta := 0.01; { this controls the speed of the effect }

    { main loop }
    while not console.KeyPressed do
    begin
      { get surface data }
      pitch := surface.pitch;

      { light time (makes the effect loop) }
      vlightt := 320 * Abs(Sin(time/5));

      t := 13 - 0.1822 * vlightt;
      cz := 1 - 0.01 * vlightt;
      {tmp.Init(Sin(t)*5, Cos(t*-0.675+4543)*5, 15);
      vl.Move(tmp);
      tmp.Init(0, 0, -15);
      vl.Move(tmp);}
      tmp.Init(t, 0, 22);
      vl.Move(tmp);
      tmp.Init(-t, -7, 28);
      vl2.Move(tmp);

      camposn.Init(cz*4+9, cz, -t/7-13);
      tmp.Init(0, 0, 0);
      camaxis.Row[2] := tmp - camposn;
      camaxis.Normalise;

      FillChar(idx, SizeOf(idx), 25);

      { swap them around }
      for c1 := 0 to 99 do
      begin
        c2 := Random(190);
        c3 := Random(190);
        ti := order[c2, 0]; order[c2, 0] := order[c3, 0]; order[c3, 0] := ti;
        ti := order[c2, 1]; order[c2, 1] := order[c3, 1]; order[c3, 1] := ti;
      end;
      for zz := 0 to 189 do
      begin
        xx := order[zz, 0];
        yy := order[zz, 1];
        i := 0;

        { lock surface }
        screenbuf := surface.lock;
        try
          c2 := idx[yy, xx] shr 1;
          for c1 := 0 to c2 - 1 do
          begin
            a := frand and 255;
            x := xx * 16 + (a and 15) + 6 + 4;
            y := yy * 16 + (a shr 4) + 6;

            col.Init(0, 0, 0);
            ray.Init(camposn, camaxis.Row[2]*camf+camaxis.Row[0]*(x-160)+camaxis.Row[1]*(y-100));
            col := col + vl.Light(ray);
            col := col + vl2.Light(ray);

            c := (CLIPC(col.X) shl 16) + (CLIPC(col.Y) shl 8) + (CLIPC(col.Z));
            pd := screenbuf + x*4 + y*pitch;
            Inc(i, Abs(Integer(c and 255)-Integer(pd[321] and 255)) + Abs(Integer(c shr 16)-Integer(pd[321] shr 16)));
            if c1 <> 0 then
              Inc(i, Abs(Integer(c and 255)-Integer(oc and 255)) + Abs(Integer(c shr 16)-Integer(oc shr 16)));
            oc := c;

            c2_ := (c shr 1) and $7F7F7F;
            PUint32(pd)[1] := ((PUint32(pd)[1]) shr 1) and $7F7F7F+ c2_;
            PUint32(pd)[2] := ((PUint32(pd)[2]) shr 1) and $7F7F7F+ c2_;
            Inc(pd, pitch);
            PUint32(pd)[0] := ((PUint32(pd)[0]) shr 1) and $7F7F7F+ c2_;
            PUint32(pd)[1] := c;
            PUint32(pd)[2] := c;
            PUint32(pd)[3] := ((PUint32(pd)[3]) shr 1) and $7F7F7F+ c2_;
            Inc(pd, pitch);
            PUint32(pd)[0] := ((PUint32(pd)[0]) shr 1) and $7F7F7F+ c2_;
            PUint32(pd)[1] := c;
            PUint32(pd)[2] := c;
            PUint32(pd)[3] := ((PUint32(pd)[3]) shr 1) and $7F7F7F+ c2_;
            Inc(pd, pitch);
            PUint32(pd)[1] := ((PUint32(pd)[1]) shr 1) and $7F7F7F+ c2_;
            PUint32(pd)[2] := ((PUint32(pd)[2]) shr 1) and $7F7F7F+ c2_;
          end;
          i := i * 5;
          i := i div (3*idx[yy, xx]);
          if i < 2 then
            i := 2;
          if i > {256}255 then
            i := {256}255;
          idx[yy, xx] := i;
        finally
          { unlock surface }
          surface.unlock;
        end;

        if (zz mod 95) = 0 then
        begin
          { copy surface to console }
          surface.copy(console);

          { update console }
          console.update;
        end;
      end;
      { update time }
      time := time + delta;
    end;
  finally
    FreeMem(MaskMap);
    vl.Free;
    vl2.Free;
  end;
end;

var
  format: IPTCFormat;
  console: IPTCConsole;
  surface: IPTCSurface;
begin
  try
    try
      { create format }
      format := TPTCFormatFactory.CreateNew(32, $00FF0000, $0000FF00, $000000FF);

      { create console }
      console := TPTCConsoleFactory.CreateNew;

      { open console }
      console.open('mojo by statix', 320, 200, format);

      { create main drawing surface }
      surface := TPTCSurfaceFactory.CreateNew(320, 200, format);

      { do the light effect }
      VLightPart(console, surface);

    finally
      { close console }
      if Assigned(console) then
        console.close;
    end;

    { print message to stdout }
    Writeln('mojo by alex "statix" evans');
    Writeln('to be used as an example of bad coding and good ptc');
    Writeln('no responsibility taken for this!');
    Writeln('enjoy ptc! it''s great');
    Writeln;
    Writeln('-statix 13/1/98');
  except
    on error: TPTCError do
      { report error }
      error.report;
  end;
end.
