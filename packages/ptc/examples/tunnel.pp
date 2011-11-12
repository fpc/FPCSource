{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 Tunnel demo for OpenPTC 1.0 C++ API
 Originally coded by Thomas Rizos (rizos@swipnet.se)
 Adapted for OpenPTC by Glenn Fiedler (ptc@gaffer.org)
 This source code is licensed under the GNU GPL
}

program Tunnel;

{$MODE objfpc}

uses
  ptc, Math;

type
  { tunnel class }
  TTunnel = class
    public
    constructor Create;
    destructor Destroy; override;
    procedure setup;
    procedure draw(buffer: PUint32; t: Single);
    private
    { tunnel data }
    tunnel: PUint32;
    texture: PUint8;
  end;

constructor TTunnel.Create;
begin
  { allocate tables }
  tunnel := GetMem(320*200*SizeOf(Uint32));
  texture := GetMem(256*256*2*SizeOf(Uint8));

  { setup }
  setup;
end;

destructor TTunnel.Destroy;
begin
  { free tables }
  FreeMem(tunnel);
  FreeMem(texture);

  inherited Destroy;
end;

procedure TTunnel.setup;
var
  index: Integer;
  x, y: Integer;
  angle, angle1, angle2, radius, u, v: Double;
begin
  { tunnel index }
  index := 0;

  { generate tunnel table }
  for y := 100 DownTo -99 do
    for x := -160 to 159 do
    begin
      { calculate angle from center }
      angle := arctan2(y, x) * 256 / pi / 2;

      { calculate radius from center }
      radius := sqrt(x * x + y * y);

      { clamp radius to minimum }
      if radius < 1 then
        radius := 1;

      { texture coordinates }
      u := angle;
      v := 6000 / radius;

      { calculate texture index for (u,v) }
      tunnel[index] := (Trunc(v) and $FF) * 256 + (Trunc(u) and $FF);
      Inc(index);
    end;

  { generate blue plasma texture }
  index := 0;
  angle2 := pi * 2/256 * 230;
  for y := 0 to 256 * 2 - 1 do
  begin
    angle1 := pi * 2/256 * 100;
    for x := 0 to 256-1 do
    begin
      texture[index] := Trunc(sin(angle1)*80 + sin(angle2)*40 + 128);
      angle1 := angle1 + pi*2/256*3;
      Inc(index);
    end;
    angle2 := angle2 + pi * 2/256 *2;
  end;
end;

procedure TTunnel.draw(buffer: PUint32; t: Single);
var
  x, y: Integer;
  scroll: Uint32;
  i: Integer;
begin
  { tunnel control functions }
  x := Trunc(sin(t) * 99.9);
  y := Trunc(t * 200);

  { calculate tunnel scroll offset }
  scroll := ((y and $FF) shl 8) + (x and $FF);

  { loop through each pixel }
  for i := 0 to 64000-1 do
    { lookup tunnel texture }
    buffer[i] := texture[tunnel[i] + scroll];
end;

var
  format: IPTCFormat;
  console: IPTCConsole;
  surface: IPTCSurface;
  TheTunnel: TTunnel = nil;
  time, delta: Single;
  buffer: PUint32;
begin
  try
    try
      { create format }
      format := TPTCFormatFactory.CreateNew(32, $00FF0000, $0000FF00, $000000FF);

      { create console }
      console := TPTCConsoleFactory.CreateNew;

      { open console }
      console.open('Tunnel demo', 320, 200, format);

      { create surface }
      surface := TPTCSurfaceFactory.CreateNew(320, 200, format);

      { create tunnel }
      TheTunnel := TTunnel.Create;

      { time data }
      time := 0;
      delta := 0.03;

      { loop until a key is pressed }
      while not console.KeyPressed do
      begin
        { lock surface }
        buffer := surface.lock;
        try
          { draw tunnel }
          TheTunnel.draw(buffer, time);
        finally
          { unlock surface }
          surface.unlock;
        end;

        { copy to console }
        surface.copy(console);

        { update console }
        console.update;

        { update time }
        time := time + delta;
      end;
    finally
      TheTunnel.Free;
      if Assigned(console) then
        console.close;
    end;
  except
    on error: TPTCError do
      { report error }
      error.report;
  end;
end.
