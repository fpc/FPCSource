{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 Tunnel demo for OpenPTC 1.0 C++ API
 Originally coded by Thomas Rizos (rizos@swipnet.se)
 Adapted for OpenPTC by Glenn Fiedler (ptc@gaffer.org)
 This source code is licensed under the GNU GPL
}

Program Tunnel;

{$MODE objfpc}

Uses
  ptc, Math;

Type
  { tunnel class }
  TTunnel = Class(TObject)
    Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure setup;
    Procedure draw(buffer : PUint32; t : Single);
    Private
    { tunnel data }
    tunnel : PUint32;
    texture : PUint8;
  End;

Constructor TTunnel.Create;

Begin
  tunnel := Nil;
  texture := Nil;
  
  { allocate tables }
  tunnel := GetMem(320*200*SizeOf(Uint32));
  texture := GetMem(256*256*2*SizeOf(Uint8));

  { setup }
  setup;
End;

Destructor TTunnel.Destroy;

Begin
  { free tables }
  If assigned(tunnel) Then
    FreeMem(tunnel);
  If assigned(texture) Then
    FreeMem(texture);
  
  Inherited Destroy;
End;

Procedure TTunnel.setup;

Var
  index : Integer;
  x, y : Integer;
  angle, angle1, angle2, radius, u, v : Double;

Begin
  { tunnel index }
  index := 0;
  
  { generate tunnel table }
  For y := 100 DownTo -99 Do
    For x := -160 To 159 Do
    Begin
      { calculate angle from center }
      angle := arctan2(y, x) * 256 / pi / 2;

      { calculate radius from center }
      radius := sqrt(x * x + y * y);

      { clamp radius to minimum }
      If radius < 1 Then
	radius := 1;

      { texture coordinates }
      u := angle;
      v := 6000 / radius;

      { calculate texture index for (u,v) }
      tunnel[index] := (Trunc(v) And $FF) * 256 + (Trunc(u) And $FF);
      Inc(index);
    End;

  { generate blue plasma texture }
  index := 0;
  angle2 := pi * 2/256 * 230;
  For y := 0 To 256 * 2 - 1 Do
  Begin
    angle1 := pi * 2/256 * 100;
    For x := 0 To 256-1 Do
    Begin
      texture[index] := Trunc(sin(angle1)*80 + sin(angle2)*40 + 128);
      angle1 := angle1 + pi*2/256*3;
      Inc(index);
    End;
    angle2 := angle2 + pi * 2/256 *2;
  End;
End;

Procedure TTunnel.draw(buffer : PUint32; t : Single);

Var
  x, y : Integer;
  scroll : Uint32;
  i : Integer;

Begin
  { tunnel control functions }
  x := Trunc(sin(t) * 99.9);
  y := Trunc(t * 200);

  { calculate tunnel scroll offset }
  scroll := ((y And $FF) Shl 8) + (x And $FF);

  { loop through each pixel }
  For i := 0 To 64000-1 Do
    { lookup tunnel texture }
    buffer[i] := texture[tunnel[i] + scroll];
End;

Var
  format : TPTCFormat;
  console : TPTCConsole;
  surface : TPTCSurface;
  TheTunnel : TTunnel;
  time, delta : Single;
  buffer : PUint32;

Begin
  format := Nil;
  surface := Nil;
  console := Nil;
  TheTunnel := Nil;
  Try
    Try
      { create format }
      format := TPTCFormat.Create(32, $00FF0000, $0000FF00, $000000FF);

      { create console }
      console := TPTCConsole.Create;

      { open console }
      console.open('Tunnel demo', 320, 200, format);

      { create surface }
      surface := TPTCSurface.Create(320, 200, format);
    
      { create tunnel }
      TheTunnel := TTunnel.Create;

      { time data }
      time := 0;
      delta := 0.03;

      { loop until a key is pressed }
      While Not console.KeyPressed Do
      Begin
        { lock surface }
        buffer := surface.lock;
	Try
          { draw tunnel }
          TheTunnel.draw(buffer, time);
	Finally
          { unlock surface }
          surface.unlock;
	End;

        { copy to console }
        surface.copy(console);

        { update console }
        console.update;

        { update time }
        time += delta;
      End;
    Finally
      TheTunnel.Free;
      surface.Free;
      console.close;
      console.Free;
      format.Free;
    End;
  Except
    On error : TPTCError Do
      { report error }
      error.report;
  End;
End.
