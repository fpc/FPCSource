{
    Copyright (c) 2018 Karoly Balogh

    Rotating 3D cube on PalmOS
    Example program for Free Pascal's PalmOS bindings

    This example program is in the Public Domain under the terms of
    Unlicense: http://unlicense.org/

 **********************************************************************}

{$APPID FPCB}
{$APPNAME FPC Cube}
program palmcube;

uses
  event_, sysevent, systemmgr, window, font,
  errorbase, rect;

type
  tvertex = record
    x: longint;
    y: longint;
    z: longint;
  end;

const
  cube: array[0..7] of tvertex = (
     ( x: -1; y: -1; z: -1; ), // 0
     ( x:  1; y: -1; z: -1; ), // 1
     ( x:  1; y:  1; z: -1; ), // 2
     ( x: -1; y:  1; z: -1; ), // 3

     ( x: -1; y: -1; z:  1; ), // 4
     ( x:  1; y: -1; z:  1; ), // 5
     ( x:  1; y:  1; z:  1; ), // 6
     ( x: -1; y:  1; z:  1; )  // 7
  );

type
  tface = record
    v1, v2, v3: longint;
    edge: longint;
  end;

const
  faces: array[0..11] of tface = (
    ( v1: 0; v2: 2; v3: 1; edge: 6),  // front
    ( v1: 2; v2: 0; v3: 3; edge: 6),

    ( v1: 0; v2: 1; v3: 4; edge: 5),  // top
    ( v1: 1; v2: 5; v3: 4; edge: 3),

    ( v1: 3; v2: 0; v3: 7; edge: 5),  // left
    ( v1: 0; v2: 4; v3: 7; edge: 3),

    ( v1: 1; v2: 2; v3: 5; edge: 5),  // right
    ( v1: 1; v2: 6; v3: 5; edge: 6),

    ( v1: 2; v2: 3; v3: 6; edge: 5),  // bottom
    ( v1: 3; v2: 7; v3: 6; edge: 3),

    ( v1: 4; v2: 5; v3: 6; edge: 3),  // back
    ( v1: 6; v2: 7; v3: 4; edge: 3)
  );

const
  sincos_table: array[0..255] of longint = (
         0,  1608,  3216,  4821,  6424,  8022,  9616, 11204,
     12785, 14359, 15924, 17479, 19024, 20557, 22078, 23586,
     25079, 26557, 28020, 29465, 30893, 32302, 33692, 35061,
     36409, 37736, 39039, 40319, 41575, 42806, 44011, 45189,
     46340, 47464, 48558, 49624, 50659, 51664, 52638, 53580,
     54490, 55367, 56211, 57021, 57797, 58537, 59243, 59913,
     60546, 61144, 61704, 62227, 62713, 63161, 63571, 63943,
     64276, 64570, 64826, 65042, 65219, 65357, 65456, 65515,
     65535, 65515, 65456, 65357, 65219, 65042, 64826, 64570,
     64276, 63943, 63571, 63161, 62713, 62227, 61704, 61144,
     60546, 59913, 59243, 58537, 57797, 57021, 56211, 55367,
     54490, 53580, 52638, 51664, 50659, 49624, 48558, 47464,
     46340, 45189, 44011, 42806, 41575, 40319, 39039, 37736,
     36409, 35061, 33692, 32302, 30893, 29465, 28020, 26557,
     25079, 23586, 22078, 20557, 19024, 17479, 15924, 14359,
     12785, 11204,  9616,  8022,  6424,  4821,  3216,  1608,
         0, -1608, -3216, -4821, -6424, -8022, -9616,-11204,
    -12785,-14359,-15924,-17479,-19024,-20557,-22078,-23586,
    -25079,-26557,-28020,-29465,-30893,-32302,-33692,-35061,
    -36409,-37736,-39039,-40319,-41575,-42806,-44011,-45189,
    -46340,-47464,-48558,-49624,-50659,-51664,-52638,-53580,
    -54490,-55367,-56211,-57021,-57797,-58537,-59243,-59913,
    -60546,-61144,-61704,-62227,-62713,-63161,-63571,-63943,
    -64276,-64570,-64826,-65042,-65219,-65357,-65456,-65515,
    -65535,-65515,-65456,-65357,-65219,-65042,-64826,-64570,
    -64276,-63943,-63571,-63161,-62713,-62227,-61704,-61144,
    -60546,-59913,-59243,-58537,-57797,-57021,-56211,-55367,
    -54490,-53580,-52638,-51664,-50659,-49624,-48558,-47464,
    -46340,-45189,-44011,-42806,-41575,-40319,-39039,-37736,
    -36409,-35061,-33692,-32302,-30893,-29465,-28020,-26557,
    -25079,-23586,-22078,-20557,-19024,-17479,-15924,-14359,
    -12785,-11204, -9616, -8022, -6424, -4821, -3216, -1608
  );

function sin(x: longint): longint; inline;
begin
  sin:=sincos_table[x and 255];
end;

function cos(x: longint): longint; inline;
begin
  cos:=sincos_table[(x + 64) and 255];
end;

function mulfp(a, b: longint): longint; inline;
begin
  mulfp:=sarint64((int64(a) * b),16);
end;

function divfp(a, b: longint): longint;
begin
  divfp:=(int64(a) shl 16) div b;
end;

procedure rotate_vertex(const v: tvertex; var vr: tvertex; xa, ya, za: longint);
var
  x,y,z: longint;
  s,c: longint;
begin
  s   :=sin(ya);
  c   :=cos(ya);
  x   :=mulfp(c,v.x) - mulfp(s,v.z);
  z   :=mulfp(s,v.x) + mulfp(c,v.z);
  if za <> 0 then
    begin
      vr.x:=mulfp(cos(za),x)   + mulfp(sin(za),v.y);
      y   :=mulfp(cos(za),v.y) - mulfp(sin(za),x);
    end
  else
    begin
      vr.x:=x;
      y:=v.y;
    end;
  vr.z:=mulfp(cos(xa),z)   - mulfp(sin(xa),y);
  vr.y:=mulfp(sin(xa),z)   + mulfp(cos(xa),y);
end;

procedure perspective_vertex(const v: tvertex; zc: longint; var xr,yr: longint);
var
  rzc: longint;
begin
  rzc:=divfp(1 shl 16,(v.z - zc));
  xr:=mulfp(mulfp(v.x,zc),rzc);
  yr:=mulfp(mulfp(v.y,zc),rzc);
end;

procedure init_cube;
var
  i: longint;
begin
  for i:=low(cube) to high(cube) do
    begin
      cube[i].x:=cube[i].x shl 16;
      cube[i].y:=cube[i].y shl 16;
      cube[i].z:=cube[i].z shl 16;
    end;
end;

function min(a, b: smallint): smallint;
begin
  if a < b then
    min:=a
  else
    min:=b;
end;

procedure paintcube(tx,ty: longint);
var
  i,cx,cy,vx,vy: longint;
  rcube: array[low(cube)..high(cube)] of tvertex;
  w, h: smallint;
  vr: tvertex;
  scale: longint;
  sx,sy: string[64];
begin
  WinGetWindowExtent(w,h);

  scale:=(min(h,w) div 5) shl 16;
  cx:=w div 2;
  cy:=h div 2;
  for i:=low(cube) to high(cube) do
    begin
      rotate_vertex(cube[i],vr,-ty,-tx,0);
      perspective_vertex(vr,3 shl 16,vx,vy);
      rcube[i].x:=cx + sarlongint(mulfp(vx,scale),16);
      rcube[i].y:=cy + sarlongint(mulfp(vy,scale),16);
    end;

  str(tx,sx);
  str(ty,sy);
  sx:='FPC Cube! X:'+sx+' Y:'+sy;

  WinEraseWindow();
  WinDrawChars(@sx[1],length(sx),1,h-FntLineHeight);

  for i:=low(faces) to high(faces) do
    begin
      with faces[i] do
        begin
          if (edge and 1) > 0 then
            WinDrawLine(rcube[v1].x,rcube[v1].y,
                        rcube[v2].x,rcube[v2].y);
          if (edge and 2) > 0 then
            WinDrawLine(rcube[v2].x,rcube[v2].y,
                        rcube[v3].x,rcube[v3].y);
          if (edge and 4) > 0 then
            WinDrawLine(rcube[v3].x,rcube[v3].y,
                        rcube[v1].x,rcube[v1].y);
        end;
    end;
end;

function CreateOffscreenWin(var offScreen: WinHandle; var screen: WinHandle; var r: RectangleType): boolean;
var
  err: word;
  w, h: smallint;
begin
  WinGetWindowExtent(w,h);
  offScreen:=WinCreateOffscreenWindow(w,h,screenFormat,err);
  screen:= WinGetDrawWindow();
  if err = 0 then
     WinSetDrawWindow(offScreen);
  r.topLeft.x:=0;
  r.topLeft.y:=0;
  r.extent.x:=h;
  r.extent.y:=w;
  CreateOffscreenWin:=err = 0;
end;

procedure EventLoop;
var
  event: EventType;
  prevX,prevY: smallint;
  offscreen: boolean;
  offScrWin, scrWin: WinHandle;
  r: RectangleType;
begin
  prevX:=-1;
  prevY:=-1;
  offScreen:=CreateOffscreenWin(offScrWin,scrWin,r);
  repeat
    EvtGetEvent(event, evtWaitForever);
    if not SysHandleEvent(event) and 
       ((event.screenX<>prevX) or (event.screenY<>prevY)) then
      begin
        prevX:=event.screenX;
        prevY:=event.screenY;
        paintcube(prevX,prevY);
        if offscreen then
          WinCopyRectangle(offScrWin, scrWin, r, 0, 0, winPaint);
      end;
  until (event.eType = appStopEvent);
  if offscreen then
    WinDeleteWindow(offScrWin, false);
end;

begin
  init_cube;
  EventLoop;
end.
