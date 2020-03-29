{
    Copyright (c) 2017 Karoly Balogh

    Rotating 3D cube in a GEM window
    Example program for Free Pascal's Atari TOS bindings

    This example program is in the Public Domain under the terms of
    Unlicense: http://unlicense.org/

 **********************************************************************}

{$APPTYPE GUI}
program gemcube;

uses
  aes, vdi, xbios;

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

const
  win_info: array[0..63] of char = '';

var
  appl_h: smallint;
  win_h: smallint;
  win_name: pchar;
  vdi_h: smallint;
  mx, my: smallint;

const
  WIN_KIND = NAME or INFO or CLOSER or MOVER or SIZER or FULLER;

function open_vwk: smallint;
var
  work_in: array[0..16] of smallint;
  work_out: array[0..64] of smallint;
  dummy, i: smallint;
  handle: smallint;
  xyarray: array[0..3] of smallint;
begin
  handle:=graf_handle(@dummy,@dummy,@dummy,@dummy);

  work_in[0]:=2+xbios_getrez();
  for i:=1 to 9 do work_in[i]:=1;
  work_in[10]:=2;

  v_opnvwk(@work_in, @handle, @work_out);
  xyarray[0]:=0;
  xyarray[1]:=0;
  xyarray[2]:=work_out[0];
  xyarray[3]:=work_out[1];
  vs_clip(handle,1,@xyarray);

  open_vwk:=handle;
end;

function open_win: smallint;
var
  handle: smallint;
  dim: TGRECT;
begin
  handle:=wind_create(WIN_KIND, 0, 0, 0, 0);

  win_name:='FPC GEM Cube';
  wind_set(handle, WF_NAME, hi(ptruint(win_name)), lo(ptruint(win_name)), 0, 0);
  win_info:='Spinning...';
  wind_set(handle, WF_INFO, hi(ptruint(@win_info)), lo(ptruint(@win_info)), 0, 0);

  wind_get(0, WF_WORKXYWH, @dim.x, @dim.y, @dim.w, @dim.h);

  dim.x:=dim.x + (dim.w div 20);
  dim.y:=dim.y + (dim.h div 20);
  dim.w:=dim.w - (dim.w div 20) * 2;
  dim.h:=dim.h - (dim.h div 20) * 2;

  wind_open(handle, dim.x, dim.y, dim.w, dim.h);

  open_win:=handle;
end;

procedure wind_set_grect(wh: smallint; rect: PGRECT);
var
  fsrect: TGRECT;
begin
  if rect = nil then
    begin
      wind_get(0, WF_WORKXYWH, @fsrect.x, @fsrect.y, @fsrect.w, @fsrect.h);
      rect:=@fsrect;
    end;

  wind_set(wh,WF_CURRXYWH,rect^.x,rect^.y,rect^.w,rect^.h);
end;

function min(a, b: smallint): smallint;
begin
  if a < b then
    min:=a
  else
    min:=b;
end;

procedure draw_line(x1,y1,x2,y2: smallint);
var
  xyarray: array[0..7] of smallint;
begin
  xyarray[0]:=x1;
  xyarray[1]:=y1;
  xyarray[2]:=x2;
  xyarray[3]:=y2;
  v_pline(vdi_h,2,@xyarray);
end;

procedure wind_redraw(wh: smallint; rect: PGRECT);
var
  i,cx,cy,vx,vy: longint;
  xyarray: array[0..7] of smallint;
  rcube: array[low(cube)..high(cube)] of tvertex;
  wrect: TGRECT;
  vr: tvertex;
  scale: longint;
begin
  wind_update(BEG_UPDATE);

  wind_get(win_h,WF_WORKXYWH,@wrect.x,@wrect.y,@wrect.w,@wrect.h);

  scale:=(min(wrect.h,wrect.w) div 5) shl 16;
  cx:=wrect.x + wrect.w div 2;
  cy:=wrect.y + wrect.h div 2;
  for i:=low(cube) to high(cube) do
    begin
      rotate_vertex(cube[i],vr,-my,-mx,0);
      perspective_vertex(vr,3 shl 16,vx,vy);
      rcube[i].x:=cx + sarlongint(mulfp(vx,scale),16);
      rcube[i].y:=cy + sarlongint(mulfp(vy,scale),16);
    end;

  xyarray[0]:=wrect.x;
  xyarray[1]:=wrect.y;
  xyarray[2]:=wrect.x+wrect.w-1;
  xyarray[3]:=wrect.y+wrect.h-1;

  v_hide_c(vdi_h);
  vsf_color(vdi_h,WHITE);
  v_bar(vdi_h,@xyarray);

  vsl_color(vdi_h,RED);
  for i:=low(faces) to high(faces) do
    begin
      if (faces[i].edge and 1) > 0 then
        draw_line(rcube[faces[i].v1].x,rcube[faces[i].v1].y,
                  rcube[faces[i].v2].x,rcube[faces[i].v2].y);
      if (faces[i].edge and 2) > 0 then
        draw_line(rcube[faces[i].v2].x,rcube[faces[i].v2].y,
                  rcube[faces[i].v3].x,rcube[faces[i].v3].y);
      if (faces[i].edge and 4) > 0 then
        draw_line(rcube[faces[i].v3].x,rcube[faces[i].v3].y,
                  rcube[faces[i].v1].x,rcube[faces[i].v1].y);
    end;

  v_show_c(vdi_h,1);
  wind_update(END_UPDATE);
end;

procedure event_loop;
var
  msg_buf: array[0..7] of smallint;
  sx,sy: string[16];
  nmx,nmy: smallint;
  dummy: smallint;
  e: smallint;
begin
  repeat
    dummy:=0;
    e:=evnt_multi(MU_TIMER or MU_MESAG,dummy,dummy,dummy,
                  dummy,dummy,dummy,dummy,dummy,
                  dummy,dummy,dummy,dummy,dummy,
                  @msg_buf,
                  50,0,
                  @dummy,@dummy,@dummy,@dummy,
                  @dummy,@dummy);
    if e = MU_TIMER then
      begin
        graf_mkstate(@nmx,@nmy,@dummy,@dummy);
        if (nmx <> mx) or (nmy <> my) then
          begin
            mx:=nmx;
            my:=nmy;
            str(mx,sx);
            str(my,sy);
            win_info:='Spinning... X:'+sx+' Y:'+sy;
            wind_set(win_h, WF_INFO, hi(ptruint(@win_info)), lo(ptruint(@win_info)), 0, 0);

            msg_buf[0]:=WM_REDRAW;
            msg_buf[1]:=appl_h;
            msg_buf[2]:=0;
            msg_buf[3]:=win_h;
            msg_buf[4]:=0;
            msg_buf[5]:=0;
            msg_buf[6]:=0;
            msg_buf[7]:=0;
            appl_write(appl_h, sizeof(msg_buf), @msg_buf);
          end;
      end;
    if e = MU_MESAG then
      case msg_buf[0] of
        WM_CLOSED:
          break;
        WM_REDRAW:
          wind_redraw(win_h,PGRECT(@msg_buf[4]));
        WM_MOVED,
        WM_SIZED:
          wind_set_grect(win_h,PGRECT(@msg_buf[4]));
        WM_FULLED:
          wind_set_grect(win_h,nil);
      end;
  until false;
end;

begin
  appl_h:=appl_init;

  init_cube;

  vdi_h:=open_vwk;
  win_h:=open_win;

  event_loop;

  wind_close(win_h);
  wind_delete(win_h);
  v_clsvwk(vdi_h);

  appl_exit;
end.
