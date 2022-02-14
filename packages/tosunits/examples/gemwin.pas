{
    Copyright (c) 2017 Karoly Balogh

    Simple, resizable and movable GEM Window
    Example program for Free Pascal's Atari TOS bindings

    This example program is in the Public Domain under the terms of
    Unlicense: http://unlicense.org/

 **********************************************************************}

{$APPTYPE GUI}
{$MODESWITCH OUT+}
{$WARN 3124 OFF}
{$WARN 4055 OFF}
program gemwin;

uses
  aes, vdi;

var
  win_h: smallint;
  win_name: pchar;
  win_info: pchar;
  vdi_h: smallint;

const
  WIN_KIND = NAME or INFO or CLOSER or MOVER or SIZER or FULLER;

function open_vwk: smallint;
var
  work_in: array[0..16] of smallint;
  work_out: array[0..64] of smallint;
  dummy, i: smallint;
  handle: smallint;
begin
  handle:=graf_handle(@dummy,@dummy,@dummy,@dummy);

  for i:=0 to 9 do work_in[i]:=1;
  work_in[10]:=2;

  v_opnvwk(@work_in, @handle, @work_out);

  open_vwk:=handle;
end;

function wind_get_grect(wh, what: smallint; rect: PGRECT): boolean;
begin
   wind_get_grect:=wind_get(wh, what, @rect^.x, @rect^.y, @rect^.w, @rect^.h)<>0;
end;

function open_win: smallint;
var
  handle: smallint;
  dim: TGRECT;
begin
  handle:=wind_create(WIN_KIND, 0, 0, 0, 0);

  win_name:='FPC GEM Window';
  wind_set(handle, WF_NAME, hi(ptruint(win_name)), lo(ptruint(win_name)), 0, 0);
  win_info:='Move me and resize me...';
  wind_set(handle, WF_INFO, hi(ptruint(win_info)), lo(ptruint(win_info)), 0, 0);

  wind_get_grect(0, WF_WORKXYWH, @dim);

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
      wind_get_grect(0, WF_WORKXYWH, @fsrect);
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

function max(a, b: smallint): smallint;
begin
  if a > b then
    max:=a
  else
    max:=b;
end;

function rc_intersect(p1: PGRECT; p2: PGRECT): boolean;
var
  tx, ty, tw, th: smallint;
begin
  tw:=min(p2^.x+p2^.w, p1^.x+p1^.w);
  th:=min(p2^.y+p2^.h, p1^.y+p1^.h);
  tx:=max(p2^.x, p1^.x);
  ty:=max(p2^.y, p1^.y);

  p2^.x:=tx;
  p2^.y:=ty;
  p2^.w:=tw-tx;
  p2^.h:=th-ty;

  rc_intersect:=(tw > tx) and (th > ty);
end;

procedure wind_redraw(wh: smallint; rect: PGRECT);
var
  xyarray: array[0..3] of smallint;
  wrect: TGRECT;
begin
  wind_update(BEG_UPDATE);
  v_hide_c(vdi_h);

  wind_get_grect(wh,WF_FIRSTXYWH,@wrect);
  while (wrect.w<>0) and (wrect.h<>0) do
    begin
      if rc_intersect(rect,@wrect) then
        begin
          xyarray[0]:=wrect.x;
          xyarray[1]:=wrect.y;
          xyarray[2]:=wrect.x+wrect.w-1;
          xyarray[3]:=wrect.y+wrect.h-1;
          vs_clip(vdi_h, 1, @xyarray);

          vsf_color(vdi_h,WHITE);
          v_bar(vdi_h,@xyarray);
        end;
      wind_get_grect(wh,WF_NEXTXYWH,@wrect);
    end;

  v_show_c(vdi_h,0);
  wind_update(END_UPDATE);
end;

procedure event_loop;
var
  msg_buf: array[0..7] of smallint;
begin
  graf_mouse(ARROW, nil);
  repeat
    evnt_mesag(@msg_buf);
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
      WM_TOPPED,WM_NEWTOP:
        wind_set(win_h,WF_TOP,0,0,0,0);
    end;
  until false;
end;

begin
  appl_init;

  vdi_h:=open_vwk;
  win_h:=open_win;

  event_loop;

  wind_close(win_h);
  wind_delete(win_h);
  v_clsvwk(vdi_h);

  appl_exit;
end.
