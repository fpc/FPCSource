{
    Copyright (c) 2024 Karoly Balogh

    32K color gradients on a 256x256 screen
    Example program for Free Pascal's Human 68k bindings

    This example program is in the Public Domain under the terms of
    Unlicense: http://unlicense.org/

 **********************************************************************}

program gradient;

uses
  h68kdos, h68kiocs;

const
  GVRAM_START = $C00000;
  COMPONENT_MASK = %11111000;

var
  super: longint;
  lastmode: longint;

procedure gfx_init;
begin
  lastmode:=_iocs_crtmod(-1);
  _iocs_crtmod(14);  { 256x256, 64k, 31Khz }
  _iocs_vpage(0);
  _iocs_g_clr_on;
  _iocs_b_curoff;
end;

procedure gfx_done;
begin
  writeln('Press Enter...');
  readln;
  _iocs_crtmod(lastmode);
  _iocs_b_curon;
end;

procedure gfx_gradient;
var
  addr: pword;
  x,y: longint;
  r,b: longint;
begin
  addr:=pword(GVRAM_START);
  super:=h68kdos_super(0);

  for y:=0 to 255 do
    begin
      r:=(y and COMPONENT_MASK) shl 3;
      b:=((255-y) and COMPONENT_MASK) shr 2;
      for x:=0 to 255 do
        begin
          addr^:=((x and COMPONENT_MASK) shl 8) or
                 r or b or 1;
          inc(addr);
        end;
      inc(addr,256);
    end;

  h68kdos_super(super);
end;

begin
  gfx_init;
  gfx_gradient;
  gfx_done;
end.
