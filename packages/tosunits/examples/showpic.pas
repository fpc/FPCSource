{
    Copyright (c) 2020 Karoly Balogh

    Shows an Atari DEGAS picture on full screen
    Example program for Free Pascal's Atari TOS bindings

    This example program is in the Public Domain under the terms of
    Unlicense: http://unlicense.org/

 **********************************************************************}

{$APPTYPE CONSOLE}
program showpic;

uses
  xbios, gemdos;

{$i showpic.inc}

type
  Tdegas_picture = record
    mode: smallint;
    palette: array[0..15] of smallint;
    data: array[0..31999] of byte;
  end;
  Pdegas_picture = ^Tdegas_picture;

var
  old_palette: array[0..16] of smallint;
  old_rez: smallint;
  screen: pword;
  pic: Pdegas_picture;

procedure save_palette(palette: pword);
var
  i: smallint;
begin
  for i:=0 to 15 do
    palette[i]:=xbios_setcolor(i,-1);
end;

procedure init(mode: smallint);
begin
  { obtain the old resolution and palette }
  old_rez:=xbios_getrez;
  save_palette(@old_palette[0]);

  { set the screen mode and get the framebuffer address }
  xbios_setscreen(pointer(-1),pointer(-1),mode);
  screen:=xbios_logbase;
end;

procedure done;
begin
  { restore original screen resolution and palette }
  xbios_setscreen(pointer(-1),pointer(-1),old_rez);
  xbios_setpalette(@old_palette);
end;

begin
  { this uses a compiled-in picture, but it's easy to load
    something from disk at this point instead }
  pic:=Pdegas_picture(@cheetah_pic);

  init(pic^.mode);

  { set the palette and move picture data to the framebuffer }
  xbios_setpalette(@pic^.palette);
  system.move(pic^.data,screen^,sizeof(pic^.data));

  { wait for keypress }
  gemdos_cnecin;

  done;
end.
