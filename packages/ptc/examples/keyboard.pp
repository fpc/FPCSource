{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 Keyboard example for OpenPTC 1.0 C++ implementation
 Copyright (c) Glenn Fiedler (ptc@gaffer.org)
 This source code is in the public domain
}

program KeyboardExample;

{$MODE objfpc}

uses
  ptc;

var
  console: TPTCConsole = nil;
  surface: TPTCSurface = nil;
  format: TPTCFormat = nil;
  color: TPTCColor = nil;
  key: TPTCKeyEvent = nil;
  area: TPTCArea;
  x, y: Integer;
  size: Integer;
  delta: Integer;
begin
  try
    try
      { create key }
      key := TPTCKeyEvent.Create;

      { create console }
      console := TPTCConsole.Create;

      { create format }
      format := TPTCFormat.Create(32, $00FF0000, $0000FF00, $000000FF);

      { open the console }
      console.open('Keyboard example', format);

      { create surface matching console dimensions }
      surface := TPTCSurface.Create(console.width, console.height, format);

      { setup cursor data }
      x := surface.width div 2;
      y := surface.height div 2;
      size := surface.width div 10;
      color := TPTCColor.Create(1, 1, 1);

      { main loop }
      repeat
        { check for key press }
        if console.KeyPressed then
        begin
          { read console key press }
          console.ReadKey(key);

          { shift modifier }
          if key.shift then
            { move fast }
            delta := 10
          else
            { move slow }
            delta := 1;

          { handle cursor keys }
          case key.code of
            PTCKEY_LEFT: Dec(x, delta);
            PTCKEY_RIGHT: Inc(x, delta);
            PTCKEY_UP: Dec(y, delta);
            PTCKEY_DOWN: Inc(y, delta);
            { exit when escape is pressed }
            PTCKEY_ESCAPE: Break;
          end;
        end;

        { clear surface }
        surface.clear;

        { setup cursor area }
        area := TPTCArea.Create(x - size, y - size, x + size, y + size);
        try
          { draw cursor as a quad }
          surface.clear(color, area);
        finally
          area.Free;
        end;

        { copy to console }
        surface.copy(console);

        { update console }
        console.update;
      until False;
    finally
      color.Free;
      console.close;
      console.Free;
      surface.Free;
      key.Free;
      format.Free;
    end;
  except
    on error: TPTCError do
      { report error }
      error.report;
  end;
end.
