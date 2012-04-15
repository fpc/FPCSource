{
 Keyboard example for the PTCPas library
 This source code is in the public domain
}

program KeyboardExample2;

{$MODE objfpc}

uses
  ptc;

var
  console: IPTCConsole;
  surface: IPTCSurface;
  format: IPTCFormat;
  color: IPTCColor;
  timer: IPTCTimer;
  key: IPTCKeyEvent;
  x, y, delta: Real;
  left, right, up, down: Boolean;
  size: Integer;
  Done: Boolean;
begin
  left := False;
  right := False;
  up := False;
  down := False;
  try
    try
      { create console }
      console := TPTCConsoleFactory.CreateNew;

      { enable key release events }
      console.KeyReleaseEnabled := True;

      { create format }
      format := TPTCFormatFactory.CreateNew(32, $00FF0000, $0000FF00, $000000FF);

      { open the console }
      console.open('Keyboard example 2', format);

      { create timer }
      timer := TPTCTimerFactory.CreateNew;

      { create surface matching console dimensions }
      surface := TPTCSurfaceFactory.CreateNew(console.width, console.height, format);

      { setup cursor data }
      x := surface.width div 2;
      y := surface.height div 2;
      size := surface.width div 10;
      color := TPTCColorFactory.CreateNew(1, 1, 1);

      { start timer }
      timer.start;

      { main loop }
      Done := False;
      repeat
        { check for key press/release }
        while console.KeyPressed do
        begin
          console.ReadKey(key);
          case key.code of
            PTCKEY_LEFT: left := key.press;
            PTCKEY_RIGHT: right := key.press;
            PTCKEY_UP: up := key.press;
            PTCKEY_DOWN: down := key.press;
            PTCKEY_ESCAPE: begin
              Done := True;
              Break;
            end;
          end;
        end;

        { move square }
        delta := timer.delta*100;
        if left then
          x := x - delta;
        if right then
          x := x + delta;
        if up then
          y := y - delta;
        if down then
          y := y + delta;

        { clear surface }
        surface.clear;

        { draw cursor as a quad }
        surface.clear(color, TPTCAreaFactory.CreateNew(Trunc(x) - size, Trunc(y) - size, Trunc(x) + size, Trunc(y) + size));

        { copy to console }
        surface.copy(console);

        { update console }
        console.update;
      until Done;
    finally
      if Assigned(console) then
        console.close;
    end;
  except
    on error: TPTCError do
      { report error }
      error.report;
  end;
end.
