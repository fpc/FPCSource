{
Ported to FPC by Nikolay Nikolov (nickysn@users.sourceforge.net)
}

{
 Timer example for OpenPTC 1.0 C++ implementation
 Copyright (c) Glenn Fiedler (ptc@gaffer.org)
 This source code is in the public domain
}

program TimerExample;

{$MODE objfpc}

uses
  ptc;

var
  console: IPTCConsole;
  format: IPTCFormat;
  surface: IPTCSurface;
  timer: IPTCTimer;
  time, t: Double;
  pixels: PDWord;
  width, height: Integer;
  repeats, center, magnitude, intensity, sx: Single;
  x, y: Integer;
begin
  try
    try
      { create console }
      console := TPTCConsoleFactory.CreateNew;

      { create format }
      format := TPTCFormatFactory.CreateNew(32, $00FF0000, $0000FF00, $000000FF);

      { open the console }
      console.open('Timer example', format);

      { create surface matching console dimensions }
      surface := TPTCSurfaceFactory.CreateNew(console.width, console.height, format);

      { create timer }
      timer := TPTCTimerFactory.CreateNew;

      { start timer }
      timer.start;

      { loop until a key is pressed }
      while not console.KeyPressed do
      begin
        { get current time from timer }
        time := timer.time;

        { clear surface }
        surface.clear;

        { lock surface }
        pixels := surface.lock;
        try
          { get surface dimensions }
          width := surface.width;
          height := surface.height;

          { sine curve parameters }
          repeats := 2;
          center := height / 2;
          magnitude := height / 3;

          { render a sine curve }
          for x := 0 to width - 1 do
          begin
            { rescale 'x' in the range [0,2*pi] }
            sx := x / width * 2 * pi;

            { calculate time at current position }
            t := time + sx * repeats;

            { lookup sine intensity at time 't' }
            intensity := sin(t);

            { convert intensity to a y position on the surface }
            y := Trunc(center + intensity * magnitude);

            { plot pixel on sine curve }
            pixels[x + y * width] := $000000FF;
          end;
        finally
          { unlock surface }
          surface.unlock;
        end;

        { copy to console }
        surface.copy(console);

        { update console }
        console.update;
      end;
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
