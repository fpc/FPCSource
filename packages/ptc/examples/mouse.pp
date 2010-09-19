{
 Mouse example for the PTCPas library
 This source code is in the public domain
}

program MouseExample;

{$MODE objfpc}

uses
  ptc;

var
  console: TPTCConsole = nil;
  surface: TPTCSurface = nil;
  format: TPTCFormat = nil;
  event: TPTCEvent = nil;
  pixels: PUint32;
  color: Uint32;
  width, height: Integer;
  I: Integer;
  X, Y: Integer;
  button: Boolean;
  Done: Boolean = False;
begin
  try
    try
      { create console }
      console := TPTCConsole.Create;

      { create format }
      format := TPTCFormat.Create(32, $FF0000, $FF00, $FF);

      { open the console }
      console.open('Mouse example', format);

      { we're going to draw our own cursor, so disable the default cursor }
      console.option('hide cursor');

      { create surface matching console dimensions }
      surface := TPTCSurface.Create(console.width, console.height, format);

      { initialization }
      X := 0;
      Y := 0;

      repeat
        { wait for events }
        console.NextEvent(event, True, PTCAnyEvent);

        { handle mouse events }
        if event is TPTCMouseEvent then
        begin
          { if there's more than one mouse event, process them all... }
          repeat
            X := (event as TPTCMouseEvent).X;
            Y := (event as TPTCMouseEvent).Y;
            button := PTCMouseButton1 in (event as TPTCMouseEvent).ButtonState;
          until not console.NextEvent(event, False, [PTCMouseEvent]);
        end;

        { handle keyboard events }
        if (event is TPTCKeyEvent) and (event as TPTCKeyEvent).Press then
        begin
          case (event as TPTCKeyEvent).Code of
            PTCKEY_G: console.Option('grab mouse');
            PTCKEY_U: console.Option('ungrab mouse');
            PTCKEY_ESCAPE: Done := True;
          end;
        end;

        { clear surface }
        surface.clear;

        { lock surface }
        pixels := surface.lock;

        try
          { get surface dimensions }
          width := surface.width;
          height := surface.height;

          if button then
            color := $00FF00 { green cursor, if button 1 is pressed }
          else
            color := $FFFFFF; { white cursor if button 1 is not pressed }

          { draw a small cross for a cursor }
          for I := 2 to 10 do
          begin
            if (X - I) >= 0 then
              pixels[X - I + Y * width] := color;

            if (X + I) < width then
              pixels[X + I + Y * width] := color;

            if (Y - I) >= 0 then
              pixels[X + (Y - I) * width] := color;

            if (Y + I) < height then
              pixels[X + (Y + I) * width] := color;
          end;

        finally
          { unlock surface }
          surface.unlock;
        end;

        { copy to console }
        surface.copy(console);

        { update console }
        console.update;

      until Done;
    finally
      console.close;
      console.Free;
      surface.Free;
      format.Free;
      event.Free;
    end;
  except
    on error: TPTCError do
      { report error }
      error.report;
  end;
end.
