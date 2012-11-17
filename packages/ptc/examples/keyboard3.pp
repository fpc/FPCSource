{
 Keyboard example for the PTCPas library
 This source code is in the public domain
}

program KeyboardExample3;

{$MODE objfpc}

uses
  ptc;

procedure DumpKey(AKey: IPTCKeyEvent);
begin
  Writeln('Code=', AKey.Code:3, ', Unicode=$', HexStr(AKey.Unicode, 4),
    ', Press=', AKey.Press:5, ', Shift=', AKey.Shift:5, ', Alt=', AKey.Alt:5,
    ', Control=', AKey.Control:5);
end;

var
  console: IPTCConsole;
  format: IPTCFormat;
  key: IPTCKeyEvent;
  Done: Boolean;
begin
  try
    try
      { create console }
      console := TPTCConsoleFactory.CreateNew;

      { enable key release events }
      console.KeyReleaseEnabled := True;

      { create format }
      format := TPTCFormatFactory.CreateNew(32, $00FF0000, $0000FF00, $000000FF);

      { open the console }
      console.open('Keyboard example 3', format);

      { main loop }
      Done := False;
      repeat
        { check for key press/release }
        while console.KeyPressed do
        begin
          console.ReadKey(key);
          case key.code of
            PTCKEY_ESCAPE:
              begin
                Done := True;
                Break;
              end;
            else
              DumpKey(key);
          end;
        end;

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
