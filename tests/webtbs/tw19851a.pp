{ %norun }
{ Test seeking in TIOStream.

  !!! Input to standard in must begin with:
  abcdefghijklmnopqrstuvwxyz1*
  where * represents optional additional characters (can be anything).

  !!! It is recommended that this be tested interactively from the keyboard too. Run the
  test program, type the test string, and press Enter.

  See ExerciseStream.pas for exit codes.
}

{$mode objfpc}{$H+}

program tiostreamseek;

uses
  iostream,
  uw19851;

var
  AIOStream: TIOStream;

begin
  AIOStream := TIOStream.Create(iosInput);
  try
    ExitCode := Test(AIOStream);
  finally
    AIOStream.Free;
  end;
end.
