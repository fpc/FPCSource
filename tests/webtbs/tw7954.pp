{$ifdef fpc}
{$mode delphi}
{$endif}

uses
{$ifdef unix}
  cthreads,
{$endif}
  Classes, SysUtils;

var
  cs: trtlcriticalsection;
begin
  fillchar(cs,sizeof(cs),#255);
  try
    leavecriticalsection(cs);
  except on Exception do
    halt(0);
  end;
end.
