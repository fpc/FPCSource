program testvlc;

{$mode objfpc}{$H+}

uses
{$ifdef unix} cthreads,{$endif}
  Sysutils, Classes, math, libvlc, vlc;

Var
  P :  TVLCMediaPlayer;
  LP : TVLCMediaListPlayer;
  I : Integer;

begin
  // This is needed, or loading the VLC libraries will fail with a SIGFPE
  setexceptionmask([exInvalidOp, exDenormalized, exZeroDivide,
                     exOverflow, exUnderflow, exPrecision]);
  P:=TVLCMediaPlayer.Create(Nil);
  if ParamCount=1 then
    With P do
      try
        PlayFile(ParamStr(1));
        Repeat
          Sleep(100);
        until State in [libvlc_Ended,libvlc_Error];
      finally
        Free;
      end
  else
    begin
    LP:=TVLCMediaListPlayer.Create(Nil);
    try
      P:=TVLCMediaPlayer.Create(Nil);
      try
        LP.Player:=P;
        For I:=1 to ParamCount do
          begin
          TVLCMediaItem(LP.MediaItems.Add).Path:=ParamStr(i);
          end;
        LP.Play;
        Repeat
          Sleep(100);
        until LP.State in [libvlc_Ended,libvlc_Error];
      finally
        P.Free;
      end;
    finally
      LP.free;
    end;
    end;
end.

