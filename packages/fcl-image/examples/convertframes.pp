{
    Converts a file with several images (an animation, pages, icon sizes) to another format
    through TFPImageList, and lists its frames.
    See the file COPYING.FPC, included in this distribution, for details.
}
program convertframes;

{$mode objfpc}{$h+}

uses
  SysUtils, FPImage, FPImageList, FPReadBMP, FPWriteBMP, FPReadPNG, FPWritePNG, FPReadJPEG, FPWriteJPEG,
  FPReadGif, FPWriteGIF, FPReadTiff, FPWriteTiff, FPReadICO, FPWriteICO, FPReadWebP, FPWriteWebP;

const
  cKinds: array[TFPFrameKind] of String = ('animation frame', 'page', 'variant');

var
  lList: TFPImageList;
  i: Integer;

begin
  if ParamCount < 1 then
    begin
    WriteLn('Usage: convertframes input [output]');
    WriteLn('Lists the frames of input and, when output is given, writes them in the format of its extension.');
    Halt(1);
    end;
  lList := TFPImageList.Create;
  try
    if not lList.LoadFromFile(ParamStr(1)) then
      begin
      WriteLn('No reader for ', ParamStr(1));
      Halt(2);
      end;
    WriteLn(ParamStr(1), ': ', lList.Count, ' frames on ', lList.Info.Width, 'x', lList.Info.Height,
      ', loop count ', lList.Info.LoopCount);
    for i := 0 to lList.Count - 1 do
      with lList[i] do
        WriteLn(Format('  %d: %s, %dx%d, delay %d ms %s', [i, cKinds[Info.Kind], Image.Width, Image.Height,
          Info.Delay, Info.Name]));
    if ParamCount >= 2 then
      begin
      if not lList.SaveToFile(ParamStr(2)) then
        begin
        WriteLn('No writer for ', ParamStr(2));
        Halt(3);
        end;
      WriteLn('Wrote ', ParamStr(2));
      end;
  finally
    lList.Free;
  end;
end.
