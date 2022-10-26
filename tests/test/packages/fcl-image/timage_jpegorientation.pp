program timage_jpegorientation;

{$mode objfpc}{$H+}
{$R dots.rc}

uses
  FPReadJPEG, FPImage, Classes, resource;

var
  Bmp: TFPCompactImgRGBA8Bit;
  S: TResourceStream;
  Reader: TFPReaderJPEG;

  function CheckColor(x, y: integer; r, g, b: Word): Boolean;
  begin
    Result := (Byte(Bmp.Colors[x, y].Red)=r) and (Byte(Bmp.Colors[x, y].Green)=g) and (Byte(Bmp.Colors[x, y].Blue)=b);
    if not Result then
      Writeln(Byte(Bmp.Colors[x, y].Red), ':', Byte(Bmp.Colors[x, y].Green), ':', Byte(Bmp.Colors[x, y].Blue));
  end;
begin
  Bmp := TFPCompactImgRGBA8Bit.Create(0, 0);
  Reader := TFPReaderJPEG.Create;

  S := TResourceStream.Create(HINSTANCE, 'dots_5', {$ifdef FPC_OS_UNICODE}PWideChar{$else}PChar{$endif}(RT_RCDATA));
  Bmp.LoadFromStream(S, Reader);
  if not CheckColor(0, 0, 0, 0, 254) then
    Halt(1);
  if not CheckColor(1, 0, 0, 255, 0) then
    Halt(2);
  if not CheckColor(0, 1, 255, 255, 0) then
    Halt(3);
  if not CheckColor(1, 1, 254, 0, 0) then
    Halt(4);
  S.Free;

  S := TResourceStream.Create(HINSTANCE, 'dots_8', {$ifdef FPC_OS_UNICODE}PWideChar{$else}PChar{$endif}(RT_RCDATA));
  Bmp.LoadFromStream(S, Reader);
  if not CheckColor(0, 0, 255, 255, 0) then
    Halt(5);
  if not CheckColor(1, 0, 254, 0, 0) then
    Halt(6);
  if not CheckColor(0, 1, 0, 0, 254) then
    Halt(7);
  if not CheckColor(1, 1, 0, 255, 0) then
    Halt(8);
  S.Free;

  Bmp.Free;
  Reader.Free;
end.

