program BUGGY;

{$MODE delphi}

type
  TImageFormat = (ifIndex8, ifA8R8G8B8);

  TImageData = packed record
    Width: Integer;
    Height: Integer;
    Format: TImageFormat;
    Size: Integer;
    Bits: Pointer;
    Palette: Pointer;
  end;

  TDynArray = array of TImageData;

procedure ModImage(var Img: TImageData);
begin
  Img.Width := 128;
  Img.Height := 128;
end;

procedure ArrayStuff(const Arr: TDynArray);
var
  I: Integer;
begin
  for I := 0 to High(Arr) do
    ModImage(Arr[I]);
end;

var
  MyArr: TDynArray;
begin
  SetLength(MyArr, 5);
  ArrayStuff(MyArr);
end.

{
  bug-interror.pas(30,5) Fatal: Internal error 200106041
  bug-interror.pas(30,5) Fatal: Compilation aborted
  
  Error is caused by const parameter in procedure ArrayStuff(const Arr: TDynArray);
  Doesn't occur when array is var parameter.
  Only crashed in $MODE DELPHI.
  Delphi lets you change elements of array even though
  array is passed as const parameter.
}
