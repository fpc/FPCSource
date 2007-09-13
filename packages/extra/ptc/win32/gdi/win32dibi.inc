
{TODO: create DIBs with the same color depth as the desktop}

Constructor TWin32DIB.Create(AWidth, AHeight : Integer);

Begin
  FBitmapInfo := GetMem(SizeOf(BITMAPINFOHEADER) + 12);

  FillChar(FBitmapInfo^.bmiHeader, SizeOf(BITMAPINFOHEADER), 0);
  With FBitmapInfo^.bmiHeader Do
  Begin
    biSize := SizeOf(BITMAPINFOHEADER);
    biWidth := AWidth;
    biHeight := -AHeight;
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_BITFIELDS;
    biSizeImage := 0;
    biXPelsPerMeter := 0;
    biYPelsPerMeter := 0;
    biClrUsed := 0;
    biClrImportant := 0;
  End;

  PDWord(@FBitmapInfo^.bmiColors)[0] := $FF0000;
  PDWord(@FBitmapInfo^.bmiColors)[1] := $00FF00;
  PDWord(@FBitmapInfo^.bmiColors)[2] := $0000FF;

  FWidth := AWidth;
  FHeight := AHeight;
  FFormat := TPTCFormat.Create(32, $FF0000, $FF00, $FF);
  FPitch := FWidth * 4;

  FPixels := GetMem(AWidth * AHeight * 4);
  FillChar(FPixels^, AWidth * AHeight * 4, 0);
End;

Destructor TWin32DIB.Destroy;

Begin
  FreeMem(FPixels);
  FreeMem(FBitmapInfo);
  FFormat.Free;
  Inherited Destroy;
End;
