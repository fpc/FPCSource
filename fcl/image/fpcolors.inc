const
  BytesNeeded : array[TColorFormat] of byte =
      (1,1,1,1,2,3,1,2,4,2,2,3,4,6,1,2,4,8,2,2,3,4,6,1,2,4,8);

  alphaTransparant = $0000;
  alphaOpaque      = $FFFF;
  clTransparent: TFPColor = (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaTransparant);
  clBlack      : TFPColor = (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque);
  clBlue       : TFPColor = (Red: $0000; Green: $0000; Blue: $ffff; Alpha: alphaOpaque);
  clGreen      : TFPColor = (Red: $0000; Green: $ffff; Blue: $0000; Alpha: alphaOpaque);
  clCyan       : TFPColor = (Red: $0000; Green: $ffff; Blue: $ffff; Alpha: alphaOpaque);
  clRed        : TFPColor = (Red: $ffff; Green: $0000; Blue: $0000; Alpha: alphaOpaque);
  clMagenta    : TFPColor = (Red: $ffff; Green: $0000; Blue: $ffff; Alpha: alphaOpaque);
  clYellow     : TFPColor = (Red: $ffff; Green: $ffff; Blue: $0000; Alpha: alphaOpaque);
  clWhite      : TFPColor = (Red: $ffff; Green: $ffff; Blue: $ffff; Alpha: alphaOpaque);
  clGray       : TFPColor = (Red: $8000; Green: $8000; Blue: $8000; Alpha: alphaOpaque);
  clLtGray     : TFPColor = (Red: $c000; Green: $c000; Blue: $c000; Alpha: alphaOpaque);
  clDkBlue     : TFPColor = (Red: $0000; Green: $0000; Blue: $8000; Alpha: alphaOpaque);
  clDkGreen    : TFPColor = (Red: $0000; Green: $8000; Blue: $0000; Alpha: alphaOpaque);
  clDkCyan     : TFPColor = (Red: $0000; Green: $8000; Blue: $8000; Alpha: alphaOpaque);
  clDkRed      : TFPColor = (Red: $8000; Green: $0000; Blue: $0000; Alpha: alphaOpaque);
  clDkMagenta  : TFPColor = (Red: $8000; Green: $0000; Blue: $8000; Alpha: alphaOpaque);
  clDkYellow   : TFPColor = (Red: $8000; Green: $8000; Blue: $0000; Alpha: alphaOpaque);
  clMaroon     : TFPColor = (Red: $8000; Green: $0000; Blue: $0000; Alpha: alphaOpaque);
  clLtGreen    : TFPColor = (Red: $0000; Green: $8000; Blue: $0000; Alpha: alphaOpaque);
  clOlive      : TFPColor = (Red: $8000; Green: $8000; Blue: $0000; Alpha: alphaOpaque);
  clNavy       : TFPColor = (Red: $0000; Green: $0000; Blue: $8000; Alpha: alphaOpaque);
  clPurple     : TFPColor = (Red: $8000; Green: $0000; Blue: $8000; Alpha: alphaOpaque);
  clTeal       : TFPColor = (Red: $0000; Green: $8000; Blue: $8000; Alpha: alphaOpaque);
  clSilver     : TFPColor = (Red: $c000; Green: $c000; Blue: $c000; Alpha: alphaOpaque);
  clLime       : TFPColor = (Red: $0000; Green: $ffff; Blue: $0000; Alpha: alphaOpaque);
  clFuchsia    : TFPColor = (Red: $ffff; Green: $0000; Blue: $ffff; Alpha: alphaOpaque);
  clAqua       : TFPColor = (Red: $0000; Green: $ffff; Blue: $ffff; Alpha: alphaOpaque);

