{ Parsed from QTKit.framework QTKitDefines.h }

{$ifdef TYPES}
const
  QTKIT_VERSION_7_0 = 70000;
  QTKIT_VERSION_7_2 = 70200;
  QTKIT_VERSION_7_6_3 = 70603;
  QTKIT_VERSION_7_6_6 = 70606;
  QTKIT_VERSION_7_7 = 70700;
  QTKIT_VERSION_7_7_2 = 70702;
  QTKIT_VERSION_7_7_3 = 70703;
{$endif}

{$ifndef QTKIT_VERSION_MIN_REQUIRED}

{$if (MAC_OS_X_VERSION_MIN_REQUIRED >= MAC_OS_X_VERSION_10_9)}
{$ifdef TYPES}
const
  QTKIT_VERSION_MIN_REQUIRED = QTKIT_VERSION_7_7_2;
{$endif}

{$elseif (MAC_OS_X_VERSION_MIN_REQUIRED >= MAC_OS_X_VERSION_10_7)}

{$elseif (MAC_OS_X_VERSION_MIN_REQUIRED >= MAC_OS_X_VERSION_10_6)}

{$elseif (MAC_OS_X_VERSION_MIN_REQUIRED >= MAC_OS_X_VERSION_10_5)}

{$else}
{$ifdef TYPES}
const
  QTKIT_VERSION_MIN_REQUIRED = QTKIT_VERSION_7_0;
{$endif}

{$endif}
{$endif}

{$ifndef QTKIT_VERSION_MAX_ALLOWED}
{$ifdef TYPES}
const
  QTKIT_VERSION_MAX_ALLOWED = QTKIT_VERSION_7_7_3;
{$endif}

{$endif}

{$if defined(__LP64__)}
{$ifdef TYPES}
const
  kQTFileTypeAIFF = 'AIFF';
  kQTFileTypeAIFC = 'AIFC';
  kQTFileTypeDVC = 'dvc!';
  kQTFileTypeMIDI = 'Midi';
  kQTFileTypePicture = 'PICT';
  kQTFileTypeMovie = 'MooV';
  kQTFileTypeText = 'TEXT';
  kQTFileTypeWave = 'WAVE';
  kQTFileTypeSystemSevenSound = 'sfil';
  kQTFileTypeMuLaw = 'ULAW';
  kQTFileTypeAVI = 'VfW ';
  kQTFileTypeSoundDesignerII = 'Sd2f';
  kQTFileTypeAudioCDTrack = 'trak';
  kQTFileTypePICS = 'PICS';
  kQTFileTypeGIF = 'GIFf';
  kQTFileTypePNG = 'PNGf';
  kQTFileTypeTIFF = 'TIFF';
  kQTFileTypePhotoShop = '8BPS';
  kQTFileTypeSGIImage = '.SGI';
  kQTFileTypeBMP = 'BMPf';
  kQTFileTypeJPEG = 'JPEG';
  kQTFileTypeJFIF = 'JPEG';
  kQTFileTypeMacPaint = 'PNTG';
  kQTFileTypeTargaImage = 'TPIC';
  kQTFileTypeQuickDrawGXPicture = 'qdgx';
  kQTFileTypeQuickTimeImage = 'qtif';
  kQTFileType3DMF = '3DMF';
  kQTFileTypeFLC = 'FLC ';
  kQTFileTypeFlash = 'SWFL';
  kQTFileTypeFlashPix = 'FPix';
  kQTFileTypeMP4 = 'mpg4';
  kQTFileTypePDF = 'PDF ';
  kQTFileType3GPP = '3gpp';
  kQTFileTypeAMR = 'amr ';
  kQTFileTypeSDV = 'sdv ';
  kQTFileType3GP2 = '3gp2';
  kQTFileTypeAMC = 'amc ';
  kQTFileTypeJPEG2000 = 'jp2 ';
const
  codecLosslessQuality = $00000400;
  codecMaxQuality = $000003FF;
  codecMinQuality = $00000000;
  codecLowQuality = $00000100;
  codecNormalQuality = $00000200;
  codecHighQuality = $00000300;
const
  graphicsModeStraightAlpha = 256;
  graphicsModePreWhiteAlpha = 257;
  graphicsModePreBlackAlpha = 258;
  graphicsModeComposition = 259;
  graphicsModeStraightAlphaBlend = 260;
  graphicsModePreMulColorAlpha = 261;
  graphicsModePerComponentAlpha = 272;
{$endif}

{$endif}
