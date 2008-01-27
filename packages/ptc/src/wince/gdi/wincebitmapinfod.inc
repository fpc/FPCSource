Type
  TWinCEBitmapInfo = Class(TObject)
  Private
    FBitmapInfo : PBITMAPINFO;
//    FPixels : Pointer;
    FFormat : TPTCFormat;
    FWidth, FHeight, FPitch : Integer;
  Public
    Constructor Create(AWidth, AHeight : Integer);
    Destructor Destroy; Override;
    Property BMI : PBITMAPINFO Read FBitmapInfo;
    Property Width : Integer Read FWidth;
    Property Height : Integer Read FHeight;
    Property Pitch : Integer Read FPitch;
    Property Format : TPTCFormat Read FFormat;
//    Property Pixels : Pointer Read FPixels;
  End;
