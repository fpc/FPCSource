Type
  TX11Image = Class(TObject)
  Protected
    FWidth, FHeight : Integer;
    FDisplay : PDisplay;
    FImage : PXImage;
  Public
    Constructor Create(ADisplay : PDisplay; AScreen, AWidth, AHeight : Integer; AFormat : TPTCFormat); Virtual;
    Procedure Put(AWindow : TWindow; AGC : TGC; AX, AY : Integer); Virtual; Abstract;
    Procedure Put(AWindow : TWindow; AGC : TGC; ASX, ASY, ADX, ADY,
                  AWidth, AHeight : Integer); Virtual; Abstract;
    Function Lock : Pointer; Virtual; Abstract;
    Function Pitch : Integer; Virtual; Abstract;
    Function Name : String; Virtual; Abstract;
  End;

  TX11NormalImage = Class(TX11Image)
  Private
    FPixels : PUint8;
  Public
    Constructor Create(ADisplay : PDisplay; AScreen, AWidth, AHeight : Integer; AFormat : TPTCFormat); Override;
    Destructor Destroy; Override;
    Procedure Put(AWindow : TWindow; AGC : TGC; AX, AY : Integer); Override;
    Procedure Put(AWindow : TWindow; AGC : TGC; ASX, ASY, ADX, ADY,
                  AWidth, AHeight : Integer); Override;
    Function Lock : Pointer; Override;
    Function Pitch : Integer; Override;
    Function Name : String; Override;
  End;

{$IFDEF ENABLE_X11_EXTENSION_XSHM}
  TX11ShmImage = Class(TX11Image)
  Private
    FShmInfo : TXShmSegmentInfo;
    FShmAttached : Boolean;
  Public
    Constructor Create(ADisplay : PDisplay; AScreen, AWidth, AHeight : Integer; AFormat : TPTCFormat); Override;
    Destructor Destroy; Override;
    Procedure Put(AWindow : TWindow; AGC : TGC; AX, AY : Integer); Override;
    Procedure Put(AWindow : TWindow; AGC : TGC; ASX, ASY, ADX, ADY,
                  AWidth, AHeight : Integer); Override;
    Function Lock : Pointer; Override;
    Function Pitch : Integer; Override;
    Function Name : String; Override;
  End;
{$ENDIF ENABLE_X11_EXTENSION_XSHM}
