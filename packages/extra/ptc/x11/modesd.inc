Type
  TX11Modes = Class(TObject)
  Private
    FDisplay : PDisplay;
    FScreen : cint;

    FModeList : PPXF86VidModeModeInfo;
    FModeListCount : cint;
    FSavedMode : PXF86VidModeModeLine;
    FSavedDotClock : cint;

    Procedure GetModes;
    Function FindNumberOfBestMode(width, height : Integer) : Integer;
  Public
    Constructor Create(display : PDisplay; screen : cint);
    Destructor Destroy; Override;
    Procedure SetBestMode(width, height : Integer);
    Procedure RestorePreviousMode;
  End;
