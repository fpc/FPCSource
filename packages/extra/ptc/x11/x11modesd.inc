Type
  TX11Modes = Class(TObject)
  Private
    FDisplay : PDisplay;
    FScreen : cint;
  Protected
    Function GetWidth : Integer; Virtual; Abstract;
    Function GetHeight : Integer; Virtual; Abstract;
  Public
    Constructor Create(ADisplay : PDisplay; AScreen : cint); Virtual;
    Procedure GetModes(Var AModes : TPTCModeDynArray; ACurrentDesktopFormat : TPTCFormat); Virtual; Abstract;
    Procedure SetBestMode(AWidth, AHeight : Integer); Virtual; Abstract;
    Procedure RestorePreviousMode; Virtual; Abstract;
    Property Width : Integer Read GetWidth;
    Property Height : Integer Read GetHeight;
  End;

  TX11ModesNoModeSwitching = Class(TX11Modes)
  Private
    FWidth, FHeight : Integer;
  Protected
    Function GetWidth : Integer; Override;
    Function GetHeight : Integer; Override;
  Public
    Constructor Create(ADisplay : PDisplay; AScreen : cint); Override;
    Procedure GetModes(Var AModes : TPTCModeDynArray; ACurrentDesktopFormat : TPTCFormat); Override;
    Procedure SetBestMode(AWidth, AHeight : Integer); Override;
    Procedure RestorePreviousMode; Override;
  End;

{$IFDEF ENABLE_X11_EXTENSION_XRANDR}
  TX11ModesXrandr = Class(TX11Modes)
  Private
    FRoot : TWindow;
    FXRRConfig : PXRRScreenConfiguration;
  Protected
    Function GetWidth : Integer; Override;
    Function GetHeight : Integer; Override;
  Public
    Constructor Create(ADisplay : PDisplay; AScreen : cint); Override;
    Destructor Destroy; Override;
    Procedure GetModes(Var AModes : TPTCModeDynArray; ACurrentDesktopFormat : TPTCFormat); Override;
    Procedure SetBestMode(AWidth, AHeight : Integer); Override;
    Procedure RestorePreviousMode; Override;
  End;
{$ENDIF ENABLE_X11_EXTENSION_XRANDR}

{$IFDEF ENABLE_X11_EXTENSION_XF86VIDMODE}
  TX11ModesXF86VidMode = Class(TX11Modes)
  Private
    FModeList : PPXF86VidModeModeInfo;
    FModeListCount : cint;
    FSavedMode : PXF86VidModeModeLine;
    FSavedDotClock : cint;
    FWidth, FHeight : Integer;

    Procedure RetrieveModeList;
    Function FindNumberOfBestMode(AWidth, AHeight : Integer) : Integer;
  Protected
    Function GetWidth : Integer; Override;
    Function GetHeight : Integer; Override;
  Public
    Constructor Create(ADisplay : PDisplay; AScreen : cint); Override;
    Destructor Destroy; Override;
    Procedure GetModes(Var AModes : TPTCModeDynArray; ACurrentDesktopFormat : TPTCFormat); Override;
    Procedure SetBestMode(AWidth, AHeight : Integer); Override;
    Procedure RestorePreviousMode; Override;
  End;
{$ENDIF ENABLE_X11_EXTENSION_XF86VIDMODE}
