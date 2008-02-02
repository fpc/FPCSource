{$IFDEF ENABLE_X11_EXTENSION_XF86DGA1}

Type
  TX11DGA1Display = Class(TX11Display)
  Private
    Function NextEvent(Var AEvent : TPTCEvent; AWait : Boolean; Const AEventMask : TPTCEventMask) : Boolean; Override;
    Function PeekEvent(AWait : Boolean; Const AEventMask : TPTCEventMask) : TPTCEvent; Override;

    Procedure HandleEvents;

    FModeInfo : PPXF86VidModeModeInfo;
    FModeInfoNum : Integer;
    FPreviousMode : Integer;

    FDGAAddr : PByte;
    FDGALineWidth : Integer;
    FDGABankSize : Integer;
    FDGAMemSize : Integer;
    FDGAWidth, FDGAHeight : Integer;

    { Coordinates of upper left frame corner }
    FDestX, FDestY : Integer;

    FInDirect, FInMode : Boolean;
  Public
    Constructor Create(ADisplay : PDisplay; AScreen : Integer; Const AFlags : TX11Flags); Override;
    Destructor Destroy; Override;

    Procedure Open(ATitle : String; AWidth, AHeight : Integer; Const AFormat : TPTCFormat); Override;
    Procedure Open(AWindow : TWindow; Const AFormat : TPTCFormat); Override;
    Procedure Open(AWindow : TWindow; Const AFormat : TPTCFormat; AX, AY, AWidth, AHeight : Integer); Override;
    Procedure Close; Override;
    Procedure GetModes(Var AModes : TPTCModeDynArray); Override;
    Procedure Update; Override;
    Procedure Update(Const AArea : TPTCArea); Override;
    Function Lock : Pointer; Override;
    Procedure Unlock; Override;
    Procedure Palette(Const APalette : TPTCPalette); Override;
    Function GetPitch : Integer; Override;
    Function GetX11Window : TWindow; Override;
    Function IsFullScreen : Boolean; Override;
    Procedure SetCursor(AVisible : Boolean); Override;
  End;

{$ENDIF ENABLE_X11_EXTENSION_XF86DGA1}
