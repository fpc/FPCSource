Type
  TX11WindowDisplay = Class(TX11Display)
  Private
    Function NextEvent(Var AEvent : TPTCEvent; AWait : Boolean; Const AEventMask : TPTCEventMask) : Boolean; Override;
    Function PeekEvent(AWait : Boolean; Const AEventMask : TPTCEventMask) : TPTCEvent; Override;

    Procedure EnterFullScreen;
    Procedure LeaveFullScreen;
    Procedure internal_ShowCursor(AVisible : Boolean);
    Procedure HandleChangeFocus(ANewFocus : Boolean);
    Procedure HandleEvents;
    Function CreateImage(ADisplay : PDisplay; AScreen, AWidth, AHeight : Integer;
                         AFormat : TPTCFormat) : TX11Image; { Factory method }
    Function CreateModeSwitcher : TX11Modes; { Factory method }
    Procedure CreateColormap; { Register colour maps }
    {eventHandler}
    FWindow : TWindow;
    FPrimary : TX11Image;
    FDestX, FDestY : Integer;
    FGC : TGC;
    FAtomClose : TAtom; { X Atom for close window button }
    FCursorVisible : Boolean;
    FX11InvisibleCursor : TCursor; { Blank cursor }
    FFullScreen : Boolean; { Keeps a snapshot of the PTC_X11_FULLSCREEN option
                             taken at the time 'open' was called }
    FFocus : Boolean;
    FModeSwitcher : TX11Modes;

    FPreviousMouseButtonState : TPTCMouseButtonState;
    FPreviousMouseX, FPreviousMouseY : Integer; { for calculating the deltas }
    FPreviousMousePositionSaved : Boolean; { true, if FPreviousMouseX,
           FPreviousMouseY and FPreviousMouseButtonState contain valid values }
  Public
    Constructor Create(ADisplay : PDisplay; AScreen : Integer; Const AFlags : TX11Flags); Override;
    Destructor Destroy; Override;

    Procedure Open(ATitle : String; AWidth, AHeight : Integer; Const AFormat : TPTCFormat); Override;
    Procedure Open(AWindow : TWindow; Const AFormat : TPTCFormat); Override;
    Procedure Open(AWindow : TWindow; Const AFormat : TPTCFormat; AX, AY, AWidth, AHeight : Integer); Override;
    Procedure Close; Override;
    Procedure Update; Override;
    Procedure Update(Const AArea : TPTCArea); Override;
    Function Lock : Pointer; Override;
    Procedure Unlock; Override;
    Procedure GetModes(Var AModes : TPTCModeDynArray); Override;
    Procedure Palette(Const APalette : TPTCPalette); Override;
    Function GetPitch : Integer; Override;
    Function GetX11Window : TWindow; Override;
    Function GetX11GC : TGC; Virtual;
    Function IsFullScreen : Boolean; Override;
    Procedure SetCursor(AVisible : Boolean); Override;
  End;
