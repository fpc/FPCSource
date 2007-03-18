{$IFDEF ENABLE_X11_EXTENSION_XF86DGA2}

Type
  TX11DGA2Display = Class(TX11Display)
  Private
    Function NextEvent(Var event : TPTCEvent; wait : Boolean; Const EventMask : TPTCEventMask) : Boolean; Override;
    Function PeekEvent(wait : Boolean; Const EventMask : TPTCEventMask) : TPTCEvent; Override;

    Procedure HandleEvents;

    { The list of available modes (todo: move to local vars in the open function) }
    FXDGAModes : PXDGAMode;
    FXDGAModesNum : cint;

    { Holds the pointer to the framebuffer and all the other information for
      the current mode (or nil, if a mode isn't open) }
    FXDGADevice : PXDGADevice;

    { Coordinates of upper left frame corner }
    m_destx, m_desty : Integer;

    FModeIsSet : Boolean;
    FFramebufferIsOpen : Boolean;
  Public
    Constructor Create(ADisplay : PDisplay; AScreen : Integer; Const AFlags : TX11Flags); Override;
    Destructor Destroy; Override;

    Procedure open(title : String; _width, _height : Integer; Const _format : TPTCFormat); Override;
    Procedure open(w : TWindow; Const _format : TPTCFormat); Override;
    Procedure open(_window : TWindow; Const _format : TPTCFormat; x, y, w, h : Integer); Override;
    Procedure close; Override;
    Procedure GetModes(Var AModes : TPTCModeDynArray); Override;
    Procedure update; Override;
    Procedure update(Const _area : TPTCArea); Override;
    Function lock : Pointer; Override;
    Procedure unlock; Override;
    Procedure palette(Const _palette : TPTCPalette); Override;
    Function GetPitch : Integer; Override;
    Function getX11Window : TWindow; Override;
    Function isFullScreen : Boolean; Override;
    Procedure SetCursor(visible : Boolean); Override;
  End;

{$ENDIF ENABLE_X11_EXTENSION_XF86DGA2}
