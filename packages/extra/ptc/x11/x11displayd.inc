Type
  TX11FlagsEnum = (PTC_X11_FULLSCREEN,
                   PTC_X11_LEAVE_DISPLAY,
                   PTC_X11_LEAVE_WINDOW,
                   PTC_X11_TRY_DGA,
                   PTC_X11_TRY_XF86VIDMODE,
                   PTC_X11_TRY_XRANDR,
                   PTC_X11_TRY_XSHM,
                   PTC_X11_DITHER,
                   PTC_X11_FULLSCREEN_CURSOR_VISIBLE,
                   PTC_X11_WINDOWED_CURSOR_INVISIBLE);
  TX11Flags = Set Of TX11FlagsEnum;

Type
  TX11Display = Class(TObject)
  Protected
    Function NextEvent(Var AEvent : TPTCEvent; AWait : Boolean; Const AEventMask : TPTCEventMask) : Boolean; Virtual; Abstract;
    Function PeekEvent(AWait : Boolean; Const AEventMask : TPTCEventMask) : TPTCEvent; Virtual; Abstract;

    Function GetX11Format(Const AFormat : TPTCFormat) : TPTCFormat;

    { initialise the keyboard mapping table }
    Procedure SetKeyMapping;

    { Data access }
    Function GetWidth : Integer;
    Function GetHeight : Integer;
    Function GetPitch : Integer; Virtual; Abstract;
    Function GetFormat : TPTCFormat;
    Function GetArea : TPTCArea;

    { Conversion object }
    FCopy : TPTCCopy;
    FClear : TPTCClear;
    FPalette : TPTCPalette;

    FArea : TPTCArea;
    FClip : TPTCArea;

    FEventQueue : TEventQueue;

    FFlags : TX11Flags;
    FWidth, FHeight : DWord;
    FFormat : TPTCFormat;

    FDisplay : PDisplay;
    FScreen : Integer;

    FCMap : TColormap;
    FColours : PXColor;

    FFunctionKeys : PInteger;
    FNormalKeys : PInteger;

    {m_thread : pthread_t;}
  Public
    Constructor Create(ADisplay : PDisplay; AScreen : Integer; Const AFlags : TX11Flags); Virtual;
    Destructor Destroy; Override;

    Procedure Open(ATitle : String; AWidth, AHeight : Integer; Const AFormat : TPTCFormat); Virtual; Abstract;

    { This will always return a windowed console. The first version
      fills the whole window, the second one has a custom size }
    Procedure Open(AWindow : TWindow; Const AFormat : TPTCFormat); Virtual; Abstract;
    Procedure Open(AWindow : TWindow; Const AFormat : TPTCFormat; AX, AY, AWidth, AHeight : Integer); Virtual; Abstract;

    Procedure Close; Virtual; Abstract;

    Procedure Update; Virtual; Abstract;
    Procedure Update(Const AArea : TPTCArea); Virtual; Abstract;

    Function Lock : Pointer; Virtual; Abstract;
    Procedure Unlock; Virtual; Abstract;

    { load pixels to console }
    Procedure Load(Const APixels : Pointer; AWidth, AHeight, APitch : Integer;
                   Const AFormat : TPTCFormat; Const APalette : TPTCPalette); Virtual;
    Procedure Load(Const APixels : Pointer; AWidth, AHeight, APitch : Integer;
                   Const AFormat : TPTCFormat; Const APalette : TPTCPalette;
                   Const ASource, ADestination : TPTCArea); Virtual;

    { save console pixels }
    Procedure Save(APixels : Pointer; AWidth, AHeight, APitch : Integer;
                   Const AFormat : TPTCFormat; Const APalette : TPTCPalette); Virtual;
    Procedure Save(APixels : Pointer; AWidth, AHeight, APitch : Integer;
                   Const AFormat : TPTCFormat; Const APalette : TPTCPalette;
                   Const ASource, ADestination : TPTCArea); Virtual;

    { clear surface }
    Procedure Clear(Const AColor : TPTCColor); Virtual;
    Procedure Clear(Const AColor : TPTCColor; Const AArea : TPTCArea); Virtual;

    { Console palette }
    Procedure Palette(Const APalette : TPTCPalette); Virtual; Abstract;
    Function Palette : TPTCPalette; Virtual;

    { console clip area }
    Procedure Clip(Const AArea : TPTCArea);

    { cursor control }
    Procedure SetCursor(AVisible : Boolean); Virtual; Abstract;

    { Data access }
    Function Clip : TPTCArea;

    Function IsFullScreen : Boolean; Virtual; Abstract;

    { Set flags (only used internally now!) }
    Procedure SetFlags(AFlags : TX11Flags);

    Procedure GetModes(Var AModes : TPTCModeDynArray); Virtual; Abstract;

    { X11 helper functions for your enjoyment }

    { return the display we are using }
    Function GetX11Display : PDisplay;

    { return the screen we are using }
    Function GetX11Screen : Integer;

    { return our window (0 if DGA) }
    Function GetX11Window : TWindow; Virtual; Abstract;

    Property Width : Integer Read GetWidth;
    Property Height : Integer Read GetHeight;
    Property Pitch : Integer Read GetPitch;
    Property Area : TPTCArea Read GetArea;
    Property Format : TPTCFormat Read GetFormat;
  End;
