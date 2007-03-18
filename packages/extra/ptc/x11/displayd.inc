Type
  TX11FlagsEnum = (PTC_X11_FULLSCREEN,
                   PTC_X11_LEAVE_DISPLAY,
                   PTC_X11_LEAVE_WINDOW,
		   PTC_X11_TRY_DGA,
                   PTC_X11_PEDANTIC_DGA,
                   PTC_X11_DITHER,
                   PTC_X11_FULLSCREEN_CURSOR_VISIBLE,
                   PTC_X11_WINDOWED_CURSOR_INVISIBLE);
  TX11Flags = Set Of TX11FlagsEnum;

Type
  TX11Display = Class(TObject)
  Protected
    Function NextEvent(Var event : TPTCEvent; wait : Boolean; Const EventMask : TPTCEventMask) : Boolean; Virtual; Abstract;
    Function PeekEvent(wait : Boolean; Const EventMask : TPTCEventMask) : TPTCEvent; Virtual; Abstract;
    
    Function getFormat(Const _format : TPTCFormat) : TPTCFormat;
    
    { initialise the keyboard mapping table }
    Procedure setKeyMapping;
    
    { Conversion object }
    m_copy : TPTCCopy;
    m_clear : TPTCClear;
    m_palette : TPTCPalette;
    
    m_area : TPTCArea;
    m_clip : TPTCArea;
    
    FEventQueue : TEventQueue;
    
    m_flags : TX11Flags;
    m_width, m_height : DWord;
    m_format : TPTCFormat;
    
    m_disp : PDisplay;
    m_screen : Integer;
    
    m_cmap : TColormap;
    m_colours : PXColor;
    
    m_functionkeys : PInteger;
    m_normalkeys : PInteger;
    
    {m_thread : pthread_t;}
  Public
    Constructor Create;
    Destructor Destroy; Override;
    
    {checkDGA}
    
    Procedure open(title : String; _width, _height : Integer; Const _format : TPTCFormat; disp : PDisplay; screen : Integer); Virtual; Abstract;
    
    { This will always return a windowed console. The first version
      fills the whole window, the second one has a custom size }
    Procedure open(disp : PDisplay; screen : Integer; w : TWindow; Const _format : TPTCFormat); Virtual; Abstract;
    Procedure open(disp : PDisplay; screen : Integer; _window : TWindow; Const _format : TPTCFormat; x, y, w, h : Integer); Virtual; Abstract;

    Procedure close; Virtual; Abstract;
    
    Procedure update; Virtual; Abstract;
    Procedure update(Const _area : TPTCArea); Virtual; Abstract;
    
    Function lock : Pointer; Virtual; Abstract;
    Procedure unlock; Virtual; Abstract;
    
    { load pixels to console }
    Procedure load(Const pixels : Pointer; _width, _height, _pitch : Integer;
                   Const _format : TPTCFormat; Const _palette : TPTCPalette); Virtual;
    Procedure load(Const pixels : Pointer; _width, _height, _pitch : Integer;
                   Const _format : TPTCFormat; Const _palette : TPTCPalette;
		   Const source, destination : TPTCArea); Virtual;
    
    { save console pixels }
    Procedure save(pixels : Pointer; _width, _height, _pitch : Integer;
                   Const _format : TPTCFormat; Const _palette : TPTCPalette); Virtual;
    Procedure save(pixels : Pointer; _width, _height, _pitch : Integer;
                   Const _format : TPTCFormat; Const _palette : TPTCPalette;
		   Const source, destination : TPTCArea); Virtual;
    
    { clear surface }
    Procedure clear(Const color : TPTCColor); Virtual;
    Procedure clear(Const color : TPTCColor; Const _area : TPTCArea); Virtual;
    
    { Console palette }
    Procedure palette(Const _palette : TPTCPalette); Virtual; Abstract;
    Function palette : TPTCPalette; Virtual;

    { console clip area }
    Procedure clip(Const _area : TPTCArea);
    
    { cursor control }
    Procedure SetCursor(visible : Boolean); Virtual; Abstract;
    
    { Data access }
    Function width : Integer;
    Function height : Integer;
    Function pitch : Integer; Virtual; Abstract;
    Function clip : TPTCArea;
    Function area : TPTCArea;
    Function format : TPTCFormat;
    
    Function isFullScreen : Boolean; Virtual; Abstract;
    
    { Set flags (only used internally now!) }
    Procedure flags(_flags : TX11Flags);
    
    { X11 helper functions for your enjoyment }
    
    { return the display we are using }
    Function getX11Display : PDisplay;
    
    { return the screen we are using }
    Function getX11Screen : Integer;
    
    { return our window (0 if DGA) }
    Function getX11Window : TWindow; Virtual; Abstract;
  End;
