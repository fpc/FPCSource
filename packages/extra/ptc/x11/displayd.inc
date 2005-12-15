Type
  TX11Display = Class(TObject)
  Protected
    Procedure internal_ReadKey(k : TPTCKey); Virtual; Abstract;
    Function internal_PeekKey(k : TPTCKey) : Boolean; Virtual; Abstract;
    
    Function getFormat(Const _format : TPTCFormat) : TPTCFormat;
    
    { initialise the keyboard mapping table }
    Procedure setKeyMapping;
    
    { Conversion object }
    m_copy : TPTCCopy;
    m_clear : TPTCClear;
    m_palette : TPTCPalette;
    
    m_area : TPTCArea;
    m_clip : TPTCArea;
    
    m_flags : LongInt;
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
    
    { Data access }
    Function width : Integer;
    Function height : Integer;
    Function pitch : Integer; Virtual; Abstract;
    Function clip : TPTCArea;
    Function area : TPTCArea;
    Function format : TPTCFormat;
    
    { Set flags (only used internally now!) }
    Procedure flags(_flags : LongInt);
    
    { X11 helper functions for your enjoyment }
    
    { return the display we are using }
    Function getX11Display : PDisplay;
    
    { return the screen we are using }
    Function getX11Screen : Integer;
    
    { return our window (0 if DGA) }
    Function getX11Window : TWindow; Virtual; Abstract;
  End;
