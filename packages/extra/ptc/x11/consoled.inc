Const
  PTC_X11_NODGA = 1;
  PTC_X11_LEAVE_DISPLAY = 2;
  PTC_X11_LEAVE_WINDOW = 4;
  PTC_X11_PEDANTIC_DGA = 8;
  PTC_X11_DITHER = 16;

Type
  TX11Console = Class(TPTCBaseConsole)
  Private
    Procedure setTitle(_title : String);
    x11disp : TX11Display;
    m_title : String;
    m_flags : LongInt;
    m_modes : Array[0..255] Of TPTCMode;
  Protected
    Procedure internal_ReadKey(k : TPTCKey); Override;
    Function internal_PeekKey(k : TPTCKey) : Boolean; Override;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure configure(Const _file : String); Override;
    Function option(Const _option : String) : Boolean; Override;
    Function modes : PPTCMode; Override;
    Procedure open(Const _title : String; _pages : Integer); Overload; Override;
    Procedure open(Const _title : String; Const _format : TPTCFormat;
		   _pages : Integer); Overload; Override;
    Procedure open(Const _title : String; _width, _height : Integer;
		   Const _format : TPTCFormat; _pages : Integer); Overload; Override;
    Procedure open(Const _title : String; Const _mode : TPTCMode;
		   _pages : Integer); Overload; Override;
    Procedure close; Override;
    Procedure flush; Override;
    Procedure finish; Override;
    Procedure update; Override;
    Procedure update(Const _area : TPTCArea); Override;
    Procedure copy(Var surface : TPTCBaseSurface); Override;
    Procedure copy(Var surface : TPTCBaseSurface;
		   Const source, destination : TPTCArea); Override;
    Function lock : Pointer; Override;
    Procedure unlock; Override;
    Procedure load(Const pixels : Pointer;
		   _width, _height, _pitch : Integer;
		   Const _format : TPTCFormat;
		   Const _palette : TPTCPalette); Override;
    Procedure load(Const pixels : Pointer;
		   _width, _height, _pitch : Integer;
		   Const _format : TPTCFormat;
		   Const _palette : TPTCPalette;
		   Const source, destination : TPTCArea); Override;
    Procedure save(pixels : Pointer;
		   _width, _height, _pitch : Integer;
		   Const _format : TPTCFormat;
		   Const _palette : TPTCPalette); Override;
    Procedure save(pixels : Pointer;
		   _width, _height, _pitch : Integer;
		   Const _format : TPTCFormat;
		   Const _palette : TPTCPalette;
		   Const source, destination : TPTCArea); Override;
    Procedure clear; Override;
    Procedure clear(Const color : TPTCColor); Override;
    Procedure clear(Const color : TPTCColor;
		    Const _area : TPTCArea); Override;
    Procedure palette(Const _palette : TPTCPalette); Override;
    Function palette : TPTCPalette; Override;
    Procedure clip(Const _area : TPTCArea); Override;
    Function width : Integer; Override;
    Function height : Integer; Override;
    Function pitch : Integer; Override;
    Function pages : Integer; Override;
    Function area : TPTCArea; Override;
    Function clip : TPTCArea; Override;
    Function format : TPTCFormat; Override;
    Function name : String; Override;
    Function title : String; Override;
    Function information : String; Override;
  End;
