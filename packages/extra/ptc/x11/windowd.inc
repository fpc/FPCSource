Type
  TX11WindowDisplay = Class(TX11Display)
  Private
    Procedure internal_ReadKey(k : TPTCKey); Override;
    Function internal_PeekKey(k : TPTCKey) : Boolean; Override;
    
    Function createImage(disp : PDisplay; screen, _width, _height : Integer;
                         _format : TPTCFormat) : TX11Image; { Factory method }
    Procedure createColormap; { Register colour maps }
    {eventHandler}
    m_has_shm : Boolean;
    m_window : TWindow;
    m_primary : TX11Image;
    m_destx, m_desty : Integer;
    m_gc : TGC;
    m_atom_close : TAtom; { X Atom for close window button }
    m_keypressed : Boolean; { Key pressed since the last call to key() ? }
{    m_keylast : TPTCKey;} { Last key pressed (scancode) }
  Public
    Constructor Create;
    Destructor Destroy; Override;
    
    Procedure open(title : String; _width, _height : Integer; Const _format : TPTCFormat; disp : PDisplay; screen : Integer); Override;
    Procedure open(disp : PDisplay; screen : Integer; w : TWindow; Const _format : TPTCFormat); Override;
    Procedure open(disp : PDisplay; screen : Integer; _window : TWindow; Const _format : TPTCFormat; x, y, w, h : Integer); Override;
    Procedure close; Override;
    Procedure update; Override;
    Procedure update(Const _area : TPTCArea); Override;
    Function lock : Pointer; Override;
    Procedure unlock; Override;
    Procedure palette(Const _palette : TPTCPalette); Override;
    Function pitch : Integer; Override;
    Function getX11Window : TWindow; Override;
    Function getX11GC : TGC; Virtual;
  End;
