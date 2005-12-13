Type
  TX11Image = Class(TObject)
  Protected
    m_width, m_height : DWord;
    m_disp : PDisplay;
    m_image : PXImage;
  Public
    Constructor Create(display : PDisplay; screen, width, height : Integer; format : TPTCFormat);
    Destructor Destroy; Override;
    Procedure put(w : TWindow; gc : TGC; x, y : Integer); Virtual; Abstract;
    Procedure put(w : TWindow; gc : TGC; sx, sy, dx, dy,
                  width, height : Integer); Virtual; Abstract;
    Function lock : Pointer; Virtual; Abstract;
    Function pitch : Integer; Virtual; Abstract;
  End;
  TX11NormalImage = Class(TX11Image)
  Private
    m_pixels : Pchar8;
  Public
    Constructor Create(display : PDisplay; screen, width, height : Integer; format : TPTCFormat);
    Destructor Destroy; Override;
    Procedure put(w : TWindow; gc : TGC; x, y : Integer); Override;
    Procedure put(w : TWindow; gc : TGC; sx, sy, dx, dy,
                  width, height : Integer); Override;
    Function lock : Pointer; Override;
    Function pitch : Integer; Override;
  End;
{$IFDEF HAVE_X11_EXTENSIONS_XSHM}
  TX11SHMImage = Class(TX11Image)
  Public
    Constructor Create(display : PDisplay; screen, width, height : Integer; format : TPTCFormat);
    Destructor Destroy; Override;
    Procedure put(w : TWindow; gc : TGC; x, y : Integer); Override;
    Procedure put(w : TWindow; gc : TGC; sx, sy, dx, dy,
                  width, height : Integer); Override;
    Function lock : Pointer; Override;
    Function pitch : Integer; Override;
  Private
    shminfo : TXShmSegmentInfo;
    FShmAttached : Boolean;
  End;
{$ENDIF}
