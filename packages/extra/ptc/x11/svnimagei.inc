
{$WARNING this should be in the IPC unit!!!}
Const
{  IPC_CREAT = $200;
  IPC_EXCL = $400;
  IPC_NOWAIT = $800;}

  IPC_PRIVATE = 0;

Constructor TX11Image.Create(display : PDisplay; screen, width, height : Integer; format : TPTCFormat);

Begin
  m_width := width;
  m_height := height;
  m_disp := display;
  m_image := Nil;
End;

Destructor TX11Image.Destroy;

Begin
  Inherited Destroy;
End;

Constructor TX11NormalImage.Create(display : PDisplay; screen, width, height : Integer; format : TPTCFormat);

Var
  xpad, xpitch : Integer;
  tmp_m_pixels : PChar;

Begin
  { cerr << "Creating normal image" << endl << flush; }
  m_image := Nil;
  m_pixels := Nil;
  Inherited Create(display, screen, width, height, format);
  xpad := format.bits;
  If format.bits = 24 Then
    xpad := 32;
  xpitch := width * format.bits Div 8;
  Inc(xpitch, 3);
  xpitch := xpitch And (Not 3);
  m_pixels := GetMem(xpitch * height);
  Pointer(tmp_m_pixels) := Pointer(m_pixels);
  m_image := XCreateImage(display, DefaultVisual(display, screen),
                          DefaultDepth(display, screen),
			  ZPixmap, 0, tmp_m_pixels,
			  width, height, xpad, 0);
  If m_image = Nil Then
    Raise TPTCError.Create('cannot create XImage');
End;

Destructor TX11NormalImage.Destroy;

Begin
  If m_image <> Nil Then
  Begin
    { Restore XImage's buffer pointer }
    m_image^.data := Nil;
    XDestroyImage(m_image);
  End;
  If m_pixels <> Nil Then
    FreeMem(m_pixels);
  Inherited Destroy;
End;

Procedure TX11NormalImage.put(w : TWindow; gc : TGC; x, y : Integer);

Begin
  XPutImage(m_disp, w, gc, m_image, 0, 0, x, y, m_width, m_height);
  XSync(m_disp, False);
End;

Procedure TX11NormalImage.put(w : TWindow; gc : TGC; sx, sy, dx, dy,
                    width, height : Integer);

Begin
  XPutImage(m_disp, w, gc, m_image, sx, sy, dx, dy, width, height);
  XSync(m_disp, False);
End;

Function TX11NormalImage.lock : Pointer;

Begin
  lock := m_pixels;
End;

Function TX11NormalImage.pitch : Integer;

Begin
  pitch := m_image^.bytes_per_line;
End;

{$IFDEF HAVE_X11_EXTENSIONS_XSHM}

Var
  Fshm_error : Boolean;
  Fshm_oldhandler : Function(disp : PDisplay; xev : PXErrorEvent) : Integer; CDecl;

Function Fshm_errorhandler(disp : PDisplay; xev : PXErrorEvent) : Integer; CDecl;

Begin
  If xev^.error_code=BadAccess Then
  Begin
    Fshm_error := True;
    Result := 0;
  End
  Else
    Result := Fshm_oldhandler(disp, xev);
End;

Constructor TX11SHMImage.Create(display : PDisplay; screen, width, height : Integer; format : TPTCFormat);

Begin
  { cerr << "Creating SHM image" << endl << flush; }
  shminfo.shmid := -1;
  shminfo.shmaddr := Pointer(-1);
  FShmAttached := False;
  m_image := Nil;
  Inherited Create(display, screen, width, height, format);
  m_image := XShmCreateImage(display, DefaultVisual(display, screen),
                             DefaultDepth(display, screen),
			     ZPixmap, Nil, @shminfo, width, height);
  If m_image = Nil Then
    Raise TPTCError.Create('cannot create SHM image');

  shminfo.shmid := shmget(IPC_PRIVATE, m_image^.bytes_per_line * m_image^.height,
                          IPC_CREAT Or &777);
  If shminfo.shmid = -1 Then
    Raise TPTCError.Create('cannot get shared memory segment');
  
  shminfo.shmaddr := shmat(shminfo.shmid, Nil, 0);
  shminfo.readOnly := False;
  m_image^.data := shminfo.shmaddr;
  
  If Pointer(shminfo.shmaddr) = Pointer(-1) Then
    Raise TPTCError.Create('cannot allocate shared memory');

  // Try and attach the segment to the server. Bugfix: Have to catch
  // bad access errors in case it runs over the net.
  Fshm_error := False;
  Fshm_oldhandler := XSetErrorHandler(@Fshm_errorhandler);
  Try
    If XShmAttach(display, @shminfo) = 0 Then
      Raise TPTCError.Create('cannot attach shared memory segment to display');

    XSync(display, False);
    If Fshm_error Then
      Raise TPTCError.Create('cannot attach shared memory segment to display');
    FShmAttached := True;
  Finally
    XSetErrorHandler(Fshm_oldhandler);
  End;
End;

Destructor TX11SHMImage.Destroy;

Begin
  If FShmAttached Then
  Begin
    XShmDetach(m_disp, @shminfo);
    XSync(m_disp, False);
  End;
  If m_image <> Nil Then
    XDestroyImage(m_image);
  If Pointer(shminfo.shmaddr) <> Pointer(-1) Then
    shmdt(shminfo.shmaddr);
  If shminfo.shmid <> -1 Then
    shmctl(shminfo.shmid, IPC_RMID, Nil);
  Inherited Destroy;
End;

Procedure TX11SHMImage.put(w : TWindow; gc : TGC; x, y : Integer);

Begin
  XShmPutImage(m_disp, w, gc, m_image, 0, 0, x, y, m_width, m_height, False);
  XSync(m_disp, False);
End;

Procedure TX11SHMImage.put(w : TWindow; gc : TGC; sx, sy, dx, dy,
                    width, height : Integer);

Begin
  XShmPutImage(m_disp, w, gc, m_image, sx, sy, dx, dy, width, height, False);
  XSync(m_disp, False);
End;

Function TX11SHMImage.lock : Pointer;

Begin
  lock := Pointer(shminfo.shmaddr);
End;

Function TX11SHMImage.pitch : Integer;

Begin
  pitch := m_image^.bytes_per_line;
End;
{$ENDIF}
