Const
{$WARNING this belongs to the ipc unit}
  IPC_PRIVATE = 0;

Constructor TX11Image.Create(ADisplay : PDisplay; AScreen, AWidth, AHeight : Integer; AFormat : TPTCFormat);

Begin
  FWidth := AWidth;
  FHeight := AHeight;
  FDisplay := ADisplay;
End;

Constructor TX11NormalImage.Create(ADisplay : PDisplay; AScreen, AWidth, AHeight : Integer; AFormat : TPTCFormat);

Var
  xpad, xpitch : Integer;
  tmp_FPixels : PChar;

Begin
  Inherited;

  xpad := AFormat.Bits;
  If AFormat.Bits = 24 Then
    xpad := 32;
  xpitch := AWidth * AFormat.Bits Div 8;
  Inc(xpitch, 3);
  xpitch := xpitch And (Not 3);
  FPixels := GetMem(xpitch * AHeight);
  Pointer(tmp_FPixels) := Pointer(FPixels);
  FImage := XCreateImage(ADisplay, DefaultVisual(ADisplay, AScreen),
                         DefaultDepth(ADisplay, AScreen),
                         ZPixmap, 0, tmp_FPixels,
                         AWidth, AHeight, xpad, 0);
  If FImage = Nil Then
    Raise TPTCError.Create('cannot create XImage');
End;

Destructor TX11NormalImage.Destroy;

Begin
  If FImage <> Nil Then
  Begin
    { Restore XImage's buffer pointer }
    FImage^.data := Nil;
    XDestroyImage(FImage);
  End;

  If FPixels <> Nil Then
    FreeMem(FPixels);

  Inherited Destroy;
End;

Procedure TX11NormalImage.Put(AWindow : TWindow; AGC : TGC; AX, AY : Integer);

Begin
  XPutImage(FDisplay, AWindow, AGC, FImage, 0, 0, AX, AY, FWidth, FHeight);
  XSync(FDisplay, False);
End;

Procedure TX11NormalImage.Put(AWindow : TWindow; AGC : TGC; ASX, ASY, ADX, ADY,
                    AWidth, AHeight : Integer);

Begin
  XPutImage(FDisplay, AWindow, AGC, FImage, ASX, ASY, ADX, ADY, AWidth, AHeight);
  XSync(FDisplay, False);
End;

Function TX11NormalImage.Lock : Pointer;

Begin
  Result := FPixels;
End;

Function TX11NormalImage.Pitch : Integer;

Begin
  Result := FImage^.bytes_per_line;
End;

Function TX11NormalImage.Name : String;

Begin
  Result := 'XImage';
End;

{$IFDEF ENABLE_X11_EXTENSION_XSHM}

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

Constructor TX11ShmImage.Create(ADisplay : PDisplay; AScreen, AWidth, AHeight : Integer; AFormat : TPTCFormat);

Begin
  Inherited;

  FShmInfo.shmid := -1;
  FShmInfo.shmaddr := Pointer(-1);
  FImage := XShmCreateImage(ADisplay, DefaultVisual(ADisplay, AScreen),
                            DefaultDepth(ADisplay, AScreen),
                            ZPixmap, Nil, @FShmInfo, AWidth, AHeight);
  If FImage = Nil Then
    Raise TPTCError.Create('cannot create SHM image');

  FShmInfo.shmid := shmget(IPC_PRIVATE, FImage^.bytes_per_line * FImage^.height,
                           IPC_CREAT Or &777);
  If FShmInfo.shmid = -1 Then
    Raise TPTCError.Create('cannot get shared memory segment');

  FShmInfo.shmaddr := shmat(FShmInfo.shmid, Nil, 0);
  FShmInfo.readOnly := False;
  FImage^.data := FShmInfo.shmaddr;

  If Pointer(FShmInfo.shmaddr) = Pointer(-1) Then
    Raise TPTCError.Create('cannot allocate shared memory');

  // Try and attach the segment to the server. Bugfix: Have to catch
  // bad access errors in case it runs over the net.
  Fshm_error := False;
  Fshm_oldhandler := XSetErrorHandler(@Fshm_errorhandler);
  Try
    If XShmAttach(ADisplay, @FShmInfo) = 0 Then
      Raise TPTCError.Create('cannot attach shared memory segment to display');

    XSync(ADisplay, False);
    If Fshm_error Then
      Raise TPTCError.Create('cannot attach shared memory segment to display');
    FShmAttached := True;
  Finally
    XSetErrorHandler(Fshm_oldhandler);
  End;
End;

Destructor TX11ShmImage.Destroy;

Begin
  If FShmAttached Then
  Begin
    XShmDetach(FDisplay, @FShmInfo);
    XSync(FDisplay, False);
  End;
  If FImage <> Nil Then
    XDestroyImage(FImage);
  If Pointer(FShmInfo.shmaddr) <> Pointer(-1) Then
    shmdt(FShmInfo.shmaddr);
  If FShmInfo.shmid <> -1 Then
    shmctl(FShmInfo.shmid, IPC_RMID, Nil);

  Inherited Destroy;
End;

Procedure TX11ShmImage.Put(AWindow : TWindow; AGC : TGC; AX, AY : Integer);

Begin
  XShmPutImage(FDisplay, AWindow, AGC, FImage, 0, 0, AX, AY, FWidth, FHeight, False);
  XSync(FDisplay, False);
End;

Procedure TX11ShmImage.Put(AWindow : TWindow; AGC : TGC; ASX, ASY, ADX, ADY,
                    AWidth, AHeight : Integer);

Begin
  XShmPutImage(FDisplay, AWindow, AGC, FImage, ASX, ASY, ADX, ADY, FWidth, FHeight, False);
  XSync(FDisplay, False);
End;

Function TX11ShmImage.Lock : Pointer;

Begin
  Result := Pointer(FShmInfo.shmaddr);
End;

Function TX11ShmImage.Pitch : Integer;

Begin
  Result := FImage^.bytes_per_line;
End;

Function TX11ShmImage.Name : String;

Begin
  Result := 'MIT-Shm';
End;
{$ENDIF ENABLE_X11_EXTENSION_XSHM}
