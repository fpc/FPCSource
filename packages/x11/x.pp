unit x;

   { Changes to the original conversion marked MVC - Michael Van Canneyt}

{$LinkLib X11}

  interface

    const
       X_PROTOCOL = 11;

       X_PROTOCOL_REVISION = 0;


    type
    { Inserted the following by hand. It was under a ifndef _XSERVER64. All
      were unsigned longs originally
      -- MVC. }
        TXID = cardinal ;
        TMask = cardinal ;
        TAtom = cardinal ;
        TVisualID = cardinal ;
        TTime = cardinal ;
        { End of insert }
       TWindow = TXID;
       TDrawable = TXID;
       TFont = TXID;
       TPixmap = TXID;
       TCursor = TXID;
       TColormap = TXID;
       TGContext = TXID;
       TKeySym = TXID;
       TKeyCode = char;
       PKeyCode = ^TKeyCode;
       { Pointers to all these things. Needed for C calls. MVC }
       PAtom = ^TAtom;
       PPAtom = ^PAtom;
       PWindow = ^TWindow;
       PColormap = ^TColormap;
       PKeySym = ^TKeysym;
       PPixmap = ^TPixmap;

    {*****************************************************************
     * RESERVED RESOURCE AND CONSTANT DEFINITIONS
     *****************************************************************}
    {* universal null resource or null atom *}

    const
       None = 0;
    {* background pixmap in CreateWindow
                                    and ChangeWindowAttributes *}
       ParentRelative = 1;
    {* border pixmap in CreateWindow
                                       and ChangeWindowAttributes
                                   special VisualID and special window
                                       class passed to CreateWindow *}
       CopyFromParent = 0;
    {* destination window in SendEvent *}
       PointerWindow = 0;
    {* destination window in SendEvent *}
       InputFocus = 1;
    {* focus window in SetInputFocus *}
       PointerRoot = 1;
    {* special Atom, passed to GetProperty *}
       AnyPropertyType = 0;
    {* special Key Code, passed to GrabKey *}
       AnyKey = 0;
    {* special Button Code, passed to GrabButton *}
       AnyButton = 0;
    {* special Resource ID passed to KillClient *}
       AllTemporary = 0;
    {* special Time *}
       CurrentTime = 0;
    {* special KeySym *}
    {*****************************************************************
     * EVENT DEFINITIONS
     *****************************************************************}
    {* Input Event Masks. Used as event-mask window attribute and as arguments
       to Grab requests.  Not to be confused with event names.  *}
       NoSymbol = 0;
       NoEventMask = 0;
       KeyPressMask = (1) shl (0);
       KeyReleaseMask = (1) shl (1);
       ButtonPressMask = (1) shl (2);
       ButtonReleaseMask = (1) shl (3);
       EnterWindowMask = (1) shl (4);
       LeaveWindowMask = (1) shl (5);
       PointerMotionMask = (1) shl (6);
       PointerMotionHintMask = (1) shl (7);
       Button1MotionMask = (1) shl (8);
       Button2MotionMask = (1) shl (9);
       Button3MotionMask = (1) shl (10);
       Button4MotionMask = (1) shl (11);
       Button5MotionMask = (1) shl (12);
       ButtonMotionMask = (1) shl (13);
       KeymapStateMask = (1) shl (14);
       ExposureMask = (1) shl (15);
       VisibilityChangeMask = (1) shl (16);
       StructureNotifyMask = (1) shl (17);
       ResizeRedirectMask = (1) shl (18);
       SubstructureNotifyMask = (1) shl (19);
       SubstructureRedirectMask = (1) shl (20);
       FocusChangeMask = (1) shl (21);
       PropertyChangeMask = (1) shl (22);
       ColormapChangeMask = (1) shl (23);
    {* Event names.  Used in "type" field in XEvent structures.  Not to be
    confused with event masks above.  They start from 2 because 0 and 1
    are reserved in the protocol for errors and replies. *}
       OwnerGrabButtonMask = (1) shl (24);
       KeyPress = 2;
       KeyRelease = 3;
       ButtonPress = 4;
       ButtonRelease = 5;
       MotionNotify = 6;
       EnterNotify = 7;
       LeaveNotify = 8;
       FocusIn = 9;
       FocusOut = 10;
       KeymapNotify = 11;
       Expose = 12;
       GraphicsExpose = 13;
       NoExpose = 14;
       VisibilityNotify = 15;
       CreateNotify = 16;
       DestroyNotify = 17;
       UnmapNotify = 18;
       MapNotify = 19;
       MapRequest = 20;
       ReparentNotify = 21;
       ConfigureNotify = 22;
       ConfigureRequest = 23;
       GravityNotify = 24;
       ResizeRequest = 25;
       CirculateNotify = 26;
       CirculateRequest = 27;
       PropertyNotify = 28;
       SelectionClear = 29;
       SelectionRequest = 30;
       SelectionNotify = 31;
       ColormapNotify = 32;
       ClientMessage = 33;
       MappingNotify = 34;
    {* must be bigger than any event # *}
    {* Key masks. Used as modifiers to GrabButton and GrabKey, results of QueryPointer,
       state in various key-, mouse-, and button-related events. *}
       LASTEvent = 35;
       ShiftMask = (1) shl (0);
       LockMask = (1) shl (1);
       ControlMask = (1) shl (2);
       Mod1Mask = (1) shl (3);
       Mod2Mask = (1) shl (4);
       Mod3Mask = (1) shl (5);
       Mod4Mask = (1) shl (6);
    {* modifier names.  Used to build a SetModifierMapping request or
       to read a GetModifierMapping request.  These correspond to the
       masks defined above. *}
       Mod5Mask = (1) shl (7);
       ShiftMapIndex = 0;
       LockMapIndex = 1;
       ControlMapIndex = 2;
       Mod1MapIndex = 3;
       Mod2MapIndex = 4;
       Mod3MapIndex = 5;
       Mod4MapIndex = 6;
    {* button masks.  Used in same manner as Key masks above. Not to be confused
       with button names below. *}
       Mod5MapIndex = 7;
       Button1Mask = (1) shl (8);
       Button2Mask = (1) shl (9);
       Button3Mask = (1) shl (10);
       Button4Mask = (1) shl (11);
       Button5Mask = (1) shl (12);
    {* used in GrabButton, GrabKey *}
    {* button names. Used as arguments to GrabButton and as detail in ButtonPress
       and ButtonRelease events.  Not to be confused with button masks above.
       Note that 0 is already defined above as "AnyButton".  *}
       AnyModifier = (1) shl (15);
       Button1 = 1;
       Button2 = 2;
       Button3 = 3;
       Button4 = 4;
    {* Notify modes *}
       Button5 = 5;
       NotifyNormal = 0;
       NotifyGrab = 1;
       NotifyUngrab = 2;
       NotifyWhileGrabbed = 3;
    {* for MotionNotify events *}
    {* Notify detail *}
       NotifyHint = 1;
       NotifyAncestor = 0;
       NotifyVirtual = 1;
       NotifyInferior = 2;
       NotifyNonlinear = 3;
       NotifyNonlinearVirtual = 4;
       NotifyPointer = 5;
       NotifyPointerRoot = 6;
    {* Visibility notify *}
       NotifyDetailNone = 7;
       VisibilityUnobscured = 0;
       VisibilityPartiallyObscured = 1;
    {* Circulation request *}
       VisibilityFullyObscured = 2;
       PlaceOnTop = 0;
    {* protocol families *}
       PlaceOnBottom = 1;
       FamilyInternet = 0;
       FamilyDECnet = 1;
    {* Property notification *}
       FamilyChaos = 2;
       PropertyNewValue = 0;
    {* Color Map notification *}
       PropertyDelete = 1;
       ColormapUninstalled = 0;
    {* GrabPointer, GrabButton, GrabKeyboard, GrabKey Modes *}
       ColormapInstalled = 1;
       GrabModeSync = 0;
    {* GrabPointer, GrabKeyboard reply status *}
       GrabModeAsync = 1;
       GrabSuccess = 0;
       AlreadyGrabbed = 1;
       GrabInvalidTime = 2;
       GrabNotViewable = 3;
    {* AllowEvents modes *}
       GrabFrozen = 4;
       AsyncPointer = 0;
       SyncPointer = 1;
       ReplayPointer = 2;
       AsyncKeyboard = 3;
       SyncKeyboard = 4;
       ReplayKeyboard = 5;
       AsyncBoth = 6;
    {* Used in SetInputFocus, GetInputFocus *}
       SyncBoth = 7;
    {*****************************************************************
     * ERROR CODES
     *****************************************************************}
       RevertToParent = 2;
    {* everything's okay *}
       Success = 0;
    {* bad request code *}
       BadRequest = 1;
    {* int parameter out of range *}
       BadValue = 2;
    {* parameter not a Window *}
       BadWindow = 3;
    {* parameter not a Pixmap *}
       BadPixmap = 4;
    {* parameter not an Atom *}
       BadAtom = 5;
    {* parameter not a Cursor *}
       BadCursor = 6;
    {* parameter not a Font *}
       BadFont = 7;
    {* parameter mismatch *}
       BadMatch = 8;
    {* parameter not a Pixmap or Window *}
       BadDrawable = 9;
    {* depending on context:
                                 - key/button already grabbed
                                 - attempt to free an illegal
                                   cmap entry
                                - attempt to store into a read-only
                                   color map entry.
                                - attempt to modify the access control
                                   list from other than the local host.
                                *}
       BadAccess = 10;
    {* insufficient resources *}
       BadAlloc = 11;
    {* no such colormap *}
       BadColor = 12;
    {* parameter not a GC *}
       BadGC = 13;
    {* choice not in range or already used *}
       BadIDChoice = 14;
    {* font or color name doesn't exist *}
       BadName = 15;
    {* Request length incorrect *}
       BadLength = 16;
    {* server is defective *}
       BadImplementation = 17;
       FirstExtensionError = 128;
    {*****************************************************************
     * WINDOW DEFINITIONS
     *****************************************************************}
    {* Window classes used by CreateWindow *}
    {* Note that CopyFromParent is already defined as 0 above *}
       LastExtensionError = 255;
       InputOutput = 1;
    {* Window attributes for CreateWindow and ChangeWindowAttributes *}
       InputOnly = 2;
       CWBackPixmap = (1) shl (0);
       CWBackPixel = (1) shl (1);
       CWBorderPixmap = (1) shl (2);
       CWBorderPixel = (1) shl (3);
       CWBitGravity = (1) shl (4);
       CWWinGravity = (1) shl (5);
       CWBackingStore = (1) shl (6);
       CWBackingPlanes = (1) shl (7);
       CWBackingPixel = (1) shl (8);
       CWOverrideRedirect = (1) shl (9);
       CWSaveUnder = (1) shl (10);
       CWEventMask = (1) shl (11);
       CWDontPropagate = (1) shl (12);
       CWColormap = (1) shl (13);
    {* ConfigureWindow structure *}
       CWCursor = (1) shl (14);
       CWX = (1) shl (0);
       CWY = (1) shl (1);
       CWWidth = (1) shl (2);
       CWHeight = (1) shl (3);
       CWBorderWidth = (1) shl (4);
       CWSibling = (1) shl (5);
    {* Bit Gravity *}
       CWStackMode = (1) shl (6);
       ForgetGravity = 0;
       NorthWestGravity = 1;
       NorthGravity = 2;
       NorthEastGravity = 3;
       WestGravity = 4;
       CenterGravity = 5;
       EastGravity = 6;
       SouthWestGravity = 7;
       SouthGravity = 8;
       SouthEastGravity = 9;
    {* Window gravity + bit gravity above *}
       StaticGravity = 10;
    {* Used in CreateWindow for backing-store hint *}
       UnmapGravity = 0;
       NotUseful = 0;
       WhenMapped = 1;
    {* Used in GetWindowAttributes reply *}
       Always = 2;
       IsUnmapped = 0;
       IsUnviewable = 1;
    {* Used in ChangeSaveSet *}
       IsViewable = 2;
       SetModeInsert = 0;
    {* Used in ChangeCloseDownMode *}
       SetModeDelete = 1;
       DestroyAll = 0;
       RetainPermanent = 1;
    {* Window stacking method (in configureWindow) *}
       RetainTemporary = 2;
       Above = 0;
       Below = 1;
       TopIf = 2;
       BottomIf = 3;
    {* Circulation direction *}
       Opposite = 4;
       RaiseLowest = 0;
    {* Property modes *}
       LowerHighest = 1;
       PropModeReplace = 0;
       PropModePrepend = 1;
    {*****************************************************************
     * GRAPHICS DEFINITIONS
     *****************************************************************}
    {* graphics functions, as in GC.alu *}
       PropModeAppend = 2;
    {* 0 *}
       GXclear = $0;
    {* src AND dst *}
       GXand = $1;
    {* src AND NOT dst *}
       GXandReverse = $2;
    {* src *}
       GXcopy = $3;
    {* NOT src AND dst *}
       GXandInverted = $4;
    {* dst *}
       GXnoop = $5;
    {* src XOR dst *}
       GXxor = $6;
    {* src OR dst *}
       GXor = $7;
    {* NOT src AND NOT dst *}
       GXnor = $8;
    {* NOT src XOR dst *}
       GXequiv = $9;
    {* NOT dst *}
       GXinvert = $a;
    {* src OR NOT dst *}
       GXorReverse = $b;
    {* NOT src *}
       GXcopyInverted = $c;
    {* NOT src OR dst *}
       GXorInverted = $d;
    {* NOT src OR NOT dst *}
       GXnand = $e;
    {* 1 *}
    {* LineStyle *}
       GXset = $f;
       LineSolid = 0;
       LineOnOffDash = 1;
    {* capStyle *}
       LineDoubleDash = 2;
       CapNotLast = 0;
       CapButt = 1;
       CapRound = 2;
    {* joinStyle *}
       CapProjecting = 3;
       JoinMiter = 0;
       JoinRound = 1;
    {* fillStyle *}
       JoinBevel = 2;
       FillSolid = 0;
       FillTiled = 1;
       FillStippled = 2;
    {* fillRule *}
       FillOpaqueStippled = 3;
       EvenOddRule = 0;
    {* subwindow mode *}
       WindingRule = 1;
       ClipByChildren = 0;
    {* SetClipRectangles ordering *}
       IncludeInferiors = 1;
       Unsorted = 0;
       YSorted = 1;
       YXSorted = 2;
    {* CoordinateMode for drawing routines *}
       YXBanded = 3;
    {* relative to the origin *}
       CoordModeOrigin = 0;
    {* relative to previous point *}
    {* Polygon shapes *}
       CoordModePrevious = 1;
    {* paths may intersect *}
       Complex = 0;
    {* no paths intersect, but not convex *}
       Nonconvex = 1;
    {* wholly convex *}
    {* Arc modes for PolyFillArc *}
       Convex = 2;
    {* join endpoints of arc *}
       ArcChord = 0;
    {* join endpoints to center of arc *}
    {* GC components: masks used in CreateGC, CopyGC, ChangeGC, OR'ed into
       GC.stateChanges *}
       ArcPieSlice = 1;
       GCFunction = (1) shl (0);
       GCPlaneMask = (1) shl (1);
       GCForeground = (1) shl (2);
       GCBackground = (1) shl (3);
       GCLineWidth = (1) shl (4);
       GCLineStyle = (1) shl (5);
       GCCapStyle = (1) shl (6);
       GCJoinStyle = (1) shl (7);
       GCFillStyle = (1) shl (8);
       GCFillRule = (1) shl (9);
       GCTile = (1) shl (10);
       GCStipple = (1) shl (11);
       GCTileStipXOrigin = (1) shl (12);
       GCTileStipYOrigin = (1) shl (13);
       GCFont = (1) shl (14);
       GCSubwindowMode = (1) shl (15);
       GCGraphicsExposures = (1) shl (16);
       GCClipXOrigin = (1) shl (17);
       GCClipYOrigin = (1) shl (18);
       GCClipMask = (1) shl (19);
       GCDashOffset = (1) shl (20);
       GCDashList = (1) shl (21);
       GCArcMode = (1) shl (22);
    {*****************************************************************
     * FONTS
     *****************************************************************}
    {* used in QueryFont -- draw direction *}
       GCLastBit = 22;
       FontLeftToRight = 0;
       FontRightToLeft = 1;
    {*****************************************************************
     *  IMAGING
     *****************************************************************}
    {* ImageFormat -- PutImage, GetImage *}
       FontChange = 255;
    {* depth 1, XYFormat *}
       XYBitmap = 0;
    {* depth == drawable depth *}
       XYPixmap = 1;
    {* depth == drawable depth *}
    {*****************************************************************
     *  COLOR MAP STUFF
     *****************************************************************}
    {* For CreateColormap *}
       ZPixmap = 2;
    {* create map with no entries *}
       AllocNone = 0;
    {* allocate entire map writeable *}
    {* Flags used in StoreNamedColor, StoreColors *}
       AllocAll = 1;
       DoRed = (1) shl (0);
       DoGreen = (1) shl (1);
    {*****************************************************************
     * CURSOR STUFF
     *****************************************************************}
    {* QueryBestSize Class *}
       DoBlue = (1) shl (2);
    {* largest size that can be displayed *}
       CursorShape = 0;
    {* size tiled fastest *}
       TileShape = 1;
    {* size stippled fastest *}
    {*****************************************************************
     * KEYBOARD/POINTER STUFF
     *****************************************************************}
       StippleShape = 2;
       AutoRepeatModeOff = 0;
       AutoRepeatModeOn = 1;
       AutoRepeatModeDefault = 2;
       LedModeOff = 0;
    {* masks for ChangeKeyboardControl *}
       LedModeOn = 1;
       KBKeyClickPercent = (1) shl (0);
       KBBellPercent = (1) shl (1);
       KBBellPitch = (1) shl (2);
       KBBellDuration = (1) shl (3);
       KBLed = (1) shl (4);
       KBLedMode = (1) shl (5);
       KBKey = (1) shl (6);
       KBAutoRepeatMode = (1) shl (7);
       MappingSuccess = 0;
       MappingBusy = 1;
       MappingFailed = 2;
       MappingModifier = 0;
       MappingKeyboard = 1;
    {*****************************************************************
     * SCREEN SAVER STUFF
     *****************************************************************}
       MappingPointer = 2;
       DontPreferBlanking = 0;
       PreferBlanking = 1;
       DefaultBlanking = 2;
       DisableScreenSaver = 0;
       DisableScreenInterval = 0;
       DontAllowExposures = 0;
       AllowExposures = 1;
    {* for ForceScreenSaver *}
       DefaultExposures = 2;
       ScreenSaverReset = 0;
    {*****************************************************************
     * HOSTS AND CONNECTIONS
     *****************************************************************}
    {* for ChangeHosts *}
       ScreenSaverActive = 1;
       HostInsert = 0;
    {* for ChangeAccessControl *}
       HostDelete = 1;
       EnableAccess = 1;
    {* Display classes  used in opening the connection
     * Note that the statically allocated ones are even numbered and the
     * dynamically changeable ones are odd numbered *}
       DisableAccess = 0;
       StaticGray = 0;
       GrayScale = 1;
       StaticColor = 2;
       PseudoColor = 3;
       TrueColor = 4;
    {* Byte order  used in imageByteOrder and bitmapBitOrder *}
       DirectColor = 5;
       LSBFirst = 0;
       MSBFirst = 1;

    { Pointer Declarations }

  implementation

end.
