{
  $Id$
}
Unit XUtil;
interface
uses
  x,xlib;

{$LinkLib C}
{$LinkLib X11}

{
  Bitmask returned by XParseGeometry().  Each bit tells if the corresponding
  value (x, y, width, height) was found in the parsed string.
}
Const
  NoValue       = $0000;
  XValue        = $0001;
  YValue        = $0002;
  WidthValue    = $0004;
  HeightValue   = $0008;
  AllValues     = $000F;
  XNegative     = $0010;
  YNegative     = $0020;

{
  new version containing base_width, base_height, and win_gravity fields;
  used with WM_NORMAL_HINTS.
}
Type
  long = Cardinal; { Untill we know better. M.}

Type
  TAspectRecord = record
    x,y : longint;
  end;

  TXSizeHints = record
        flags : Long;   { marks which fields in this structure are defined }
        x, y : longint ;                { obsolete for new window mgrs, but clients }
        width, height: longint ;        { should set so old wm's don't mess up }
        min_width, min_height : longint ;
        max_width, max_height : longint ;
        width_inc, height_inc : longint ;
        min_aspect, max_aspect : TAspectRecord;
        base_width, base_height : longint ;             { added by ICCCM version 1 }
        win_gravity : longint ;                 { added by ICCCM version 1 }
  end;
  PXSizeHints=^TXSizeHints;

{
  The next block of definitions are for window manager properties that
  clients and applications use for communication.
}

{ flags argument in size hints }
Const
  USPosition     = 1 shl 0; { user specified x, y }
  USSize         = 1 shl 1; { user specified width, height }

  PPosition      = 1 shl 2; { program specified position }
  PSize          = 1 shl 3; { program specified size }
  PMinSize       = 1 shl 4; { program specified minimum size }
  PMaxSize       = 1 shl 5; { program specified maximum size }
  PResizeInc     = 1 shl 6; { program specified resize increments }
  PAspect        = 1 shl 7; { program specified min and max aspect ratios }
  PBaseSize      = 1 shl 8; { program specified base for incrementing }
  PWinGravity    = 1 shl 9; { program specified window gravity }
{ obsolete }
  PAllHints = PPosition or PSize or PMinSize or PMaxSize or PResizeInc or PAspect;

Type
  TXWMHints = record
        flags : long;   { marks which fields in this structure are defined }
        input : TBool ; { does this application rely on the window manager to
                        get keyboard input? }
        initial_state : longint ;       { see below }
        icon_pixmap : TPixmap ; { pixmap to be used as icon }
        icon_window : TWindow ;         { window to be used as icon }
        icon_x, icon_y : longint ;      { initial position of icon }
        icon_mask : TPixmap ;   { icon mask bitmap }
        window_group : TXID ;   { id of related window group }
        { this structure may be extended in the future }
  end;

{ definition for flags of XWMHints }
Const
  InputHint              = 1 shl 0;
  StateHint              = 1 shl 1;
  IconPixmapHint         = 1 shl 2;
  IconWindowHint         = 1 shl 3;
  IconPositionHint       = 1 shl 4;
  IconMaskHint           = 1 shl 5;
  AllHints = InputHint or StateHint or IconPixmapHint or IconWindowHint or
             IconPositionHint or IconMaskHint {or WindowGroupHint};
  XUrgencyHint           = 1 shl 8;
{ definitions for initial window state }
  WithdrawnState = 0;    { for windows that are not mapped }
  NormalState    = 1;    { most applications want to start this way }
  IconicState    = 3;    { application wants to start as an icon }
{ Obsolete states no longer defined by ICCCM }
  DontCareState  = 0;    { don't know or care }
  ZoomState      = 2;    { application wants to start zoomed }
  InactiveState  = 4;    { application believes it is seldom used; }
                         { some wm's may put it on inactive menu }

{
  new structure for manipulating TEXT properties; used with WM_NAME,
  WM_ICON_NAME, WM_CLIENT_MACHINE, and WM_COMMAND.
}
type
  TXTextProperty = record
    value : pchar;              { same as Property routines }
    encoding : TAtom;                   { prop type }
    format : longint ;                          { prop data format: 8, 16, or 32 }
    nitems : Cardinal;          { number of data items in value }
  end;
  PXTextProperty = ^TXTextProperty;

Const
  XNoMemory              =-1;
  XLocaleNotSupported    =-2;
  XConverterNotFound     =-3;

Type
  TXICCEncodingStyle = longint;

Const
  XStringStyle        = 0;            { STRING }
  XCompoundTextStyle  = 1;            { COMPOUND_TEXT }
  XTextStyle          = 2;            { text in owner's encoding (current locale)}
  XStdICCTextStyle    = 3;            { STRING, else COMPOUND_TEXT }

Type
  TXIconSize = record
        min_width, min_height,
        max_width, max_height,
        width_inc, height_inc : longint;
  end;
  PXIconSize = ^TXIconSize;

type
  TXClassHint = record
        res_name,
        res_class : pchar;
  end;
  PXClassHint = ^TXClassHint;

{
 * These macros are used to give some sugar to the image routines so that
 * naive people are more comfortable with them.
}

{
 XDestroyImage(ximage) \
        ((*((ximage)->f.destroy_image))((ximage)))
 XGetPixel(ximage, x, y) \
        ((*((ximage)->f.get_pixel))((ximage), (x), (y)))
 XPutPixel(ximage, x, y, pixel) \
        ((*((ximage)->f.put_pixel))((ximage), (x), (y), (pixel)))
 XSubImage(ximage, x, y, width, height)  \
        ((*((ximage)->f.sub_image))((ximage), (x), (y), (width), (height)))
 XAddPixel(ximage, value) \
        ((*((ximage)->f.add_pixel))((ximage), (value)))
}
{
 * Compose sequence status structure, used in calling XLookupString.
 }

  TXComposeStatus = record
    compose_ptr : TXPointer ;   { state table pointer }
    chars_matched : longint ;           { match state }
  end;
  PTXComposeStatus = ^TXComposeStatus;

{
 * Keysym macros, used on Keysyms to test for classes of symbols
 }
{
 IsKeypadKey(keysym) \
  (((KeySym)(keysym) >= XK_KP_Space) && ((KeySym)(keysym) <= XK_KP_Equal))

 IsPrivateKeypadKey(keysym) \
  (((KeySym)(keysym) >= 0x11000000) && ((KeySym)(keysym) <= 0x1100FFFF))

 IsCursorKey(keysym) \
  (((KeySym)(keysym) >= XK_Home)     && ((KeySym)(keysym) <  XK_Select))

 IsPFKey(keysym) \
  (((KeySym)(keysym) >= XK_KP_F1)     && ((KeySym)(keysym) <= XK_KP_F4))

 IsFunctionKey(keysym) \
  (((KeySym)(keysym) >= XK_F1)       && ((KeySym)(keysym) <= XK_F35))

 IsMiscFunctionKey(keysym) \
  (((KeySym)(keysym) >= XK_Select)   && ((KeySym)(keysym) <= XK_Break))

 IsModifierKey(keysym) \
  ((((KeySym)(keysym) >= XK_Shift_L) && ((KeySym)(keysym) <= XK_Hyper_R)) \
    or  or  ((KeySym)(keysym) == XK_Mode_switch) \
    or  or  ((KeySym)(keysym) == XK_Num_Lock))
}
{
 * opaque reference to Region data type
 }
{
typedef struct _XRegion *Region;
}

{ Return values from XRectInRegion() }
Const
 RectangleOut =0;
 RectangleIn  =1;
 RectanglePart=2;


{
 * Information used by the visual utility routines to find desired visual
 * type from the many visuals a display may support.
 }

Type
  TXVisualInfo = record
    visual : PVisual;
    visualid : TVisualID ;
    screen : longint;
    depth : longint;
    c_class : longint;
    red_mask : cardinal;
    green_mask : cardinal;
    blue_mask : Cardinal;
    colormap_size : longint;
    bits_per_rgb : longint;
  end;
  PXVisualInfo = ^TXVisualInfo;

Const
  VisualNoMask           = $0;
  VisualIDMask           = $1;
  VisualScreenMask       = $2;
  VisualDepthMask        = $4;
  VisualClassMask        = $8;
  VisualRedMaskMask      = $10;
  VisualGreenMaskMask    = $20;
  VisualBlueMaskMask     = $40;
  VisualColormapSizeMask = $80;
  VisualBitsPerRGBMask   = $100;
  VisualAllMask          = $1FF;

{
 * This defines a window manager property that clients may use to
 * share standard color maps of type RGB_COLOR_MAP:
 }
Type
  TXStandardColormap = record
        colormap : TColormap ;
        red_max : Cardinal;
        red_mult : Cardinal;
        green_max : Cardinal;
        green_mult : Cardinal;
        blue_max : Cardinal;
        blue_mult : Cardinal;
        base_pixel : Cardinal;
        visualid : TVisualID;           { added by ICCCM version 1 }
        killid : TXID ;                 { added by ICCCM version 1 }
  end;
  PXStandardColormap= ^TXStandardColormap;

Const
  ReleaseByFreeingColormap = 1;  { for killid field above }
{ return codes for XReadBitmapFile and XWriteBitmapFile }
  BitmapSuccess          =0;
  BitmapOpenFailed       =1;
  BitmapFileInvalid      =2;
  BitmapNoMemory         =3;


{***************************************************************
 * Context Management
 ***************************************************************}

const
{ Associative lookup table return codes }
  XCSUCCESS =0;  { No error. }
  XCNOMEM   =1;    { Out of memory }
  XCNOENT   =2;    { No entry in table }
type
  TXContext = longint;
{
 XUniqueContext()       ((XContext) XrmUniqueQuark())
 XStringToContext(string)   ((XContext) XrmStringToQuark(string))
}

function XGetVisualInfo(display: PDisplay; vinfo_mask: LongInt;
  vinfo_template: PXVisualInfo; var nitems_return: longint): PXVisualInfo;
  cdecl; external;

(*
_XFUNCPROTOBEGIN

{ The following declarations are alphabetized. }

extern XClassHlongint *XAllocClassHlongint (
#if NeedFunctionPrototypes
    void
#endif
);

extern XIconSize *XAllocIconSize (
#if NeedFunctionPrototypes
    void
#endif
);

extern XSizeHints *XAllocSizeHints (
#if NeedFunctionPrototypes
    void
#endif
);

extern XStandardColormap *XAllocStandardColormap (
#if NeedFunctionPrototypes
    void
#endif
);

extern XWMHints *XAllocWMHints (
#if NeedFunctionPrototypes
    void
#endif
);

extern XClipBox(
#if NeedFunctionPrototypes
    Region              { r },
    XRectangle*         { rect_return }
#endif
);

extern Region XCreateRegion(
#if NeedFunctionPrototypes
    void
#endif
);

extern char *XDefaultString(
#if NeedFunctionPrototypes
    void
#endif
);

extern longint XDeleteContext(
#if NeedFunctionPrototypes
    Display*            { display },
    XID                 { rid },
    XContext            { context }
#endif
);

extern XDestroyRegion(
#if NeedFunctionPrototypes
    Region              { r }
#endif
);

extern XEmptyRegion(
#if NeedFunctionPrototypes
    Region              { r }
#endif
);

extern XEqualRegion(
#if NeedFunctionPrototypes
    Region              { r1 },
    Region              { r2 }
#endif
);

extern longint XFindContext(
#if NeedFunctionPrototypes
    Display*            { display },
    XID                 { rid },
    XContext            { context },
    XPointer*           { data_return }
#endif
);

extern Status XGetClassHint(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XClassHint*         { class_hints_return }
#endif
);

extern Status XGetIconSizes(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XIconSize**         { size_list_return },
    int*                { count_return }
#endif
);

extern Status XGetNormalHints(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XSizeHints*         { hints_return }
#endif
);

extern Status XGetRGBColormaps(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XStandardColormap** { stdcmap_return },
    int*                { count_return },
    Atom                { property }
#endif
);

extern Status XGetSizeHints(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XSizeHints*         { hints_return },
    Atom                { property }
#endif
);

extern Status XGetStandardColormap(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XStandardColormap*  { colormap_return },
    Atom                { property }
#endif
);

extern Status XGetTextProperty(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { window },
    XTextProperty*      { text_prop_return },
    Atom                { property }
#endif
);

extern XVisualInfo *XGetVisualInfo(
#if NeedFunctionPrototypes
    Display*            { display },
    long                { vinfo_mask },
    XVisualInfo*        { vinfo_template },
    int*                { nitems_return }
#endif
);

extern Status XGetWMClientMachine(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XTextProperty*      { text_prop_return }
#endif
);

extern XWMHints *XGetWMHints(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w }
#endif
);

extern Status XGetWMIconName(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XTextProperty*      { text_prop_return }
#endif
);

extern Status XGetWMName(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XTextProperty*      { text_prop_return }
#endif
);

extern Status XGetWMNormalHints(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XSizeHints*         { hints_return },
    long*               { supplied_return }
#endif
);

extern Status XGetWMSizeHints(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XSizeHints*         { hints_return },
    long*               { supplied_return },
    Atom                { property }
#endif
);

extern Status XGetZoomHints(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XSizeHints*         { zhints_return }
#endif
);

extern XIntersectRegion(
#if NeedFunctionPrototypes
    Region              { sra },
    Region              { srb },
    Region              { dr_return }
#endif
);

extern void XConvertCase(
#if NeedFunctionPrototypes
    KeySym              { sym },
    KeySym*             { lower },
    KeySym*             { upper }
#endif
);

extern longint XLookupString(
#if NeedFunctionPrototypes
    XKeyEvent*          { event_struct },
    char*               { buffer_return },
    int                 { bytes_buffer },
    KeySym*             { keysym_return },
    XComposeStatus*     { status_in_out }
#endif
);

extern Status XMatchVisualInfo(
#if NeedFunctionPrototypes
    Display*            { display },
    int                 { screen },
    int                 { depth },
    int                 { class },
    XVisualInfo*        { vinfo_return }
#endif
);

extern XOffsetRegion(
#if NeedFunctionPrototypes
    Region              { r },
    int                 { dx },
    int                 { dy }
#endif
);

extern Bool XPointInRegion(
#if NeedFunctionPrototypes
    Region              { r },
    int                 { x },
    int                 { y }
#endif
);

extern Region XPolygonRegion(
#if NeedFunctionPrototypes
    XPoint*             { points },
    int                 { n },
    int                 { fill_rule }
#endif
);

extern longint XRectInRegion(
#if NeedFunctionPrototypes
    Region              { r },
    int                 { x },
    int                 { y },
    unsigned int        { width },
    unsigned int        { height }
#endif
);

extern longint XSaveContext(
#if NeedFunctionPrototypes
    Display*            { display },
    XID                 { rid },
    XContext            { context },
    _Xconst char*       { data }
#endif
);

extern XSetClassHint(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XClassHint*         { class_hints }
#endif
);

extern XSetIconSizes(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XIconSize*          { size_list },
    int                 { count }
#endif
);

extern XSetNormalHints(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XSizeHints*         { hints }
#endif
);

extern void XSetRGBColormaps(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XStandardColormap*  { stdcmaps },
    int                 { count },
    Atom                { property }
#endif
);

extern XSetSizeHints(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XSizeHints*         { hints },
    Atom                { property }
#endif
);

extern XSetStandardProperties(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    _Xconst char*       { window_name },
    _Xconst char*       { icon_name },
    Pixmap              { icon_pixmap },
    char**              { argv },
    int                 { argc },
    XSizeHints*         { hints }
#endif
);

extern void XSetTextProperty(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XTextProperty*      { text_prop },
    Atom                { property }
#endif
);

extern void XSetWMClientMachine(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XTextProperty*      { text_prop }
#endif
);

extern XSetWMHints(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XWMHints*           { wm_hints }
#endif
);

extern void XSetWMIconName(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XTextProperty*      { text_prop }
#endif
);

extern void XSetWMName(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XTextProperty*      { text_prop }
#endif
);

extern void XSetWMNormalHints(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XSizeHints*         { hints }
#endif
);

extern void XSetWMProperties(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XTextProperty*      { window_name },
    XTextProperty*      { icon_name },
    char**              { argv },
    int                 { argc },
    XSizeHints*         { normal_hints },
    XWMHints*           { wm_hints },
    XClassHint*         { class_hints }
#endif
);

extern void XmbSetWMProperties(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    _Xconst char*       { window_name },
    _Xconst char*       { icon_name },
    char**              { argv },
    int                 { argc },
    XSizeHints*         { normal_hints },
    XWMHints*           { wm_hints },
    XClassHint*         { class_hints }
#endif
);

extern void XSetWMSizeHints(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XSizeHints*         { hints },
    Atom                { property }
#endif
);

extern XSetRegion(
#if NeedFunctionPrototypes
    Display*            { display },
    GC                  { gc },
    Region              { r }
#endif
);

extern void XSetStandardColormap(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XStandardColormap*  { colormap },
    Atom                { property }
#endif
);

extern XSetZoomHints(
#if NeedFunctionPrototypes
    Display*            { display },
    Window              { w },
    XSizeHints*         { zhints }
#endif
);

extern XShrinkRegion(
#if NeedFunctionPrototypes
    Region              { r },
    int                 { dx },
    int                 { dy }
#endif
);

extern Status XStringListToTextProperty(
#if NeedFunctionPrototypes
    char**              { list },
    int                 { count },
    XTextProperty*      { text_prop_return }
#endif
);

extern XSubtractRegion(
#if NeedFunctionPrototypes
    Region              { sra },
    Region              { srb },
    Region              { dr_return }
#endif
);

extern longint XmbTextListToTextProperty(
#if NeedFunctionPrototypes
    Display*            { display },
    char**              { list },
    int                 { count },
    XICCEncodingStyle   { style },
    XTextProperty*      { text_prop_return }
#endif
);

extern longint XwcTextListToTextProperty(
#if NeedFunctionPrototypes
    Display*            { display },
    wchar_t**           { list },
    int                 { count },
    XICCEncodingStyle   { style },
    XTextProperty*      { text_prop_return }
#endif
);

extern void XwcFreeStringList(
#if NeedFunctionPrototypes
    wchar_t**           { list }
#endif
);

extern Status XTextPropertyToStringList(
#if NeedFunctionPrototypes
    XTextProperty*      { text_prop },
    char***             { list_return },
    int*                { count_return }
#endif
);

extern longint XmbTextPropertyToTextList(
#if NeedFunctionPrototypes
    Display*            { display },
    XTextProperty*      { text_prop },
    char***             { list_return },
    int*                { count_return }
#endif
);

extern longint XwcTextPropertyToTextList(
#if NeedFunctionPrototypes
    Display*            { display },
    XTextProperty*      { text_prop },
    wchar_t***          { list_return },
    int*                { count_return }
#endif
);

extern XUnionRectWithRegion(
#if NeedFunctionPrototypes
    XRectangle*         { rectangle },
    Region              { src_region },
    Region              { dest_region_return }
#endif
);

extern XUnionRegion(
#if NeedFunctionPrototypes
    Region              { sra },
    Region              { srb },
    Region              { dr_return }
#endif
);

extern longint XWMGeometry(
#if NeedFunctionPrototypes
    Display*            { display },
    int                 { screen_number },
    _Xconst char*       { user_geometry },
    _Xconst char*       { default_geometry },
    unsigned int        { border_width },
    XSizeHints*         { hints },
    int*                { x_return },
    int*                { y_return },
    int*                { width_return },
    int*                { height_return },
    int*                { gravity_return }
#endif
);

extern XXorRegion(
#if NeedFunctionPrototypes
    Region              { sra },
    Region              { srb },
    Region              { dr_return }
#endif
);

_XFUNCPROTOEND

#endif { _XUTIL_H_ }
*)

Implementation

end.
{
  $Log$
  Revision 1.4  2000-02-27 14:39:54  peter
    * added explicit linklib c

  Revision 1.3  2000/02/27 13:11:31  peter
    * cleanup, removed warnings
    * external decls moved from implementation to interface

}
