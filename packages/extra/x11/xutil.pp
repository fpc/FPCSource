unit xutil;
interface
uses
  x,xlib;

{$ifndef os2}
  {$LinkLib c}
  {$LinkLib X11}
const
  libX11='X11';
{$else}
const
  libX11='X11';
{$endif}

{
  Automatically converted by H2Pas 0.99.15 from xutil.h
  The following command line parameters were used:
    -p
    -T
    -S
    -d
    -c
    xutil.h
}

{$PACKRECORDS C}

const
   NoValue = $0000;
   XValue = $0001;
   YValue = $0002;
   WidthValue = $0004;
   HeightValue = $0008;
   AllValues = $000F;
   XNegative = $0010;
   YNegative = $0020;
type

   PXSizeHints = ^TXSizeHints;
   TXSizeHints = record
        flags : longint;
        x : longint;
        y : longint;
        width : longint;
        height : longint;
        min_width : longint;
        min_height : longint;
        max_width : longint;
        max_height : longint;
        width_inc : longint;
        height_inc : longint;
        min_aspect : record
             x : longint;
             y : longint;
          end;
        max_aspect : record
             x : longint;
             y : longint;
          end;
        base_width : longint;
        base_height : longint;
        win_gravity : longint;
     end;

const
   USPosition = 1 shl 0;
   USSize = 1 shl 1;
   PPosition = 1 shl 2;
   PSize = 1 shl 3;
   PMinSize = 1 shl 4;
   PMaxSize = 1 shl 5;
   PResizeInc = 1 shl 6;
   PAspect = 1 shl 7;
   PBaseSize = 1 shl 8;
   PWinGravity = 1 shl 9;
   PAllHints = ((((PPosition or PSize) or PMinSize) or PMaxSize) or PResizeInc) or PAspect;
type

   PXWMHints = ^TXWMHints;
   TXWMHints = record
        flags : longint;
        input : TBool;
        initial_state : longint;
        icon_pixmap : TPixmap;
        icon_window : TWindow;
        icon_x : longint;
        icon_y : longint;
        icon_mask : TPixmap;
        window_group : TXID;
     end;

const
   InputHint = 1 shl 0;
   StateHint = 1 shl 1;
   IconPixmapHint = 1 shl 2;
   IconWindowHint = 1 shl 3;
   IconPositionHint = 1 shl 4;
   IconMaskHint = 1 shl 5;
   WindowGroupHint = 1 shl 6;
   AllHints = (((((InputHint or StateHint) or IconPixmapHint) or IconWindowHint) or IconPositionHint) or IconMaskHint) or WindowGroupHint;
   XUrgencyHint = 1 shl 8;
   WithdrawnState = 0;
   NormalState = 1;
   IconicState = 3;
   DontCareState = 0;
   ZoomState = 2;
   InactiveState = 4;
type

   PXTextProperty = ^TXTextProperty;
   TXTextProperty = record
        value : Pbyte;
        encoding : TAtom;
        format : longint;
        nitems : dword;
     end;

const
   XNoMemory = -(1);
   XLocaleNotSupported = -(2);
   XConverterNotFound = -(3);
type

   PXICCEncodingStyle = ^TXICCEncodingStyle;
   TXICCEncodingStyle = (XStringStyle,XCompoundTextStyle,XTextStyle,
     XStdICCTextStyle);

   PPXIconSize = ^PXIconSize;
   PXIconSize = ^TXIconSize;
   TXIconSize = record
        min_width : longint;
        min_height : longint;
        max_width : longint;
        max_height : longint;
        width_inc : longint;
        height_inc : longint;
     end;

   PXClassHint = ^TXClassHint;
   TXClassHint = record
        res_name : Pchar;
        res_class : Pchar;
     end;
{$ifdef MACROS}
function XGetPixel(ximage,x,y : longint) : longint;

function XPutPixel(ximage,x,y,pixel : longint) : longint;

function XSubImage(ximage,x,y,width,height : longint) : longint;

function XAddPixel(ximage,value : longint) : longint;
{$endif MACROS}

type

   PXComposeStatus = ^TXComposeStatus;
   TXComposeStatus = record
        compose_ptr : TXPointer;
        chars_matched : longint;
     end;
{$ifdef MACROS}
function IsKeypadKey(keysym : longint) : longint;

function IsPrivateKeypadKey(keysym : longint) : longint;

function IsCursorKey(keysym : longint) : longint;

function IsPFKey(keysym : longint) : longint;

function IsFunctionKey(keysym : longint) : longint;

function IsMiscFunctionKey(keysym : longint) : longint;
{$endif MACROS}

type

   PXRegion = ^TXRegion;
   TXRegion = record
     end;
   TRegion = PXRegion;
   PRegion = ^TRegion;

const
   RectangleOut = 0;
   RectangleIn = 1;
   RectanglePart = 2;
type

   PXVisualInfo = ^TXVisualInfo;
   TXVisualInfo = record
        visual : PVisual;
        visualid : TVisualID;
        screen : longint;
        depth : longint;
        _class : longint;
        red_mask : dword;
        green_mask : dword;
        blue_mask : dword;
        colormap_size : longint;
        bits_per_rgb : longint;
     end;

const
   VisualNoMask = $0;
   VisualIDMask = $1;
   VisualScreenMask = $2;
   VisualDepthMask = $4;
   VisualClassMask = $8;
   VisualRedMaskMask = $10;
   VisualGreenMaskMask = $20;
   VisualBlueMaskMask = $40;
   VisualColormapSizeMask = $80;
   VisualBitsPerRGBMask = $100;
   VisualAllMask = $1FF;
type

   PPXStandardColormap = ^PXStandardColormap;
   PXStandardColormap = ^TXStandardColormap;
   TXStandardColormap = record
        colormap : TColormap;
        red_max : dword;
        red_mult : dword;
        green_max : dword;
        green_mult : dword;
        blue_max : dword;
        blue_mult : dword;
        base_pixel : dword;
        visualid : TVisualID;
        killid : TXID;
     end;

{$ifdef MACROS}
function ReleaseByFreeingColormap : TXID;
{$endif MACROS}

const
   BitmapSuccess = 0;
   BitmapOpenFailed = 1;
   BitmapFileInvalid = 2;
   BitmapNoMemory = 3;
   XCSUCCESS = 0;
   XCNOMEM = 1;
   XCNOENT = 2;
type

   PXContext = ^TXContext;
   TXContext = longint;
{$ifdef MACROS}
function XUniqueContext : TXContext;

function XStringToContext(_string : longint) : TXContext;
{$endif MACROS}

function XAllocClassHint:PXClassHint;cdecl;external libX11;
function XAllocIconSize:PXIconSize;cdecl;external libX11;
function XAllocSizeHints:PXSizeHints;cdecl;external libX11;
function XAllocStandardColormap:PXStandardColormap;cdecl;external libX11;
function XAllocWMHints:PXWMHints;cdecl;external libX11;
function XClipBox(para1:TRegion; para2:PXRectangle):longint;cdecl;external libX11;
function XCreateRegion:TRegion;cdecl;external libX11;
function XDefaultString:Pchar;cdecl;external libX11;
function XDeleteContext(para1:PDisplay; para2:TXID; para3:TXContext):longint;cdecl;external libX11;
function XDestroyRegion(para1:TRegion):longint;cdecl;external libX11;
function XEmptyRegion(para1:TRegion):longint;cdecl;external libX11;
function XEqualRegion(para1:TRegion; para2:TRegion):longint;cdecl;external libX11;
function XFindContext(para1:PDisplay; para2:TXID; para3:TXContext; para4:PXPointer):longint;cdecl;external libX11;
function XGetClassHint(para1:PDisplay; para2:TWindow; para3:PXClassHint):TStatus;cdecl;external libX11;
function XGetIconSizes(para1:PDisplay; para2:TWindow; para3:PPXIconSize; para4:Plongint):TStatus;cdecl;external libX11;
function XGetNormalHints(para1:PDisplay; para2:TWindow; para3:PXSizeHints):TStatus;cdecl;external libX11;
function XGetRGBColormaps(para1:PDisplay; para2:TWindow; para3:PPXStandardColormap; para4:Plongint; para5:TAtom):TStatus;cdecl;external libX11;
function XGetSizeHints(para1:PDisplay; para2:TWindow; para3:PXSizeHints; para4:TAtom):TStatus;cdecl;external libX11;
function XGetStandardColormap(para1:PDisplay; para2:TWindow; para3:PXStandardColormap; para4:TAtom):TStatus;cdecl;external libX11;
function XGetTextProperty(para1:PDisplay; para2:TWindow; para3:PXTextProperty; para4:TAtom):TStatus;cdecl;external libX11;
function XGetVisualInfo(para1:PDisplay; para2:longint; para3:PXVisualInfo; para4:Plongint):PXVisualInfo;cdecl;external libX11;
function XGetWMClientMachine(para1:PDisplay; para2:TWindow; para3:PXTextProperty):TStatus;cdecl;external libX11;
function XGetWMHints(para1:PDisplay; para2:TWindow):PXWMHints;cdecl;external libX11;
function XGetWMIconName(para1:PDisplay; para2:TWindow; para3:PXTextProperty):TStatus;cdecl;external libX11;
function XGetWMName(para1:PDisplay; para2:TWindow; para3:PXTextProperty):TStatus;cdecl;external libX11;
function XGetWMNormalHints(para1:PDisplay; para2:TWindow; para3:PXSizeHints; para4:Plongint):TStatus;cdecl;external libX11;
function XGetWMSizeHints(para1:PDisplay; para2:TWindow; para3:PXSizeHints; para4:Plongint; para5:TAtom):TStatus;cdecl;external libX11;
function XGetZoomHints(para1:PDisplay; para2:TWindow; para3:PXSizeHints):TStatus;cdecl;external libX11;
function XIntersectRegion(para1:TRegion; para2:TRegion; para3:TRegion):longint;cdecl;external libX11;
procedure XConvertCase(para1:TKeySym; para2:PKeySym; para3:PKeySym);cdecl;external libX11;
function XLookupString(para1:PXKeyEvent; para2:Pchar; para3:longint; para4:PKeySym; para5:PXComposeStatus):longint;cdecl;external libX11;
function XMatchVisualInfo(para1:PDisplay; para2:longint; para3:longint; para4:longint; para5:PXVisualInfo):TStatus;cdecl;external libX11;
function XOffsetRegion(para1:TRegion; para2:longint; para3:longint):longint;cdecl;external libX11;
function XPointInRegion(para1:TRegion; para2:longint; para3:longint):TBool;cdecl;external libX11;
function XPolygonRegion(para1:PXPoint; para2:longint; para3:longint):TRegion;cdecl;external libX11;
function XRectInRegion(para1:TRegion; para2:longint; para3:longint; para4:dword; para5:dword):longint;cdecl;external libX11;
function XSaveContext(para1:PDisplay; para2:TXID; para3:TXContext; para4:Pchar):longint;cdecl;external libX11;
function XSetClassHint(para1:PDisplay; para2:TWindow; para3:PXClassHint):longint;cdecl;external libX11;
function XSetIconSizes(para1:PDisplay; para2:TWindow; para3:PXIconSize; para4:longint):longint;cdecl;external libX11;
function XSetNormalHints(para1:PDisplay; para2:TWindow; para3:PXSizeHints):longint;cdecl;external libX11;
procedure XSetRGBColormaps(para1:PDisplay; para2:TWindow; para3:PXStandardColormap; para4:longint; para5:TAtom);cdecl;external libX11;
function XSetSizeHints(para1:PDisplay; para2:TWindow; para3:PXSizeHints; para4:TAtom):longint;cdecl;external libX11;
function XSetStandardProperties(para1:PDisplay; para2:TWindow; para3:Pchar; para4:Pchar; para5:TPixmap;
           para6:PPchar; para7:longint; para8:PXSizeHints):longint;cdecl;external libX11;
procedure XSetTextProperty(para1:PDisplay; para2:TWindow; para3:PXTextProperty; para4:TAtom);cdecl;external libX11;
procedure XSetWMClientMachine(para1:PDisplay; para2:TWindow; para3:PXTextProperty);cdecl;external libX11;
function XSetWMHints(para1:PDisplay; para2:TWindow; para3:PXWMHints):longint;cdecl;external libX11;
procedure XSetWMIconName(para1:PDisplay; para2:TWindow; para3:PXTextProperty);cdecl;external libX11;
procedure XSetWMName(para1:PDisplay; para2:TWindow; para3:PXTextProperty);cdecl;external libX11;
procedure XSetWMNormalHints(para1:PDisplay; para2:TWindow; para3:PXSizeHints);cdecl;external libX11;
procedure XSetWMProperties(para1:PDisplay; para2:TWindow; para3:PXTextProperty; para4:PXTextProperty; para5:PPchar;
            para6:longint; para7:PXSizeHints; para8:PXWMHints; para9:PXClassHint);cdecl;external libX11;
procedure XmbSetWMProperties(para1:PDisplay; para2:TWindow; para3:Pchar; para4:Pchar; para5:PPchar;
            para6:longint; para7:PXSizeHints; para8:PXWMHints; para9:PXClassHint);cdecl;external libX11;
procedure XSetWMSizeHints(para1:PDisplay; para2:TWindow; para3:PXSizeHints; para4:TAtom);cdecl;external libX11;
function XSetRegion(para1:PDisplay; para2:TGC; para3:TRegion):longint;cdecl;external libX11;
procedure XSetStandardColormap(para1:PDisplay; para2:TWindow; para3:PXStandardColormap; para4:TAtom);cdecl;external libX11;
function XSetZoomHints(para1:PDisplay; para2:TWindow; para3:PXSizeHints):longint;cdecl;external libX11;
function XShrinkRegion(para1:TRegion; para2:longint; para3:longint):longint;cdecl;external libX11;
function XStringListToTextProperty(para1:PPchar; para2:longint; para3:PXTextProperty):TStatus;cdecl;external libX11;
function XSubtractRegion(para1:TRegion; para2:TRegion; para3:TRegion):longint;cdecl;external libX11;
function XmbTextListToTextProperty(para1:PDisplay; para2:PPchar; para3:longint; para4:TXICCEncodingStyle; para5:PXTextProperty):longint;cdecl;external libX11;
function XwcTextListToTextProperty(para1:PDisplay; para2:PPWideChar; para3:longint; para4:TXICCEncodingStyle; para5:PXTextProperty):longint;cdecl;external libX11;
procedure XwcFreeStringList(para1:PPWideChar);cdecl;external libX11;
function XTextPropertyToStringList(para1:PXTextProperty; para2:PPPchar; para3:Plongint):TStatus;cdecl;external libX11;
function XmbTextPropertyToTextList(para1:PDisplay; para2:PXTextProperty; para3:PPPchar; para4:Plongint):longint;cdecl;external libX11;
function XwcTextPropertyToTextList(para1:PDisplay; para2:PXTextProperty; para3:PPPWideChar; para4:Plongint):longint;cdecl;external libX11;
function XUnionRectWithRegion(para1:PXRectangle; para2:TRegion; para3:TRegion):longint;cdecl;external libX11;
function XUnionRegion(para1:TRegion; para2:TRegion; para3:TRegion):longint;cdecl;external libX11;
function XWMGeometry(para1:PDisplay; para2:longint; para3:Pchar; para4:Pchar; para5:dword;
           para6:PXSizeHints; para7:Plongint; para8:Plongint; para9:Plongint; para10:Plongint;
           para11:Plongint):longint;cdecl;external libX11;
function XXorRegion(para1:TRegion; para2:TRegion; para3:TRegion):longint;cdecl;external libX11;

implementation

{$ifdef MACROS}
function XGetPixel(ximage,x,y : longint) : longint;
begin
   XGetPixel:=ximage^.(f.get_pixel)(ximagexy);
end;

function XPutPixel(ximage,x,y,pixel : longint) : longint;
begin
   XPutPixel:=ximage^.(f.put_pixel)(ximagexypixel);
end;

function XSubImage(ximage,x,y,width,height : longint) : longint;
begin
   XSubImage:=ximage^.(f.sub_image)(ximagexywidthheight);
end;

function XAddPixel(ximage,value : longint) : longint;
begin
   XAddPixel:=ximage^.(f.add_pixel)(ximagevalue);
end;

function IsKeypadKey(keysym : longint) : longint;
begin
   IsKeypadKey:=((TKeySym(keysym)) >= XK_KP_Space) and (@((TKeySym(keysym)) <= XK_KP_Equal));
end;

function IsPrivateKeypadKey(keysym : longint) : longint;
begin
   IsPrivateKeypadKey:=((TKeySym(keysym)) >= $11000000) and (@((TKeySym(keysym)) <= $1100FFFF));
end;

function IsCursorKey(keysym : longint) : longint;
begin
   IsCursorKey:=((TKeySym(keysym)) >= XK_Home) and (@((TKeySym(keysym)) < XK_Select));
end;

function IsPFKey(keysym : longint) : longint;
begin
   IsPFKey:=((TKeySym(keysym)) >= XK_KP_F1) and (@((TKeySym(keysym)) <= XK_KP_F4));
end;

function IsFunctionKey(keysym : longint) : longint;
begin
   IsFunctionKey:=((TKeySym(keysym)) >= XK_F1) and (@((TKeySym(keysym)) <= XK_F35));
end;

function IsMiscFunctionKey(keysym : longint) : longint;
begin
   IsMiscFunctionKey:=((TKeySym(keysym)) >= XK_Select) and (@((TKeySym(keysym)) <= XK_Break));
end;

function ReleaseByFreeingColormap : TXID;
  begin
     ReleaseByFreeingColormap:=TXID(1);
  end;

function XUniqueContext : TXContext;
begin
   XUniqueContext:=TXContext(XrmUniqueQuark);
end;

function XStringToContext(string : longint) : TXContext;
begin
   XStringToContext:=TXContext(XrmStringToQuark(_string));
end;
{$endif MACROS}

end.
