unit xlib;
interface
uses
  x;

{$LinkLib c}
{$LinkLib X11}

{
  Automatically converted by H2Pas 0.99.15 from xlib.h
  The following command line parameters were used:
    -p
    -T
    -S
    -d
    -c
    xlib.h
}

{$PACKRECORDS C}


const
   XlibSpecificationRelease = 6;
type

   PXPointer = ^TXPointer;
   TXPointer = char;
   PBool = ^TBool;
   TBool = longbool;
   PStatus = ^TStatus;
   TStatus = longint;

const
   QueuedAlready = 0;
   QueuedAfterReading = 1;
   QueuedAfterFlush = 2;
{$ifdef MACROS}
function ConnectionNumber(dpy : longint) : longint;

function RootWindow(dpy,scr : longint) : longint;

function DefaultScreen(dpy : longint) : longint;

function DefaultRootWindow(dpy : longint) : longint;

function DefaultVisual(dpy,scr : longint) : longint;

function DefaultGC(dpy,scr : longint) : longint;

function BlackPixel(dpy,scr : longint) : longint;

function WhitePixel(dpy,scr : longint) : longint;

function AllPlanes : dword;

function QLength(dpy : longint) : longint;

function DisplayWidth(dpy,scr : longint) : longint;

function DisplayHeight(dpy,scr : longint) : longint;

function DisplayWidthMM(dpy,scr : longint) : longint;

function DisplayHeightMM(dpy,scr : longint) : longint;

function DisplayPlanes(dpy,scr : longint) : longint;

function DisplayCells(dpy,scr : longint) : longint;

function ScreenCount(dpy : longint) : longint;

function ServerVendor(dpy : longint) : longint;

function ProtocolVersion(dpy : longint) : longint;

function ProtocolRevision(dpy : longint) : longint;

function VendorRelease(dpy : longint) : longint;

function DisplayString(dpy : longint) : longint;

function DefaultDepth(dpy,scr : longint) : longint;

function DefaultColormap(dpy,scr : longint) : longint;

function BitmapUnit(dpy : longint) : longint;

function BitmapBitOrder(dpy : longint) : longint;

function BitmapPad(dpy : longint) : longint;

function ImageByteOrder(dpy : longint) : longint;

function NextRequest(dpy : longint) : longint;

function LastKnownRequestProcessed(dpy : longint) : longint;

function ScreenOfDisplay(dpy,scr : longint) : longint;

function DefaultScreenOfDisplay(dpy : longint) : longint;

function DisplayOfScreen(s : longint) : longint;

function RootWindowOfScreen(s : longint) : longint;

function BlackPixelOfScreen(s : longint) : longint;

function WhitePixelOfScreen(s : longint) : longint;

function DefaultColormapOfScreen(s : longint) : longint;

function DefaultDepthOfScreen(s : longint) : longint;

function DefaultGCOfScreen(s : longint) : longint;

function DefaultVisualOfScreen(s : longint) : longint;

function WidthOfScreen(s : longint) : longint;

function HeightOfScreen(s : longint) : longint;

function WidthMMOfScreen(s : longint) : longint;

function HeightMMOfScreen(s : longint) : longint;

function PlanesOfScreen(s : longint) : longint;

function CellsOfScreen(s : longint) : longint;

function MinCmapsOfScreen(s : longint) : longint;

function MaxCmapsOfScreen(s : longint) : longint;

function DoesSaveUnders(s : longint) : longint;

function DoesBackingStore(s : longint) : longint;

function EventMaskOfScreen(s : longint) : longint;

function XAllocID(dpy : longint) : longint;
{$endif MACROS}

type

   PPXExtData = ^PXExtData;
   PXExtData = ^TXExtData;
   TXExtData = record
        number : longint;
        next : PXExtData;
        free_private : function (extension:PXExtData):longint;cdecl;
        private_data : TXPointer;
     end;

   PXExtCodes = ^TXExtCodes;
   TXExtCodes = record
        extension : longint;
        major_opcode : longint;
        first_event : longint;
        first_error : longint;
     end;

   PXPixmapFormatValues = ^TXPixmapFormatValues;
   TXPixmapFormatValues = record
        depth : longint;
        bits_per_pixel : longint;
        scanline_pad : longint;
     end;

   PXGCValues = ^TXGCValues;
   TXGCValues = record
        _function : longint;
        plane_mask : dword;
        foreground : dword;
        background : dword;
        line_width : longint;
        line_style : longint;
        cap_style : longint;
        join_style : longint;
        fill_style : longint;
        fill_rule : longint;
        arc_mode : longint;
        tile : TPixmap;
        stipple : TPixmap;
        ts_x_origin : longint;
        ts_y_origin : longint;
        font : TFont;
        subwindow_mode : longint;
        graphics_exposures : TBool;
        clip_x_origin : longint;
        clip_y_origin : longint;
        clip_mask : TPixmap;
        dash_offset : longint;
        dashes : char;
     end;

   PXGC = ^TXGC;
   TXGC = record
     end;
   TGC = PXGC;
   PGC = ^TGC;

   PVisual = ^TVisual;
   TVisual = record
        ext_data : PXExtData;
        visualid : TVisualID;
        _class : longint;
        red_mask : dword;
        green_mask : dword;
        blue_mask : dword;
        bits_per_rgb : longint;
        map_entries : longint;
     end;

   PDepth = ^TDepth;
   TDepth = record
        depth : longint;
        nvisuals : longint;
        visuals : PVisual;
     end;
   PXDisplay = ^TXDisplay;
   TXDisplay = record
     end;


   PScreen = ^TScreen;
   TScreen = record
        ext_data : PXExtData;
        display : PXDisplay;
        root : TWindow;
        width : longint;
        height : longint;
        mwidth : longint;
        mheight : longint;
        ndepths : longint;
        depths : PDepth;
        root_depth : longint;
        root_visual : PVisual;
        default_gc : TGC;
        cmap : TColormap;
        white_pixel : dword;
        black_pixel : dword;
        max_maps : longint;
        min_maps : longint;
        backing_store : longint;
        save_unders : TBool;
        root_input_mask : longint;
     end;

   PScreenFormat = ^TScreenFormat;
   TScreenFormat = record
        ext_data : PXExtData;
        depth : longint;
        bits_per_pixel : longint;
        scanline_pad : longint;
     end;

   PXSetWindowAttributes = ^TXSetWindowAttributes;
   TXSetWindowAttributes = record
        background_pixmap : TPixmap;
        background_pixel : dword;
        border_pixmap : TPixmap;
        border_pixel : dword;
        bit_gravity : longint;
        win_gravity : longint;
        backing_store : longint;
        backing_planes : dword;
        backing_pixel : dword;
        save_under : TBool;
        event_mask : longint;
        do_not_propagate_mask : longint;
        override_redirect : TBool;
        colormap : TColormap;
        cursor : TCursor;
     end;

   PXWindowAttributes = ^TXWindowAttributes;
   TXWindowAttributes = record
        x : longint;
        y : longint;
        width : longint;
        height : longint;
        border_width : longint;
        depth : longint;
        visual : PVisual;
        root : TWindow;
        _class : longint;
        bit_gravity : longint;
        win_gravity : longint;
        backing_store : longint;
        backing_planes : dword;
        backing_pixel : dword;
        save_under : TBool;
        colormap : TColormap;
        map_installed : TBool;
        map_state : longint;
        all_event_masks : longint;
        your_event_mask : longint;
        do_not_propagate_mask : longint;
        override_redirect : TBool;
        screen : PScreen;
     end;

   PXHostAddress = ^TXHostAddress;
   TXHostAddress = record
        family : longint;
        length : longint;
        address : Pchar;
     end;

   PXImage = ^TXImage;
   TXImage = record
        width : longint;
        height : longint;
        xoffset : longint;
        format : longint;
        data : Pchar;
        byte_order : longint;
        bitmap_unit : longint;
        bitmap_bit_order : longint;
        bitmap_pad : longint;
        depth : longint;
        bytes_per_line : longint;
        bits_per_pixel : longint;
        red_mask : dword;
        green_mask : dword;
        blue_mask : dword;
        obdata : TXPointer;
        f : record
             create_image : function (para1:PXDisplay; para2:PVisual; para3:dword; para4:longint; para5:longint;
                          para6:Pchar; para7:dword; para8:dword; para9:longint; para10:longint):PXImage;cdecl;
             destroy_image : function (para1:PXImage):longint;
             get_pixel : function (para1:PXImage; para2:longint; para3:longint):dword;
             put_pixel : function (para1:PXImage; para2:longint; para3:longint; para4:dword):longint;
             sub_image : function (para1:PXImage; para2:longint; para3:longint; para4:dword; para5:dword):PXImage;
             add_pixel : function (para1:PXImage; para2:longint):longint;
          end;
     end;

   PXWindowChanges = ^TXWindowChanges;
   TXWindowChanges = record
        x : longint;
        y : longint;
        width : longint;
        height : longint;
        border_width : longint;
        sibling : TWindow;
        stack_mode : longint;
     end;

   PXColor = ^TXColor;
   TXColor = record
        pixel : dword;
        red : word;
        green : word;
        blue : word;
        flags : char;
        pad : char;
     end;

   PXSegment = ^TXSegment;
   TXSegment = record
        x1 : smallint;
        y1 : smallint;
        x2 : smallint;
        y2 : smallint;
     end;

   PXPoint = ^TXPoint;
   TXPoint = record
        x : smallint;
        y : smallint;
     end;

   PXRectangle = ^TXRectangle;
   TXRectangle = record
        x : smallint;
        y : smallint;
        width : word;
        height : word;
     end;

   PXArc = ^TXArc;
   TXArc = record
        x : smallint;
        y : smallint;
        width : word;
        height : word;
        angle1 : smallint;
        angle2 : smallint;
     end;

   PXKeyboardControl = ^TXKeyboardControl;
   TXKeyboardControl = record
        key_click_percent : longint;
        bell_percent : longint;
        bell_pitch : longint;
        bell_duration : longint;
        led : longint;
        led_mode : longint;
        key : longint;
        auto_repeat_mode : longint;
     end;

   PXKeyboardState = ^TXKeyboardState;
   TXKeyboardState = record
        key_click_percent : longint;
        bell_percent : longint;
        bell_pitch : dword;
        bell_duration : dword;
        led_mask : dword;
        global_auto_repeat : longint;
        auto_repeats : array[0..31] of char;
     end;

   PXTimeCoord = ^TXTimeCoord;
   TXTimeCoord = record
        time : TTime;
        x : smallint;
        y : smallint;
     end;

   PXModifierKeymap = ^TXModifierKeymap;
   TXModifierKeymap = record
        max_keypermod : longint;
        modifiermap : PKeyCode;
     end;

   PDisplay = ^TDisplay;
   TDisplay = TXDisplay;

   PXPrivate = ^TXPrivate;
   TXPrivate = record
     end;

   PXrmHashBucketRec = ^TXrmHashBucketRec;
   TXrmHashBucketRec = record
     end;


   PXPrivDisplay = ^TXPrivDisplay;
   TXPrivDisplay = record
        ext_data : PXExtData;
        private1 : PXPrivate;
        fd : longint;
        private2 : longint;
        proto_major_version : longint;
        proto_minor_version : longint;
        vendor : Pchar;
        private3 : TXID;
        private4 : TXID;
        private5 : TXID;
        private6 : longint;
        resource_alloc : function (para1:PXDisplay):TXID;cdecl;
        byte_order : longint;
        bitmap_unit : longint;
        bitmap_pad : longint;
        bitmap_bit_order : longint;
        nformats : longint;
        pixmap_format : PScreenFormat;
        private8 : longint;
        release : longint;
        private9 : PXPrivate;
        private10 : PXPrivate;
        qlen : longint;
        last_request_read : dword;
        request : dword;
        private11 : TXPointer;
        private12 : TXPointer;
        private13 : TXPointer;
        private14 : TXPointer;
        max_request_size : dword;
        db : PXrmHashBucketRec;
        private15 : function (para1:PXDisplay):longint;
        display_name : Pchar;
        default_screen : longint;
        nscreens : longint;
        screens : PScreen;
        motion_buffer : dword;
        private16 : dword;
        min_keycode : longint;
        max_keycode : longint;
        private17 : TXPointer;
        private18 : TXPointer;
        private19 : longint;
        xdefaults : Pchar;
     end;

   PXKeyEvent = ^TXKeyEvent;
   TXKeyEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        window : TWindow;
        root : TWindow;
        subwindow : TWindow;
        time : TTime;
        x : longint;
        y : longint;
        x_root : longint;
        y_root : longint;
        state : dword;
        keycode : dword;
        same_screen : TBool;
     end;

   PXKeyPressedEvent = ^TXKeyPressedEvent;
   TXKeyPressedEvent = TXKeyEvent;

   PXKeyReleasedEvent = ^TXKeyReleasedEvent;
   TXKeyReleasedEvent = TXKeyEvent;

   PXButtonEvent = ^TXButtonEvent;
   TXButtonEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        window : TWindow;
        root : TWindow;
        subwindow : TWindow;
        time : TTime;
        x : longint;
        y : longint;
        x_root : longint;
        y_root : longint;
        state : dword;
        button : dword;
        same_screen : TBool;
     end;

   PXButtonPressedEvent = ^TXButtonPressedEvent;
   TXButtonPressedEvent = TXButtonEvent;

   PXButtonReleasedEvent = ^TXButtonReleasedEvent;
   TXButtonReleasedEvent = TXButtonEvent;

   PXMotionEvent = ^TXMotionEvent;
   TXMotionEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        window : TWindow;
        root : TWindow;
        subwindow : TWindow;
        time : TTime;
        x : longint;
        y : longint;
        x_root : longint;
        y_root : longint;
        state : dword;
        is_hint : char;
        same_screen : TBool;
     end;

   PXPointerMovedEvent = ^TXPointerMovedEvent;
   TXPointerMovedEvent = TXMotionEvent;

   PXCrossingEvent = ^TXCrossingEvent;
   TXCrossingEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        window : TWindow;
        root : TWindow;
        subwindow : TWindow;
        time : TTime;
        x : longint;
        y : longint;
        x_root : longint;
        y_root : longint;
        mode : longint;
        detail : longint;
        same_screen : TBool;
        focus : TBool;
        state : dword;
     end;

   PXEnterWindowEvent = ^TXEnterWindowEvent;
   TXEnterWindowEvent = TXCrossingEvent;

   PXLeaveWindowEvent = ^TXLeaveWindowEvent;
   TXLeaveWindowEvent = TXCrossingEvent;

   PXFocusChangeEvent = ^TXFocusChangeEvent;
   TXFocusChangeEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        window : TWindow;
        mode : longint;
        detail : longint;
     end;

   PXFocusInEvent = ^TXFocusInEvent;
   TXFocusInEvent = TXFocusChangeEvent;

   PXFocusOutEvent = ^TXFocusOutEvent;
   TXFocusOutEvent = TXFocusChangeEvent;

   PXKeymapEvent = ^TXKeymapEvent;
   TXKeymapEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        window : TWindow;
        key_vector : array[0..31] of char;
     end;

   PXExposeEvent = ^TXExposeEvent;
   TXExposeEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        window : TWindow;
        x : longint;
        y : longint;
        width : longint;
        height : longint;
        count : longint;
     end;

   PXGraphicsExposeEvent = ^TXGraphicsExposeEvent;
   TXGraphicsExposeEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        drawable : TDrawable;
        x : longint;
        y : longint;
        width : longint;
        height : longint;
        count : longint;
        major_code : longint;
        minor_code : longint;
     end;

   PXNoExposeEvent = ^TXNoExposeEvent;
   TXNoExposeEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        drawable : TDrawable;
        major_code : longint;
        minor_code : longint;
     end;

   PXVisibilityEvent = ^TXVisibilityEvent;
   TXVisibilityEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        window : TWindow;
        state : longint;
     end;

   PXCreateWindowEvent = ^TXCreateWindowEvent;
   TXCreateWindowEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        parent : TWindow;
        window : TWindow;
        x : longint;
        y : longint;
        width : longint;
        height : longint;
        border_width : longint;
        override_redirect : TBool;
     end;

   PXDestroyWindowEvent = ^TXDestroyWindowEvent;
   TXDestroyWindowEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        event : TWindow;
        window : TWindow;
     end;

   PXUnmapEvent = ^TXUnmapEvent;
   TXUnmapEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        event : TWindow;
        window : TWindow;
        from_configure : TBool;
     end;

   PXMapEvent = ^TXMapEvent;
   TXMapEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        event : TWindow;
        window : TWindow;
        override_redirect : TBool;
     end;

   PXMapRequestEvent = ^TXMapRequestEvent;
   TXMapRequestEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        parent : TWindow;
        window : TWindow;
     end;

   PXReparentEvent = ^TXReparentEvent;
   TXReparentEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        event : TWindow;
        window : TWindow;
        parent : TWindow;
        x : longint;
        y : longint;
        override_redirect : TBool;
     end;

   PXConfigureEvent = ^TXConfigureEvent;
   TXConfigureEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        event : TWindow;
        window : TWindow;
        x : longint;
        y : longint;
        width : longint;
        height : longint;
        border_width : longint;
        above : TWindow;
        override_redirect : TBool;
     end;

   PXGravityEvent = ^TXGravityEvent;
   TXGravityEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        event : TWindow;
        window : TWindow;
        x : longint;
        y : longint;
     end;

   PXResizeRequestEvent = ^TXResizeRequestEvent;
   TXResizeRequestEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        window : TWindow;
        width : longint;
        height : longint;
     end;

   PXConfigureRequestEvent = ^TXConfigureRequestEvent;
   TXConfigureRequestEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        parent : TWindow;
        window : TWindow;
        x : longint;
        y : longint;
        width : longint;
        height : longint;
        border_width : longint;
        above : TWindow;
        detail : longint;
        value_mask : dword;
     end;

   PXCirculateEvent = ^TXCirculateEvent;
   TXCirculateEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        event : TWindow;
        window : TWindow;
        place : longint;
     end;

   PXCirculateRequestEvent = ^TXCirculateRequestEvent;
   TXCirculateRequestEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        parent : TWindow;
        window : TWindow;
        place : longint;
     end;

   PXPropertyEvent = ^TXPropertyEvent;
   TXPropertyEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        window : TWindow;
        atom : TAtom;
        time : TTime;
        state : longint;
     end;

   PXSelectionClearEvent = ^TXSelectionClearEvent;
   TXSelectionClearEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        window : TWindow;
        selection : TAtom;
        time : TTime;
     end;

   PXSelectionRequestEvent = ^TXSelectionRequestEvent;
   TXSelectionRequestEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        owner : TWindow;
        requestor : TWindow;
        selection : TAtom;
        target : TAtom;
        _property : TAtom;
        time : TTime;
     end;

   PXSelectionEvent = ^TXSelectionEvent;
   TXSelectionEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        requestor : TWindow;
        selection : TAtom;
        target : TAtom;
        _property : TAtom;
        time : TTime;
     end;

   PXColormapEvent = ^TXColormapEvent;
   TXColormapEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        window : TWindow;
        colormap : TColormap;
        _new : TBool;
        state : longint;
     end;

   PXClientMessageEvent = ^TXClientMessageEvent;
   TXClientMessageEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        window : TWindow;
        message_type : TAtom;
        format : longint;
        data : record
            case longint of
               0 : ( b : array[0..19] of char );
               1 : ( s : array[0..9] of smallint );
               2 : ( l : array[0..4] of longint );
            end;
     end;

   PXMappingEvent = ^TXMappingEvent;
   TXMappingEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        window : TWindow;
        request : longint;
        first_keycode : longint;
        count : longint;
     end;

   PXErrorEvent = ^TXErrorEvent;
   TXErrorEvent = record
        _type : longint;
        display : PDisplay;
        resourceid : TXID;
        serial : dword;
        error_code : byte;
        request_code : byte;
        minor_code : byte;
     end;

   PXAnyEvent = ^TXAnyEvent;
   TXAnyEvent = record
        _type : longint;
        serial : dword;
        send_event : TBool;
        display : PDisplay;
        window : TWindow;
     end;

   PXEvent = ^TXEvent;
   TXEvent = record
       case longint of
          0 : ( _type : longint );
          1 : ( xany : TXAnyEvent );
          2 : ( xkey : TXKeyEvent );
          3 : ( xbutton : TXButtonEvent );
          4 : ( xmotion : TXMotionEvent );
          5 : ( xcrossing : TXCrossingEvent );
          6 : ( xfocus : TXFocusChangeEvent );
          7 : ( xexpose : TXExposeEvent );
          8 : ( xgraphicsexpose : TXGraphicsExposeEvent );
          9 : ( xnoexpose : TXNoExposeEvent );
          10 : ( xvisibility : TXVisibilityEvent );
          11 : ( xcreatewindow : TXCreateWindowEvent );
          12 : ( xdestroywindow : TXDestroyWindowEvent );
          13 : ( xunmap : TXUnmapEvent );
          14 : ( xmap : TXMapEvent );
          15 : ( xmaprequest : TXMapRequestEvent );
          16 : ( xreparent : TXReparentEvent );
          17 : ( xconfigure : TXConfigureEvent );
          18 : ( xgravity : TXGravityEvent );
          19 : ( xresizerequest : TXResizeRequestEvent );
          20 : ( xconfigurerequest : TXConfigureRequestEvent );
          21 : ( xcirculate : TXCirculateEvent );
          22 : ( xcirculaterequest : TXCirculateRequestEvent );
          23 : ( xproperty : TXPropertyEvent );
          24 : ( xselectionclear : TXSelectionClearEvent );
          25 : ( xselectionrequest : TXSelectionRequestEvent );
          26 : ( xselection : TXSelectionEvent );
          27 : ( xcolormap : TXColormapEvent );
          28 : ( xclient : TXClientMessageEvent );
          29 : ( xmapping : TXMappingEvent );
          30 : ( xerror : TXErrorEvent );
          31 : ( xkeymap : TXKeymapEvent );
          32 : ( pad : array[0..23] of longint );
       end;

type

   PXCharStruct = ^TXCharStruct;
   TXCharStruct = record
        lbearing : smallint;
        rbearing : smallint;
        width : smallint;
        ascent : smallint;
        descent : smallint;
        attributes : word;
     end;

   PXFontProp = ^TXFontProp;
   TXFontProp = record
        name : TAtom;
        card32 : dword;
     end;

   PPPXFontStruct = ^PPXFontStruct;
   PPXFontStruct = ^PXFontStruct;
   PXFontStruct = ^TXFontStruct;
   TXFontStruct = record
        ext_data : PXExtData;
        fid : TFont;
        direction : dword;
        min_char_or_byte2 : dword;
        max_char_or_byte2 : dword;
        min_byte1 : dword;
        max_byte1 : dword;
        all_chars_exist : TBool;
        default_char : dword;
        n_properties : longint;
        properties : PXFontProp;
        min_bounds : TXCharStruct;
        max_bounds : TXCharStruct;
        per_char : PXCharStruct;
        ascent : longint;
        descent : longint;
     end;

   PXTextItem = ^TXTextItem;
   TXTextItem = record
        chars : Pchar;
        nchars : longint;
        delta : longint;
        font : TFont;
     end;

   PXChar2b = ^TXChar2b;
   TXChar2b = record
        byte1 : byte;
        byte2 : byte;
     end;

   PXTextItem16 = ^TXTextItem16;
   TXTextItem16 = record
        chars : PXChar2b;
        nchars : longint;
        delta : longint;
        font : TFont;
     end;

   PXEDataObject = ^TXEDataObject;
   TXEDataObject = record
       case longint of
          0 : ( display : PDisplay );
          1 : ( gc : TGC );
          2 : ( visual : PVisual );
          3 : ( screen : PScreen );
          4 : ( pixmap_format : PScreenFormat );
          5 : ( font : PXFontStruct );
       end;

   PXFontSetExtents = ^TXFontSetExtents;
   TXFontSetExtents = record
        max_ink_extent : TXRectangle;
        max_logical_extent : TXRectangle;
     end;

   PXOM = ^TXOM;
   TXOM = record
     end;

   PXOC = ^TXOC;
   TXOC = record
     end;
   TXFontSet = PXOC;
   PXFontSet = ^TXFontSet;

   PXmbTextItem = ^TXmbTextItem;
   TXmbTextItem = record
        chars : Pchar;
        nchars : longint;
        delta : longint;
        font_set : TXFontSet;
     end;

   PXwcTextItem = ^TXwcTextItem;
   TXwcTextItem = record
        chars : PWideChar;
        nchars : longint;
        delta : longint;
        font_set : TXFontSet;
     end;

const
   XNRequiredCharSet = 'requiredCharSet';
   XNQueryOrientation = 'queryOrientation';
   XNBaseFontName = 'baseFontName';
   XNOMAutomatic = 'omAutomatic';
   XNMissingCharSet = 'missingCharSet';
   XNDefaultString = 'defaultString';
   XNOrientation = 'orientation';
   XNDirectionalDependentDrawing = 'directionalDependentDrawing';
   XNContextualDrawing = 'contextualDrawing';
   XNFontInfo = 'fontInfo';
type

   PXOMCharSetList = ^TXOMCharSetList;
   TXOMCharSetList = record
        charset_count : longint;
        charset_list : PPChar;
     end;

   PXOrientation = ^TXOrientation;
   TXOrientation = (XOMOrientation_LTR_TTB,XOMOrientation_RTL_TTB,
     XOMOrientation_TTB_LTR,XOMOrientation_TTB_RTL,
     XOMOrientation_Context);

   PXOMOrientation = ^TXOMOrientation;
   TXOMOrientation = record
        num_orientation : longint;
        orientation : PXOrientation;
     end;

   PXOMFontInfo = ^TXOMFontInfo;
   TXOMFontInfo = record
        num_font : longint;
        font_struct_list : ^PXFontStruct;
        font_name_list : PPChar;
     end;

   PXIM = ^TXIM;
   TXIM = record
     end;

   PXIC = ^TXIC;
   TXIC = record
     end;

   TXIMProc = procedure (para1:TXIM; para2:TXPointer; para3:TXPointer);cdecl;

   TXICProc = function (para1:TXIC; para2:TXPointer; para3:TXPointer):TBool;cdecl;

   TXIDProc = procedure (para1:PDisplay; para2:TXPointer; para3:TXPointer);cdecl;

   PXIMStyle = ^TXIMStyle;
   TXIMStyle = dword;

   PXIMStyles = ^TXIMStyles;
   TXIMStyles = record
        count_styles : word;
        supported_styles : PXIMStyle;
     end;

const
   XIMPreeditArea = $0001;
   XIMPreeditCallbacks = $0002;
   XIMPreeditPosition = $0004;
   XIMPreeditNothing = $0008;
   XIMPreeditNone = $0010;
   XIMStatusArea = $0100;
   XIMStatusCallbacks = $0200;
   XIMStatusNothing = $0400;
   XIMStatusNone = $0800;
   XNVaNestedList = 'XNVaNestedList';
   XNQueryInputStyle = 'queryInputStyle';
   XNClientWindow = 'clientWindow';
   XNInputStyle = 'inputStyle';
   XNFocusWindow = 'focusWindow';
   XNResourceName = 'resourceName';
   XNResourceClass = 'resourceClass';
   XNGeometryCallback = 'geometryCallback';
   XNDestroyCallback = 'destroyCallback';
   XNFilterEvents = 'filterEvents';
   XNPreeditStartCallback = 'preeditStartCallback';
   XNPreeditDoneCallback = 'preeditDoneCallback';
   XNPreeditDrawCallback = 'preeditDrawCallback';
   XNPreeditCaretCallback = 'preeditCaretCallback';
   XNPreeditStateNotifyCallback = 'preeditStateNotifyCallback';
   XNPreeditAttributes = 'preeditAttributes';
   XNStatusStartCallback = 'statusStartCallback';
   XNStatusDoneCallback = 'statusDoneCallback';
   XNStatusDrawCallback = 'statusDrawCallback';
   XNStatusAttributes = 'statusAttributes';
   XNArea = 'area';
   XNAreaNeeded = 'areaNeeded';
   XNSpotLocation = 'spotLocation';
   XNColormap = 'colorMap';
   XNStdColormap = 'stdColorMap';
   XNForeground = 'foreground';
   XNBackground = 'background';
   XNBackgroundPixmap = 'backgroundPixmap';
   XNFontSet = 'fontSet';
   XNLineSpace = 'lineSpace';
   XNCursor = 'cursor';
   XNQueryIMValuesList = 'queryIMValuesList';
   XNQueryICValuesList = 'queryICValuesList';
   XNVisiblePosition = 'visiblePosition';
   XNR6PreeditCallback = 'r6PreeditCallback';
   XNStringConversionCallback = 'stringConversionCallback';
   XNStringConversion = 'stringConversion';
   XNResetState = 'resetState';
   XNHotKey = 'hotKey';
   XNHotKeyState = 'hotKeyState';
   XNPreeditState = 'preeditState';
   XNSeparatorofNestedList = 'separatorofNestedList';
   XBufferOverflow = -(1);
   XLookupNone = 1;
   XLookupChars = 2;
   XLookupKeySymVal = 3;
   XLookupBoth = 4;
type

   PXVaNestedList = ^TXVaNestedList;
   TXVaNestedList = pointer;

   PXIMCallback = ^TXIMCallback;
   TXIMCallback = record
        client_data : TXPointer;
        callback : TXIMProc;
     end;

   PXICCallback = ^TXICCallback;
   TXICCallback = record
        client_data : TXPointer;
        callback : TXICProc;
     end;

   PXIMFeedback = ^TXIMFeedback;
   TXIMFeedback = dword;

const
   XIMReverse = 1;
   XIMUnderline = 1 shl 1;
   XIMHighlight = 1 shl 2;
   XIMPrimary = 1 shl 5;
   XIMSecondary = 1 shl 6;
   XIMTertiary = 1 shl 7;
   XIMVisibleToForward = 1 shl 8;
   XIMVisibleToBackword = 1 shl 9;
   XIMVisibleToCenter = 1 shl 10;
type

   PXIMText = ^TXIMText;
   TXIMText = record
        length : word;
        feedback : PXIMFeedback;
        encoding_is_wchar : TBool;
        _string : record
            case longint of
               0 : ( multi_byte : Pchar );
               1 : ( wide_char : PWideChar );
            end;
     end;

   PXIMPreeditState = ^TXIMPreeditState;
   TXIMPreeditState = dword;

const
   XIMPreeditUnKnown = 0;
   XIMPreeditEnable = 1;
   XIMPreeditDisable = 1 shl 1;
type

   PXIMPreeditStateNotifyCallbackStruct = ^TXIMPreeditStateNotifyCallbackStruct;
   TXIMPreeditStateNotifyCallbackStruct = record
        state : TXIMPreeditState;
     end;

   PXIMResetState = ^TXIMResetState;
   TXIMResetState = dword;

const
   XIMInitialState = 1;
   XIMPreserveState = 1 shl 1;
type

   PXIMStringConversionFeedback = ^TXIMStringConversionFeedback;
   TXIMStringConversionFeedback = dword;

const
   XIMStringConversionLeftEdge = $00000001;
   XIMStringConversionRightEdge = $00000002;
   XIMStringConversionTopEdge = $00000004;
   XIMStringConversionBottomEdge = $00000008;
   XIMStringConversionConcealed = $00000010;
   XIMStringConversionWrapped = $00000020;
type

   PXIMStringConversionText = ^TXIMStringConversionText;
   TXIMStringConversionText = record
        length : word;
        feedback : PXIMStringConversionFeedback;
        encoding_is_wchar : TBool;
        _string : record
            case longint of
               0 : ( mbs : Pchar );
               1 : ( wcs : PWideChar );
            end;
     end;

   PXIMStringConversionPosition = ^TXIMStringConversionPosition;
   TXIMStringConversionPosition = word;

   PXIMStringConversionType = ^TXIMStringConversionType;
   TXIMStringConversionType = word;

const
   XIMStringConversionBuffer = $0001;
   XIMStringConversionLine = $0002;
   XIMStringConversionWord = $0003;
   XIMStringConversionChar = $0004;
type

   PXIMStringConversionOperation = ^TXIMStringConversionOperation;
   TXIMStringConversionOperation = word;

const
   XIMStringConversionSubstitution = $0001;
   XIMStringConversionRetrieval = $0002;
type

   PXIMCaretDirection = ^TXIMCaretDirection;
   TXIMCaretDirection = (XIMForwardChar,XIMBackwardChar,XIMForwardWord,
     XIMBackwardWord,XIMCaretUp,XIMCaretDown,
     XIMNextLine,XIMPreviousLine,XIMLineStart,
     XIMLineEnd,XIMAbsolutePosition,XIMDontChange
     );

   PXIMStringConversionCallbackStruct = ^TXIMStringConversionCallbackStruct;
   TXIMStringConversionCallbackStruct = record
        position : TXIMStringConversionPosition;
        direction : TXIMCaretDirection;
        operation : TXIMStringConversionOperation;
        factor : word;
        text : PXIMStringConversionText;
     end;

   PXIMPreeditDrawCallbackStruct = ^TXIMPreeditDrawCallbackStruct;
   TXIMPreeditDrawCallbackStruct = record
        caret : longint;
        chg_first : longint;
        chg_length : longint;
        text : PXIMText;
     end;

   PXIMCaretStyle = ^TXIMCaretStyle;
   TXIMCaretStyle = (XIMIsInvisible,XIMIsPrimary,XIMIsSecondary
     );

   PXIMPreeditCaretCallbackStruct = ^TXIMPreeditCaretCallbackStruct;
   TXIMPreeditCaretCallbackStruct = record
        position : longint;
        direction : TXIMCaretDirection;
        style : TXIMCaretStyle;
     end;

   PXIMStatusDataType = ^TXIMStatusDataType;
   TXIMStatusDataType = (XIMTextType,XIMBitmapType);

   PXIMStatusDrawCallbackStruct = ^TXIMStatusDrawCallbackStruct;
   TXIMStatusDrawCallbackStruct = record
        _type : TXIMStatusDataType;
        data : record
            case longint of
               0 : ( text : PXIMText );
               1 : ( bitmap : TPixmap );
            end;
     end;

   PXIMHotKeyTrigger = ^TXIMHotKeyTrigger;
   TXIMHotKeyTrigger = record
        keysym : TKeySym;
        modifier : longint;
        modifier_mask : longint;
     end;

   PXIMHotKeyTriggers = ^TXIMHotKeyTriggers;
   TXIMHotKeyTriggers = record
        num_hot_key : longint;
        key : PXIMHotKeyTrigger;
     end;

   PXIMHotKeyState = ^TXIMHotKeyState;
   TXIMHotKeyState = dword;

const
   XIMHotKeyStateON = $0001;
   XIMHotKeyStateOFF = $0002;
type

   PXIMValuesList = ^TXIMValuesList;
   TXIMValuesList = record
        count_values : word;
        supported_values : PPChar;
     end;
  var
     _Xdebug : longint;cvar;external;
type
  funclong = function :longint;
  funcbool = function :TBool;
  chararr32 = array[0..31] of char;

function XLoadQueryFont(para1:PDisplay; para2:Pchar):PXFontStruct;cdecl;external;
function XQueryFont(para1:PDisplay; para2:TXID):PXFontStruct;cdecl;external;
function XGetMotionEvents(para1:PDisplay; para2:TWindow; para3:TTime; para4:TTime; para5:Plongint):PXTimeCoord;cdecl;external;
function XDeleteModifiermapEntry(para1:PXModifierKeymap; para2:TKeyCode; para3:longint):PXModifierKeymap;cdecl;external;
function XGetModifierMapping(para1:PDisplay):PXModifierKeymap;cdecl;external;
function XInsertModifiermapEntry(para1:PXModifierKeymap; para2:TKeyCode; para3:longint):PXModifierKeymap;cdecl;external;
function XNewModifiermap(para1:longint):PXModifierKeymap;cdecl;external;
function XCreateImage(para1:PDisplay; para2:PVisual; para3:dword; para4:longint; para5:longint;
           para6:Pchar; para7:dword; para8:dword; para9:longint; para10:longint):PXImage;cdecl;external;
function XInitImage(para1:PXImage):TStatus;cdecl;external;
function XGetImage(para1:PDisplay; para2:TDrawable; para3:longint; para4:longint; para5:dword;
           para6:dword; para7:dword; para8:longint):PXImage;cdecl;external;
function XGetSubImage(para1:PDisplay; para2:TDrawable; para3:longint; para4:longint; para5:dword;
           para6:dword; para7:dword; para8:longint; para9:PXImage; para10:longint;
           para11:longint):PXImage;cdecl;external;
function XOpenDisplay(para1:Pchar):PDisplay;cdecl;external;
procedure XrmInitialize;cdecl;external;
function XFetchBytes(para1:PDisplay; para2:Plongint):Pchar;cdecl;external;
function XFetchBuffer(para1:PDisplay; para2:Plongint; para3:longint):Pchar;cdecl;external;
function XGetAtomName(para1:PDisplay; para2:TAtom):Pchar;cdecl;external;
function XGetAtomNames(para1:PDisplay; para2:PAtom; para3:longint; para4:PPchar):TStatus;cdecl;external;
function XGetDefault(para1:PDisplay; para2:Pchar; para3:Pchar):Pchar;cdecl;external;
function XDisplayName(para1:Pchar):Pchar;cdecl;external;
function XKeysymToString(para1:TKeySym):Pchar;cdecl;external;
function XSynchronize(para1:PDisplay; para2:TBool):funclong;cdecl;external;
function XSetAfterFunction(para1:PDisplay; para2:funclong):funclong;cdecl;external;
function XInternAtom(para1:PDisplay; para2:Pchar; para3:TBool):TAtom;cdecl;external;
function XInternAtoms(para1:PDisplay; para2:PPchar; para3:longint; para4:TBool; para5:PAtom):TStatus;cdecl;external;
function XCopyColormapAndFree(para1:PDisplay; para2:TColormap):TColormap;cdecl;external;
function XCreateColormap(para1:PDisplay; para2:TWindow; para3:PVisual; para4:longint):TColormap;cdecl;external;
function XCreatePixmapCursor(para1:PDisplay; para2:TPixmap; para3:TPixmap; para4:PXColor; para5:PXColor;
           para6:dword; para7:dword):TCursor;cdecl;external;
function XCreateGlyphCursor(para1:PDisplay; para2:TFont; para3:TFont; para4:dword; para5:dword;
           para6:PXColor; para7:PXColor):TCursor;cdecl;external;
function XCreateFontCursor(para1:PDisplay; para2:dword):TCursor;cdecl;external;
function XLoadFont(para1:PDisplay; para2:Pchar):TFont;cdecl;external;
function XCreateGC(para1:PDisplay; para2:TDrawable; para3:dword; para4:PXGCValues):TGC;cdecl;external;
function XGContextFromGC(para1:TGC):TGContext;cdecl;external;
procedure XFlushGC(para1:PDisplay; para2:TGC);cdecl;external;
function XCreatePixmap(para1:PDisplay; para2:TDrawable; para3:dword; para4:dword; para5:dword):TPixmap;cdecl;external;
function XCreateBitmapFromData(para1:PDisplay; para2:TDrawable; para3:Pchar; para4:dword; para5:dword):TPixmap;cdecl;external;
function XCreatePixmapFromBitmapData(para1:PDisplay; para2:TDrawable; para3:Pchar; para4:dword; para5:dword;
           para6:dword; para7:dword; para8:dword):TPixmap;cdecl;external;
function XCreateSimpleWindow(para1:PDisplay; para2:TWindow; para3:longint; para4:longint; para5:dword;
           para6:dword; para7:dword; para8:dword; para9:dword):TWindow;cdecl;external;
function XGetSelectionOwner(para1:PDisplay; para2:TAtom):TWindow;cdecl;external;
function XCreateWindow(para1:PDisplay; para2:TWindow; para3:longint; para4:longint; para5:dword;
           para6:dword; para7:dword; para8:longint; para9:dword; para10:PVisual;
           para11:dword; para12:PXSetWindowAttributes):TWindow;cdecl;external;
function XListInstalledColormaps(para1:PDisplay; para2:TWindow; para3:Plongint):PColormap;cdecl;external;
function XListFonts(para1:PDisplay; para2:Pchar; para3:longint; para4:Plongint):PPChar;cdecl;external;
function XListFontsWithInfo(para1:PDisplay; para2:Pchar; para3:longint; para4:Plongint; para5:PPXFontStruct):PPChar;cdecl;external;
function XGetFontPath(para1:PDisplay; para2:Plongint):PPChar;cdecl;external;
function XListExtensions(para1:PDisplay; para2:Plongint):PPChar;cdecl;external;
function XListProperties(para1:PDisplay; para2:TWindow; para3:Plongint):PAtom;cdecl;external;
function XListHosts(para1:PDisplay; para2:Plongint; para3:PBool):PXHostAddress;cdecl;external;
function XKeycodeToKeysym(para1:PDisplay; para2:TKeyCode; para3:longint):TKeySym;cdecl;external;
function XLookupKeysym(para1:PXKeyEvent; para2:longint):TKeySym;cdecl;external;
function XGetKeyboardMapping(para1:PDisplay; para2:TKeyCode; para3:longint; para4:Plongint):PKeySym;cdecl;external;
function XStringToKeysym(para1:Pchar):TKeySym;cdecl;external;
function XMaxRequestSize(para1:PDisplay):longint;cdecl;external;
function XExtendedMaxRequestSize(para1:PDisplay):longint;cdecl;external;
function XResourceManagerString(para1:PDisplay):Pchar;cdecl;external;
function XScreenResourceString(para1:PScreen):Pchar;cdecl;external;
function XDisplayMotionBufferSize(para1:PDisplay):dword;cdecl;external;
function XVisualIDFromVisual(para1:PVisual):TVisualID;cdecl;external;
function XInitThreads:TStatus;cdecl;external;
procedure XLockDisplay(para1:PDisplay);cdecl;external;
procedure XUnlockDisplay(para1:PDisplay);cdecl;external;
function XInitExtension(para1:PDisplay; para2:Pchar):PXExtCodes;cdecl;external;
function XAddExtension(para1:PDisplay):PXExtCodes;cdecl;external;
function XFindOnExtensionList(para1:PPXExtData; para2:longint):PXExtData;cdecl;external;
function XEHeadOfExtensionList(para1:TXEDataObject):PPXExtData;cdecl;external;
function XRootWindow(para1:PDisplay; para2:longint):TWindow;cdecl;external;
function XDefaultRootWindow(para1:PDisplay):TWindow;cdecl;external;
function XRootWindowOfScreen(para1:PScreen):TWindow;cdecl;external;
function XDefaultVisual(para1:PDisplay; para2:longint):PVisual;cdecl;external;
function XDefaultVisualOfScreen(para1:PScreen):PVisual;cdecl;external;
function XDefaultGC(para1:PDisplay; para2:longint):TGC;cdecl;external;
function XDefaultGCOfScreen(para1:PScreen):TGC;cdecl;external;
function XBlackPixel(para1:PDisplay; para2:longint):dword;cdecl;external;
function XWhitePixel(para1:PDisplay; para2:longint):dword;cdecl;external;
function XAllPlanes:dword;cdecl;external;
function XBlackPixelOfScreen(para1:PScreen):dword;cdecl;external;
function XWhitePixelOfScreen(para1:PScreen):dword;cdecl;external;
function XNextRequest(para1:PDisplay):dword;cdecl;external;
function XLastKnownRequestProcessed(para1:PDisplay):dword;cdecl;external;
function XServerVendor(para1:PDisplay):Pchar;cdecl;external;
function XDisplayString(para1:PDisplay):Pchar;cdecl;external;
function XDefaultColormap(para1:PDisplay; para2:longint):TColormap;cdecl;external;
function XDefaultColormapOfScreen(para1:PScreen):TColormap;cdecl;external;
function XDisplayOfScreen(para1:PScreen):PDisplay;cdecl;external;
function XScreenOfDisplay(para1:PDisplay; para2:longint):PScreen;cdecl;external;
function XDefaultScreenOfDisplay(para1:PDisplay):PScreen;cdecl;external;
function XEventMaskOfScreen(para1:PScreen):longint;cdecl;external;
function XScreenNumberOfScreen(para1:PScreen):longint;cdecl;external;
type

   TXErrorHandler = function (para1:PDisplay; para2:PXErrorEvent):longint;cdecl;

function XSetErrorHandler(para1:TXErrorHandler):TXErrorHandler;cdecl;external;
type

   TXIOErrorHandler = function (para1:PDisplay):longint;cdecl;

function XSetIOErrorHandler(para1:TXIOErrorHandler):TXIOErrorHandler;cdecl;external;
function XListPixmapFormats(para1:PDisplay; para2:Plongint):PXPixmapFormatValues;cdecl;external;
function XListDepths(para1:PDisplay; para2:longint; para3:Plongint):Plongint;cdecl;external;
function XReconfigureWMWindow(para1:PDisplay; para2:TWindow; para3:longint; para4:dword; para5:PXWindowChanges):TStatus;cdecl;external;
function XGetWMProtocols(para1:PDisplay; para2:TWindow; para3:PPAtom; para4:Plongint):TStatus;cdecl;external;
function XSetWMProtocols(para1:PDisplay; para2:TWindow; para3:PAtom; para4:longint):TStatus;cdecl;external;
function XIconifyWindow(para1:PDisplay; para2:TWindow; para3:longint):TStatus;cdecl;external;
function XWithdrawWindow(para1:PDisplay; para2:TWindow; para3:longint):TStatus;cdecl;external;
function XGetCommand(para1:PDisplay; para2:TWindow; para3:PPPchar; para4:Plongint):TStatus;cdecl;external;
function XGetWMColormapWindows(para1:PDisplay; para2:TWindow; para3:PPWindow; para4:Plongint):TStatus;cdecl;external;
function XSetWMColormapWindows(para1:PDisplay; para2:TWindow; para3:PWindow; para4:longint):TStatus;cdecl;external;
procedure XFreeStringList(para1:PPchar);cdecl;external;
function XSetTransientForHint(para1:PDisplay; para2:TWindow; para3:TWindow):longint;cdecl;external;
function XActivateScreenSaver(para1:PDisplay):longint;cdecl;external;
function XAddHost(para1:PDisplay; para2:PXHostAddress):longint;cdecl;external;
function XAddHosts(para1:PDisplay; para2:PXHostAddress; para3:longint):longint;cdecl;external;
function XAddToExtensionList(para1:PPXExtData; para2:PXExtData):longint;cdecl;external;
function XAddToSaveSet(para1:PDisplay; para2:TWindow):longint;cdecl;external;
function XAllocColor(para1:PDisplay; para2:TColormap; para3:PXColor):TStatus;cdecl;external;
function XAllocColorCells(para1:PDisplay; para2:TColormap; para3:TBool; para4:Pdword; para5:dword;
           para6:Pdword; para7:dword):TStatus;cdecl;external;
function XAllocColorPlanes(para1:PDisplay; para2:TColormap; para3:TBool; para4:Pdword; para5:longint;
           para6:longint; para7:longint; para8:longint; para9:Pdword; para10:Pdword;
           para11:Pdword):TStatus;cdecl;external;
function XAllocNamedColor(para1:PDisplay; para2:TColormap; para3:Pchar; para4:PXColor; para5:PXColor):TStatus;cdecl;external;
function XAllowEvents(para1:PDisplay; para2:longint; para3:TTime):longint;cdecl;external;
function XAutoRepeatOff(para1:PDisplay):longint;cdecl;external;
function XAutoRepeatOn(para1:PDisplay):longint;cdecl;external;
function XBell(para1:PDisplay; para2:longint):longint;cdecl;external;
function XBitmapBitOrder(para1:PDisplay):longint;cdecl;external;
function XBitmapPad(para1:PDisplay):longint;cdecl;external;
function XBitmapUnit(para1:PDisplay):longint;cdecl;external;
function XCellsOfScreen(para1:PScreen):longint;cdecl;external;
function XChangeActivePointerGrab(para1:PDisplay; para2:dword; para3:TCursor; para4:TTime):longint;cdecl;external;
function XChangeGC(para1:PDisplay; para2:TGC; para3:dword; para4:PXGCValues):longint;cdecl;external;
function XChangeKeyboardControl(para1:PDisplay; para2:dword; para3:PXKeyboardControl):longint;cdecl;external;
function XChangeKeyboardMapping(para1:PDisplay; para2:longint; para3:longint; para4:PKeySym; para5:longint):longint;cdecl;external;
function XChangePointerControl(para1:PDisplay; para2:TBool; para3:TBool; para4:longint; para5:longint;
           para6:longint):longint;cdecl;external;
function XChangeProperty(para1:PDisplay; para2:TWindow; para3:TAtom; para4:TAtom; para5:longint;
           para6:longint; para7:Pbyte; para8:longint):longint;cdecl;external;
function XChangeSaveSet(para1:PDisplay; para2:TWindow; para3:longint):longint;cdecl;external;
function XChangeWindowAttributes(para1:PDisplay; para2:TWindow; para3:dword; para4:PXSetWindowAttributes):longint;cdecl;external;
function XCheckIfEvent(para1:PDisplay; para2:PXEvent; para3:funcbool; para4:TXPointer):TBool;cdecl;external;
function XCheckMaskEvent(para1:PDisplay; para2:longint; para3:PXEvent):TBool;cdecl;external;
function XCheckTypedEvent(para1:PDisplay; para2:longint; para3:PXEvent):TBool;cdecl;external;
function XCheckTypedWindowEvent(para1:PDisplay; para2:TWindow; para3:longint; para4:PXEvent):TBool;cdecl;external;
function XCheckWindowEvent(para1:PDisplay; para2:TWindow; para3:longint; para4:PXEvent):TBool;cdecl;external;
function XCirculateSubwindows(para1:PDisplay; para2:TWindow; para3:longint):longint;cdecl;external;
function XCirculateSubwindowsDown(para1:PDisplay; para2:TWindow):longint;cdecl;external;
function XCirculateSubwindowsUp(para1:PDisplay; para2:TWindow):longint;cdecl;external;
function XClearArea(para1:PDisplay; para2:TWindow; para3:longint; para4:longint; para5:dword;
           para6:dword; para7:TBool):longint;cdecl;external;
function XClearWindow(para1:PDisplay; para2:TWindow):longint;cdecl;external;
function XCloseDisplay(para1:PDisplay):longint;cdecl;external;
function XConfigureWindow(para1:PDisplay; para2:TWindow; para3:dword; para4:PXWindowChanges):longint;cdecl;external;
function XConnectionNumber(para1:PDisplay):longint;cdecl;external;
function XConvertSelection(para1:PDisplay; para2:TAtom; para3:TAtom; para4:TAtom; para5:TWindow;
           para6:TTime):longint;cdecl;external;
function XCopyArea(para1:PDisplay; para2:TDrawable; para3:TDrawable; para4:TGC; para5:longint;
           para6:longint; para7:dword; para8:dword; para9:longint; para10:longint):longint;cdecl;external;
function XCopyGC(para1:PDisplay; para2:TGC; para3:dword; para4:TGC):longint;cdecl;external;
function XCopyPlane(para1:PDisplay; para2:TDrawable; para3:TDrawable; para4:TGC; para5:longint;
           para6:longint; para7:dword; para8:dword; para9:longint; para10:longint;
           para11:dword):longint;cdecl;external;
function XDefaultDepth(para1:PDisplay; para2:longint):longint;cdecl;external;
function XDefaultDepthOfScreen(para1:PScreen):longint;cdecl;external;
function XDefaultScreen(para1:PDisplay):longint;cdecl;external;
function XDefineCursor(para1:PDisplay; para2:TWindow; para3:TCursor):longint;cdecl;external;
function XDeleteProperty(para1:PDisplay; para2:TWindow; para3:TAtom):longint;cdecl;external;
function XDestroyWindow(para1:PDisplay; para2:TWindow):longint;cdecl;external;
function XDestroySubwindows(para1:PDisplay; para2:TWindow):longint;cdecl;external;
function XDoesBackingStore(para1:PScreen):longint;cdecl;external;
function XDoesSaveUnders(para1:PScreen):TBool;cdecl;external;
function XDisableAccessControl(para1:PDisplay):longint;cdecl;external;
function XDisplayCells(para1:PDisplay; para2:longint):longint;cdecl;external;
function XDisplayHeight(para1:PDisplay; para2:longint):longint;cdecl;external;
function XDisplayHeightMM(para1:PDisplay; para2:longint):longint;cdecl;external;
function XDisplayKeycodes(para1:PDisplay; para2:Plongint; para3:Plongint):longint;cdecl;external;
function XDisplayPlanes(para1:PDisplay; para2:longint):longint;cdecl;external;
function XDisplayWidth(para1:PDisplay; para2:longint):longint;cdecl;external;
function XDisplayWidthMM(para1:PDisplay; para2:longint):longint;cdecl;external;
function XDrawArc(para1:PDisplay; para2:TDrawable; para3:TGC; para4:longint; para5:longint;
           para6:dword; para7:dword; para8:longint; para9:longint):longint;cdecl;external;
function XDrawArcs(para1:PDisplay; para2:TDrawable; para3:TGC; para4:PXArc; para5:longint):longint;cdecl;external;
function XDrawImageString(para1:PDisplay; para2:TDrawable; para3:TGC; para4:longint; para5:longint;
           para6:Pchar; para7:longint):longint;cdecl;external;
function XDrawImageString16(para1:PDisplay; para2:TDrawable; para3:TGC; para4:longint; para5:longint;
           para6:PXChar2b; para7:longint):longint;cdecl;external;
function XDrawLine(para1:PDisplay; para2:TDrawable; para3:TGC; para4:longint; para5:longint;
           para6:longint; para7:longint):longint;cdecl;external;
function XDrawLines(para1:PDisplay; para2:TDrawable; para3:TGC; para4:PXPoint; para5:longint;
           para6:longint):longint;cdecl;external;
function XDrawPoint(para1:PDisplay; para2:TDrawable; para3:TGC; para4:longint; para5:longint):longint;cdecl;external;
function XDrawPoints(para1:PDisplay; para2:TDrawable; para3:TGC; para4:PXPoint; para5:longint;
           para6:longint):longint;cdecl;external;
function XDrawRectangle(para1:PDisplay; para2:TDrawable; para3:TGC; para4:longint; para5:longint;
           para6:dword; para7:dword):longint;cdecl;external;
function XDrawRectangles(para1:PDisplay; para2:TDrawable; para3:TGC; para4:PXRectangle; para5:longint):longint;cdecl;external;
function XDrawSegments(para1:PDisplay; para2:TDrawable; para3:TGC; para4:PXSegment; para5:longint):longint;cdecl;external;
function XDrawString(para1:PDisplay; para2:TDrawable; para3:TGC; para4:longint; para5:longint;
           para6:Pchar; para7:longint):longint;cdecl;external;
function XDrawString16(para1:PDisplay; para2:TDrawable; para3:TGC; para4:longint; para5:longint;
           para6:PXChar2b; para7:longint):longint;cdecl;external;
function XDrawText(para1:PDisplay; para2:TDrawable; para3:TGC; para4:longint; para5:longint;
           para6:PXTextItem; para7:longint):longint;cdecl;external;
function XDrawText16(para1:PDisplay; para2:TDrawable; para3:TGC; para4:longint; para5:longint;
           para6:PXTextItem16; para7:longint):longint;cdecl;external;
function XEnableAccessControl(para1:PDisplay):longint;cdecl;external;
function XEventsQueued(para1:PDisplay; para2:longint):longint;cdecl;external;
function XFetchName(para1:PDisplay; para2:TWindow; para3:PPchar):TStatus;cdecl;external;
function XFillArc(para1:PDisplay; para2:TDrawable; para3:TGC; para4:longint; para5:longint;
           para6:dword; para7:dword; para8:longint; para9:longint):longint;cdecl;external;
function XFillArcs(para1:PDisplay; para2:TDrawable; para3:TGC; para4:PXArc; para5:longint):longint;cdecl;external;
function XFillPolygon(para1:PDisplay; para2:TDrawable; para3:TGC; para4:PXPoint; para5:longint;
           para6:longint; para7:longint):longint;cdecl;external;
function XFillRectangle(para1:PDisplay; para2:TDrawable; para3:TGC; para4:longint; para5:longint;
           para6:dword; para7:dword):longint;cdecl;external;
function XFillRectangles(para1:PDisplay; para2:TDrawable; para3:TGC; para4:PXRectangle; para5:longint):longint;cdecl;external;
function XFlush(para1:PDisplay):longint;cdecl;external;
function XForceScreenSaver(para1:PDisplay; para2:longint):longint;cdecl;external;
function XFree(para1:pointer):longint;cdecl;external;
function XFreeColormap(para1:PDisplay; para2:TColormap):longint;cdecl;external;
function XFreeColors(para1:PDisplay; para2:TColormap; para3:Pdword; para4:longint; para5:dword):longint;cdecl;external;
function XFreeCursor(para1:PDisplay; para2:TCursor):longint;cdecl;external;
function XFreeExtensionList(para1:PPchar):longint;cdecl;external;
function XFreeFont(para1:PDisplay; para2:PXFontStruct):longint;cdecl;external;
function XFreeFontInfo(para1:PPchar; para2:PXFontStruct; para3:longint):longint;cdecl;external;
function XFreeFontNames(para1:PPchar):longint;cdecl;external;
function XFreeFontPath(para1:PPchar):longint;cdecl;external;
function XFreeGC(para1:PDisplay; para2:TGC):longint;cdecl;external;
function XFreeModifiermap(para1:PXModifierKeymap):longint;cdecl;external;
function XFreePixmap(para1:PDisplay; para2:TPixmap):longint;cdecl;external;
function XGeometry(para1:PDisplay; para2:longint; para3:Pchar; para4:Pchar; para5:dword;
           para6:dword; para7:dword; para8:longint; para9:longint; para10:Plongint;
           para11:Plongint; para12:Plongint; para13:Plongint):longint;cdecl;external;
function XGetErrorDatabaseText(para1:PDisplay; para2:Pchar; para3:Pchar; para4:Pchar; para5:Pchar;
           para6:longint):longint;cdecl;external;
function XGetErrorText(para1:PDisplay; para2:longint; para3:Pchar; para4:longint):longint;cdecl;external;
function XGetFontProperty(para1:PXFontStruct; para2:TAtom; para3:Pdword):TBool;cdecl;external;
function XGetGCValues(para1:PDisplay; para2:TGC; para3:dword; para4:PXGCValues):TStatus;cdecl;external;
function XGetGeometry(para1:PDisplay; para2:TDrawable; para3:PWindow; para4:Plongint; para5:Plongint;
           para6:Pdword; para7:Pdword; para8:Pdword; para9:Pdword):TStatus;cdecl;external;
function XGetIconName(para1:PDisplay; para2:TWindow; para3:PPchar):TStatus;cdecl;external;
function XGetInputFocus(para1:PDisplay; para2:PWindow; para3:Plongint):longint;cdecl;external;
function XGetKeyboardControl(para1:PDisplay; para2:PXKeyboardState):longint;cdecl;external;
function XGetPointerControl(para1:PDisplay; para2:Plongint; para3:Plongint; para4:Plongint):longint;cdecl;external;
function XGetPointerMapping(para1:PDisplay; para2:Pbyte; para3:longint):longint;cdecl;external;
function XGetScreenSaver(para1:PDisplay; para2:Plongint; para3:Plongint; para4:Plongint; para5:Plongint):longint;cdecl;external;
function XGetTransientForHint(para1:PDisplay; para2:TWindow; para3:PWindow):TStatus;cdecl;external;
function XGetWindowProperty(para1:PDisplay; para2:TWindow; para3:TAtom; para4:longint; para5:longint;
           para6:TBool; para7:TAtom; para8:PAtom; para9:Plongint; para10:Pdword;
           para11:Pdword; para12:PPbyte):longint;cdecl;external;
function XGetWindowAttributes(para1:PDisplay; para2:TWindow; para3:PXWindowAttributes):TStatus;cdecl;external;
function XGrabButton(para1:PDisplay; para2:dword; para3:dword; para4:TWindow; para5:TBool;
           para6:dword; para7:longint; para8:longint; para9:TWindow; para10:TCursor):longint;cdecl;external;
function XGrabKey(para1:PDisplay; para2:longint; para3:dword; para4:TWindow; para5:TBool;
           para6:longint; para7:longint):longint;cdecl;external;
function XGrabKeyboard(para1:PDisplay; para2:TWindow; para3:TBool; para4:longint; para5:longint;
           para6:TTime):longint;cdecl;external;
function XGrabPointer(para1:PDisplay; para2:TWindow; para3:TBool; para4:dword; para5:longint;
           para6:longint; para7:TWindow; para8:TCursor; para9:TTime):longint;cdecl;external;
function XGrabServer(para1:PDisplay):longint;cdecl;external;
function XHeightMMOfScreen(para1:PScreen):longint;cdecl;external;
function XHeightOfScreen(para1:PScreen):longint;cdecl;external;
function XIfEvent(para1:PDisplay; para2:PXEvent; para3:funcbool; para4:TXPointer):longint;cdecl;external;
function XImageByteOrder(para1:PDisplay):longint;cdecl;external;
function XInstallColormap(para1:PDisplay; para2:TColormap):longint;cdecl;external;
function XKeysymToKeycode(para1:PDisplay; para2:TKeySym):TKeyCode;cdecl;external;
function XKillClient(para1:PDisplay; para2:TXID):longint;cdecl;external;
function XLookupColor(para1:PDisplay; para2:TColormap; para3:Pchar; para4:PXColor; para5:PXColor):TStatus;cdecl;external;
function XLowerWindow(para1:PDisplay; para2:TWindow):longint;cdecl;external;
function XMapRaised(para1:PDisplay; para2:TWindow):longint;cdecl;external;
function XMapSubwindows(para1:PDisplay; para2:TWindow):longint;cdecl;external;
function XMapWindow(para1:PDisplay; para2:TWindow):longint;cdecl;external;
function XMaskEvent(para1:PDisplay; para2:longint; para3:PXEvent):longint;cdecl;external;
function XMaxCmapsOfScreen(para1:PScreen):longint;cdecl;external;
function XMinCmapsOfScreen(para1:PScreen):longint;cdecl;external;
function XMoveResizeWindow(para1:PDisplay; para2:TWindow; para3:longint; para4:longint; para5:dword;
           para6:dword):longint;cdecl;external;
function XMoveWindow(para1:PDisplay; para2:TWindow; para3:longint; para4:longint):longint;cdecl;external;
function XNextEvent(para1:PDisplay; para2:PXEvent):longint;cdecl;external;
function XNoOp(para1:PDisplay):longint;cdecl;external;
function XParseColor(para1:PDisplay; para2:TColormap; para3:Pchar; para4:PXColor):TStatus;cdecl;external;
function XParseGeometry(para1:Pchar; para2:Plongint; para3:Plongint; para4:Pdword; para5:Pdword):longint;cdecl;external;
function XPeekEvent(para1:PDisplay; para2:PXEvent):longint;cdecl;external;
function XPeekIfEvent(para1:PDisplay; para2:PXEvent; para3:funcbool; para4:TXPointer):longint;cdecl;external;
function XPending(para1:PDisplay):longint;cdecl;external;
function XPlanesOfScreen(para1:PScreen):longint;cdecl;external;
function XProtocolRevision(para1:PDisplay):longint;cdecl;external;
function XProtocolVersion(para1:PDisplay):longint;cdecl;external;
function XPutBackEvent(para1:PDisplay; para2:PXEvent):longint;cdecl;external;
function XPutImage(para1:PDisplay; para2:TDrawable; para3:TGC; para4:PXImage; para5:longint;
           para6:longint; para7:longint; para8:longint; para9:dword; para10:dword):longint;cdecl;external;
function XQLength(para1:PDisplay):longint;cdecl;external;
function XQueryBestCursor(para1:PDisplay; para2:TDrawable; para3:dword; para4:dword; para5:Pdword;
           para6:Pdword):TStatus;cdecl;external;
function XQueryBestSize(para1:PDisplay; para2:longint; para3:TDrawable; para4:dword; para5:dword;
           para6:Pdword; para7:Pdword):TStatus;cdecl;external;
function XQueryBestStipple(para1:PDisplay; para2:TDrawable; para3:dword; para4:dword; para5:Pdword;
           para6:Pdword):TStatus;cdecl;external;
function XQueryBestTile(para1:PDisplay; para2:TDrawable; para3:dword; para4:dword; para5:Pdword;
           para6:Pdword):TStatus;cdecl;external;
function XQueryColor(para1:PDisplay; para2:TColormap; para3:PXColor):longint;cdecl;external;
function XQueryColors(para1:PDisplay; para2:TColormap; para3:PXColor; para4:longint):longint;cdecl;external;
function XQueryExtension(para1:PDisplay; para2:Pchar; para3:Plongint; para4:Plongint; para5:Plongint):TBool;cdecl;external;
function XQueryKeymap(para1:PDisplay; para2:chararr32):longint;cdecl;external;
function XQueryPointer(para1:PDisplay; para2:TWindow; para3:PWindow; para4:PWindow; para5:Plongint;
           para6:Plongint; para7:Plongint; para8:Plongint; para9:Pdword):TBool;cdecl;external;
function XQueryTextExtents(para1:PDisplay; para2:TXID; para3:Pchar; para4:longint; para5:Plongint;
           para6:Plongint; para7:Plongint; para8:PXCharStruct):longint;cdecl;external;
function XQueryTextExtents16(para1:PDisplay; para2:TXID; para3:PXChar2b; para4:longint; para5:Plongint;
           para6:Plongint; para7:Plongint; para8:PXCharStruct):longint;cdecl;external;
function XQueryTree(para1:PDisplay; para2:TWindow; para3:PWindow; para4:PWindow; para5:PPWindow;
           para6:Pdword):TStatus;cdecl;external;
function XRaiseWindow(para1:PDisplay; para2:TWindow):longint;cdecl;external;
function XReadBitmapFile(para1:PDisplay; para2:TDrawable; para3:Pchar; para4:Pdword; para5:Pdword;
           para6:PPixmap; para7:Plongint; para8:Plongint):longint;cdecl;external;
function XReadBitmapFileData(para1:Pchar; para2:Pdword; para3:Pdword; para4:PPbyte; para5:Plongint;
           para6:Plongint):longint;cdecl;external;
function XRebindKeysym(para1:PDisplay; para2:TKeySym; para3:PKeySym; para4:longint; para5:Pbyte;
           para6:longint):longint;cdecl;external;
function XRecolorCursor(para1:PDisplay; para2:TCursor; para3:PXColor; para4:PXColor):longint;cdecl;external;
function XRefreshKeyboardMapping(para1:PXMappingEvent):longint;cdecl;external;
function XRemoveFromSaveSet(para1:PDisplay; para2:TWindow):longint;cdecl;external;
function XRemoveHost(para1:PDisplay; para2:PXHostAddress):longint;cdecl;external;
function XRemoveHosts(para1:PDisplay; para2:PXHostAddress; para3:longint):longint;cdecl;external;
function XReparentWindow(para1:PDisplay; para2:TWindow; para3:TWindow; para4:longint; para5:longint):longint;cdecl;external;
function XResetScreenSaver(para1:PDisplay):longint;cdecl;external;
function XResizeWindow(para1:PDisplay; para2:TWindow; para3:dword; para4:dword):longint;cdecl;external;
function XRestackWindows(para1:PDisplay; para2:PWindow; para3:longint):longint;cdecl;external;
function XRotateBuffers(para1:PDisplay; para2:longint):longint;cdecl;external;
function XRotateWindowProperties(para1:PDisplay; para2:TWindow; para3:PAtom; para4:longint; para5:longint):longint;cdecl;external;
function XScreenCount(para1:PDisplay):longint;cdecl;external;
function XSelectInput(para1:PDisplay; para2:TWindow; para3:longint):longint;cdecl;external;
function XSendEvent(para1:PDisplay; para2:TWindow; para3:TBool; para4:longint; para5:PXEvent):TStatus;cdecl;external;
function XSetAccessControl(para1:PDisplay; para2:longint):longint;cdecl;external;
function XSetArcMode(para1:PDisplay; para2:TGC; para3:longint):longint;cdecl;external;
function XSetBackground(para1:PDisplay; para2:TGC; para3:dword):longint;cdecl;external;
function XSetClipMask(para1:PDisplay; para2:TGC; para3:TPixmap):longint;cdecl;external;
function XSetClipOrigin(para1:PDisplay; para2:TGC; para3:longint; para4:longint):longint;cdecl;external;
function XSetClipRectangles(para1:PDisplay; para2:TGC; para3:longint; para4:longint; para5:PXRectangle;
           para6:longint; para7:longint):longint;cdecl;external;
function XSetCloseDownMode(para1:PDisplay; para2:longint):longint;cdecl;external;
function XSetCommand(para1:PDisplay; para2:TWindow; para3:PPchar; para4:longint):longint;cdecl;external;
function XSetDashes(para1:PDisplay; para2:TGC; para3:longint; para4:Pchar; para5:longint):longint;cdecl;external;
function XSetFillRule(para1:PDisplay; para2:TGC; para3:longint):longint;cdecl;external;
function XSetFillStyle(para1:PDisplay; para2:TGC; para3:longint):longint;cdecl;external;
function XSetFont(para1:PDisplay; para2:TGC; para3:TFont):longint;cdecl;external;
function XSetFontPath(para1:PDisplay; para2:PPchar; para3:longint):longint;cdecl;external;
function XSetForeground(para1:PDisplay; para2:TGC; para3:dword):longint;cdecl;external;
function XSetFunction(para1:PDisplay; para2:TGC; para3:longint):longint;cdecl;external;
function XSetGraphicsExposures(para1:PDisplay; para2:TGC; para3:TBool):longint;cdecl;external;
function XSetIconName(para1:PDisplay; para2:TWindow; para3:Pchar):longint;cdecl;external;
function XSetInputFocus(para1:PDisplay; para2:TWindow; para3:longint; para4:TTime):longint;cdecl;external;
function XSetLineAttributes(para1:PDisplay; para2:TGC; para3:dword; para4:longint; para5:longint;
           para6:longint):longint;cdecl;external;
function XSetModifierMapping(para1:PDisplay; para2:PXModifierKeymap):longint;cdecl;external;
function XSetPlaneMask(para1:PDisplay; para2:TGC; para3:dword):longint;cdecl;external;
function XSetPointerMapping(para1:PDisplay; para2:Pbyte; para3:longint):longint;cdecl;external;
function XSetScreenSaver(para1:PDisplay; para2:longint; para3:longint; para4:longint; para5:longint):longint;cdecl;external;
function XSetSelectionOwner(para1:PDisplay; para2:TAtom; para3:TWindow; para4:TTime):longint;cdecl;external;
function XSetState(para1:PDisplay; para2:TGC; para3:dword; para4:dword; para5:longint;
           para6:dword):longint;cdecl;external;
function XSetStipple(para1:PDisplay; para2:TGC; para3:TPixmap):longint;cdecl;external;
function XSetSubwindowMode(para1:PDisplay; para2:TGC; para3:longint):longint;cdecl;external;
function XSetTSOrigin(para1:PDisplay; para2:TGC; para3:longint; para4:longint):longint;cdecl;external;
function XSetTile(para1:PDisplay; para2:TGC; para3:TPixmap):longint;cdecl;external;
function XSetWindowBackground(para1:PDisplay; para2:TWindow; para3:dword):longint;cdecl;external;
function XSetWindowBackgroundPixmap(para1:PDisplay; para2:TWindow; para3:TPixmap):longint;cdecl;external;
function XSetWindowBorder(para1:PDisplay; para2:TWindow; para3:dword):longint;cdecl;external;
function XSetWindowBorderPixmap(para1:PDisplay; para2:TWindow; para3:TPixmap):longint;cdecl;external;
function XSetWindowBorderWidth(para1:PDisplay; para2:TWindow; para3:dword):longint;cdecl;external;
function XSetWindowColormap(para1:PDisplay; para2:TWindow; para3:TColormap):longint;cdecl;external;
function XStoreBuffer(para1:PDisplay; para2:Pchar; para3:longint; para4:longint):longint;cdecl;external;
function XStoreBytes(para1:PDisplay; para2:Pchar; para3:longint):longint;cdecl;external;
function XStoreColor(para1:PDisplay; para2:TColormap; para3:PXColor):longint;cdecl;external;
function XStoreColors(para1:PDisplay; para2:TColormap; para3:PXColor; para4:longint):longint;cdecl;external;
function XStoreName(para1:PDisplay; para2:TWindow; para3:Pchar):longint;cdecl;external;
function XStoreNamedColor(para1:PDisplay; para2:TColormap; para3:Pchar; para4:dword; para5:longint):longint;cdecl;external;
function XSync(para1:PDisplay; para2:TBool):longint;cdecl;external;
function XTextExtents(para1:PXFontStruct; para2:Pchar; para3:longint; para4:Plongint; para5:Plongint;
           para6:Plongint; para7:PXCharStruct):longint;cdecl;external;
function XTextExtents16(para1:PXFontStruct; para2:PXChar2b; para3:longint; para4:Plongint; para5:Plongint;
           para6:Plongint; para7:PXCharStruct):longint;cdecl;external;
function XTextWidth(para1:PXFontStruct; para2:Pchar; para3:longint):longint;cdecl;external;
function XTextWidth16(para1:PXFontStruct; para2:PXChar2b; para3:longint):longint;cdecl;external;
function XTranslateCoordinates(para1:PDisplay; para2:TWindow; para3:TWindow; para4:longint; para5:longint;
           para6:Plongint; para7:Plongint; para8:PWindow):TBool;cdecl;external;
function XUndefineCursor(para1:PDisplay; para2:TWindow):longint;cdecl;external;
function XUngrabButton(para1:PDisplay; para2:dword; para3:dword; para4:TWindow):longint;cdecl;external;
function XUngrabKey(para1:PDisplay; para2:longint; para3:dword; para4:TWindow):longint;cdecl;external;
function XUngrabKeyboard(para1:PDisplay; para2:TTime):longint;cdecl;external;
function XUngrabPointer(para1:PDisplay; para2:TTime):longint;cdecl;external;
function XUngrabServer(para1:PDisplay):longint;cdecl;external;
function XUninstallColormap(para1:PDisplay; para2:TColormap):longint;cdecl;external;
function XUnloadFont(para1:PDisplay; para2:TFont):longint;cdecl;external;
function XUnmapSubwindows(para1:PDisplay; para2:TWindow):longint;cdecl;external;
function XUnmapWindow(para1:PDisplay; para2:TWindow):longint;cdecl;external;
function XVendorRelease(para1:PDisplay):longint;cdecl;external;
function XWarpPointer(para1:PDisplay; para2:TWindow; para3:TWindow; para4:longint; para5:longint;
           para6:dword; para7:dword; para8:longint; para9:longint):longint;cdecl;external;
function XWidthMMOfScreen(para1:PScreen):longint;cdecl;external;
function XWidthOfScreen(para1:PScreen):longint;cdecl;external;
function XWindowEvent(para1:PDisplay; para2:TWindow; para3:longint; para4:PXEvent):longint;cdecl;external;
function XWriteBitmapFile(para1:PDisplay; para2:Pchar; para3:TPixmap; para4:dword; para5:dword;
           para6:longint; para7:longint):longint;cdecl;external;
function XSupportsLocale:TBool;cdecl;external;
function XSetLocaleModifiers(para1:Pchar):Pchar;cdecl;external;
function XOpenOM(para1:PDisplay; para2:PXrmHashBucketRec; para3:Pchar; para4:Pchar):TXOM;cdecl;external;
function XCloseOM(para1:TXOM):TStatus;cdecl;external;
function XSetOMValues:Pchar;cdecl;external;
function XGetOMValues:Pchar;cdecl;external;
function XDisplayOfOM(para1:TXOM):PDisplay;cdecl;external;
function XLocaleOfOM(para1:TXOM):Pchar;cdecl;external;
function XCreateOC:TXOC;cdecl;external;
procedure XDestroyOC(para1:TXOC);cdecl;external;
function XOMOfOC(para1:TXOC):TXOM;cdecl;external;
function XSetOCValues:Pchar;cdecl;external;
function XGetOCValues:Pchar;cdecl;external;
function XCreateFontSet(para1:PDisplay; para2:Pchar; para3:PPPchar; para4:Plongint; para5:PPchar):TXFontSet;cdecl;external;
procedure XFreeFontSet(para1:PDisplay; para2:TXFontSet);cdecl;external;
function XFontsOfFontSet(para1:TXFontSet; para2:PPPXFontStruct; para3:PPPchar):longint;cdecl;external;
function XBaseFontNameListOfFontSet(para1:TXFontSet):Pchar;cdecl;external;
function XLocaleOfFontSet(para1:TXFontSet):Pchar;cdecl;external;
function XContextDependentDrawing(para1:TXFontSet):TBool;cdecl;external;
function XDirectionalDependentDrawing(para1:TXFontSet):TBool;cdecl;external;
function XContextualDrawing(para1:TXFontSet):TBool;cdecl;external;
function XExtentsOfFontSet(para1:TXFontSet):PXFontSetExtents;cdecl;external;
function XmbTextEscapement(para1:TXFontSet; para2:Pchar; para3:longint):longint;cdecl;external;
function XwcTextEscapement(para1:TXFontSet; para2:PWideChar; para3:longint):longint;cdecl;external;
function XmbTextExtents(para1:TXFontSet; para2:Pchar; para3:longint; para4:PXRectangle; para5:PXRectangle):longint;cdecl;external;
function XwcTextExtents(para1:TXFontSet; para2:PWideChar; para3:longint; para4:PXRectangle; para5:PXRectangle):longint;cdecl;external;
function XmbTextPerCharExtents(para1:TXFontSet; para2:Pchar; para3:longint; para4:PXRectangle; para5:PXRectangle;
           para6:longint; para7:Plongint; para8:PXRectangle; para9:PXRectangle):TStatus;cdecl;external;
function XwcTextPerCharExtents(para1:TXFontSet; para2:PWideChar; para3:longint; para4:PXRectangle; para5:PXRectangle;
           para6:longint; para7:Plongint; para8:PXRectangle; para9:PXRectangle):TStatus;cdecl;external;
procedure XmbDrawText(para1:PDisplay; para2:TDrawable; para3:TGC; para4:longint; para5:longint;
            para6:PXmbTextItem; para7:longint);cdecl;external;
procedure XwcDrawText(para1:PDisplay; para2:TDrawable; para3:TGC; para4:longint; para5:longint;
            para6:PXwcTextItem; para7:longint);cdecl;external;
procedure XmbDrawString(para1:PDisplay; para2:TDrawable; para3:TXFontSet; para4:TGC; para5:longint;
            para6:longint; para7:Pchar; para8:longint);cdecl;external;
procedure XwcDrawString(para1:PDisplay; para2:TDrawable; para3:TXFontSet; para4:TGC; para5:longint;
            para6:longint; para7:PWideChar; para8:longint);cdecl;external;
procedure XmbDrawImageString(para1:PDisplay; para2:TDrawable; para3:TXFontSet; para4:TGC; para5:longint;
            para6:longint; para7:Pchar; para8:longint);cdecl;external;
procedure XwcDrawImageString(para1:PDisplay; para2:TDrawable; para3:TXFontSet; para4:TGC; para5:longint;
            para6:longint; para7:PWideChar; para8:longint);cdecl;external;
function XOpenIM(para1:PDisplay; para2:PXrmHashBucketRec; para3:Pchar; para4:Pchar):TXIM;cdecl;external;
function XCloseIM(para1:TXIM):TStatus;cdecl;external;
function XGetIMValues:Pchar;cdecl;external;
function XDisplayOfIM(para1:TXIM):PDisplay;cdecl;external;
function XLocaleOfIM(para1:TXIM):Pchar;cdecl;external;
function XCreateIC:TXIC;cdecl;external;
procedure XDestroyIC(para1:TXIC);cdecl;external;
procedure XSetICFocus(para1:TXIC);cdecl;external;
procedure XUnsetICFocus(para1:TXIC);cdecl;external;
function XwcResetIC(para1:TXIC):PWideChar;cdecl;external;
function XmbResetIC(para1:TXIC):Pchar;cdecl;external;
function XSetICValues:Pchar;cdecl;external;
function XGetICValues:Pchar;cdecl;external;
function XIMOfIC(para1:TXIC):TXIM;cdecl;external;
function XFilterEvent(para1:PXEvent; para2:TWindow):TBool;cdecl;external;
function XmbLookupString(para1:TXIC; para2:PXKeyPressedEvent; para3:Pchar; para4:longint; para5:PKeySym;
           para6:PStatus):longint;cdecl;external;
function XwcLookupString(para1:TXIC; para2:PXKeyPressedEvent; para3:PWideChar; para4:longint; para5:PKeySym;
           para6:PStatus):longint;cdecl;external;
function XVaCreateNestedList:TXVaNestedList;cdecl;external;
function XRegisterIMInstantiateCallback(para1:PDisplay; para2:PXrmHashBucketRec; para3:Pchar; para4:Pchar; para5:TXIDProc;
           para6:TXPointer):TBool;cdecl;external;
function XUnregisterIMInstantiateCallback(para1:PDisplay; para2:PXrmHashBucketRec; para3:Pchar; para4:Pchar; para5:TXIDProc;
           para6:TXPointer):TBool;cdecl;external;
type

   TXConnectionWatchProc = procedure (para1:PDisplay; para2:TXPointer; para3:longint; para4:TBool; para5:PXPointer);cdecl;

function XInternalConnectionNumbers(para1:PDisplay; para2:PPlongint; para3:Plongint):TStatus;cdecl;external;
procedure XProcessInternalConnection(para1:PDisplay; para2:longint);cdecl;external;
function XAddConnectionWatch(para1:PDisplay; para2:TXConnectionWatchProc; para3:TXPointer):TStatus;cdecl;external;
procedure XRemoveConnectionWatch(para1:PDisplay; para2:TXConnectionWatchProc; para3:TXPointer);cdecl;external;
procedure XSetAuthorization(para1:Pchar; para2:longint; para3:Pchar; para4:longint);cdecl;external;

implementation

{$ifdef MACROS}
function ConnectionNumber(dpy : longint) : longint;
begin
   ConnectionNumber:=(TXPrivDisplay(dpy))^.fd;
end;

function RootWindow(dpy,scr : longint) : longint;
begin
   RootWindow:=(ScreenOfDisplay(dpy,scr))^.root;
end;

function DefaultScreen(dpy : longint) : longint;
begin
   DefaultScreen:=(TXPrivDisplay(dpy))^.default_screen;
end;

function DefaultRootWindow(dpy : longint) : longint;
begin
   DefaultRootWindow:=(ScreenOfDisplay(dpy,DefaultScreen(dpy)))^.root;
end;

function DefaultVisual(dpy,scr : longint) : longint;
begin
   DefaultVisual:=(ScreenOfDisplay(dpy,scr))^.root_visual;
end;

function DefaultGC(dpy,scr : longint) : longint;
begin
   DefaultGC:=(ScreenOfDisplay(dpy,scr))^.default_gc;
end;

function BlackPixel(dpy,scr : longint) : longint;
begin
   BlackPixel:=(ScreenOfDisplay(dpy,scr))^.black_pixel;
end;

function WhitePixel(dpy,scr : longint) : longint;
begin
   WhitePixel:=(ScreenOfDisplay(dpy,scr))^.white_pixel;
end;

function AllPlanes : dword;
  begin
     AllPlanes:=dword( not (0));
  end;

function QLength(dpy : longint) : longint;
begin
   QLength:=(TXPrivDisplay(dpy))^.qlen;
end;

function DisplayWidth(dpy,scr : longint) : longint;
begin
   DisplayWidth:=(ScreenOfDisplay(dpy,scr))^.width;
end;

function DisplayHeight(dpy,scr : longint) : longint;
begin
   DisplayHeight:=(ScreenOfDisplay(dpy,scr))^.height;
end;

function DisplayWidthMM(dpy,scr : longint) : longint;
begin
   DisplayWidthMM:=(ScreenOfDisplay(dpy,scr))^.mwidth;
end;

function DisplayHeightMM(dpy,scr : longint) : longint;
begin
   DisplayHeightMM:=(ScreenOfDisplay(dpy,scr))^.mheight;
end;

function DisplayPlanes(dpy,scr : longint) : longint;
begin
   DisplayPlanes:=(ScreenOfDisplay(dpy,scr))^.root_depth;
end;

function DisplayCells(dpy,scr : longint) : longint;
begin
   DisplayCells:=(DefaultVisual(dpy,scr))^.map_entries;
end;

function ScreenCount(dpy : longint) : longint;
begin
   ScreenCount:=(TXPrivDisplay(dpy))^.nscreens;
end;

function ServerVendor(dpy : longint) : longint;
begin
   ServerVendor:=(TXPrivDisplay(dpy))^.vendor;
end;

function ProtocolVersion(dpy : longint) : longint;
begin
   ProtocolVersion:=(TXPrivDisplay(dpy))^.proto_major_version;
end;

function ProtocolRevision(dpy : longint) : longint;
begin
   ProtocolRevision:=(TXPrivDisplay(dpy))^.proto_minor_version;
end;

function VendorRelease(dpy : longint) : longint;
begin
   VendorRelease:=(TXPrivDisplay(dpy))^.release;
end;

function DisplayString(dpy : longint) : longint;
begin
   DisplayString:=(TXPrivDisplay(dpy))^.display_name;
end;

function DefaultDepth(dpy,scr : longint) : longint;
begin
   DefaultDepth:=(ScreenOfDisplay(dpy,scr))^.root_depth;
end;

function DefaultColormap(dpy,scr : longint) : longint;
begin
   DefaultColormap:=(ScreenOfDisplay(dpy,scr))^.cmap;
end;

function BitmapUnit(dpy : longint) : longint;
begin
   BitmapUnit:=(TXPrivDisplay(dpy))^.bitmap_unit;
end;

function BitmapBitOrder(dpy : longint) : longint;
begin
   BitmapBitOrder:=(TXPrivDisplay(dpy))^.bitmap_bit_order;
end;

function BitmapPad(dpy : longint) : longint;
begin
   BitmapPad:=(TXPrivDisplay(dpy))^.bitmap_pad;
end;

function ImageByteOrder(dpy : longint) : longint;
begin
   ImageByteOrder:=(TXPrivDisplay(dpy))^.byte_order;
end;

function NextRequest(dpy : longint) : longint;
begin
   NextRequest:=((TXPrivDisplay(dpy))^.request) + 1;
end;

function LastKnownRequestProcessed(dpy : longint) : longint;
begin
   LastKnownRequestProcessed:=(TXPrivDisplay(dpy))^.last_request_read;
end;

function ScreenOfDisplay(dpy,scr : longint) : longint;
begin
   ScreenOfDisplay:=@((TXPrivDisplay(dpy))^.(screens[scr]));
end;

function DefaultScreenOfDisplay(dpy : longint) : longint;
begin
   DefaultScreenOfDisplay:=ScreenOfDisplay(dpy,DefaultScreen(dpy));
end;

function DisplayOfScreen(s : longint) : longint;
begin
   DisplayOfScreen:=s^.display;
end;

function RootWindowOfScreen(s : longint) : longint;
begin
   RootWindowOfScreen:=s^.root;
end;

function BlackPixelOfScreen(s : longint) : longint;
begin
   BlackPixelOfScreen:=s^.black_pixel;
end;

function WhitePixelOfScreen(s : longint) : longint;
begin
   WhitePixelOfScreen:=s^.white_pixel;
end;

function DefaultColormapOfScreen(s : longint) : longint;
begin
   DefaultColormapOfScreen:=s^.cmap;
end;

function DefaultDepthOfScreen(s : longint) : longint;
begin
   DefaultDepthOfScreen:=s^.root_depth;
end;

function DefaultGCOfScreen(s : longint) : longint;
begin
   DefaultGCOfScreen:=s^.default_gc;
end;

function DefaultVisualOfScreen(s : longint) : longint;
begin
   DefaultVisualOfScreen:=s^.root_visual;
end;

function WidthOfScreen(s : longint) : longint;
begin
   WidthOfScreen:=s^.width;
end;

function HeightOfScreen(s : longint) : longint;
begin
   HeightOfScreen:=s^.height;
end;

function WidthMMOfScreen(s : longint) : longint;
begin
   WidthMMOfScreen:=s^.mwidth;
end;

function HeightMMOfScreen(s : longint) : longint;
begin
   HeightMMOfScreen:=s^.mheight;
end;

function PlanesOfScreen(s : longint) : longint;
begin
   PlanesOfScreen:=s^.root_depth;
end;

function CellsOfScreen(s : longint) : longint;
begin
   CellsOfScreen:=(DefaultVisualOfScreen(s))^.map_entries;
end;

function MinCmapsOfScreen(s : longint) : longint;
begin
   MinCmapsOfScreen:=s^.min_maps;
end;

function MaxCmapsOfScreen(s : longint) : longint;
begin
   MaxCmapsOfScreen:=s^.max_maps;
end;

function DoesSaveUnders(s : longint) : longint;
begin
   DoesSaveUnders:=s^.save_unders;
end;

function DoesBackingStore(s : longint) : longint;
begin
   DoesBackingStore:=s^.backing_store;
end;

function EventMaskOfScreen(s : longint) : longint;
begin
   EventMaskOfScreen:=s^.root_input_mask;
end;

function XAllocID(dpy : longint) : longint;
begin
   XAllocID:=(TXPrivDisplay(dpy))^.resource_alloc(dpy);
end;
{$endif MACROS}


end.
