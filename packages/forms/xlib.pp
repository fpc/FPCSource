
unit xlib;
  { Changes after conversion from Xlib.h marked MVC -- Michael Van Canneyt
    Removed many many comments. 
    Including copyright. should be pasted in again. MVC }

  interface

uses X; { This is needed MVC.}

    type
       TXPointer = ^char;
       TBool = Longint; 
       PBool = ^TBool;
       TStatus = Longint; { Also a choice. MVC }
       { Some pointers : }
       pppchar = ^ppchar;
       ppwindow = ^pwindow;
       Twchar_t = cardinal;
       Pwchar_t = ^Twchar_t;
       Pstatus = ^TStatus;
       PXPointer = ^TXPointer;
       Plongint = ^Longint;
       PPLOngint = ^Plongint;

    const

{
 Removed, standard in Pascal - MVC 
       True = 1;
       False = 0;
}       
	QueuedAlready = 0;
	QueuedAfterReading = 1;

    {* !!!!!
    #define ConnectionNumber(dpy) 	(((_XPrivDisplay)dpy)->fd)
    #define RootWindow(dpy, scr) 	(ScreenOfDisplay(dpy,scr)->root)
    #define DefaultScreen(dpy) 	(((_XPrivDisplay)dpy)->default_screen)
    #define DefaultRootWindow(dpy) 	(ScreenOfDisplay(dpy,DefaultScreen(dpy))->root)
    #define DefaultVisual(dpy, scr) (ScreenOfDisplay(dpy,scr)->root_visual)
    #define DefaultGC(dpy, scr) 	(ScreenOfDisplay(dpy,scr)->default_gc)
    #define BlackPixel(dpy, scr) 	(ScreenOfDisplay(dpy,scr)->black_pixel)
    #define WhitePixel(dpy, scr) 	(ScreenOfDisplay(dpy,scr)->white_pixel)
    #define AllPlanes 		((unsigned long)~0L)
    #define QLength(dpy) 		(((_XPrivDisplay)dpy)->qlen)
    #define DisplayWidth(dpy, scr) 	(ScreenOfDisplay(dpy,scr)->width)
    #define DisplayHeight(dpy, scr) (ScreenOfDisplay(dpy,scr)->height)
    #define DisplayWidthMM(dpy, scr)(ScreenOfDisplay(dpy,scr)->mwidth)
    #define DisplayHeightMM(dpy, scr)(ScreenOfDisplay(dpy,scr)->mheight)
    #define DisplayPlanes(dpy, scr) (ScreenOfDisplay(dpy,scr)->root_depth)
    #define DisplayCells(dpy, scr) 	(DefaultVisual(dpy,scr)->map_entries)
    #define ScreenCount(dpy) 	(((_XPrivDisplay)dpy)->nscreens)
    #define ServerVendor(dpy) 	(((_XPrivDisplay)dpy)->vendor)
    #define ProtocolVersion(dpy) 	(((_XPrivDisplay)dpy)->proto_major_version)
    #define ProtocolRevision(dpy) 	(((_XPrivDisplay)dpy)->proto_minor_version)
    #define VendorRelease(dpy) 	(((_XPrivDisplay)dpy)->release)
    #define DisplayString(dpy) 	(((_XPrivDisplay)dpy)->display_name)
    #define DefaultDepth(dpy, scr) 	(ScreenOfDisplay(dpy,scr)->root_depth)
    #define DefaultColormap(dpy, scr)(ScreenOfDisplay(dpy,scr)->cmap)
    #define BitmapUnit(dpy) 	(((_XPrivDisplay)dpy)->bitmap_unit)
    #define BitmapBitOrder(dpy) 	(((_XPrivDisplay)dpy)->bitmap_bit_order)
    #define BitmapPad(dpy) 		(((_XPrivDisplay)dpy)->bitmap_pad)
    #define ImageByteOrder(dpy) 	(((_XPrivDisplay)dpy)->byte_order)
    
    #ifdef CRAY /* unable to get WORD64 without pulling in other symbols
    #define NextRequest(dpy)	XNextRequest(dpy)
    #else
    #define NextRequest(dpy)	(((_XPrivDisplay)dpy)->request + 1)
    #endif
    #define LastKnownRequestProcessed(dpy)	(((_XPrivDisplay)dpy)->last_request_read)
    
    /* macros for screen oriented applications (toolkit)
    
    #define ScreenOfDisplay(dpy, scr)(&((_XPrivDisplay)dpy)->screens[scr])
    #define DefaultScreenOfDisplay(dpy) ScreenOfDisplay(dpy,DefaultScreen(dpy))
    #define DisplayOfScreen(s)	((s)->display)
    #define RootWindowOfScreen(s)	((s)->root)
    #define BlackPixelOfScreen(s)	((s)->black_pixel)
    #define WhitePixelOfScreen(s)	((s)->white_pixel)
    #define DefaultColormapOfScreen(s)((s)->cmap)
    #define DefaultDepthOfScreen(s)	((s)->root_depth)
    #define DefaultGCOfScreen(s)	((s)->default_gc)
    #define DefaultVisualOfScreen(s)((s)->root_visual)
    #define WidthOfScreen(s)	((s)->width)
    #define HeightOfScreen(s)	((s)->height)
    #define WidthMMOfScreen(s)	((s)->mwidth)
    #define HeightMMOfScreen(s)	((s)->mheight)
    #define PlanesOfScreen(s)	((s)->root_depth)
    #define CellsOfScreen(s)	(DefaultVisualOfScreen((s))->map_entries)
    #define MinCmapsOfScreen(s)	((s)->min_maps)
    #define MaxCmapsOfScreen(s)	((s)->max_maps)
    #define DoesSaveUnders(s)	((s)->save_unders)
    #define DoesBackingStore(s)	((s)->backing_store)
    #define EventMaskOfScreen(s)	((s)->root_input_mask)
    *}
    {*
     * Extensions need a way to hang private data on some structures.
     *}
       QueuedAfterFlush = 2;
    {* number returned by XRegisterExtension *}
    {* next item on list of data for structure *}
    {*!!!!!
    	int (*free_private)();	/* called to free private storage *}
    {* data private to this extension. *}

    type
       { Added this for functions at the end. }
       PCardinal = ^Cardinal;
                 
       PXExtData = ^TXExtData ;
       PPXExtData = ^PXExtData ; 
       { Put this in to accomodate for the next in the following record. MVC}
       TXExtData = record
            number : longint;
            next : PXExtData; { incorrectly converted. MVC }
            private_data : TXPointer;
         end;
       TXExtCodes = record
            extension : longint;
            major_opcode : longint;
            first_event : longint;
            first_error : longint;
         end;
       PXExtCodes = ^TXExtCodes;
    {*
     * Data structure for retrieving info about pixmap formats.
     *}
       TXPixmapFormatValues = record
            depth : longint;
            bits_per_pixel : longint;
            scanline_pad : longint;
         end;
       PXPixmapFormatValues = ^TXPixmapFormatValues;
       TXGCValues = record
            Xfunction : longint; { Renamed function to Xfunction }
            plane_mask : cardinal;
            foreground : cardinal;
            background : cardinal;
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
                                 { This was Bool - error in converter ? MVC}
            clip_x_origin : longint;
            clip_y_origin : longint;
            clip_mask : TPixmap;
            dash_offset : longint;
            dashes : char;
         end;
       PXGCValues = ^TXGCValues;
    { The GC got defined as GC = ^record - probably from *struct, so I
      defined a GCrecord. MVC. }
       TGC = ^TGCrecord;   
       TGCrecord = record
            ext_data : PXExtData;
            gid : TGContext;
         end;
    {*
     * Visual structure; contains information about colormapping possible.
     *}
       TVisual = record
            ext_data : PXExtData;
            visualid : TVisualID;
            c_class : longint;
            classes : longint; { Renamed class to classes. MVC}
            red_mask : cardinal;
            green_mask : cardinal;
            blue_mask : cardinal;
            bits_per_rgb : longint;
            map_entries : longint;
         end;
       PVisual = ^TVisual;
    {*
     * Depth structure; contains information for each possible depth.
     *}
       TDepth = record
            depth : longint;
            nvisuals : longint;
            visuals : PVisual;
         end;
       PDepth=^TDepth;
    {*
     * Information about the screen.  The contents of this structure are
     * implementation dependent.  A Screen should be treated as opaque
     * by application code.
     *}
    {*
      struct _XDisplay;  Forward declare before use for C++ 
    *}
    { Defined this, needed for the display field in the following. 
      Should be a pointer to the previous _Xdisplay... MVC}
      p_XDisplay = pointer; 
       TScreen = record
            ext_data : PXExtData;
            display : p_XDisplay;
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
            white_pixel : cardinal;
            black_pixel : cardinal;
            max_maps : longint;
            min_maps : longint;
            backing_store : longint;
            save_unders : TBool;
            root_input_mask : longint;
         end;
         PScreen = ^TScreen;
    {*
     * Format structure; describes ZFormat data the screen will understand.
     *}
       TScreenFormat = record
            ext_data : PXExtData;
            depth : longint;
            bits_per_pixel : longint;
            scanline_pad : longint;
         end;
       PScreenFormat=^TScreenFormat;
    {*
     * Data structure for setting window attributes.
     *}
       TXSetWindowAttributes = record
            background_pixmap : TPixmap;
            background_pixel : cardinal;
            border_pixmap : TPixmap;
            border_pixel : cardinal;
            bit_gravity : longint;
            win_gravity : longint;
            backing_store : longint;
            backing_planes : cardinal;
            backing_pixel : cardinal;
            save_under : TBool;
            event_mask : longint;
            do_not_propagate_mask : longint;
            override_redirect : TBool;
            colormap : TColormap;
            cursor : TCursor;
         end;
       PXSetWindowAttributes = ^TXSetWindowAttributes;
       TXWindowAttributes = record
            x : longint;
            y : longint;
            width : longint;
            height : longint;
            border_width : longint;
            depth : longint;
            visual : PVisual;
            root : TWindow;
            c_class : longint;
            classes : longint; { Renamed from class. MVC}
            bit_gravity : longint;
            win_gravity : longint;
            backing_store : longint;
            backing_planes : cardinal;
            backing_pixel : cardinal;
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
       PXWindowAttributes = ^TXWindowAttributes;
    {*
     * Data structure for host setting; getting routines.
     *
     *}
       TXHostAddress = record
            family : longint;
            length : longint;
            address : pchar;
         end;
       PXHostAddress = ^TXHostAddress ;
    {*
     * Data structure for "image" data, used by image manipulation routines.
     *}
     { Added the following empty record - needed in Ximage. 
       Fill in later. MVC }
       Funcsrecord = record
          dummy : integer; { Just something stupid }
          end;
       TXImage = record
            width : longint;
            height : longint;
            xoffset : longint;
            format : longint;
            data : pchar;
            byte_order : longint;
            bitmap_unit : longint;
            bitmap_bit_order : longint;
            bitmap_pad : longint;
            depth : longint;
            bytes_per_line : longint;
            bits_per_pixel : longint;
            red_mask : cardinal;
            green_mask : cardinal;
            blue_mask : cardinal;
            obdata : TXPointer;
            funcs : funcsrecord {Added this. Incorrectly converted}
         end;
      PXImage = ^TXImage;
    {* 
     * Data structure for XReconfigureWindow
     *}
       TXWindowChanges = record
            x : longint;
            y : longint;
            width : longint;
            height : longint;
            border_width : longint;
            sibling : TWindow;
            stack_mode : longint;
         end;
        PXWindowChanges =  ^TXWindowChanges;
    {*
     * Data structure used by color operations
     *}
    {* do_red, do_green, do_blue *}
       TXColor = record
            pixel : cardinal;
            red : word;
            green : word;
            blue : word;
            flags : char;
            pad : char;
         end;
       PXColor = ^TXColor;
    {*
     * Data structures for graphics operations.  On most machines, these are
     * congruent with the wire protocol structures, so reformatting the data
     * can be avoided on these architectures.
     *}
       TXSegment = record
            x1 : integer;
            y1 : integer;
            x2 : integer;
            y2 : integer;
         end;
       PXSegment = ^TXSegment;
       TXPoint = record
            x : integer;
            y : integer;
         end;
       PXPoint = ^TXPoint;
       TXRectangle = record
            x : integer;
            y : integer;
            width : word;
            height : word;
         end;
       PXRectangle = ^TXRectangle;
       TXArc = record
            x : integer;
            y : integer;
            width : word;
            height : word;
            angle1 : integer;
            angle2 : integer;
         end;
       PXarc = ^TXarc;
    {* Data structure for XChangeKeyboardControl *}
    {* On, Off, Default *}
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
        PXKeyboardControl = ^TXKeyboardControl;
    {* Data structure for XGetKeyboardControl *}
       TXKeyboardState = record
            key_click_percent : longint;
            bell_percent : longint;
            bell_pitch : cardinal;
            bell_duration : cardinal;
            led_mask : cardinal;
            global_auto_repeat : longint;
            auto_repeats : array[0..(32)-1] of char;
         end;
       PXKeyboardState = ^TXKeyboardState;
    {* Data structure for XGetMotionEvents.  *}
       TXTimeCoord = record
            time : TTime;
            x : integer;
            y : integer;
         end;
       PXTimeCoord = ^TXTimeCoord;
    {* Data structure for X{Set,Get}ModifierMapping *}
    {* The server's max # of keys per modifier *}
    {* An 8 by max_keypermod array of modifiers *}
       TXModifierKeymap = record
            max_keypermod : longint;
            modifiermap : PKeyCode;
         end;
       PXModifierKeymap = ^TXModifierKeymap;
    {*
     * Display datatype maintaining display specific data.
     * The contents of this structure are implementation dependent.
     * A Display should be treated as opaque by application code.
     *}
    { I commented the following. It is a complete mess in the XLib.h !!
      MVC
           Display = Display;
     }
    {*
    struct _XPrivate;		/* Forward declare before use for C++
    struct _XrmHashBucketRec;
    *}
       TDisplay = record
            ext_data : PXExtData;
            private1 : pointer ; { Should be to a _Xprivate struct. MVC }
            fd : longint;
            private2 : longint;
            proto_major_version : longint;
            proto_minor_version : longint;
            vendor : pchar;
            private3 : TXID;
            private4 : TXID;
            private5 : TXID;
            private6 : longint;
            resource_alloc : Function : TXID; cdecl;
            byte_order : longint;
            bitmap_unit : longint;
            bitmap_pad : longint;
            bitmap_bit_order : longint;
            nformats : longint;
            pixmap_format : PScreenFormat;
            private8 : longint;
            release : longint;
            private9 : pointer ; { Both of them to a _XPRivate struct. MVC}
            private10 : pointer ;
            qlen : longint;
            last_request_read : cardinal;
            request : cardinal;
            private11 : TXPointer;
            private12 : TXPointer;
            private13 : TXPointer;
            private14 : TXPointer;
            max_request_size : cardinal;
            db : pointer; { To a _XrmHashBucketRec struct. MVC }
            private15 : Function : longint; cdecl;
            display_name : pchar;
            default_screen : longint;
            nscreens : longint;
            screens : PScreen;
            motion_buffer : cardinal;
            private16 : cardinal;
            min_keycode : longint;
            max_keycode : longint;
            private17 : TXPointer;
            private18 : TXPointer;
            private19 : longint;
            xdefaults : pchar;
         end;
         PDisplay=^TDisplay;
    {*
     * Definitions of specific events.
     *}
    { !! Changed all 'type' definitions to 'eventttype' MVC } 
    { !! Changed al display definitions to 'whatdisplay' MVC }
    { !! Idem dito for window,time,property !!}
       TXKeyEvent = record
            eventtype : longint;
            serial : cardinal;
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
            state : cardinal;
            keycode : cardinal;
            same_screen : TBool;
         end;
       PXKeyEvent =^TXKeyEvent;
       TXKeyPressedEvent = TXKeyEvent;
       PXKeyPressedEvent= ^TXKeyPressedEvent;
       XKeyReleasedEvent = TXKeyEvent;
       TXButtonEvent = record
            eventtype : longint;
            serial : cardinal;
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
            state : cardinal;
            button : cardinal;
            same_screen : TBool;
         end;
       TXButtonPressedEvent = TXButtonEvent;
       TXButtonReleasedEvent = TXButtonEvent;
       TXMotionEvent = record
            eventtype : longint;
            serial : cardinal;
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
            state : cardinal;
            is_hint : char;
            same_screen : TBool;
         end;
       TXPointerMovedEvent = TXMotionEvent;
    {*
    	 * NotifyAncestor, NotifyVirtual, NotifyInferior, 
    	 * NotifyNonlinear,NotifyNonlinearVirtual
    	 *}
       TXCrossingEvent = record
            eventtype : longint;
            serial : cardinal;
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
            state : cardinal;
         end;
       TXEnterWindowEvent = TXCrossingEvent;
       TXLeaveWindowEvent = TXCrossingEvent;
       TXFocusChangeEvent = record
            eventtype : longint;
            serial : cardinal;
            send_event : TBool;
            display : PDisplay;
            window : TWindow;
            mode : longint;
            detail : longint;
         end;
       TXFocusInEvent = TXFocusChangeEvent;
       TXFocusOutEvent = TXFocusChangeEvent;
       TXKeymapEvent = record
            eventtype : longint;
            serial : cardinal;
            send_event : TBool;
            display : PDisplay;
            window : TWindow;
            key_vector : array[0..(32)-1] of char;
         end;
       TXExposeEvent = record
            eventtype : longint;
            serial : cardinal;
            send_event : TBool;
            display : PDisplay;
            window : TWindow;
            x : longint;
            y : longint;
            width : longint;
            height : longint;
            count : longint;
         end;
       TXGraphicsExposeEvent = record
            eventtype : longint;
            serial : cardinal;
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
       TXNoExposeEvent = record
            eventtype : longint;
            serial : cardinal;
            send_event : TBool;
            display : PDisplay;
            drawable : TDrawable;
            major_code : longint;
            minor_code : longint;
         end;
       TXVisibilityEvent = record
            eventtype : longint;
            serial : cardinal;
            send_event : TBool;
            display : PDisplay;
            window : TWindow;
            state : longint;
         end;
       TXCreateWindowEvent = record
            eventtype : longint;
            serial : cardinal;
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
       TXDestroyWindowEvent = record
            eventtype : longint;
            serial : cardinal;
            send_event : TBool;
            display : PDisplay;
            event : TWindow;
            window : TWindow;
         end;
       TXUnmapEvent = record
            eventtype : longint;
            serial : cardinal;
            send_event : TBool;
            display : PDisplay;
            event : TWindow;
            window : TWindow;
            from_configure : TBool;
         end;
       TXMapEvent = record
            eventtype : longint;
            serial : cardinal;
            send_event : TBool;
            display : PDisplay;
            event : TWindow;
            window : TWindow;
            override_redirect : TBool;
         end;
       TXMapRequestEvent = record
            eventtype : longint;
            serial : cardinal;
            send_event : TBool;
            display : PDisplay;
            parent : TWindow;
            window : TWindow;
         end;
       TXReparentEvent = record
            eventtype : longint;
            serial : cardinal;
            send_event : TBool;
            display : PDisplay;
            event : TWindow;
            window : TWindow;
            parent : TWindow;
            x : longint;
            y : longint;
            override_redirect : TBool;
         end;
       TXConfigureEvent = record
            eventtype : longint;
            serial : cardinal;
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
       TXGravityEvent = record
            eventtype : longint;
            serial : cardinal;
            send_event : TBool;
            display : PDisplay;
            event : TWindow;
            window : TWindow;
            x : longint;
            y : longint;
         end;
       TXResizeRequestEvent = record
            eventtype : longint;
            serial : cardinal;
            send_event : TBool;
            display : PDisplay;
            window : TWindow;
            width : longint;
            height : longint;
         end;
       TXConfigureRequestEvent = record
            eventtype : longint;
            serial : cardinal;
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
            value_mask : cardinal;
         end;
       TXCirculateEvent = record
            eventtype : longint;
            serial : cardinal;
            send_event : TBool;
            display : PDisplay;
            event : TWindow;
            window : TWindow;
            place : longint;
         end;
       TXCirculateRequestEvent = record
            eventtype : longint;
            serial : cardinal;
            send_event : TBool;
            display : PDisplay;
            parent : TWindow;
            window : TWindow;
            place : longint;
         end;
       TXPropertyEvent = record
            eventtype : longint;
            serial : cardinal;
            send_event : TBool;
            display : PDisplay;
            window : TWindow;
            atom : TAtom;
            time : TTime;
            state : longint;
         end;
       TXSelectionClearEvent = record
            eventtype : longint;
            serial : cardinal;
            send_event : TBool;
            display : PDisplay;
            window : TWindow;
            selection : TAtom;
            time : TTime;
         end;
       TXSelectionRequestEvent = record
            eventtype : longint;
            serial : cardinal;
            send_event : TBool;
            display : PDisplay;
            owner : TWindow;
            requestor : TWindow;
            selection : TAtom;
            target : TAtom;
            {whatproperty : TAtom;}
            time : TTime;
         end;
       TXSelectionEvent = record
            eventtype : longint;
            serial : cardinal;
            send_event : TBool;
            display : PDisplay;
            requestor : TWindow;
            selection : TAtom;
            target : TAtom;
            whatproperty : TAtom;
            whattime : TTime;
         end;
       TXColormapEvent = record
            eventtype : longint;
            serial : cardinal;
            send_event : TBool;
            display : PDisplay;
            window : TWindow;
            colormap : TColormap;
            c_new : TBool;
            news : TBool; { Was new. MVC }
            state : longint;
         end;
       TXClientMessageEvent = record
            eventtype : longint;
            serial : cardinal;
            send_event : TBool;
            display : PDisplay;
            window : TWindow;
            message_eventtype : TAtom;
            format : longint;
            data : record
                case longint of
                   0 : (b : array[0..(20)-1] of char);
                   1 : (s : array[0..(10)-1] of integer);
                   2 : (l : array[0..(5)-1] of longint);
              end;
         end;
       TXMappingEvent = record
            eventtype : longint;
            serial : cardinal;
            send_event : TBool;
            display : PDisplay;
            window : TWindow;
            request : longint;
            first_keycode : longint;
            count : longint;
         end;
       PXMappingEvent = ^TXMappingEvent;
       TXErrorEvent = record
            eventtype : longint;
            Whatdisplay : PDisplay;
            resourceid : TXID;
            serial : cardinal;
            error_code : char;
            request_code : char;
            minor_code : char;
         end;
       PXErrorEvent = ^TXErrorEvent;
       TXAnyEvent = record
            eventtype : longint;
            serial : cardinal;
            send_event : TBool;
            display : PDisplay;
            window : TWindow;
         end;
       TXEvent = record
           case longint of
              0 : (eventtype : longint);
              1 : (xany : TXAnyEvent);
              2 : (xkey : TXKeyEvent);
              3 : (xbutton : TXButtonEvent);
              4 : (xmotion : TXMotionEvent);
              5 : (xcrossing : TXCrossingEvent);
              6 : (xfocus : TXFocusChangeEvent);
              7 : (xexpose : TXExposeEvent);
              8 : (xgraphicsexpose : TXGraphicsExposeEvent);
              9 : (xnoexpose : TXNoExposeEvent);
              10 : (xvisibility : TXVisibilityEvent);
              11 : (xcreatewindow : TXCreateWindowEvent);
              12 : (xdestroywindow : TXDestroyWindowEvent);
              13 : (xunmap : TXUnmapEvent);
              14 : (xmap : TXMapEvent);
              15 : (xmaprequest : TXMapRequestEvent);
              16 : (xreparent : TXReparentEvent);
              17 : (xconfigure : TXConfigureEvent);
              18 : (xgravity : TXGravityEvent);
              19 : (xresizerequest : TXResizeRequestEvent);
              20 : (xconfigurerequest : TXConfigureRequestEvent);
              21 : (xcirculate : TXCirculateEvent);
              22 : (xcirculaterequest : TXCirculateRequestEvent);
              23 : (xproperty : TXPropertyEvent);
              24 : (xselectionclear : TXSelectionClearEvent);
              25 : (xselectionrequest : TXSelectionRequestEvent);
              26 : (xselection : TXSelectionEvent);
              27 : (xcolormap : TXColormapEvent);
              28 : (xclient : TXClientMessageEvent);
              29 : (xmapping : TXMappingEvent);
              30 : (xerror : TXErrorEvent);
              31 : (xkeymap : TXKeymapEvent);
              32 : (pad : array[0..(24)-1] of longint);
         end;
     PXEvent = ^TXEvent;
    {*!!!!!!
    #define XAllocID(dpy) ((*((_XPrivDisplay)dpy)->resource_alloc)((dpy)))
    *}
    {*
     * per character font metric information.
     *}
       TXCharStruct = record
            lbearing : integer;
            rbearing : integer;
            width : integer;
            ascent : integer;
            descent : integer;
            attributes : word;
         end;
       PXCharStruct = ^TXCharStruct;
       TXFontProp = record
            name : TAtom;
            card32 : cardinal;
         end;
       PXFontProp = ^TXFontProp;
       PXFontstruct = ^TXFontStruct;
       PPXFontstruct = ^PXFontStruct;
       PPPXFontstruct = ^PPXFontStruct;
       TXFontStruct = record
            ext_data : PXExtData;
            fid : TFont;
            direction : cardinal;
            min_char_or_byte2 : cardinal;
            max_char_or_byte2 : cardinal;
            min_byte1 : cardinal;
            max_byte1 : cardinal;
            all_chars_exist : TBool;
            default_char : cardinal;
            n_properties : longint;
            properties : PXFontProp;
            min_bounds : TXCharStruct;
            max_bounds : TXCharStruct;
            per_char : PXCharStruct;
            ascent : longint;
            descent : longint;
         end;
       TXTextItem = record
            chars : pchar;
            nchars : longint;
            delta : longint;
            font : TFont;
         end;
       PXTextItem = ^TXTextItem;
    {* normal 16 bit characters are two bytes *}
       TXChar2b = record
            byte1 : char;
            byte2 : char;
         end;
       PXChar2b = ^TXChar2b;
       TXTextItem16 = record
            chars : PXChar2b;
            nchars : longint;
            delta : longint;
            font : TFont;
         end;
       PXTextItem16 = ^TXTextItem16;
       TXEDataObject = record
           case longint of
              0 : (display : PDisplay);
              1 : (gc : TGC);
              2 : (visual : PVisual);
              3 : (screen : PScreen);
              4 : (pixmap_format : PScreenFormat);
              5 : (font : PXFontStruct);
         end;
       TXFontSetExtents = record
            max_ink_extent : TXRectangle;
            max_logical_extent : TXRectangle;
         end;
       PXFontSetExtents = ^TXFontSetExtents;
       TXOMProc = Procedure; cdecl;
       
       TXOMrec = record
         dummy : integer;
       end;
       TXOCrec = record
         dummy : integer;
       end;
       TXOM = ^TXOMrec;
       TXOC = ^TXOCrec;
       { Again an accomodation. MVC }
       TXfontSetRecord = record
         dummy : integer;
         end;
       TXFontSet = ^TXFontsetRecord;
       TXmbTextItem = record
            chars : pchar;
            nchars : longint;
            delta : longint;
            font_set : TXFontSet;
         end;
       PXmbTextItem = ^TXmbTextItem;
       TXwcTextItem = record
            chars : Pwchar_t;
            nchars : longint;
            delta : longint;
            font_set : TXFontSet;
         end;
       PXwcTextItem = ^TXwcTextItem;

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
       TXOMCharSetList = record
            charset_count : longint;
            charset_list : ^pchar;
         end;
       TXOrientation = (
         XOMOrientation_LTR_TTB,
         XOMOrientation_RTL_TTB,
         XOMOrientation_TTB_LTR,
         XOMOrientation_TTB_RTL,
         XOMOrientation_Context
         );
    {* Input Text description *}
       TXOMOrientation = record
            num_orient : longint;
            orient : ^TXOrientation;
         end;
       TXOMFontInfo = record
            num_font : longint;
            font_struct_list : ^PXFontStruct;
            font_name_list : ^pchar;
         end;
       TXIMProc = Procedure; cdecl;
       { Defined the following to accomodate XIM. MVC}
       TXIMrecord = record
         field : integer
         end;
       TXIM = ^TXIMrecord;
       { Defined the following to accomodate XIC. MVC}
       TXICrecord = record
         field : integer
         end;
       TXIC = ^TXICrecord;
       TXIMStyle = cardinal;
       TXIMStyles = record
            count_styles : word;
            supported_styles : ^TXIMStyle;
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
       XLookupKeySym = 3;
       XLookupBoth = 4;

    type
       TXVaNestedList = TXPointer;
       TXIMCallback = record
            client_data : TXPointer;
            callback : TXIMProc;
         end;
       TXIMFeedback = cardinal;

    const
       XIMReverse = 1;
       XIMUnderline = (1) shl (1);
       XIMHighlight = (1) shl (2);
       XIMPrimary = (1) shl (5);
       XIMSecondary = (1) shl (6);
       XIMTertiary = (1) shl (7);
       XIMVisibleToForward = (1) shl (8);
       XIMVisibleToBackword = (1) shl (9);
       XIMVisibleToCenter = (1) shl (10);

    type
       TXIMText = record
            length : word;
            feedback : ^TXIMFeedback;
            encoding_is_wchar : TBool;
            thestring : record { This was string. MVC}
                 case longint of
                   0 : (multi_byte : ^char);
                   1 : (wide_char : Pwchar_t);
              end;
         end;
       PXIMText=^TXIMText;
       TXIMPreeditState = cardinal;

    const
       XIMPreeditUnKnown = 0;
       XIMPreeditEnable = 1;
       XIMPreeditDisable = (1) shl (1);

    type
       TXIMPreeditStateNotifyCallbackStruct = record
            state : TXIMPreeditState;
         end;
       TXIMResetState = cardinal;

    const
       XIMInitialState = 1;
       XIMPreserveState = (1) shl (1);

    type
       TXIMStringConversionFeedback = cardinal;

    const
       XIMStringConversionLeftEdge = $00000001;
       XIMStringConversionRightEdge = $00000002;
       XIMStringConversionTopEdge = $00000004;
       XIMStringConversionBottomEdge = $00000008;
       XIMStringConversionConcealed = $00000010;
       XIMStringConversionWrapped = $00000020;

    type
       TXIMStringConversionText = record
            length : word;
            feedback : ^TXIMStringConversionFeedback;
            encoding_is_wchar : TBool;
            thestring : record { This was string. MVC}
                case longint of
                   0 : (mbs : pchar);
                   1 : (wcs : pwchar_t);
              end;
         end;
       TXIMStringConversionPosition = word;
       TXIMStringConversionType = word;

    const
       XIMStringConversionBuffer = $0001;
       XIMStringConversionLine = $0002;
       XIMStringConversionWord = $0003;
       XIMStringConversionChar = $0004;

    type
       TXIMStringConversionOperation = word;

    const
       TXIMStringConversionSubstitution = $0001;
       TXIMStringConversionRetrival = $0002;

    type
       TXIMStringConversionCallbackStruct = record
            position : TXIMStringConversionPosition;
            thetype : TXIMStringConversionType; { This wsa type. MVC}
            operation : TXIMStringConversionOperation;
            factor : word;
            thetext : ^TXIMStringConversionText; { This was text. MVC}
         end;
       TXIMPreeditDrawCallbackStruct = record
            caret : longint;
            chg_first : longint;
            chg_length : longint;
            text : ^TXIMText;
         end;
       TXIMCaretDirection = (
         XIMForwardChar,
         XIMBackwardChar,
         XIMForwardWord,
         XIMBackwardWord,
         XIMCaretUp,
         XIMCaretDown,
         XIMNextLine,
         XIMPreviousLine,
         XIMLineStart,
         XIMLineEnd,
         XIMAbsolutePosition,
         XIMDontChange
         );
       TXIMCaretStyle = (
         XIMIsInvisible,
         XIMIsPrimary,
         XIMIsSecondary
         );
       TXIMPreeditCaretCallbackStruct = record
            position : longint;
            direction : TXIMCaretDirection;
            style : TXIMCaretStyle;
         end;
       TXIMStatusDataType = (
         XIMTextType,
         XIMBitmapType
         );
       TXIMStatusDrawCallbackStruct = record
            thetype : TXIMStatusDataType; { Was Type. MVC }
            data : record
                case longint of
                   0 : (text : PXIMText);
                   1 : (bitmap : TPixmap);
              end;
         end;
       TXIMHotKeyTrigger = record
            keysym : TKeySym;
            modifier : longint;
            modifier_mask : longint;
         end;
       TXIMHotKeyTriggers = record
            num_hot_key : longint;
            key : ^TXIMHotKeyTrigger;
         end;
       TXIMHotKeyState = cardinal;

    const
       XIMHotKeyStateON = $0001;
       XIMHotKeyStateOFF = $0002;

    type
       XIMValuesList = record
            count_values : word;
            supported_values : ^pchar;
         end;

type
  TXErrorHandler = Function(_para1 : PDisplay;_para2 : PXErrorEvent) : longint; cdecl;
  TXIOErrorHandler = Function(_para1 : PDisplay) : longint; cdecl;
  TXConnectionWatchProc = Procedure(_para1 : PDisplay;_para2 : TXPointer;_para3 : longint;_para4 : TBool;_para5 : PXPointer); cdecl;


{ Here start the Function definitions in the C header file. MVC } cdecl;

Function XLoadQueryFont (para1 : PDisplay; para2 : pchar) : PXFontStruct; cdecl;
Function XQueryFont(_para1 : PDisplay;_para2 : TXID) : PXFontStruct; cdecl;
Function XGetMotionEvents(_para1 : PDisplay;_para2 : TWindow;_para3 : TTime;_para4 : TTime;_para5 : Plongint) : PXTimeCoord; cdecl;
Function XDeleteModifiermapEntry(_para1 : PXModifierKeymap;_para2 : cardinal;_para3 : TKeyCode;_para4 : longint) : PXModifierKeymap; cdecl;
Function XGetModifierMapping(_para1 : PDisplay) : PXModifierKeymap; cdecl;
Function XInsertModifiermapEntry(_para1 : PXModifierKeymap;_para2 : cardinal;_para3 : TKeyCode;_para4 : longint) : PXModifierKeymap; cdecl;
Function XNewModifiermap(_para1 : longint) : PXModifierKeymap; cdecl;
Function XCreateImage(_para1 : PDisplay;_para2 : PVisual;_para3 : cardinal;_para4 : longint;_para5 : longint;_para6 : pchar;_para7 : cardinal;_para8 : cardinal;_para9 : longint;_para10 : longint) : PXImage; cdecl;
Function XInitImage(_para1 : PXImage) : TStatus; cdecl;
Function XGetImage(_para1 : PDisplay;_para2 : TDrawable;_para3 : longint;_para4 : longint;_para5 : cardinal;_para6 : cardinal;_para7 : cardinal;_para8 : longint) : PXImage; cdecl;
Function XGetSubImage(_para1 : PDisplay;_para2 : TDrawable;_para3 : longint;_para4 : longint;_para5 : cardinal;_para6 : cardinal;_para7 : cardinal;_para8 : longint;_para9 : PXImage;_para10 : longint;_para11 : longint) : PXImage; cdecl;
Function XOpenDisplay(_para1 : pchar) : PDisplay; cdecl;
Procedure XrmInitialize; cdecl;
Function XFetchBytes(_para1 : PDisplay;_para2 : Plongint) : pchar; cdecl;
Function XFetchBuffer(_para1 : PDisplay;_para2 : Plongint;_para3 : longint) : pchar; cdecl;
Function XGetAtomName(_para1 : PDisplay;_para2 : TAtom) : pchar; cdecl;
Function XGetAtomNames(_para1 : PDisplay;_para2 : PAtom;_para3 : longint;_para4 : PPChar) : TStatus; cdecl;
Function XGetDefault(_para1 : PDisplay;_para2 : pchar;_para3 : pchar) : pchar; cdecl;
Function XDisplayName(_para1 : pchar) : pchar; cdecl;
Function XKeysymToString(_para1 : TKeySym) : pchar; cdecl;
Function XSynchronize(_para1 : PDisplay;_para2 : TBool) : longint; cdecl;
{ WHat to do with this ??? MVC
Function XSetAfterFunction(_para1 : PDisplay;_para2 : function(_para1 : PDisplay) : longint) : function : longint; cdecl;
}
Function XInternAtom(_para1 : PDisplay;_para2 : pchar;_para3 : TBool) : TAtom; cdecl;
Function XInternAtoms(_para1 : PDisplay;_para2 : PPChar;_para3 : longint;_para4 : TBool;_para5 : PAtom) : TStatus; cdecl;
Function XCopyColormapAndFree(_para1 : PDisplay;_para2 : TColormap) : TColormap; cdecl;
Function XCreateColormap(_para1 : PDisplay;_para2 : TWindow;_para3 : PVisual;_para4 : longint) : TColormap; cdecl;
Function XCreatePixmapCursor(_para1 : PDisplay;_para2 : TPixmap;_para3 : TPixmap;_para4 : PXColor;_para5 : PXColor;_para6 : cardinal;_para7 : cardinal) : TCursor; cdecl;
Function XCreateGlyphCursor(_para1 : PDisplay;_para2 : TFont;_para3 : TFont;_para4 : cardinal;_para5 : cardinal;_para6 : PXColor;_para7 : PXColor) : TCursor; cdecl;
Function XCreateFontCursor(_para1 : PDisplay;_para2 : cardinal) : TCursor; cdecl;
Function XLoadFont(_para1 : PDisplay;_para2 : pchar) : TFont; cdecl;
Function XCreateGC(_para1 : PDisplay;_para2 : TDrawable;_para3 : cardinal;_para4 : PXGCValues) : TGC; cdecl;
Function XGContextFromGC(_para1 : TGC) : TGContext; cdecl;
Procedure XFlushGC(_para1 : PDisplay;_para2 : TGC); cdecl;
Function XCreatePixmap(_para1 : PDisplay;_para2 : TDrawable;_para3 : cardinal;_para4 : cardinal;_para5 : cardinal) : TPixmap; cdecl;
Function XCreateBitmapFromData(_para1 : PDisplay;_para2 : TDrawable;_para3 : pchar;_para4 : cardinal;_para5 : cardinal) : TPixmap; cdecl;
Function XCreatePixmapFromBitmapData(_para1 : PDisplay;_para2 : TDrawable;_para3 : pchar;_para4 : cardinal;_para5 : cardinal;_para6 : cardinal;_para7 : cardinal;_para8 : cardinal) : TPixmap; cdecl;
Function XCreateSimpleWindow(_para1 : PDisplay;_para2 : TWindow;_para3 : longint;_para4 : longint;_para5 : cardinal;_para6 : cardinal;_para7 : cardinal;_para8 : cardinal;_para9 : cardinal) : TWindow; cdecl;
Function XGetSelectionOwner(_para1 : PDisplay;_para2 : TAtom) : TWindow; cdecl;
Function XCreateWindow(_para1 : PDisplay;_para2 : TWindow;_para3 : longint;_para4 : longint;_para5 : cardinal;_para6 : cardinal;_para7 : cardinal;_para8 : longint;_para9 : cardinal;_para10 : PVisual;_para11 : cardinal;_para12 : PXSetWindowAttributes) : TWindow; cdecl;
Function XListInstalledColormaps(_para1 : PDisplay;_para2 : TWindow;_para3 : Plongint) : PColormap; cdecl;
Function XListFonts(_para1 : PDisplay;_para2 : pchar;_para3 : longint;_para4 : Plongint) : PPChar; cdecl;
Function XListFontsWithInfo(_para1 : PDisplay;_para2 : pchar;_para3 : longint;_para4 : Plongint;_para5 : PPXFontStruct) : PPChar; cdecl;
Function XGetFontPath(_para1 : PDisplay;_para2 : Plongint) : PPChar; cdecl;
Function XListExtensions(_para1 : PDisplay;_para2 : Plongint) : PPChar; cdecl;
Function XListProperties(_para1 : PDisplay;_para2 : TWindow;_para3 : Plongint) : PAtom; cdecl;
Function XListHosts(_para1 : PDisplay;_para2 : Plongint;_para3 : PBool) : PXHostAddress; cdecl;
Function XKeycodeToKeysym(_para1 : PDisplay;_para3 : TKeyCode;_para4 : longint) : TKeySym; cdecl;
{ Doesn't want ? MVC 
Function XLookupKeysym(_para1 : PXKeyEvent;_para2 : longint) : TKeySym; cdecl;
}
Function XGetKeyboardMapping(_para1 : PDisplay;_para2 : cardinal;_para3 : TKeyCode;_para4 : longint;_para5 : Plongint) : PKeySym; cdecl;
Function XStringToKeysym(_para1 : pchar) : TKeySym; cdecl;
Function XMaxRequestSize(_para1 : PDisplay) : longint; cdecl;
Function XExtendedMaxRequestSize(_para1 : PDisplay) : longint; cdecl;
Function XResourceManagerString(_para1 : PDisplay) : pchar; cdecl;
Function XScreenResourceString(_para1 : PScreen) : pchar; cdecl;
Function XDisplayMotionBufferSize(_para1 : PDisplay) : cardinal; cdecl;
Function XVisualIDFromVisual(_para1 : PVisual) : TVisualID; cdecl;
Function XInitThreads : TStatus; cdecl;
Procedure XLockDisplay(_para1 : PDisplay); cdecl;
Procedure XUnlockDisplay(_para1 : PDisplay); cdecl;
Function XInitExtension(_para1 : PDisplay;_para2 : pchar) : PXExtCodes; cdecl;
Function XAddExtension(_para1 : PDisplay) : PXExtCodes; cdecl;
Function XFindOnExtensionList(_para1 : PPXExtData;_para2 : longint) : PXExtData; cdecl;
Function XEHeadOfExtensionList(_para1 : TXEDataObject) : PPXExtData; cdecl;
Function XRootWindow(_para1 : PDisplay;_para2 : longint) : TWindow; cdecl;
Function XDefaultRootWindow(_para1 : PDisplay) : TWindow; cdecl;
Function XRootWindowOfScreen(_para1 : PScreen) : TWindow; cdecl;
Function XDefaultVisual(_para1 : PDisplay;_para2 : longint) : PVisual; cdecl;
Function XDefaultVisualOfScreen(_para1 : PScreen) : PVisual; cdecl;
Function XDefaultGC(_para1 : PDisplay;_para2 : longint) : TGC; cdecl;
Function XDefaultGCOfScreen(_para1 : PScreen) : TGC; cdecl;
Function XBlackPixel(_para1 : PDisplay;_para2 : longint) : cardinal; cdecl;
Function XWhitePixel(_para1 : PDisplay;_para2 : longint) : cardinal; cdecl;
Function XAllPlanes : cardinal; cdecl;
Function XBlackPixelOfScreen(_para1 : PScreen) : cardinal; cdecl;
Function XWhitePixelOfScreen(_para1 : PScreen) : cardinal; cdecl;
Function XNextRequest(_para1 : PDisplay) : cardinal; cdecl;
{
Function XLastKnownRequestProcessed(_para1 : PDisplay) : cardinal; cdecl;
}
Function XServerVendor(_para1 : PDisplay) : pchar; cdecl;
Function XDisplayString(_para1 : PDisplay) : pchar; cdecl;
Function XDefaultColormap(_para1 : PDisplay;_para2 : longint) : TColormap; cdecl;
Function XDefaultColormapOfScreen(_para1 : PScreen) : TColormap; cdecl;
Function XDisplayOfScreen(_para1 : PScreen) : PDisplay; cdecl;
Function XScreenOfDisplay(_para1 : PDisplay;_para2 : longint) : PScreen; cdecl;
Function XDefaultScreenOfDisplay(_para1 : PDisplay) : PScreen; cdecl;
Function XEventMaskOfScreen(_para1 : PScreen) : longint; cdecl;
Function XScreenNumberOfScreen(_para1 : PScreen) : longint; cdecl;
Function XSetErrorHandler(_para1 : TXErrorHandler) : TXErrorHandler; cdecl;
Function XSetIOErrorHandler(_para1 : TXIOErrorHandler) : TXIOErrorHandler; cdecl;
Function XListPixmapFormats(_para1 : PDisplay;_para2 : Plongint) : PXPixmapFormatValues; cdecl;
Function XListDepths(_para1 : PDisplay;_para2 : longint;_para3 : Plongint) : Plongint; cdecl;
Function XReconfigureWMWindow(_para1 : PDisplay;_para2 : TWindow;_para3 : longint;_para4 : cardinal;_para5 : PXWindowChanges) : TStatus; cdecl;
Function XGetWMProtocols(_para1 : PDisplay;_para2 : TWindow;_para3 : PPAtom;_para4 : Plongint) : TStatus; cdecl;
Function XSetWMProtocols(_para1 : PDisplay;_para2 : TWindow;_para3 : PAtom;_para4 : longint) : TStatus; cdecl;
Function XIconifyWindow(_para1 : PDisplay;_para2 : TWindow;_para3 : longint) : TStatus; cdecl;
Function XWithdrawWindow(_para1 : PDisplay;_para2 : TWindow;_para3 : longint) : TStatus; cdecl;
Function XGetCommand(_para1 : PDisplay;_para2 : TWindow;_para3 : PPPChar;_para4 : Plongint) : TStatus; cdecl;
Function XGetWMColormapWindows(_para1 : PDisplay;_para2 : TWindow;_para3 : PPWindow;_para4 : Plongint) : TStatus; cdecl;
Function XSetWMColormapWindows(_para1 : PDisplay;_para2 : TWindow;_para3 : PWindow;_para4 : longint) : TStatus; cdecl;
Procedure XFreeStringList(_para1 : PPChar); cdecl;
Function XSetTransientForHint(_para1 : PDisplay;_para2 : TWindow;_para3 : TWindow) : longint; cdecl;
Function XActivateScreenSaver(_para1 : PDisplay) : longint; cdecl;
Function XAddHost(_para1 : PDisplay;_para2 : PXHostAddress) : longint; cdecl;
Function XAddHosts(_para1 : PDisplay;_para2 : PXHostAddress;_para3 : longint) : longint; cdecl;
{
Function XAddToExtensionList(_para1 : ^^*;_para2 : PXExtData) : longint; cdecl;
}
Function XAddToSaveSet(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;
Function XAllocColor(_para1 : PDisplay;_para2 : TColormap;_para3 : PXColor) : TStatus; cdecl;
Function XAllocColorCells(_para1 : PDisplay;_para2 : TColormap;_para3 : TBool;_para4 : PCardinal;_para5 : cardinal;_para6 : PCardinal;_para7 : cardinal) : TStatus; cdecl;
Function XAllocColorPlanes(_para1 : PDisplay;_para2 : TColormap;_para3 : TBool;_para4 : PCardinal;_para5 : longint;_para6 : longint;_para7 : longint;_para8 : longint;_para9 : PCardinal;_para10 : PCardinal;_para11 : PCardinal) : TStatus; cdecl;
Function XAllocNamedColor(_para1 : PDisplay;_para2 : TColormap;_para3 : pchar;_para4 : PXColor;_para5 : PXColor) : TStatus; cdecl;
Function XAllowEvents(_para1 : PDisplay;_para2 : longint;_para3 : TTime) : longint; cdecl;
Function XAutoRepeatOff(_para1 : PDisplay) : longint; cdecl;
Function XAutoRepeatOn(_para1 : PDisplay) : longint; cdecl;
Function XBell(_para1 : PDisplay;_para2 : longint) : longint; cdecl;
Function XBitmapBitOrder(_para1 : PDisplay) : longint; cdecl;
Function XBitmapPad(_para1 : PDisplay) : longint; cdecl;
Function XBitmapUnit(_para1 : PDisplay) : longint; cdecl;
Function XCellsOfScreen(_para1 : PScreen) : longint; cdecl;
Function XChangeActivePointerGrab(_para1 : PDisplay;_para2 : cardinal;_para3 : TCursor;_para4 : TTime) : longint; cdecl;
Function XChangeGC(_para1 : PDisplay;_para2 : TGC;_para3 : cardinal;_para4 : PXGCValues) : longint; cdecl;
Function XChangeKeyboardControl(_para1 : PDisplay;_para2 : cardinal;_para3 : PXKeyboardControl) : longint; cdecl;
Function XChangeKeyboardMapping(_para1 : PDisplay;_para2 : longint;_para3 : longint;_para4 : PKeySym;_para5 : longint) : longint; cdecl;
Function XChangePointerControl(_para1 : PDisplay;_para2 : TBool;_para3 : TBool;_para4 : longint;_para5 : longint;_para6 : longint) : longint; cdecl;
Function XChangeProperty(_para1 : PDisplay;_para2 : TWindow;_para3 : TAtom;_para4 : TAtom;_para5 : longint;_para6 : longint;_para7 : pchar;_para8 : longint) : longint; cdecl;
Function XChangeSaveSet(_para1 : PDisplay;_para2 : TWindow;_para3 : longint) : longint; cdecl;
Function XChangeWindowAttributes(_para1 : PDisplay;_para2 : TWindow;_para3 : cardinal;_para4 : PXSetWindowAttributes) : longint; cdecl;
{
Function XCheckIfEvent(_para1 : PDisplay;_para2 : PXEvent;_para3 : function(_para1 : PDisplay;_para2 : PXEvent;_para3 : XPointer) : TBool;_para4 : XPointer) : TBool; cdecl;
}
Function XCheckMaskEvent(_para1 : PDisplay;_para2 : longint;_para3 : PXEvent) : TBool; cdecl;
Function XCheckTypedEvent(_para1 : PDisplay;_para2 : longint;_para3 : PXEvent) : TBool; cdecl;
Function XCheckTypedWindowEvent(_para1 : PDisplay;_para2 : TWindow;_para3 : longint;_para4 : PXEvent) : TBool; cdecl;
Function XCheckWindowEvent(_para1 : PDisplay;_para2 : TWindow;_para3 : longint;_para4 : PXEvent) : TBool; cdecl;
Function XCirculateSubwindows(_para1 : PDisplay;_para2 : TWindow;_para3 : longint) : longint; cdecl;
Function XCirculateSubwindowsDown(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;
Function XCirculateSubwindowsUp(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;
Function XClearArea(_para1 : PDisplay;_para2 : TWindow;_para3 : longint;_para4 : longint;_para5 : cardinal;_para6 : cardinal;_para7 : TBool) : longint; cdecl;
Function XClearWindow(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;
Function XCloseDisplay(_para1 : PDisplay) : longint; cdecl;
Function XConfigureWindow(_para1 : PDisplay;_para2 : TWindow;_para3 : cardinal;_para4 : PXWindowChanges) : longint; cdecl;
Function XConnectionNumber(_para1 : PDisplay) : longint; cdecl;
Function XConvertSelection(_para1 : PDisplay;_para2 : TAtom;_para3 : TAtom;_para4 : TAtom;_para5 : TWindow;_para6 : TTime) : longint; cdecl;
Function XCopyArea(_para1 : PDisplay;_para2 : TDrawable;_para3 : TDrawable;_para4 : TGC;_para5 : longint;_para6 : longint;_para7 : cardinal;_para8 : cardinal;_para9 : longint;_para10 : longint) : longint; cdecl;
Function XCopyGC(_para1 : PDisplay;_para2 : TGC;_para3 : cardinal;_para4 : TGC) : longint; cdecl;
Function XCopyPlane(_para1 : PDisplay;_para2 : TDrawable;_para3 : TDrawable;_para4 : TGC;_para5 : longint;_para6 : longint;_para7 : cardinal;_para8 : cardinal;_para9 : longint;_para10 : longint;_para11 : cardinal) : longint; cdecl;
Function XDefaultDepth(_para1 : PDisplay;_para2 : longint) : longint; cdecl;
Function XDefaultDepthOfScreen(_para1 : PScreen) : longint; cdecl;
Function XDefaultScreen(_para1 : PDisplay) : longint; cdecl;
Function XDefineCursor(_para1 : PDisplay;_para2 : TWindow;_para3 : TCursor) : longint; cdecl;
Function XDeleteProperty(_para1 : PDisplay;_para2 : TWindow;_para3 : TAtom) : longint; cdecl;
Function XDestroyWindow(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;
Function XDestroySubwindows(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;
Function XDoesBackingStore(_para1 : PScreen) : longint; cdecl;
Function XDoesSaveUnders(_para1 : PScreen) : TBool; cdecl;
Function XDisableAccessControl(_para1 : PDisplay) : longint; cdecl;
Function XDisplayCells(_para1 : PDisplay;_para2 : longint) : longint; cdecl;
Function XDisplayHeight(_para1 : PDisplay;_para2 : longint) : longint; cdecl;
Function XDisplayHeightMM(_para1 : PDisplay;_para2 : longint) : longint; cdecl;
Function XDisplayKeycodes(_para1 : PDisplay;_para2 : Plongint;_para3 : Plongint) : longint; cdecl;
Function XDisplayPlanes(_para1 : PDisplay;_para2 : longint) : longint; cdecl;
Function XDisplayWidth(_para1 : PDisplay;_para2 : longint) : longint; cdecl;
Function XDisplayWidthMM(_para1 : PDisplay;_para2 : longint) : longint; cdecl;
Function XDrawArc(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : cardinal;_para7 : cardinal;_para8 : longint;_para9 : longint) : longint; cdecl;
Function XDrawArcs(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : PXArc;_para5 : longint) : longint; cdecl;
Function XDrawImageString(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : pchar;_para7 : longint) : longint; cdecl;
Function XDrawImageString16(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : PXChar2b;_para7 : longint) : longint; cdecl;
Function XDrawLine(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : longint;_para7 : longint) : longint; cdecl;
Function XDrawLines(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : PXPoint;_para5 : longint;_para6 : longint) : longint; cdecl;
Function XDrawPoint(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint) : longint; cdecl;
Function XDrawPoints(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : PXPoint;_para5 : longint;_para6 : longint) : longint; cdecl;
Function XDrawRectangle(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : cardinal;_para7 : cardinal) : longint; cdecl;
Function XDrawRectangles(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : PXRectangle;_para5 : longint) : longint; cdecl;
Function XDrawSegments(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : PXSegment;_para5 : longint) : longint; cdecl;
Function XDrawString(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : pchar;_para7 : longint) : longint; cdecl;
Function XDrawString16(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : PXChar2b;_para7 : longint) : longint; cdecl;
Function XDrawText(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : PXTextItem;_para7 : longint) : longint; cdecl;
Function XDrawText16(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : PXTextItem16;_para7 : longint) : longint; cdecl;
Function XEnableAccessControl(_para1 : PDisplay) : longint; cdecl;
Function XEventsQueued(_para1 : PDisplay;_para2 : longint) : longint; cdecl;
Function XFetchName(_para1 : PDisplay;_para2 : TWindow;_para3 : PPChar) : TStatus; cdecl;
Function XFillArc(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : cardinal;_para7 : cardinal;_para8 : longint;_para9 : longint) : longint; cdecl;
Function XFillArcs(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : PXArc;_para5 : longint) : longint; cdecl;
Function XFillPolygon(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : PXPoint;_para5 : longint;_para6 : longint;_para7 : longint) : longint; cdecl;
Function XFillRectangle(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : cardinal;_para7 : cardinal) : longint; cdecl;
Function XFillRectangles(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : PXRectangle;_para5 : longint) : longint; cdecl;
Function XFlush(_para1 : PDisplay) : longint; cdecl;
Function XForceScreenSaver(_para1 : PDisplay;_para2 : longint) : longint; cdecl;
Function XFree(_para1 : pointer) : longint; cdecl;
Function XFreeColormap(_para1 : PDisplay;_para2 : TColormap) : longint; cdecl;
Function XFreeColors(_para1 : PDisplay;_para2 : TColormap;_para3 : PCardinal;_para4 : longint;_para5 : cardinal) : longint; cdecl;
Function XFreeCursor(_para1 : PDisplay;_para2 : TCursor) : longint; cdecl;
Function XFreeExtensionList(_para1 : PPChar) : longint; cdecl;
Function XFreeFont(_para1 : PDisplay;_para2 : PXFontStruct) : longint; cdecl;
Function XFreeFontInfo(_para1 : PPChar;_para2 : PXFontStruct;_para3 : longint) : longint; cdecl;
Function XFreeFontNames(_para1 : PPChar) : longint; cdecl;
Function XFreeFontPath(_para1 : PPChar) : longint; cdecl;
Function XFreeGC(_para1 : PDisplay;_para2 : TGC) : longint; cdecl;
Function XFreeModifiermap(_para1 : PXModifierKeymap) : longint; cdecl;
Function XFreePixmap(_para1 : PDisplay;_para2 : TPixmap) : longint; cdecl;
Function XGeometry(_para1 : PDisplay;_para2 : longint;_para3 : pchar;_para4 : pchar;_para5 : cardinal;_para6 : cardinal;_para7 : cardinal;_para8 : longint;_para9 : longint;_para10 : Plongint;_para11 : Plongint;_para12 : Plongint;_para13 : Plongint) : longint; cdecl;
Function XGetErrorDatabaseText(_para1 : PDisplay;_para2 : pchar;_para3 : pchar;_para4 : pchar;_para5 : pchar;_para6 : longint) : longint; cdecl;
Function XGetErrorText(_para1 : PDisplay;_para2 : longint;_para3 : pchar;_para4 : longint) : longint; cdecl;
Function XGetFontProperty(_para1 : PXFontStruct;_para2 : TAtom;_para3 : PCardinal) : TBool; cdecl;
Function XGetGCValues(_para1 : PDisplay;_para2 : TGC;_para3 : cardinal;_para4 : PXGCValues) : TStatus; cdecl;
Function XGetGeometry(_para1 : PDisplay;_para2 : TDrawable;_para3 : PWindow;_para4 : Plongint;_para5 : Plongint;_para6 : PCardinal;_para7 : PCardinal;_para8 : PCardinal;_para9 : PCardinal) : TStatus; cdecl;
Function XGetIconName(_para1 : PDisplay;_para2 : TWindow;_para3 : PPChar) : TStatus; cdecl;
Function XGetInputFocus(_para1 : PDisplay;_para2 : PWindow;_para3 : Plongint) : longint; cdecl;
Function XGetKeyboardControl(_para1 : PDisplay;_para2 : PXKeyboardState) : longint; cdecl;
Function XGetPointerControl(_para1 : PDisplay;_para2 : Plongint;_para3 : Plongint;_para4 : Plongint) : longint; cdecl;
Function XGetPointerMapping(_para1 : PDisplay;_para2 : pchar;_para3 : longint) : longint; cdecl;
Function XGetScreenSaver(_para1 : PDisplay;_para2 : Plongint;_para3 : Plongint;_para4 : Plongint;_para5 : Plongint) : longint; cdecl;
Function XGetTransientForHint(_para1 : PDisplay;_para2 : TWindow;_para3 : PWindow) : TStatus; cdecl;
Function XGetWindowProperty(_para1 : PDisplay;_para2 : TWindow;_para3 : TAtom;_para4 : longint;_para5 : longint;_para6 : TBool;_para7 : TAtom;_para8 : PAtom;_para9 : Plongint;_para10 : PCardinal;_para11 : PCardinal;_para12 : PPChar) : longint; cdecl;
Function XGetWindowAttributes(_para1 : PDisplay;_para2 : TWindow;_para3 : PXWindowAttributes) : TStatus; cdecl;
Function XGrabButton(_para1 : PDisplay;_para2 : cardinal;_para3 : cardinal;_para4 : TWindow;_para5 : TBool;_para6 : cardinal;_para7 : longint;_para8 : longint;_para9 : TWindow;_para10 : TCursor) : longint; cdecl;
Function XGrabKey(_para1 : PDisplay;_para2 : longint;_para3 : cardinal;_para4 : TWindow;_para5 : TBool;_para6 : longint;_para7 : longint) : longint; cdecl;
Function XGrabKeyboard(_para1 : PDisplay;_para2 : TWindow;_para3 : TBool;_para4 : longint;_para5 : longint;_para6 : TTime) : longint; cdecl;
Function XGrabPointer(_para1 : PDisplay;_para2 : TWindow;_para3 : TBool;_para4 : cardinal;_para5 : longint;_para6 : longint;_para7 : TWindow;_para8 : TCursor;_para9 : TTime) : longint; cdecl;
Function XGrabServer(_para1 : PDisplay) : longint; cdecl;
Function XHeightMMOfScreen(_para1 : PScreen) : longint; cdecl;
Function XHeightOfScreen(_para1 : PScreen) : longint; cdecl;
{
Function XIfEvent(_para1 : PDisplay;_para2 : PXEvent;_para3 : function(_para1 : PDisplay;_para2 : PXEvent;_para3 : XPointer) : TBool;_para4 : XPointer) : longint; cdecl;
}
Function XImageByteOrder(_para1 : PDisplay) : longint; cdecl;
Function XInstallColormap(_para1 : PDisplay;_para2 : TColormap) : longint; cdecl;
Function XKeysymToKeycode(_para1 : PDisplay;_para2 : TKeySym) : TKeyCode; cdecl;
Function XKillClient(_para1 : PDisplay;_para2 : TXID) : longint; cdecl;
Function XLastKnownRequestProcessed(_para1 : PDisplay) : cardinal; cdecl;
Function XLookupColor(_para1 : PDisplay;_para2 : TColormap;_para3 : pchar;_para4 : PXColor;_para5 : PXColor) : TStatus; cdecl;
Function XLowerWindow(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;
Function XMapRaised(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;
Function XMapSubwindows(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;
Function XMapWindow(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;
Function XMaskEvent(_para1 : PDisplay;_para2 : longint;_para3 : PXEvent) : longint; cdecl;
Function XMaxCmapsOfScreen(_para1 : PScreen) : longint; cdecl;
Function XMinCmapsOfScreen(_para1 : PScreen) : longint; cdecl;
Function XMoveResizeWindow(_para1 : PDisplay;_para2 : TWindow;_para3 : longint;_para4 : longint;_para5 : cardinal;_para6 : cardinal) : longint; cdecl;
Function XMoveWindow(_para1 : PDisplay;_para2 : TWindow;_para3 : longint;_para4 : longint) : longint; cdecl;
Function XNextEvent(_para1 : PDisplay;_para2 : PXEvent) : longint; cdecl;
Function XNoOp(_para1 : PDisplay) : longint; cdecl;
Function XParseColor(_para1 : PDisplay;_para2 : TColormap;_para3 : pchar;_para4 : PXColor) : TStatus; cdecl;
Function XParseGeometry(_para1 : pchar;_para2 : Plongint;_para3 : Plongint;_para4 : PCardinal;_para5 : PCardinal) : longint; cdecl;
Function XPeekEvent(_para1 : PDisplay;_para2 : PXEvent) : longint; cdecl;
{
Function XPeekIfEvent(_para1 : PDisplay;_para2 : PXEvent;_para3 : function(_para1 : PDisplay;_para2 : PXEvent;_para3 : XPointer) : TBool;_para4 : XPointer) : longint; cdecl;
}
Function XPending(_para1 : PDisplay) : longint; cdecl;
Function XPlanesOfScreen(_para1 : PScreen) : longint; cdecl;
Function XProtocolRevision(_para1 : PDisplay) : longint; cdecl;
Function XProtocolVersion(_para1 : PDisplay) : longint; cdecl;
Function XPutBackEvent(_para1 : PDisplay;_para2 : PXEvent) : longint; cdecl;
Function XPutImage(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : PXImage;_para5 : longint;_para6 : longint;_para7 : longint;_para8 : longint;_para9 : cardinal;_para10 : cardinal) : longint; cdecl;
Function XQLength(_para1 : PDisplay) : longint; cdecl;
Function XQueryBestCursor(_para1 : PDisplay;_para2 : TDrawable;_para3 : cardinal;_para4 : cardinal;_para5 : PCardinal;_para6 : PCardinal) : TStatus; cdecl;
Function XQueryBestSize(_para1 : PDisplay;_para2 : longint;_para3 : TDrawable;_para4 : cardinal;_para5 : cardinal;_para6 : PCardinal;_para7 : PCardinal) : TStatus; cdecl;
Function XQueryBestStipple(_para1 : PDisplay;_para2 : TDrawable;_para3 : cardinal;_para4 : cardinal;_para5 : PCardinal;_para6 : PCardinal) : TStatus; cdecl;
Function XQueryBestTile(_para1 : PDisplay;_para2 : TDrawable;_para3 : cardinal;_para4 : cardinal;_para5 : PCardinal;_para6 : PCardinal) : TStatus; cdecl;
Function XQueryColor(_para1 : PDisplay;_para2 : TColormap;_para3 : PXColor) : longint; cdecl;
Function XQueryColors(_para1 : PDisplay;_para2 : TColormap;_para3 : PXColor;_para4 : longint) : longint; cdecl;
Function XQueryExtension(_para1 : PDisplay;_para2 : pchar;_para3 : Plongint;_para4 : Plongint;_para5 : Plongint) : TBool; cdecl;
{
Function XQueryKeymap(_para1 : PDisplay;_para2 : array[0..(32)-1] of char) : longint; cdecl;
}
Function XQueryPointer(_para1 : PDisplay;_para2 : TWindow;_para3 : PWindow;_para4 : PWindow;_para5 : Plongint;_para6 : Plongint;_para7 : Plongint;_para8 : Plongint;_para9 : PCardinal) : TBool; cdecl;
Function XQueryTextExtents(_para1 : PDisplay;_para2 : TXID;_para3 : pchar;_para4 : longint;_para5 : Plongint;_para6 : Plongint;_para7 : Plongint;_para8 : PXCharStruct) : longint; cdecl;
Function XQueryTextExtents16(_para1 : PDisplay;_para2 : TXID;_para3 : PXChar2b;_para4 : longint;_para5 : Plongint;_para6 : Plongint;_para7 : Plongint;_para8 : PXCharStruct) : longint; cdecl;
Function XQueryTree(_para1 : PDisplay;_para2 : TWindow;_para3 : PWindow;_para4 : PWindow;_para5 : PPWindow;_para6 : PCardinal) : TStatus; cdecl;
Function XRaiseWindow(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;
Function XReadBitmapFile(_para1 : PDisplay;_para2 : TDrawable;_para3 : pchar;_para4 : PCardinal;_para5 : PCardinal;_para6 : PPixmap;_para7 : Plongint;_para8 : Plongint) : longint; cdecl;
Function XReadBitmapFileData(_para1 : pchar;_para2 : PCardinal;_para3 : PCardinal;_para4 : PPChar;_para5 : Plongint;_para6 : Plongint) : longint; cdecl;
Function XRebindKeysym(_para1 : PDisplay;_para2 : TKeySym;_para3 : PKeySym;_para4 : longint;_para5 : pchar;_para6 : longint) : longint; cdecl;
Function XRecolorCursor(_para1 : PDisplay;_para2 : TCursor;_para3 : PXColor;_para4 : PXColor) : longint; cdecl;
Function XRefreshKeyboardMapping(_para1 : PXMappingEvent) : longint; cdecl;
Function XRemoveFromSaveSet(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;
Function XRemoveHost(_para1 : PDisplay;_para2 : PXHostAddress) : longint; cdecl;
Function XRemoveHosts(_para1 : PDisplay;_para2 : PXHostAddress;_para3 : longint) : longint; cdecl;
Function XReparentWindow(_para1 : PDisplay;_para2 : TWindow;_para3 : TWindow;_para4 : longint;_para5 : longint) : longint; cdecl;
Function XResetScreenSaver(_para1 : PDisplay) : longint; cdecl;
Function XResizeWindow(_para1 : PDisplay;_para2 : TWindow;_para3 : cardinal;_para4 : cardinal) : longint; cdecl;
Function XRestackWindows(_para1 : PDisplay;_para2 : PWindow;_para3 : longint) : longint; cdecl;
Function XRotateBuffers(_para1 : PDisplay;_para2 : longint) : longint; cdecl;
Function XRotateWindowProperties(_para1 : PDisplay;_para2 : TWindow;_para3 : PAtom;_para4 : longint;_para5 : longint) : longint; cdecl;
Function XScreenCount(_para1 : PDisplay) : longint; cdecl;
Function XSelectInput(_para1 : PDisplay;_para2 : TWindow;_para3 : longint) : longint; cdecl;
Function XSendEvent(_para1 : PDisplay;_para2 : TWindow;_para3 : TBool;_para4 : longint;_para5 : PXEvent) : TStatus; cdecl;
Function XSetAccessControl(_para1 : PDisplay;_para2 : longint) : longint; cdecl;
Function XSetArcMode(_para1 : PDisplay;_para2 : TGC;_para3 : longint) : longint; cdecl;
Function XSetBackground(_para1 : PDisplay;_para2 : TGC;_para3 : cardinal) : longint; cdecl;
Function XSetClipMask(_para1 : PDisplay;_para2 : TGC;_para3 : TPixmap) : longint; cdecl;
Function XSetClipOrigin(_para1 : PDisplay;_para2 : TGC;_para3 : longint;_para4 : longint) : longint; cdecl;
Function XSetClipRectangles(_para1 : PDisplay;_para2 : TGC;_para3 : longint;_para4 : longint;_para5 : PXRectangle;_para6 : longint;_para7 : longint) : longint; cdecl;
Function XSetCloseDownMode(_para1 : PDisplay;_para2 : longint) : longint; cdecl;
Function XSetCommand(_para1 : PDisplay;_para2 : TWindow;_para3 : PPChar;_para4 : longint) : longint; cdecl;
Function XSetDashes(_para1 : PDisplay;_para2 : TGC;_para3 : longint;_para4 : pchar;_para5 : longint) : longint; cdecl;
Function XSetFillRule(_para1 : PDisplay;_para2 : TGC;_para3 : longint) : longint; cdecl;
Function XSetFillStyle(_para1 : PDisplay;_para2 : TGC;_para3 : longint) : longint; cdecl;
Function XSetFont(_para1 : PDisplay;_para2 : TGC;_para3 : TFont) : longint; cdecl;
Function XSetFontPath(_para1 : PDisplay;_para2 : PPChar;_para3 : longint) : longint; cdecl;
Function XSetForeground(_para1 : PDisplay;_para2 : TGC;_para3 : cardinal) : longint; cdecl;
Function XSetFunction(_para1 : PDisplay;_para2 : TGC;_para3 : longint) : longint; cdecl;
Function XSetGraphicsExposures(_para1 : PDisplay;_para2 : TGC;_para3 : TBool) : longint; cdecl;
Function XSetIconName(_para1 : PDisplay;_para2 : TWindow;_para3 : pchar) : longint; cdecl;
Function XSetInputFocus(_para1 : PDisplay;_para2 : TWindow;_para3 : longint;_para4 : TTime) : longint; cdecl;
Function XSetLineAttributes(_para1 : PDisplay;_para2 : TGC;_para3 : cardinal;_para4 : longint;_para5 : longint;_para6 : longint) : longint; cdecl;
Function XSetModifierMapping(_para1 : PDisplay;_para2 : PXModifierKeymap) : longint; cdecl;
Function XSetPlaneMask(_para1 : PDisplay;_para2 : TGC;_para3 : cardinal) : longint; cdecl;
Function XSetPointerMapping(_para1 : PDisplay;_para2 : pchar;_para3 : longint) : longint; cdecl;
Function XSetScreenSaver(_para1 : PDisplay;_para2 : longint;_para3 : longint;_para4 : longint;_para5 : longint) : longint; cdecl;
Function XSetSelectionOwner(_para1 : PDisplay;_para2 : TAtom;_para3 : TWindow;_para4 : TTime) : longint; cdecl;
Function XSetState(_para1 : PDisplay;_para2 : TGC;_para3 : cardinal;_para4 : cardinal;_para5 : longint;_para6 : cardinal) : longint; cdecl;
Function XSetStipple(_para1 : PDisplay;_para2 : TGC;_para3 : TPixmap) : longint; cdecl;
Function XSetSubwindowMode(_para1 : PDisplay;_para2 : TGC;_para3 : longint) : longint; cdecl;
Function XSetTSOrigin(_para1 : PDisplay;_para2 : TGC;_para3 : longint;_para4 : longint) : longint; cdecl;
Function XSetTile(_para1 : PDisplay;_para2 : TGC;_para3 : TPixmap) : longint; cdecl;
Function XSetWindowBackground(_para1 : PDisplay;_para2 : TWindow;_para3 : cardinal) : longint; cdecl;
Function XSetWindowBackgroundPixmap(_para1 : PDisplay;_para2 : TWindow;_para3 : TPixmap) : longint; cdecl;
Function XSetWindowBorder(_para1 : PDisplay;_para2 : TWindow;_para3 : cardinal) : longint; cdecl;
Function XSetWindowBorderPixmap(_para1 : PDisplay;_para2 : TWindow;_para3 : TPixmap) : longint; cdecl;
Function XSetWindowBorderWidth(_para1 : PDisplay;_para2 : TWindow;_para3 : cardinal) : longint; cdecl;
Function XSetWindowColormap(_para1 : PDisplay;_para2 : TWindow;_para3 : TColormap) : longint; cdecl;
Function XStoreBuffer(_para1 : PDisplay;_para2 : pchar;_para3 : longint;_para4 : longint) : longint; cdecl;
Function XStoreBytes(_para1 : PDisplay;_para2 : pchar;_para3 : longint) : longint; cdecl;
Function XStoreColor(_para1 : PDisplay;_para2 : TColormap;_para3 : PXColor) : longint; cdecl;
Function XStoreColors(_para1 : PDisplay;_para2 : TColormap;_para3 : PXColor;_para4 : longint) : longint; cdecl;
Function XStoreName(_para1 : PDisplay;_para2 : TWindow;_para3 : pchar) : longint; cdecl;
Function XStoreNamedColor(_para1 : PDisplay;_para2 : TColormap;_para3 : pchar;_para4 : cardinal;_para5 : longint) : longint; cdecl;
Function XSync(_para1 : PDisplay;_para2 : TBool) : longint; cdecl;
Function XTextExtents(_para1 : PXFontStruct;_para2 : pchar;_para3 : longint;_para4 : Plongint;_para5 : Plongint;_para6 : Plongint;_para7 : PXCharStruct) : longint; cdecl;
Function XTextExtents16(_para1 : PXFontStruct;_para2 : PXChar2b;_para3 : longint;_para4 : Plongint;_para5 : Plongint;_para6 : Plongint;_para7 : PXCharStruct) : longint; cdecl;
Function XTextWidth(_para1 : PXFontStruct;_para2 : pchar;_para3 : longint) : longint; cdecl;
Function XTextWidth16(_para1 : PXFontStruct;_para2 : PXChar2b;_para3 : longint) : longint; cdecl;
Function XTranslateCoordinates(_para1 : PDisplay;_para2 : TWindow;_para3 : TWindow;_para4 : longint;_para5 : longint;_para6 : Plongint;_para7 : Plongint;_para8 : PWindow) : TBool; cdecl;
Function XUndefineCursor(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;
Function XUngrabButton(_para1 : PDisplay;_para2 : cardinal;_para3 : cardinal;_para4 : TWindow) : longint; cdecl;
Function XUngrabKey(_para1 : PDisplay;_para2 : longint;_para3 : cardinal;_para4 : TWindow) : longint; cdecl;
Function XUngrabKeyboard(_para1 : PDisplay;_para2 : TTime) : longint; cdecl;
Function XUngrabPointer(_para1 : PDisplay;_para2 : TTime) : longint; cdecl;
Function XUngrabServer(_para1 : PDisplay) : longint; cdecl;
Function XUninstallColormap(_para1 : PDisplay;_para2 : TColormap) : longint; cdecl;
Function XUnloadFont(_para1 : PDisplay;_para2 : TFont) : longint; cdecl;
Function XUnmapSubwindows(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;
Function XUnmapWindow(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;
Function XVendorRelease(_para1 : PDisplay) : longint; cdecl;
Function XWarpPointer(_para1 : PDisplay;_para2 : TWindow;_para3 : TWindow;_para4 : longint;_para5 : longint;_para6 : cardinal;_para7 : cardinal;_para8 : longint;_para9 : longint) : longint; cdecl;
Function XWidthMMOfScreen(_para1 : PScreen) : longint; cdecl;
Function XWidthOfScreen(_para1 : PScreen) : longint; cdecl;
Function XWindowEvent(_para1 : PDisplay;_para2 : TWindow;_para3 : longint;_para4 : PXEvent) : longint; cdecl;
Function XWriteBitmapFile(_para1 : PDisplay;_para2 : pchar;_para3 : TPixmap;_para4 : cardinal;_para5 : cardinal;_para6 : longint;_para7 : longint) : longint; cdecl;
Function XSupportsLocale : TBool; cdecl;
Function XSetLocaleModifiers(_para1 : pchar) : pchar; cdecl;
{
Function XOpenOM(_para1 : PDisplay;_para2 : ^*;_para3 : pchar;_para4 : pchar) : TXOM; cdecl;
}
Function XCloseOM(_para1 : TXOM) : TStatus; cdecl;
Function XDisplayOfOM(_para1 : TXOM) : PDisplay; cdecl;
Function XLocaleOfOM(_para1 : TXOM) : pchar; cdecl;
Procedure XDestroyOC(_para1 : TXOC); cdecl;
Function TXOMOfOC(_para1 : TXOC) : TXOM; cdecl;
Function XCreateFontSet(_para1 : PDisplay;_para2 : pchar;_para3 : PPPChar;_para4 : Plongint;_para5 : PPChar) : TXFontSet; cdecl;
Procedure XFreeFontSet(_para1 : PDisplay;_para2 : TXFontSet); cdecl;
Function XFontsOfFontSet(_para1 : TXFontSet;_para2 : PPPXFontStruct;_para3 : PPPChar) : longint; cdecl;
Function XBaseFontNameListOfFontSet(_para1 : TXFontSet) : pchar; cdecl;
Function XLocaleOfFontSet(_para1 : TXFontSet) : pchar; cdecl;
Function XContextDependentDrawing(_para1 : TXFontSet) : TBool; cdecl;
Function XDirectionalDependentDrawing(_para1 : TXFontSet) : TBool; cdecl;
Function XContextualDrawing(_para1 : TXFontSet) : TBool; cdecl;
Function XExtentsOfFontSet(_para1 : TXFontSet) : PXFontSetExtents; cdecl;
Function XmbTextEscapement(_para1 : TXFontSet;_para2 : pchar;_para3 : longint) : longint; cdecl;
Function XwcTextEscapement(_para1 : TXFontSet;_para2 : Pwchar_t;_para3 : longint) : longint; cdecl;
Function XmbTextExtents(_para1 : TXFontSet;_para2 : pchar;_para3 : longint;_para4 : PXRectangle;_para5 : PXRectangle) : longint; cdecl;
Function XwcTextExtents(_para1 : TXFontSet;_para2 : Pwchar_t;_para3 : longint;_para4 : PXRectangle;_para5 : PXRectangle) : longint; cdecl;
Function XmbTextPerCharExtents(_para1 : TXFontSet;_para2 : pchar;_para3 : longint;_para4 : PXRectangle;_para5 : PXRectangle;_para6 : longint;_para7 : Plongint;_para8 : PXRectangle;_para9 : PXRectangle) : TStatus; cdecl;
Function XwcTextPerCharExtents(_para1 : TXFontSet;_para2 : Pwchar_t;_para3 : longint;_para4 : PXRectangle;_para5 : PXRectangle;_para6 : longint;_para7 : Plongint;_para8 : PXRectangle;_para9 : PXRectangle) : TStatus; cdecl;
Procedure XmbDrawText(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : PXmbTextItem;_para7 : longint); cdecl;
Procedure XwcDrawText(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : PXwcTextItem;_para7 : longint); cdecl;
Procedure XmbDrawString(_para1 : PDisplay;_para2 : TDrawable;_para3 : TXFontSet;_para4 : TGC;_para5 : longint;_para6 : longint;_para7 : pchar;_para8 : longint); cdecl;
Procedure XwcDrawString(_para1 : PDisplay;_para2 : TDrawable;_para3 : TXFontSet;_para4 : TGC;_para5 : longint;_para6 : longint;_para7 : Pwchar_t;_para8 : longint); cdecl;
Procedure XmbDrawImageString(_para1 : PDisplay;_para2 : TDrawable;_para3 : TXFontSet;_para4 : TGC;_para5 : longint;_para6 : longint;_para7 : pchar;_para8 : longint); cdecl;
Procedure XwcDrawImageString(_para1 : PDisplay;_para2 : TDrawable;_para3 : TXFontSet;_para4 : TGC;_para5 : longint;_para6 : longint;_para7 : Pwchar_t;_para8 : longint); cdecl;
{
Function XOpenIM(_para1 : PDisplay;_para2 : P*;_para3 : pchar;_para4 : pchar) : TXIM; cdecl;
}
Function XCloseIM(_para1 : TXIM) : TStatus; cdecl;
Function XDisplayOfIM(_para1 : TXIM) : PDisplay; cdecl;
Function XLocaleOfIM(_para1 : TXIM) : pchar; cdecl;
Procedure XDestroyIC(_para1 : TXIC); cdecl;
Procedure XSetICFocus(_para1 : TXIC); cdecl;
Procedure XUnsetICFocus(_para1 : TXIC); cdecl;
Function XwcResetIC(_para1 : TXIC) : Pwchar_t; cdecl;
Function XmbResetIC(_para1 : TXIC) : pchar; cdecl;
Function TXIMOfIC(_para1 : TXIC) : TXIM; cdecl;
Function XFilterEvent(_para1 : PXEvent;_para2 : TWindow) : TBool; cdecl;
Function XmbLookupString(_para1 : TXIC;_para2 : PXKeyPressedEvent;_para3 : pchar;_para4 : longint;_para5 : PKeySym;_para6 : PStatus) : longint; cdecl;
Function XwcLookupString(_para1 : TXIC;_para2 : PXKeyPressedEvent;_para3 : Pwchar_t;_para4 : longint;_para5 : PKeySym;_para6 : PStatus) : longint; cdecl;
{
Function XRegisterIMInstantiateCallback(_para1 : PDisplay;_para2 : P*;_para3 : pchar;_para4 : pchar;_para5 : TXIMProc;_para6 : PXPointer) : TBool; cdecl;
Function XUnregisterIMInstantiateCallback(_para1 : PDisplay;_para2 : P*;_para3 : pchar;_para4 : pchar;_para5 : TXIMProc;_para6 : PXPointer) : TBool; cdecl;
}
Function XInternalConnectionNumbers(_para1 : PDisplay;_para2 : PPlongint;_para3 : Plongint) : TStatus; cdecl;
Procedure XProcessInternalConnection(_para1 : PDisplay;_para2 : longint); cdecl;
Function XAddConnectionWatch(_para1 : PDisplay;_para2 : TXConnectionWatchProc;_para3 : TXPointer) : TStatus; cdecl;
Procedure XRemoveConnectionWatch(_para1 : PDisplay;_para2 : TXConnectionWatchProc;_para3 : TXPointer); cdecl;

implementation

function XLoadQueryFont (para1 : PDisplay; para2 : pchar) : PXFontStruct; cdecl;external ;
function XQueryFont(_para1 : PDisplay;_para2 : TXID) : PXFontStruct; cdecl;external ;
function XGetMotionEvents(_para1 : PDisplay;_para2 : TWindow;_para3 : TTime;_para4 : TTime;_para5 : Plongint) : PXTimeCoord; cdecl;external ;
function XDeleteModifiermapEntry(_para1 : PXModifierKeymap;_para2 : cardinal;_para3 : TKeyCode;_para4 : longint) : PXModifierKeymap; cdecl;external ;
function XGetModifierMapping(_para1 : PDisplay) : PXModifierKeymap; cdecl;external ;
function XInsertModifiermapEntry(_para1 : PXModifierKeymap;_para2 : cardinal;_para3 : TKeyCode;_para4 : longint) : PXModifierKeymap; cdecl;external ;
function XNewModifiermap(_para1 : longint) : PXModifierKeymap; cdecl;external ;
function XCreateImage(_para1 : PDisplay;_para2 : PVisual;_para3 : cardinal;_para4 : longint;_para5 : longint;_para6 : pchar;_para7 : cardinal;_para8 : cardinal;_para9 : longint;_para10 : longint) : PXImage; cdecl;external ;
function XInitImage(_para1 : PXImage) : TStatus; cdecl;external ;
function XGetImage(_para1 : PDisplay;_para2 : TDrawable;_para3 : longint;_para4 : longint;_para5 : cardinal;_para6 : cardinal;_para7 : cardinal;_para8 : longint) : PXImage; cdecl;external ;
function XGetSubImage(_para1 : PDisplay;_para2 : TDrawable;_para3 : longint;_para4 : longint;_para5 : cardinal;_para6 : cardinal;_para7 : cardinal;_para8 : longint;_para9 : PXImage;_para10 : longint;_para11 : longint) : PXImage; cdecl;external ;
function XOpenDisplay(_para1 : pchar) : PDisplay; cdecl;external ;
procedure XrmInitialize; cdecl;external ;
function XFetchBytes(_para1 : PDisplay;_para2 : Plongint) : pchar; cdecl;external ;
function XFetchBuffer(_para1 : PDisplay;_para2 : Plongint;_para3 : longint) : pchar; cdecl;external ;
function XGetAtomName(_para1 : PDisplay;_para2 : TAtom) : pchar; cdecl;external ;
function XGetAtomNames(_para1 : PDisplay;_para2 : PAtom;_para3 : longint;_para4 : PPChar) : TStatus; cdecl;external ;
function XGetDefault(_para1 : PDisplay;_para2 : pchar;_para3 : pchar) : pchar; cdecl;external ;
function XDisplayName(_para1 : pchar) : pchar; cdecl;external ;
function XKeysymToString(_para1 : TKeySym) : pchar; cdecl;external ;
function XSynchronize(_para1 : PDisplay;_para2 : TBool) : longint; cdecl;external ;
{ WHat to do with this ??? MVC
function XSetAfterFunction(_para1 : PDisplay;_para2 : function(_para1 : PDisplay) : longint) : function : longint; cdecl;external ;
}
function XInternAtom(_para1 : PDisplay;_para2 : pchar;_para3 : TBool) : TAtom; cdecl;external ;
function XInternAtoms(_para1 : PDisplay;_para2 : PPChar;_para3 : longint;_para4 : TBool;_para5 : PAtom) : TStatus; cdecl;external ;
function XCopyColormapAndFree(_para1 : PDisplay;_para2 : TColormap) : TColormap; cdecl;external ;
function XCreateColormap(_para1 : PDisplay;_para2 : TWindow;_para3 : PVisual;_para4 : longint) : TColormap; cdecl;external ;
function XCreatePixmapCursor(_para1 : PDisplay;_para2 : TPixmap;_para3 : TPixmap;_para4 : PXColor;_para5 : PXColor;_para6 : cardinal;_para7 : cardinal) : TCursor; cdecl;external ;
function XCreateGlyphCursor(_para1 : PDisplay;_para2 : TFont;_para3 : TFont;_para4 : cardinal;_para5 : cardinal;_para6 : PXColor;_para7 : PXColor) : TCursor; cdecl;external ;
function XCreateFontCursor(_para1 : PDisplay;_para2 : cardinal) : TCursor; cdecl;external ;
function XLoadFont(_para1 : PDisplay;_para2 : pchar) : TFont; cdecl;external ;
function XCreateGC(_para1 : PDisplay;_para2 : TDrawable;_para3 : cardinal;_para4 : PXGCValues) : TGC; cdecl;external ;
function XGContextFromGC(_para1 : TGC) : TGContext; cdecl;external ;
procedure XFlushGC(_para1 : PDisplay;_para2 : TGC); cdecl;external ;
function XCreatePixmap(_para1 : PDisplay;_para2 : TDrawable;_para3 : cardinal;_para4 : cardinal;_para5 : cardinal) : TPixmap; cdecl;external ;
function XCreateBitmapFromData(_para1 : PDisplay;_para2 : TDrawable;_para3 : pchar;_para4 : cardinal;_para5 : cardinal) : TPixmap; cdecl;external ;
function XCreatePixmapFromBitmapData(_para1 : PDisplay;_para2 : TDrawable;_para3 : pchar;_para4 : cardinal;_para5 : cardinal;_para6 : cardinal;_para7 : cardinal;_para8 : cardinal) : TPixmap; cdecl;external ;
function XCreateSimpleWindow(_para1 : PDisplay;_para2 : TWindow;_para3 : longint;_para4 : longint;_para5 : cardinal;_para6 : cardinal;_para7 : cardinal;_para8 : cardinal;_para9 : cardinal) : TWindow; cdecl;external ;
function XGetSelectionOwner(_para1 : PDisplay;_para2 : TAtom) : TWindow; cdecl;external ;
function XCreateWindow(_para1 : PDisplay;_para2 : TWindow;_para3 : longint;_para4 : longint;_para5 : cardinal;_para6 : cardinal;_para7 : cardinal;_para8 : longint;_para9 : cardinal;_para10 : PVisual;_para11 : cardinal;_para12 : PXSetWindowAttributes) : TWindow; cdecl;external ;
function XListInstalledColormaps(_para1 : PDisplay;_para2 : TWindow;_para3 : Plongint) : PColormap; cdecl;external ;
function XListFonts(_para1 : PDisplay;_para2 : pchar;_para3 : longint;_para4 : Plongint) : PPChar; cdecl;external ;
function XListFontsWithInfo(_para1 : PDisplay;_para2 : pchar;_para3 : longint;_para4 : Plongint;_para5 : PPXFontStruct) : PPChar; cdecl;external ;
function XGetFontPath(_para1 : PDisplay;_para2 : Plongint) : PPChar; cdecl;external ;
function XListExtensions(_para1 : PDisplay;_para2 : Plongint) : PPChar; cdecl;external ;
function XListProperties(_para1 : PDisplay;_para2 : TWindow;_para3 : Plongint) : PAtom; cdecl;external ;
function XListHosts(_para1 : PDisplay;_para2 : Plongint;_para3 : PBool) : PXHostAddress; cdecl;external ;
function XKeycodeToKeysym(_para1 : PDisplay;_para3 : TKeyCode;_para4 : longint) : TKeySym; cdecl;external ;
{ Doesn't want ? MVC 
function XLookupKeysym(_para1 : PXKeyEvent;_para2 : longint) : TKeySym; cdecl;external ;
}
function XGetKeyboardMapping(_para1 : PDisplay;_para2 : cardinal;_para3 : TKeyCode;_para4 : longint;_para5 : Plongint) : PKeySym; cdecl;external ;
function XStringToKeysym(_para1 : pchar) : TKeySym; cdecl;external ;
function XMaxRequestSize(_para1 : PDisplay) : longint; cdecl;external ;
function XExtendedMaxRequestSize(_para1 : PDisplay) : longint; cdecl;external ;
function XResourceManagerString(_para1 : PDisplay) : pchar; cdecl;external ;
function XScreenResourceString(_para1 : PScreen) : pchar; cdecl;external ;
function XDisplayMotionBufferSize(_para1 : PDisplay) : cardinal; cdecl;external ;
function XVisualIDFromVisual(_para1 : PVisual) : TVisualID; cdecl;external ;
function XInitThreads : TStatus; cdecl;external ;
procedure XLockDisplay(_para1 : PDisplay); cdecl;external ;
procedure XUnlockDisplay(_para1 : PDisplay); cdecl;external ;
function XInitExtension(_para1 : PDisplay;_para2 : pchar) : PXExtCodes; cdecl;external ;
function XAddExtension(_para1 : PDisplay) : PXExtCodes; cdecl;external ;
function XFindOnExtensionList(_para1 : PPXExtData;_para2 : longint) : PXExtData; cdecl;external ;
function XEHeadOfExtensionList(_para1 : TXEDataObject) : PPXExtData; cdecl;external ;
function XRootWindow(_para1 : PDisplay;_para2 : longint) : TWindow; cdecl;external ;
function XDefaultRootWindow(_para1 : PDisplay) : TWindow; cdecl;external ;
function XRootWindowOfScreen(_para1 : PScreen) : TWindow; cdecl;external ;
function XDefaultVisual(_para1 : PDisplay;_para2 : longint) : PVisual; cdecl;external ;
function XDefaultVisualOfScreen(_para1 : PScreen) : PVisual; cdecl;external ;
function XDefaultGC(_para1 : PDisplay;_para2 : longint) : TGC; cdecl;external ;
function XDefaultGCOfScreen(_para1 : PScreen) : TGC; cdecl;external ;
function XBlackPixel(_para1 : PDisplay;_para2 : longint) : cardinal; cdecl;external ;
function XWhitePixel(_para1 : PDisplay;_para2 : longint) : cardinal; cdecl;external ;
function XAllPlanes : cardinal; cdecl;external ;
function XBlackPixelOfScreen(_para1 : PScreen) : cardinal; cdecl;external ;
function XWhitePixelOfScreen(_para1 : PScreen) : cardinal; cdecl;external ;
function XNextRequest(_para1 : PDisplay) : cardinal; cdecl;external ;
function XLastKnownRequestProcessed(_para1 : PDisplay) : cardinal; cdecl;external ;
function XServerVendor(_para1 : PDisplay) : pchar; cdecl;external ;
function XDisplayString(_para1 : PDisplay) : pchar; cdecl;external ;
function XDefaultColormap(_para1 : PDisplay;_para2 : longint) : TColormap; cdecl;external ;
function XDefaultColormapOfScreen(_para1 : PScreen) : TColormap; cdecl;external ;
function XDisplayOfScreen(_para1 : PScreen) : PDisplay; cdecl;external ;
function XScreenOfDisplay(_para1 : PDisplay;_para2 : longint) : PScreen; cdecl;external ;
function XDefaultScreenOfDisplay(_para1 : PDisplay) : PScreen; cdecl;external ;
function XEventMaskOfScreen(_para1 : PScreen) : longint; cdecl;external ;
function XScreenNumberOfScreen(_para1 : PScreen) : longint; cdecl;external ;
function XSetErrorHandler(_para1 : TXErrorHandler) : TXErrorHandler; cdecl;external ;
function XSetIOErrorHandler(_para1 : TXIOErrorHandler) : TXIOErrorHandler; cdecl;external ;
function XListPixmapFormats(_para1 : PDisplay;_para2 : Plongint) : PXPixmapFormatValues; cdecl;external ;
function XListDepths(_para1 : PDisplay;_para2 : longint;_para3 : Plongint) : Plongint; cdecl;external ;
function XReconfigureWMWindow(_para1 : PDisplay;_para2 : TWindow;_para3 : longint;_para4 : cardinal;_para5 : PXWindowChanges) : TStatus; cdecl;external ;
function XGetWMProtocols(_para1 : PDisplay;_para2 : TWindow;_para3 : PPAtom;_para4 : Plongint) : TStatus; cdecl;external ;
function XSetWMProtocols(_para1 : PDisplay;_para2 : TWindow;_para3 : PAtom;_para4 : longint) : TStatus; cdecl;external ;
function XIconifyWindow(_para1 : PDisplay;_para2 : TWindow;_para3 : longint) : TStatus; cdecl;external ;
function XWithdrawWindow(_para1 : PDisplay;_para2 : TWindow;_para3 : longint) : TStatus; cdecl;external ;
function XGetCommand(_para1 : PDisplay;_para2 : TWindow;_para3 : PPPChar;_para4 : Plongint) : TStatus; cdecl;external ;
function XGetWMColormapWindows(_para1 : PDisplay;_para2 : TWindow;_para3 : PPWindow;_para4 : Plongint) : TStatus; cdecl;external ;
function XSetWMColormapWindows(_para1 : PDisplay;_para2 : TWindow;_para3 : PWindow;_para4 : longint) : TStatus; cdecl;external ;
procedure XFreeStringList(_para1 : PPChar); cdecl;external ;
function XSetTransientForHint(_para1 : PDisplay;_para2 : TWindow;_para3 : TWindow) : longint; cdecl;external ;
function XActivateScreenSaver(_para1 : PDisplay) : longint; cdecl;external ;
function XAddHost(_para1 : PDisplay;_para2 : PXHostAddress) : longint; cdecl;external ;
function XAddHosts(_para1 : PDisplay;_para2 : PXHostAddress;_para3 : longint) : longint; cdecl;external ;
{
function XAddToExtensionList(_para1 : ^^*;_para2 : PXExtData) : longint; cdecl;external ;
}
function XAddToSaveSet(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;external ;
function XAllocColor(_para1 : PDisplay;_para2 : TColormap;_para3 : PXColor) : TStatus; cdecl;external ;
function XAllocColorCells(_para1 : PDisplay;_para2 : TColormap;_para3 : TBool;_para4 : PCardinal;_para5 : cardinal;_para6 : PCardinal;_para7 : cardinal) : TStatus; cdecl;external ;
function XAllocColorPlanes(_para1 : PDisplay;_para2 : TColormap;_para3 : TBool;_para4 : PCardinal;_para5 : longint;_para6 : longint;_para7 : longint;_para8 : longint;_para9 : PCardinal;_para10 : PCardinal;_para11 : PCardinal) : TStatus; cdecl;external ;
function XAllocNamedColor(_para1 : PDisplay;_para2 : TColormap;_para3 : pchar;_para4 : PXColor;_para5 : PXColor) : TStatus; cdecl;external ;
function XAllowEvents(_para1 : PDisplay;_para2 : longint;_para3 : TTime) : longint; cdecl;external ;
function XAutoRepeatOff(_para1 : PDisplay) : longint; cdecl;external ;
function XAutoRepeatOn(_para1 : PDisplay) : longint; cdecl;external ;
function XBell(_para1 : PDisplay;_para2 : longint) : longint; cdecl;external ;
function XBitmapBitOrder(_para1 : PDisplay) : longint; cdecl;external ;
function XBitmapPad(_para1 : PDisplay) : longint; cdecl;external ;
function XBitmapUnit(_para1 : PDisplay) : longint; cdecl;external ;
function XCellsOfScreen(_para1 : PScreen) : longint; cdecl;external ;
function XChangeActivePointerGrab(_para1 : PDisplay;_para2 : cardinal;_para3 : TCursor;_para4 : TTime) : longint; cdecl;external ;
function XChangeGC(_para1 : PDisplay;_para2 : TGC;_para3 : cardinal;_para4 : PXGCValues) : longint; cdecl;external ;
function XChangeKeyboardControl(_para1 : PDisplay;_para2 : cardinal;_para3 : PXKeyboardControl) : longint; cdecl;external ;
function XChangeKeyboardMapping(_para1 : PDisplay;_para2 : longint;_para3 : longint;_para4 : PKeySym;_para5 : longint) : longint; cdecl;external ;
function XChangePointerControl(_para1 : PDisplay;_para2 : TBool;_para3 : TBool;_para4 : longint;_para5 : longint;_para6 : longint) : longint; cdecl;external ;
function XChangeProperty(_para1 : PDisplay;_para2 : TWindow;_para3 : TAtom;_para4 : TAtom;_para5 : longint;_para6 : longint;_para7 : pchar;_para8 : longint) : longint; cdecl;external ;
function XChangeSaveSet(_para1 : PDisplay;_para2 : TWindow;_para3 : longint) : longint; cdecl;external ;
function XChangeWindowAttributes(_para1 : PDisplay;_para2 : TWindow;_para3 : cardinal;_para4 : PXSetWindowAttributes) : longint; cdecl;external ;
{
function XCheckIfEvent(_para1 : PDisplay;_para2 : PXEvent;_para3 : function(_para1 : PDisplay;_para2 : PXEvent;_para3 : XPointer) : TBool;_para4 : XPointer) : TBool; cdecl;external ;
}
function XCheckMaskEvent(_para1 : PDisplay;_para2 : longint;_para3 : PXEvent) : TBool; cdecl;external ;
function XCheckTypedEvent(_para1 : PDisplay;_para2 : longint;_para3 : PXEvent) : TBool; cdecl;external ;
function XCheckTypedWindowEvent(_para1 : PDisplay;_para2 : TWindow;_para3 : longint;_para4 : PXEvent) : TBool; cdecl;external ;
function XCheckWindowEvent(_para1 : PDisplay;_para2 : TWindow;_para3 : longint;_para4 : PXEvent) : TBool; cdecl;external ;
function XCirculateSubwindows(_para1 : PDisplay;_para2 : TWindow;_para3 : longint) : longint; cdecl;external ;
function XCirculateSubwindowsDown(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;external ;
function XCirculateSubwindowsUp(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;external ;
function XClearArea(_para1 : PDisplay;_para2 : TWindow;_para3 : longint;_para4 : longint;_para5 : cardinal;_para6 : cardinal;_para7 : TBool) : longint; cdecl;external ;
function XClearWindow(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;external ;
function XCloseDisplay(_para1 : PDisplay) : longint; cdecl;external ;
function XConfigureWindow(_para1 : PDisplay;_para2 : TWindow;_para3 : cardinal;_para4 : PXWindowChanges) : longint; cdecl;external ;
function XConnectionNumber(_para1 : PDisplay) : longint; cdecl;external ;
function XConvertSelection(_para1 : PDisplay;_para2 : TAtom;_para3 : TAtom;_para4 : TAtom;_para5 : TWindow;_para6 : TTime) : longint; cdecl;external ;
function XCopyArea(_para1 : PDisplay;_para2 : TDrawable;_para3 : TDrawable;_para4 : TGC;_para5 : longint;_para6 : longint;_para7 : cardinal;_para8 : cardinal;_para9 : longint;_para10 : longint) : longint; cdecl;external ;
function XCopyGC(_para1 : PDisplay;_para2 : TGC;_para3 : cardinal;_para4 : TGC) : longint; cdecl;external ;
function XCopyPlane(_para1 : PDisplay;_para2 : TDrawable;_para3 : TDrawable;_para4 : TGC;_para5 : longint;_para6 : longint;_para7 : cardinal;_para8 : cardinal;_para9 : longint;_para10 : longint;_para11 : cardinal) : longint; cdecl;external ;
function XDefaultDepth(_para1 : PDisplay;_para2 : longint) : longint; cdecl;external ;
function XDefaultDepthOfScreen(_para1 : PScreen) : longint; cdecl;external ;
function XDefaultScreen(_para1 : PDisplay) : longint; cdecl;external ;
function XDefineCursor(_para1 : PDisplay;_para2 : TWindow;_para3 : TCursor) : longint; cdecl;external ;
function XDeleteProperty(_para1 : PDisplay;_para2 : TWindow;_para3 : TAtom) : longint; cdecl;external ;
function XDestroyWindow(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;external ;
function XDestroySubwindows(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;external ;
function XDoesBackingStore(_para1 : PScreen) : longint; cdecl;external ;
function XDoesSaveUnders(_para1 : PScreen) : TBool; cdecl;external ;
function XDisableAccessControl(_para1 : PDisplay) : longint; cdecl;external ;
function XDisplayCells(_para1 : PDisplay;_para2 : longint) : longint; cdecl;external ;
function XDisplayHeight(_para1 : PDisplay;_para2 : longint) : longint; cdecl;external ;
function XDisplayHeightMM(_para1 : PDisplay;_para2 : longint) : longint; cdecl;external ;
function XDisplayKeycodes(_para1 : PDisplay;_para2 : Plongint;_para3 : Plongint) : longint; cdecl;external ;
function XDisplayPlanes(_para1 : PDisplay;_para2 : longint) : longint; cdecl;external ;
function XDisplayWidth(_para1 : PDisplay;_para2 : longint) : longint; cdecl;external ;
function XDisplayWidthMM(_para1 : PDisplay;_para2 : longint) : longint; cdecl;external ;
function XDrawArc(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : cardinal;_para7 : cardinal;_para8 : longint;_para9 : longint) : longint; cdecl;external ;
function XDrawArcs(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : PXArc;_para5 : longint) : longint; cdecl;external ;
function XDrawImageString(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : pchar;_para7 : longint) : longint; cdecl;external ;
function XDrawImageString16(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : PXChar2b;_para7 : longint) : longint; cdecl;external ;
function XDrawLine(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : longint;_para7 : longint) : longint; cdecl;external ;
function XDrawLines(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : PXPoint;_para5 : longint;_para6 : longint) : longint; cdecl;external ;
function XDrawPoint(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint) : longint; cdecl;external ;
function XDrawPoints(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : PXPoint;_para5 : longint;_para6 : longint) : longint; cdecl;external ;
function XDrawRectangle(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : cardinal;_para7 : cardinal) : longint; cdecl;external ;
function XDrawRectangles(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : PXRectangle;_para5 : longint) : longint; cdecl;external ;
function XDrawSegments(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : PXSegment;_para5 : longint) : longint; cdecl;external ;
function XDrawString(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : pchar;_para7 : longint) : longint; cdecl;external ;
function XDrawString16(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : PXChar2b;_para7 : longint) : longint; cdecl;external ;
function XDrawText(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : PXTextItem;_para7 : longint) : longint; cdecl;external ;
function XDrawText16(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : PXTextItem16;_para7 : longint) : longint; cdecl;external ;
function XEnableAccessControl(_para1 : PDisplay) : longint; cdecl;external ;
function XEventsQueued(_para1 : PDisplay;_para2 : longint) : longint; cdecl;external ;
function XFetchName(_para1 : PDisplay;_para2 : TWindow;_para3 : PPChar) : TStatus; cdecl;external ;
function XFillArc(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : cardinal;_para7 : cardinal;_para8 : longint;_para9 : longint) : longint; cdecl;external ;
function XFillArcs(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : PXArc;_para5 : longint) : longint; cdecl;external ;
function XFillPolygon(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : PXPoint;_para5 : longint;_para6 : longint;_para7 : longint) : longint; cdecl;external ;
function XFillRectangle(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : cardinal;_para7 : cardinal) : longint; cdecl;external ;
function XFillRectangles(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : PXRectangle;_para5 : longint) : longint; cdecl;external ;
function XFlush(_para1 : PDisplay) : longint; cdecl;external ;
function XForceScreenSaver(_para1 : PDisplay;_para2 : longint) : longint; cdecl;external ;
function XFree(_para1 : pointer) : longint; cdecl;external ;
function XFreeColormap(_para1 : PDisplay;_para2 : TColormap) : longint; cdecl;external ;
function XFreeColors(_para1 : PDisplay;_para2 : TColormap;_para3 : PCardinal;_para4 : longint;_para5 : cardinal) : longint; cdecl;external ;
function XFreeCursor(_para1 : PDisplay;_para2 : TCursor) : longint; cdecl;external ;
function XFreeExtensionList(_para1 : PPChar) : longint; cdecl;external ;
function XFreeFont(_para1 : PDisplay;_para2 : PXFontStruct) : longint; cdecl;external ;
function XFreeFontInfo(_para1 : PPChar;_para2 : PXFontStruct;_para3 : longint) : longint; cdecl;external ;
function XFreeFontNames(_para1 : PPChar) : longint; cdecl;external ;
function XFreeFontPath(_para1 : PPChar) : longint; cdecl;external ;
function XFreeGC(_para1 : PDisplay;_para2 : TGC) : longint; cdecl;external ;
function XFreeModifiermap(_para1 : PXModifierKeymap) : longint; cdecl;external ;
function XFreePixmap(_para1 : PDisplay;_para2 : TPixmap) : longint; cdecl;external ;
function XGeometry(_para1 : PDisplay;_para2 : longint;_para3 : pchar;_para4 : pchar;_para5 : cardinal;_para6 : cardinal;_para7 : cardinal;_para8 : longint;_para9 : longint;_para10 : Plongint;_para11 : Plongint;_para12 : Plongint;_para13 : Plongint) : longint; cdecl;external ;
function XGetErrorDatabaseText(_para1 : PDisplay;_para2 : pchar;_para3 : pchar;_para4 : pchar;_para5 : pchar;_para6 : longint) : longint; cdecl;external ;
function XGetErrorText(_para1 : PDisplay;_para2 : longint;_para3 : pchar;_para4 : longint) : longint; cdecl;external ;
function XGetFontProperty(_para1 : PXFontStruct;_para2 : TAtom;_para3 : PCardinal) : TBool; cdecl;external ;
function XGetGCValues(_para1 : PDisplay;_para2 : TGC;_para3 : cardinal;_para4 : PXGCValues) : TStatus; cdecl;external ;
function XGetGeometry(_para1 : PDisplay;_para2 : TDrawable;_para3 : PWindow;_para4 : Plongint;_para5 : Plongint;_para6 : PCardinal;_para7 : PCardinal;_para8 : PCardinal;_para9 : PCardinal) : TStatus; cdecl;external ;
function XGetIconName(_para1 : PDisplay;_para2 : TWindow;_para3 : PPChar) : TStatus; cdecl;external ;
function XGetInputFocus(_para1 : PDisplay;_para2 : PWindow;_para3 : Plongint) : longint; cdecl;external ;
function XGetKeyboardControl(_para1 : PDisplay;_para2 : PXKeyboardState) : longint; cdecl;external ;
function XGetPointerControl(_para1 : PDisplay;_para2 : Plongint;_para3 : Plongint;_para4 : Plongint) : longint; cdecl;external ;
function XGetPointerMapping(_para1 : PDisplay;_para2 : pchar;_para3 : longint) : longint; cdecl;external ;
function XGetScreenSaver(_para1 : PDisplay;_para2 : Plongint;_para3 : Plongint;_para4 : Plongint;_para5 : Plongint) : longint; cdecl;external ;
function XGetTransientForHint(_para1 : PDisplay;_para2 : TWindow;_para3 : PWindow) : TStatus; cdecl;external ;
function XGetWindowProperty(_para1 : PDisplay;_para2 : TWindow;_para3 : TAtom;_para4 : longint;_para5 : longint;_para6 : TBool;_para7 : TAtom;_para8 : PAtom;_para9 : Plongint;_para10 : PCardinal;_para11 : PCardinal;_para12 : PPChar) : longint; cdecl;external ;
function XGetWindowAttributes(_para1 : PDisplay;_para2 : TWindow;_para3 : PXWindowAttributes) : TStatus; cdecl;external ;
function XGrabButton(_para1 : PDisplay;_para2 : cardinal;_para3 : cardinal;_para4 : TWindow;_para5 : TBool;_para6 : cardinal;_para7 : longint;_para8 : longint;_para9 : TWindow;_para10 : TCursor) : longint; cdecl;external ;
function XGrabKey(_para1 : PDisplay;_para2 : longint;_para3 : cardinal;_para4 : TWindow;_para5 : TBool;_para6 : longint;_para7 : longint) : longint; cdecl;external ;
function XGrabKeyboard(_para1 : PDisplay;_para2 : TWindow;_para3 : TBool;_para4 : longint;_para5 : longint;_para6 : TTime) : longint; cdecl;external ;
function XGrabPointer(_para1 : PDisplay;_para2 : TWindow;_para3 : TBool;_para4 : cardinal;_para5 : longint;_para6 : longint;_para7 : TWindow;_para8 : TCursor;_para9 : TTime) : longint; cdecl;external ;
function XGrabServer(_para1 : PDisplay) : longint; cdecl;external ;
function XHeightMMOfScreen(_para1 : PScreen) : longint; cdecl;external ;
function XHeightOfScreen(_para1 : PScreen) : longint; cdecl;external ;
{
function XIfEvent(_para1 : PDisplay;_para2 : PXEvent;_para3 : function(_para1 : PDisplay;_para2 : PXEvent;_para3 : XPointer) : TBool;_para4 : XPointer) : longint; cdecl;external ;
}
function XImageByteOrder(_para1 : PDisplay) : longint; cdecl;external ;
function XInstallColormap(_para1 : PDisplay;_para2 : TColormap) : longint; cdecl;external ;
function XKeysymToKeycode(_para1 : PDisplay;_para2 : TKeySym) : TKeyCode; cdecl;external ;
function XKillClient(_para1 : PDisplay;_para2 : TXID) : longint; cdecl;external ;
{
function XLastKnownRequestProcessed(_para1 : PDisplay) : cardinal; cdecl;external ;
}
function XLookupColor(_para1 : PDisplay;_para2 : TColormap;_para3 : pchar;_para4 : PXColor;_para5 : PXColor) : TStatus; cdecl;external ;
function XLowerWindow(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;external ;
function XMapRaised(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;external ;
function XMapSubwindows(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;external ;
function XMapWindow(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;external ;
function XMaskEvent(_para1 : PDisplay;_para2 : longint;_para3 : PXEvent) : longint; cdecl;external ;
function XMaxCmapsOfScreen(_para1 : PScreen) : longint; cdecl;external ;
function XMinCmapsOfScreen(_para1 : PScreen) : longint; cdecl;external ;
function XMoveResizeWindow(_para1 : PDisplay;_para2 : TWindow;_para3 : longint;_para4 : longint;_para5 : cardinal;_para6 : cardinal) : longint; cdecl;external ;
function XMoveWindow(_para1 : PDisplay;_para2 : TWindow;_para3 : longint;_para4 : longint) : longint; cdecl;external ;
function XNextEvent(_para1 : PDisplay;_para2 : PXEvent) : longint; cdecl;external ;
function XNoOp(_para1 : PDisplay) : longint; cdecl;external ;
function XParseColor(_para1 : PDisplay;_para2 : TColormap;_para3 : pchar;_para4 : PXColor) : TStatus; cdecl;external ;
function XParseGeometry(_para1 : pchar;_para2 : Plongint;_para3 : Plongint;_para4 : PCardinal;_para5 : PCardinal) : longint; cdecl;external ;
function XPeekEvent(_para1 : PDisplay;_para2 : PXEvent) : longint; cdecl;external ;
{
function XPeekIfEvent(_para1 : PDisplay;_para2 : PXEvent;_para3 : function(_para1 : PDisplay;_para2 : PXEvent;_para3 : XPointer) : TBool;_para4 : XPointer) : longint; cdecl;external ;
}
function XPending(_para1 : PDisplay) : longint; cdecl;external ;
function XPlanesOfScreen(_para1 : PScreen) : longint; cdecl;external ;
function XProtocolRevision(_para1 : PDisplay) : longint; cdecl;external ;
function XProtocolVersion(_para1 : PDisplay) : longint; cdecl;external ;
function XPutBackEvent(_para1 : PDisplay;_para2 : PXEvent) : longint; cdecl;external ;
function XPutImage(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : PXImage;_para5 : longint;_para6 : longint;_para7 : longint;_para8 : longint;_para9 : cardinal;_para10 : cardinal) : longint; cdecl;external ;
function XQLength(_para1 : PDisplay) : longint; cdecl;external ;
function XQueryBestCursor(_para1 : PDisplay;_para2 : TDrawable;_para3 : cardinal;_para4 : cardinal;_para5 : PCardinal;_para6 : PCardinal) : TStatus; cdecl;external ;
function XQueryBestSize(_para1 : PDisplay;_para2 : longint;_para3 : TDrawable;_para4 : cardinal;_para5 : cardinal;_para6 : PCardinal;_para7 : PCardinal) : TStatus; cdecl;external ;
function XQueryBestStipple(_para1 : PDisplay;_para2 : TDrawable;_para3 : cardinal;_para4 : cardinal;_para5 : PCardinal;_para6 : PCardinal) : TStatus; cdecl;external ;
function XQueryBestTile(_para1 : PDisplay;_para2 : TDrawable;_para3 : cardinal;_para4 : cardinal;_para5 : PCardinal;_para6 : PCardinal) : TStatus; cdecl;external ;
function XQueryColor(_para1 : PDisplay;_para2 : TColormap;_para3 : PXColor) : longint; cdecl;external ;
function XQueryColors(_para1 : PDisplay;_para2 : TColormap;_para3 : PXColor;_para4 : longint) : longint; cdecl;external ;
function XQueryExtension(_para1 : PDisplay;_para2 : pchar;_para3 : Plongint;_para4 : Plongint;_para5 : Plongint) : TBool; cdecl;external ;
{
function XQueryKeymap(_para1 : PDisplay;_para2 : arraycdecl;external ;
}
function XQueryPointer(_para1 : PDisplay;_para2 : TWindow;_para3 : PWindow;_para4 : PWindow;_para5 : Plongint;_para6 : Plongint;_para7 : Plongint;_para8 : Plongint;_para9 : PCardinal) : TBool; cdecl;external ;
function XQueryTextExtents(_para1 : PDisplay;_para2 : TXID;_para3 : pchar;_para4 : longint;_para5 : Plongint;_para6 : Plongint;_para7 : Plongint;_para8 : PXCharStruct) : longint; cdecl;external ;
function XQueryTextExtents16(_para1 : PDisplay;_para2 : TXID;_para3 : PXChar2b;_para4 : longint;_para5 : Plongint;_para6 : Plongint;_para7 : Plongint;_para8 : PXCharStruct) : longint; cdecl;external ;
function XQueryTree(_para1 : PDisplay;_para2 : TWindow;_para3 : PWindow;_para4 : PWindow;_para5 : PPWindow;_para6 : PCardinal) : TStatus; cdecl;external ;
function XRaiseWindow(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;external ;
function XReadBitmapFile(_para1 : PDisplay;_para2 : TDrawable;_para3 : pchar;_para4 : PCardinal;_para5 : PCardinal;_para6 : PPixmap;_para7 : Plongint;_para8 : Plongint) : longint; cdecl;external ;
function XReadBitmapFileData(_para1 : pchar;_para2 : PCardinal;_para3 : PCardinal;_para4 : PPChar;_para5 : Plongint;_para6 : Plongint) : longint; cdecl;external ;
function XRebindKeysym(_para1 : PDisplay;_para2 : TKeySym;_para3 : PKeySym;_para4 : longint;_para5 : pchar;_para6 : longint) : longint; cdecl;external ;
function XRecolorCursor(_para1 : PDisplay;_para2 : TCursor;_para3 : PXColor;_para4 : PXColor) : longint; cdecl;external ;
function XRefreshKeyboardMapping(_para1 : PXMappingEvent) : longint; cdecl;external ;
function XRemoveFromSaveSet(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;external ;
function XRemoveHost(_para1 : PDisplay;_para2 : PXHostAddress) : longint; cdecl;external ;
function XRemoveHosts(_para1 : PDisplay;_para2 : PXHostAddress;_para3 : longint) : longint; cdecl;external ;
function XReparentWindow(_para1 : PDisplay;_para2 : TWindow;_para3 : TWindow;_para4 : longint;_para5 : longint) : longint; cdecl;external ;
function XResetScreenSaver(_para1 : PDisplay) : longint; cdecl;external ;
function XResizeWindow(_para1 : PDisplay;_para2 : TWindow;_para3 : cardinal;_para4 : cardinal) : longint; cdecl;external ;
function XRestackWindows(_para1 : PDisplay;_para2 : PWindow;_para3 : longint) : longint; cdecl;external ;
function XRotateBuffers(_para1 : PDisplay;_para2 : longint) : longint; cdecl;external ;
function XRotateWindowProperties(_para1 : PDisplay;_para2 : TWindow;_para3 : PAtom;_para4 : longint;_para5 : longint) : longint; cdecl;external ;
function XScreenCount(_para1 : PDisplay) : longint; cdecl;external ;
function XSelectInput(_para1 : PDisplay;_para2 : TWindow;_para3 : longint) : longint; cdecl;external ;
function XSendEvent(_para1 : PDisplay;_para2 : TWindow;_para3 : TBool;_para4 : longint;_para5 : PXEvent) : TStatus; cdecl;external ;
function XSetAccessControl(_para1 : PDisplay;_para2 : longint) : longint; cdecl;external ;
function XSetArcMode(_para1 : PDisplay;_para2 : TGC;_para3 : longint) : longint; cdecl;external ;
function XSetBackground(_para1 : PDisplay;_para2 : TGC;_para3 : cardinal) : longint; cdecl;external ;
function XSetClipMask(_para1 : PDisplay;_para2 : TGC;_para3 : TPixmap) : longint; cdecl;external ;
function XSetClipOrigin(_para1 : PDisplay;_para2 : TGC;_para3 : longint;_para4 : longint) : longint; cdecl;external ;
function XSetClipRectangles(_para1 : PDisplay;_para2 : TGC;_para3 : longint;_para4 : longint;_para5 : PXRectangle;_para6 : longint;_para7 : longint) : longint; cdecl;external ;
function XSetCloseDownMode(_para1 : PDisplay;_para2 : longint) : longint; cdecl;external ;
function XSetCommand(_para1 : PDisplay;_para2 : TWindow;_para3 : PPChar;_para4 : longint) : longint; cdecl;external ;
function XSetDashes(_para1 : PDisplay;_para2 : TGC;_para3 : longint;_para4 : pchar;_para5 : longint) : longint; cdecl;external ;
function XSetFillRule(_para1 : PDisplay;_para2 : TGC;_para3 : longint) : longint; cdecl;external ;
function XSetFillStyle(_para1 : PDisplay;_para2 : TGC;_para3 : longint) : longint; cdecl;external ;
function XSetFont(_para1 : PDisplay;_para2 : TGC;_para3 : TFont) : longint; cdecl;external ;
function XSetFontPath(_para1 : PDisplay;_para2 : PPChar;_para3 : longint) : longint; cdecl;external ;
function XSetForeground(_para1 : PDisplay;_para2 : TGC;_para3 : cardinal) : longint; cdecl;external ;
function XSetFunction(_para1 : PDisplay;_para2 : TGC;_para3 : longint) : longint; cdecl;external ;
function XSetGraphicsExposures(_para1 : PDisplay;_para2 : TGC;_para3 : TBool) : longint; cdecl;external ;
function XSetIconName(_para1 : PDisplay;_para2 : TWindow;_para3 : pchar) : longint; cdecl;external ;
function XSetInputFocus(_para1 : PDisplay;_para2 : TWindow;_para3 : longint;_para4 : TTime) : longint; cdecl;external ;
function XSetLineAttributes(_para1 : PDisplay;_para2 : TGC;_para3 : cardinal;_para4 : longint;_para5 : longint;_para6 : longint) : longint; cdecl;external ;
function XSetModifierMapping(_para1 : PDisplay;_para2 : PXModifierKeymap) : longint; cdecl;external ;
function XSetPlaneMask(_para1 : PDisplay;_para2 : TGC;_para3 : cardinal) : longint; cdecl;external ;
function XSetPointerMapping(_para1 : PDisplay;_para2 : pchar;_para3 : longint) : longint; cdecl;external ;
function XSetScreenSaver(_para1 : PDisplay;_para2 : longint;_para3 : longint;_para4 : longint;_para5 : longint) : longint; cdecl;external ;
function XSetSelectionOwner(_para1 : PDisplay;_para2 : TAtom;_para3 : TWindow;_para4 : TTime) : longint; cdecl;external ;
function XSetState(_para1 : PDisplay;_para2 : TGC;_para3 : cardinal;_para4 : cardinal;_para5 : longint;_para6 : cardinal) : longint; cdecl;external ;
function XSetStipple(_para1 : PDisplay;_para2 : TGC;_para3 : TPixmap) : longint; cdecl;external ;
function XSetSubwindowMode(_para1 : PDisplay;_para2 : TGC;_para3 : longint) : longint; cdecl;external ;
function XSetTSOrigin(_para1 : PDisplay;_para2 : TGC;_para3 : longint;_para4 : longint) : longint; cdecl;external ;
function XSetTile(_para1 : PDisplay;_para2 : TGC;_para3 : TPixmap) : longint; cdecl;external ;
function XSetWindowBackground(_para1 : PDisplay;_para2 : TWindow;_para3 : cardinal) : longint; cdecl;external ;
function XSetWindowBackgroundPixmap(_para1 : PDisplay;_para2 : TWindow;_para3 : TPixmap) : longint; cdecl;external ;
function XSetWindowBorder(_para1 : PDisplay;_para2 : TWindow;_para3 : cardinal) : longint; cdecl;external ;
function XSetWindowBorderPixmap(_para1 : PDisplay;_para2 : TWindow;_para3 : TPixmap) : longint; cdecl;external ;
function XSetWindowBorderWidth(_para1 : PDisplay;_para2 : TWindow;_para3 : cardinal) : longint; cdecl;external ;
function XSetWindowColormap(_para1 : PDisplay;_para2 : TWindow;_para3 : TColormap) : longint; cdecl;external ;
function XStoreBuffer(_para1 : PDisplay;_para2 : pchar;_para3 : longint;_para4 : longint) : longint; cdecl;external ;
function XStoreBytes(_para1 : PDisplay;_para2 : pchar;_para3 : longint) : longint; cdecl;external ;
function XStoreColor(_para1 : PDisplay;_para2 : TColormap;_para3 : PXColor) : longint; cdecl;external ;
function XStoreColors(_para1 : PDisplay;_para2 : TColormap;_para3 : PXColor;_para4 : longint) : longint; cdecl;external ;
function XStoreName(_para1 : PDisplay;_para2 : TWindow;_para3 : pchar) : longint; cdecl;external ;
function XStoreNamedColor(_para1 : PDisplay;_para2 : TColormap;_para3 : pchar;_para4 : cardinal;_para5 : longint) : longint; cdecl;external ;
function XSync(_para1 : PDisplay;_para2 : TBool) : longint; cdecl;external ;
function XTextExtents(_para1 : PXFontStruct;_para2 : pchar;_para3 : longint;_para4 : Plongint;_para5 : Plongint;_para6 : Plongint;_para7 : PXCharStruct) : longint; cdecl;external ;
function XTextExtents16(_para1 : PXFontStruct;_para2 : PXChar2b;_para3 : longint;_para4 : Plongint;_para5 : Plongint;_para6 : Plongint;_para7 : PXCharStruct) : longint; cdecl;external ;
function XTextWidth(_para1 : PXFontStruct;_para2 : pchar;_para3 : longint) : longint; cdecl;external ;
function XTextWidth16(_para1 : PXFontStruct;_para2 : PXChar2b;_para3 : longint) : longint; cdecl;external ;
function XTranslateCoordinates(_para1 : PDisplay;_para2 : TWindow;_para3 : TWindow;_para4 : longint;_para5 : longint;_para6 : Plongint;_para7 : Plongint;_para8 : PWindow) : TBool; cdecl;external ;
function XUndefineCursor(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;external ;
function XUngrabButton(_para1 : PDisplay;_para2 : cardinal;_para3 : cardinal;_para4 : TWindow) : longint; cdecl;external ;
function XUngrabKey(_para1 : PDisplay;_para2 : longint;_para3 : cardinal;_para4 : TWindow) : longint; cdecl;external ;
function XUngrabKeyboard(_para1 : PDisplay;_para2 : TTime) : longint; cdecl;external ;
function XUngrabPointer(_para1 : PDisplay;_para2 : TTime) : longint; cdecl;external ;
function XUngrabServer(_para1 : PDisplay) : longint; cdecl;external ;
function XUninstallColormap(_para1 : PDisplay;_para2 : TColormap) : longint; cdecl;external ;
function XUnloadFont(_para1 : PDisplay;_para2 : TFont) : longint; cdecl;external ;
function XUnmapSubwindows(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;external ;
function XUnmapWindow(_para1 : PDisplay;_para2 : TWindow) : longint; cdecl;external ;
function XVendorRelease(_para1 : PDisplay) : longint; cdecl;external ;
function XWarpPointer(_para1 : PDisplay;_para2 : TWindow;_para3 : TWindow;_para4 : longint;_para5 : longint;_para6 : cardinal;_para7 : cardinal;_para8 : longint;_para9 : longint) : longint; cdecl;external ;
function XWidthMMOfScreen(_para1 : PScreen) : longint; cdecl;external ;
function XWidthOfScreen(_para1 : PScreen) : longint; cdecl;external ;
function XWindowEvent(_para1 : PDisplay;_para2 : TWindow;_para3 : longint;_para4 : PXEvent) : longint; cdecl;external ;
function XWriteBitmapFile(_para1 : PDisplay;_para2 : pchar;_para3 : TPixmap;_para4 : cardinal;_para5 : cardinal;_para6 : longint;_para7 : longint) : longint; cdecl;external ;
function XSupportsLocale : TBool; cdecl;external ;
function XSetLocaleModifiers(_para1 : pchar) : pchar; cdecl;external ;
{
function XOpenOM(_para1 : PDisplay;_para2 : ^*;_para3 : pchar;_para4 : pchar) : TXOM; cdecl;external ;
}
function XCloseOM(_para1 : TXOM) : TStatus; cdecl;external ;
function XDisplayOfOM(_para1 : TXOM) : PDisplay; cdecl;external ;
function XLocaleOfOM(_para1 : TXOM) : pchar; cdecl;external ;
procedure XDestroyOC(_para1 : TXOC); cdecl;external ;
function TXOMOfOC(_para1 : TXOC) : TXOM; cdecl;external ;
function XCreateFontSet(_para1 : PDisplay;_para2 : pchar;_para3 : PPPChar;_para4 : Plongint;_para5 : PPChar) : TXFontSet; cdecl;external ;
procedure XFreeFontSet(_para1 : PDisplay;_para2 : TXFontSet); cdecl;external ;
function XFontsOfFontSet(_para1 : TXFontSet;_para2 : PPPXFontStruct;_para3 : PPPChar) : longint; cdecl;external ;
function XBaseFontNameListOfFontSet(_para1 : TXFontSet) : pchar; cdecl;external ;
function XLocaleOfFontSet(_para1 : TXFontSet) : pchar; cdecl;external ;
function XContextDependentDrawing(_para1 : TXFontSet) : TBool; cdecl;external ;
function XDirectionalDependentDrawing(_para1 : TXFontSet) : TBool; cdecl;external ;
function XContextualDrawing(_para1 : TXFontSet) : TBool; cdecl;external ;
function XExtentsOfFontSet(_para1 : TXFontSet) : PXFontSetExtents; cdecl;external ;
function XmbTextEscapement(_para1 : TXFontSet;_para2 : pchar;_para3 : longint) : longint; cdecl;external ;
function XwcTextEscapement(_para1 : TXFontSet;_para2 : Pwchar_t;_para3 : longint) : longint; cdecl;external ;
function XmbTextExtents(_para1 : TXFontSet;_para2 : pchar;_para3 : longint;_para4 : PXRectangle;_para5 : PXRectangle) : longint; cdecl;external ;
function XwcTextExtents(_para1 : TXFontSet;_para2 : Pwchar_t;_para3 : longint;_para4 : PXRectangle;_para5 : PXRectangle) : longint; cdecl;external ;
function XmbTextPerCharExtents(_para1 : TXFontSet;_para2 : pchar;_para3 : longint;_para4 : PXRectangle;_para5 : PXRectangle;_para6 : longint;_para7 : Plongint;_para8 : PXRectangle;_para9 : PXRectangle) : TStatus; cdecl;external ;
function XwcTextPerCharExtents(_para1 : TXFontSet;_para2 : Pwchar_t;_para3 : longint;_para4 : PXRectangle;_para5 : PXRectangle;_para6 : longint;_para7 : Plongint;_para8 : PXRectangle;_para9 : PXRectangle) : TStatus; cdecl;external ;
procedure XmbDrawText(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : PXmbTextItem;_para7 : longint); cdecl;external ;
procedure XwcDrawText(_para1 : PDisplay;_para2 : TDrawable;_para3 : TGC;_para4 : longint;_para5 : longint;_para6 : PXwcTextItem;_para7 : longint); cdecl;external ;
procedure XmbDrawString(_para1 : PDisplay;_para2 : TDrawable;_para3 : TXFontSet;_para4 : TGC;_para5 : longint;_para6 : longint;_para7 : pchar;_para8 : longint); cdecl;external ;
procedure XwcDrawString(_para1 : PDisplay;_para2 : TDrawable;_para3 : TXFontSet;_para4 : TGC;_para5 : longint;_para6 : longint;_para7 : Pwchar_t;_para8 : longint); cdecl;external ;
procedure XmbDrawImageString(_para1 : PDisplay;_para2 : TDrawable;_para3 : TXFontSet;_para4 : TGC;_para5 : longint;_para6 : longint;_para7 : pchar;_para8 : longint); cdecl;external ;
procedure XwcDrawImageString(_para1 : PDisplay;_para2 : TDrawable;_para3 : TXFontSet;_para4 : TGC;_para5 : longint;_para6 : longint;_para7 : Pwchar_t;_para8 : longint); cdecl;external ;
{
function XOpenIM(_para1 : PDisplay;_para2 : P*;_para3 : pchar;_para4 : pchar) : TXIM; cdecl;external ;
}
function XCloseIM(_para1 : TXIM) : TStatus; cdecl;external ;
function XDisplayOfIM(_para1 : TXIM) : PDisplay; cdecl;external ;
function XLocaleOfIM(_para1 : TXIM) : pchar; cdecl;external ;
procedure XDestroyIC(_para1 : TXIC); cdecl;external ;
procedure XSetICFocus(_para1 : TXIC); cdecl;external ;
procedure XUnsetICFocus(_para1 : TXIC); cdecl;external ;
function XwcResetIC(_para1 : TXIC) : Pwchar_t; cdecl;external ;
function XmbResetIC(_para1 : TXIC) : pchar; cdecl;external ;
function TXIMOfIC(_para1 : TXIC) : TXIM; cdecl;external ;
function XFilterEvent(_para1 : PXEvent;_para2 : TWindow) : TBool; cdecl;external ;
function XmbLookupString(_para1 : TXIC;_para2 : PXKeyPressedEvent;_para3 : pchar;_para4 : longint;_para5 : PKeySym;_para6 : PStatus) : longint; cdecl;external ;
function XwcLookupString(_para1 : TXIC;_para2 : PXKeyPressedEvent;_para3 : Pwchar_t;_para4 : longint;_para5 : PKeySym;_para6 : PStatus) : longint; cdecl;external ;
{
function XRegisterIMInstantiateCallback(_para1 : PDisplay;_para2 : P*;_para3 : pchar;_para4 : pchar;_para5 : TXIMProc;_para6 : PXPointer) : TBool; cdecl;external ;
function XUnregisterIMInstantiateCallback(_para1 : PDisplay;_para2 : P*;_para3 : pchar;_para4 : pchar;_para5 : TXIMProc;_para6 : PXPointer) : TBool; cdecl;external ;
}
function XInternalConnectionNumbers(_para1 : PDisplay;_para2 : PPlongint;_para3 : Plongint) : TStatus; cdecl;external ;
procedure XProcessInternalConnection(_para1 : PDisplay;_para2 : longint); cdecl;external ;
function XAddConnectionWatch(_para1 : PDisplay;_para2 : TXConnectionWatchProc;_para3 : TXPointer) : TStatus; cdecl;external ;
procedure XRemoveConnectionWatch(_para1 : PDisplay;_para2 : TXConnectionWatchProc;_para3 : TXPointer); cdecl;external ;


begin
end.
