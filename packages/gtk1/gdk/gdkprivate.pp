{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PDisplay = pointer;
     PXImage = pointer;
     PVisual = pointer;
     TWindow = longint;
     TCursor = longint;
     TAtom = longint;
     TGC = longint;
     TColorMap = longint;
     TXStandardColorMap = array [0..9] of longint;
     TVisualID = longint;
     PXEvent = pointer;
     TRegion = pointer;
     TXID = longint;
     Time_T = longint;

     PGdkWindowPrivate = ^TGdkWindowPrivate;
     TGdkWindowPrivate = record
          window : TGdkWindow;
          parent : PGdkWindow;
          xwindow : TWindow;
          xdisplay : PDisplay;
          x : gint16;
          y : gint16;
          width : guint16;
          height : guint16;
          resize_count : guint8;
          window_type : guint8;
          ref_count : guint;
          flag0 : {$ifdef win32}longint{$else}word{$endif};
          extension_events : gint;
          filters : PGList;
          colormap : PGdkColormap;
          children : PGList;
       end;

  const
     bm_TGdkWindowPrivate_destroyed = $3;
     bp_TGdkWindowPrivate_destroyed = 0;
     bm_TGdkWindowPrivate_mapped = $4;
     bp_TGdkWindowPrivate_mapped = 2;
     bm_TGdkWindowPrivate_guffaw_gravity = $8;
     bp_TGdkWindowPrivate_guffaw_gravity = 3;
function  destroyed(var a : TGdkWindowPrivate) : guint;
procedure set_destroyed(var a : TGdkWindowPrivate; __destroyed : guint);
function  mapped(var a : TGdkWindowPrivate) : guint;
procedure set_mapped(var a : TGdkWindowPrivate; __mapped : guint);
function  guffaw_gravity(var a : TGdkWindowPrivate) : guint;
procedure set_guffaw_gravity(var a : TGdkWindowPrivate; __guffaw_gravity : guint);

  type
     PGdkImagePrivate = ^TGdkImagePrivate;
     TGdkImagePrivate = record
          image : TGdkImage;
          ximage : PXImage;
          xdisplay : PDisplay;
          x_shm_info : gpointer;
          image_put : procedure (window:PGdkDrawable; gc:PGdkGC; image:PGdkImage; xsrc:gint; ysrc:gint; xdest:gint; ydest:gint; width:gint; height:gint);cdecl;
       end;

     PGdkGCPrivate = ^TGdkGCPrivate;
     TGdkGCPrivate = record
          gc : TGdkGC;
          xgc : TGC;
          xdisplay : PDisplay;
          ref_count : guint;
       end;

     TGdkColorInfoFlags = (GDK_COLOR_WRITEABLE := 1 shl 0);

     PGdkColorInfo = ^TGdkColorInfo;
     TGdkColorInfo = record
          flags : TGdkColorInfoFlags;
          ref_count : guint;
       end;

     PGdkColormapPrivate = ^TGdkColormapPrivate;
     TGdkColormapPrivate = record
          colormap : TGdkColormap;
          xcolormap : TColormap;
          xdisplay : PDisplay;
          visual : PGdkVisual;
          private_val : gint;
          hash : PGHashTable;
          info : PGdkColorInfo;
          last_sync_time : time_t;
          ref_count : guint;
       end;

     PGdkVisualPrivate = ^TGdkVisualPrivate;
     TGdkVisualPrivate = record
          visual : TGdkVisual;
          xvisual : PVisual;
       end;

     PGdkFontPrivate = ^TGdkFontPrivate;
     TGdkFontPrivate = record
          font : TGdkFont;
          xfont : gpointer;
          xdisplay : PDisplay;
          ref_count : guint;
          names : PGSList;
       end;

     PGdkCursorPrivate = ^TGdkCursorPrivate;
     TGdkCursorPrivate = record
          cursor : TGdkCursor;
          xcursor : TCursor;
          xdisplay : PDisplay;
       end;

     PGdkDndCursorInfo = ^TGdkDndCursorInfo;
     TGdkDndCursorInfo = record
          gdk_cursor_dragdefault : TCursor;
          gdk_cursor_dragok : TCursor;
          drag_pm_default : PGdkWindow;
          drag_pm_ok : PGdkWindow;
          default_hotspot : TGdkPoint;
          ok_hotspot : TGdkPoint;
          xids : PGList;
       end;

     PGdkDndGlobals = ^TGdkDndGlobals;
     TGdkDndGlobals = record
          gdk_XdeEnter : TGdkAtom;
          gdk_XdeLeave : TGdkAtom;
          gdk_XdeRequest : TGdkAtom;
          gdk_XdeDataAvailable : TGdkAtom;
          gdk_XdeDataShow : TGdkAtom;
          gdk_XdeCancel : TGdkAtom;
          gdk_XdeTypelist : TGdkAtom;
          c : PGdkDndCursorInfo;
          drag_startwindows : PPGdkWindow;
          drag_numwindows : guint;
          drag_really : gboolean;
          drag_perhaps : gboolean;
          dnd_grabbed : gboolean;
          dnd_drag_target : TWindow;
          drag_dropcoords : TGdkPoint;
          dnd_drag_start : TGdkPoint;
          dnd_drag_oldpos : TGdkPoint;
          dnd_drag_dropzone : TGdkRectangle;
          real_sw : PGdkWindowPrivate;
          dnd_drag_curwin : TWindow;
          last_drop_time : Time_T;
       end;


     PGdkEventFilter = ^TGdkEventFilter;
     TGdkEventFilter = record
          thefunction : TGdkFilterFunc;
          data : gpointer;
       end;

     PGdkClientFilter = ^TGdkClientFilter;
     TGdkClientFilter = record
          thetype : TGdkAtom;
          thefunction : TGdkFilterFunc;
          data : gpointer;
       end;

{$ifdef USE_XIM}

     PGdkICPrivate = ^TGdkICPrivate;
     TGdkICPrivate = record
          xic : XIC;
          attr : PGdkICAttr;
          mask : TGdkICAttributesType;
       end;

{$endif}

     PGdkColorContextPrivate = ^TGdkColorContextPrivate;
     TGdkColorContextPrivate = record
          color_context : TGdkColorContext;
          xdisplay : PDisplay;
          std_cmap : TXStandardColormap;
       end;

     PGdkRegionPrivate = ^TGdkRegionPrivate;
     TGdkRegionPrivate = record
          region : TGdkRegion;
          xregion : TRegion;
       end;


     TGdkDebugFlag = (GDK_DEBUG_MISC := 1 shl 0,GDK_DEBUG_EVENTS := 1 shl 1,
       GDK_DEBUG_DND := 1 shl 2,GDK_DEBUG_COLOR_CONTEXT := 1 shl 3,
       GDK_DEBUG_XIM := 1 shl 4);

procedure gdk_xid_table_insert(xid:TXID; data:gpointer);cdecl;external gdkdll name 'gdk_xid_table_insert';
function  gdk_xid_table_lookup(xid:TXID):gpointer;cdecl;external gdkdll name 'gdk_xid_table_lookup';

function  GDK_window_lookup(xid : longint) : PGdkWindow;
function  GDK_pixmap_lookup(xid : longint) : PGdkPixmap;
function  GDK_font_lookup(xid : longint) : PGdkFont;

{$ifndef os2}
    var
       gdk_selection_property : TAtom;external gdkdll name 'gdk_selection_property';
       gdk_progclass : Pgchar;external gdkdll name 'gdk_progclass';
       gdk_error_code : gint;external gdkdll name 'gdk_error_code';
       gdk_null_window_warnings : gint;external gdkdll name 'gdk_null_window_warnings';
{$endif}

{$ifndef gtkwin}
procedure gdk_events_init;cdecl;external gdkdll name 'gdk_events_init';
procedure gdk_window_init;cdecl;external gdkdll name 'gdk_window_init';
procedure gdk_visual_init;cdecl;external gdkdll name 'gdk_visual_init';
procedure gdk_dnd_init;cdecl;external gdkdll name 'gdk_dnd_init';
procedure gdk_image_init;cdecl;external gdkdll name 'gdk_image_init';
procedure gdk_image_exit;cdecl;external gdkdll name 'gdk_image_exit';
function  gdk_colormap_lookup(xcolormap:TColormap):PGdkColormap;cdecl;external gdkdll name 'gdk_colormap_lookup';
function  gdk_visual_lookup(xvisual:pVisual):PGdkVisual;cdecl;external gdkdll name 'gdk_visual_lookup';
procedure gdk_window_add_colormap_windows(window:PGdkWindow);cdecl;external gdkdll name 'gdk_window_add_colormap_windows';
procedure gdk_window_destroy_notify(window:PGdkWindow);cdecl;external gdkdll name 'gdk_window_destroy_notify';
procedure gdk_xid_table_remove(xid:TXID);cdecl;external gdkdll name 'gdk_xid_table_remove';
function  gdk_send_xevent(window:TWindow; propagate:gboolean; event_mask:glong; event_send:pXEvent):gint;cdecl;external gdkdll name 'gdk_send_xevent';
{$ifndef gtkdarwin}
procedure gdk_dnd_display_drag_cursor(x:gint; y:gint; drag_ok:gboolean; change_made:gboolean);cdecl;external gdkdll name 'gdk_dnd_display_drag_cursor';
{$endif not gtkdarwin}
function  gdk_window_xid_at(base:TWindow; bx:gint; by:gint; x:gint; y:gint; excludes:PGList; excl_child:gboolean):TWindow;cdecl;external gdkdll name 'gdk_window_xid_at';
function  gdk_window_xid_at_coords(x:gint; y:gint; excludes:PGList; excl_child:gboolean):TWindow;cdecl;external gdkdll name 'gdk_window_xid_at_coords';

{$ifndef os2}
    var
       gdk_use_xshm : gint;external gdkdll name 'gdk_use_xshm';
       gdk_display_name : Pgchar;external gdkdll name 'gdk_display_name';
       gdk_display : PDisplay;external gdkdll name 'gdk_display';
       gdk_screen : gint;external gdkdll name 'gdk_screen';
       gdk_root_window : TWindow;external gdkdll name 'gdk_root_window';
       gdk_leader_window : TWindow;external gdkdll name 'gdk_leader_window';
       gdk_root_parent : TGdkWindowPrivate;external gdkdll name 'gdk_root_parent';
       gdk_wm_delete_window : TAtom;external gdkdll name 'gdk_wm_delete_window';
       gdk_wm_take_focus : TAtom;external gdkdll name 'gdk_wm_take_focus';
       gdk_wm_protocols : TAtom;external gdkdll name 'gdk_wm_protocols';
       gdk_dnd : TGdkDndGlobals;external gdkdll name 'gdk_dnd';
       gdk_error_warnings : gint;external gdkdll name 'gdk_error_warnings';
       gdk_default_filters : PGList;external gdkdll name 'gdk_default_filters';
       gdk_nevent_masks : longint;external gdkdll name 'gdk_nevent_masks';
       gdk_xgrab_window : PGdkWindowPrivate;external gdkdll name 'gdk_xgrab_window';
{$endif}

{$ifdef USE_XIM}
function  gdk_im_open:gint;cdecl;external gdkdll name 'gdk_im_open';
procedure gdk_im_close;cdecl;external gdkdll name 'gdk_im_close';
procedure gdk_ic_cleanup;cdecl;external gdkdll name 'gdk_ic_cleanup';

    var
       gdk_xim_ic : PGdkICPrivate;external gdkdll name 'gdk_xim_ic';
       gdk_xim_window : PGdkWindow;external gdkdll name 'gdk_xim_window';
{$endif}

{$endif win32}

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  destroyed(var a : TGdkWindowPrivate) : guint;
    begin
       destroyed:=(a.flag0 and bm_TGdkWindowPrivate_destroyed) shr bp_TGdkWindowPrivate_destroyed;
    end;

procedure set_destroyed(var a : TGdkWindowPrivate; __destroyed : guint);
    begin
       a.flag0:=a.flag0 or ((__destroyed shl bp_TGdkWindowPrivate_destroyed) and bm_TGdkWindowPrivate_destroyed);
    end;

function  mapped(var a : TGdkWindowPrivate) : guint;
    begin
       mapped:=(a.flag0 and bm_TGdkWindowPrivate_mapped) shr bp_TGdkWindowPrivate_mapped;
    end;

procedure set_mapped(var a : TGdkWindowPrivate; __mapped : guint);
    begin
       a.flag0:=a.flag0 or ((__mapped shl bp_TGdkWindowPrivate_mapped) and bm_TGdkWindowPrivate_mapped);
    end;

function  guffaw_gravity(var a : TGdkWindowPrivate) : guint;
    begin
       guffaw_gravity:=(a.flag0 and bm_TGdkWindowPrivate_guffaw_gravity) shr bp_TGdkWindowPrivate_guffaw_gravity;
    end;

procedure set_guffaw_gravity(var a : TGdkWindowPrivate; __guffaw_gravity : guint);
    begin
       a.flag0:=a.flag0 or ((__guffaw_gravity shl bp_TGdkWindowPrivate_guffaw_gravity) and bm_TGdkWindowPrivate_guffaw_gravity);
    end;

function  GDK_window_lookup(xid : longint) : PGdkWindow;
    begin
       gdk_window_lookup:=PGdkWindow(gdk_xid_table_lookup(xid));
    end;

function  GDK_pixmap_lookup(xid : longint) : PGdkPixmap;
    begin
       gdk_pixmap_lookup:=PGdkPixmap(gdk_xid_table_lookup(xid));
    end;

function  GDK_font_lookup(xid : longint) : PGdkFont;
    begin
       gdk_font_lookup:=PGdkFont(gdk_xid_table_lookup(xid));
    end;

{$endif read_implementation}


