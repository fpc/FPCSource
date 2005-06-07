{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkScrolledWindow = ^TGtkScrolledWindow;
     TGtkScrolledWindow = record
          container : TGtkBin;
          hscrollbar : PGtkWidget;
          vscrollbar : PGtkWidget;
          flag0 : {$ifdef win32}longint{$else}word{$endif};
       end;

  const
     bm_TGtkScrolledWindow_hscrollbar_policy = $3;
     bp_TGtkScrolledWindow_hscrollbar_policy = 0;
     bm_TGtkScrolledWindow_vscrollbar_policy = $C;
     bp_TGtkScrolledWindow_vscrollbar_policy = 2;
     bm_TGtkScrolledWindow_hscrollbar_visible = $10;
     bp_TGtkScrolledWindow_hscrollbar_visible = 4;
     bm_TGtkScrolledWindow_vscrollbar_visible = $20;
     bp_TGtkScrolledWindow_vscrollbar_visible = 5;
     bm_TGtkScrolledWindow_window_placement = $C0;
     bp_TGtkScrolledWindow_window_placement = 6;
function  hscrollbar_policy(var a : TGtkScrolledWindow) : guint;
procedure set_hscrollbar_policy(var a : TGtkScrolledWindow; __hscrollbar_policy : guint);
function  vscrollbar_policy(var a : TGtkScrolledWindow) : guint;
procedure set_vscrollbar_policy(var a : TGtkScrolledWindow; __vscrollbar_policy : guint);
function  hscrollbar_visible(var a : TGtkScrolledWindow) : guint;
procedure set_hscrollbar_visible(var a : TGtkScrolledWindow; __hscrollbar_visible : guint);
function  vscrollbar_visible(var a : TGtkScrolledWindow) : guint;
procedure set_vscrollbar_visible(var a : TGtkScrolledWindow; __vscrollbar_visible : guint);
function  window_placement(var a : TGtkScrolledWindow) : guint;
procedure set_window_placement(var a : TGtkScrolledWindow; __window_placement : guint);

  type
     PGtkScrolledWindowClass = ^TGtkScrolledWindowClass;
     TGtkScrolledWindowClass = record
          parent_class : TGtkBinClass;
          scrollbar_spacing : gint;
       end;

Type
  GTK_SCROLLED_WINDOW=PGtkScrolledWindow;
  GTK_SCROLLED_WINDOW_CLASS=PGtkScrolledWindowClass;

function  GTK_SCROLLED_WINDOW_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_scrolled_window_get_type';
function  GTK_IS_SCROLLED_WINDOW(obj:pointer):boolean;
function  GTK_IS_SCROLLED_WINDOW_CLASS(klass:pointer):boolean;

function  gtk_scrolled_window_get_type:TGtkType;cdecl;external gtkdll name 'gtk_scrolled_window_get_type';
function  gtk_scrolled_window_new(hadjustment:PGtkAdjustment; vadjustment:PGtkAdjustment):PGtkWidget;cdecl;external gtkdll name 'gtk_scrolled_window_new';
procedure gtk_scrolled_window_set_hadjustment(scrolled_window:PGtkScrolledWindow; hadjustment:PGtkAdjustment);cdecl;external gtkdll name 'gtk_scrolled_window_set_hadjustment';
procedure gtk_scrolled_window_set_vadjustment(scrolled_window:PGtkScrolledWindow; hadjustment:PGtkAdjustment);cdecl;external gtkdll name 'gtk_scrolled_window_set_vadjustment';
function  gtk_scrolled_window_get_hadjustment(scrolled_window:PGtkScrolledWindow):PGtkAdjustment;cdecl;external gtkdll name 'gtk_scrolled_window_get_hadjustment';
function  gtk_scrolled_window_get_vadjustment(scrolled_window:PGtkScrolledWindow):PGtkAdjustment;cdecl;external gtkdll name 'gtk_scrolled_window_get_vadjustment';
procedure gtk_scrolled_window_set_policy(scrolled_window:PGtkScrolledWindow; hscrollbar_policy:TGtkPolicyType; vscrollbar_policy:TGtkPolicyType);cdecl;external gtkdll name 'gtk_scrolled_window_set_policy';
procedure gtk_scrolled_window_set_placement(scrolled_window:PGtkScrolledWindow; window_placement:TGtkCornerType);cdecl;external gtkdll name 'gtk_scrolled_window_set_placement';
procedure gtk_scrolled_window_add_with_viewport(scrolled_window:PGtkScrolledWindow; child:PGtkWidget);cdecl;external gtkdll name 'gtk_scrolled_window_add_with_viewport';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  hscrollbar_policy(var a : TGtkScrolledWindow) : guint;
    begin
       hscrollbar_policy:=(a.flag0 and bm_TGtkScrolledWindow_hscrollbar_policy) shr bp_TGtkScrolledWindow_hscrollbar_policy;
    end;

procedure set_hscrollbar_policy(var a : TGtkScrolledWindow; __hscrollbar_policy : guint);
    begin
       a.flag0:=a.flag0 or ((__hscrollbar_policy shl bp_TGtkScrolledWindow_hscrollbar_policy) and bm_TGtkScrolledWindow_hscrollbar_policy);
    end;

function  vscrollbar_policy(var a : TGtkScrolledWindow) : guint;
    begin
       vscrollbar_policy:=(a.flag0 and bm_TGtkScrolledWindow_vscrollbar_policy) shr bp_TGtkScrolledWindow_vscrollbar_policy;
    end;

procedure set_vscrollbar_policy(var a : TGtkScrolledWindow; __vscrollbar_policy : guint);
    begin
       a.flag0:=a.flag0 or ((__vscrollbar_policy shl bp_TGtkScrolledWindow_vscrollbar_policy) and bm_TGtkScrolledWindow_vscrollbar_policy);
    end;

function  hscrollbar_visible(var a : TGtkScrolledWindow) : guint;
    begin
       hscrollbar_visible:=(a.flag0 and bm_TGtkScrolledWindow_hscrollbar_visible) shr bp_TGtkScrolledWindow_hscrollbar_visible;
    end;

procedure set_hscrollbar_visible(var a : TGtkScrolledWindow; __hscrollbar_visible : guint);
    begin
       a.flag0:=a.flag0 or ((__hscrollbar_visible shl bp_TGtkScrolledWindow_hscrollbar_visible) and bm_TGtkScrolledWindow_hscrollbar_visible);
    end;

function  vscrollbar_visible(var a : TGtkScrolledWindow) : guint;
    begin
       vscrollbar_visible:=(a.flag0 and bm_TGtkScrolledWindow_vscrollbar_visible) shr bp_TGtkScrolledWindow_vscrollbar_visible;
    end;

procedure set_vscrollbar_visible(var a : TGtkScrolledWindow; __vscrollbar_visible : guint);
    begin
       a.flag0:=a.flag0 or ((__vscrollbar_visible shl bp_TGtkScrolledWindow_vscrollbar_visible) and bm_TGtkScrolledWindow_vscrollbar_visible);
    end;

function  window_placement(var a : TGtkScrolledWindow) : guint;
    begin
       window_placement:=(a.flag0 and bm_TGtkScrolledWindow_window_placement) shr bp_TGtkScrolledWindow_window_placement;
    end;

procedure set_window_placement(var a : TGtkScrolledWindow; __window_placement : guint);
    begin
       a.flag0:=a.flag0 or ((__window_placement shl bp_TGtkScrolledWindow_window_placement) and bm_TGtkScrolledWindow_window_placement);
    end;

function  GTK_IS_SCROLLED_WINDOW(obj:pointer):boolean;
begin
  GTK_IS_SCROLLED_WINDOW:=(obj<>nil) and GTK_IS_SCROLLED_WINDOW_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_SCROLLED_WINDOW_CLASS(klass:pointer):boolean;
begin
  GTK_IS_SCROLLED_WINDOW_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_SCROLLED_WINDOW_TYPE);
end;

{$endif read_implementation}


