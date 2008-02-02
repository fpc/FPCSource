// included by gdk2.pp

{$IFDEF read_interface_types}

{$ifndef GDK_DISABLE_DEPRECATED}
   PGdkSelection = ^TGdkSelection;
   TGdkSelection = TGdkAtom;

   PGdkTarget = ^TGdkTarget;
   TGdkTarget = TGdkAtom;

   PGdkSelectionType = ^TGdkSelectionType;
   TGdkSelectionType = TGdkAtom;
{$endif}
{ GDK_DISABLE_DEPRECATED  }

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
{ Predefined atoms relating to selections. In general, one will need to use
   gdk_intern_atom
  }
function GDK_SELECTION_PRIMARY : TGdkAtom;
function GDK_SELECTION_SECONDARY : TGdkAtom;
function GDK_SELECTION_CLIPBOARD : TGdkAtom;
function GDK_TARGET_BITMAP : TGdkAtom;
function GDK_TARGET_COLORMAP : TGdkAtom;
function GDK_TARGET_DRAWABLE : TGdkAtom;
function GDK_TARGET_PIXMAP : TGdkAtom;
function GDK_TARGET_STRING : TGdkAtom;
function GDK_SELECTION_TYPE_ATOM : TGdkAtom;
function GDK_SELECTION_TYPE_BITMAP : TGdkAtom;
function GDK_SELECTION_TYPE_COLORMAP : TGdkAtom;
function GDK_SELECTION_TYPE_DRAWABLE : TGdkAtom;
function GDK_SELECTION_TYPE_INTEGER : TGdkAtom;
function GDK_SELECTION_TYPE_PIXMAP : TGdkAtom;
function GDK_SELECTION_TYPE_WINDOW : TGdkAtom;
function GDK_SELECTION_TYPE_STRING : TGdkAtom;


{ Selections }

{$ifndef GDK_MULTIHEAD_SAFE}
function gdk_selection_owner_set(owner:PGdkWindow; selection:TGdkAtom; time:guint32; send_event:gboolean):gboolean; cdecl; external gdklib;
function gdk_selection_owner_get(selection:TGdkAtom):PGdkWindow; cdecl; external gdklib;
{$endif}


{ GDK_MULTIHEAD_SAFE  }

function gdk_selection_owner_set_for_display(display:PGdkDisplay; owner:PGdkWindow; selection:TGdkAtom; time:guint32; send_event:gboolean):gboolean; cdecl; external gdklib;
function gdk_selection_owner_get_for_display(display:PGdkDisplay; selection:TGdkAtom):PGdkWindow; cdecl; external gdklib;
procedure gdk_selection_convert(requestor:PGdkWindow; selection:TGdkAtom; target:TGdkAtom; time:guint32); cdecl; external gdklib;
function gdk_selection_property_get(requestor:PGdkWindow; data:PPguchar; prop_type:PGdkAtom; prop_format:Pgint):gboolean; cdecl; external gdklib;
{$ifndef GDK_MULTIHEAD_SAFE}

procedure gdk_selection_send_notify(requestor:guint32; selection:TGdkAtom; target:TGdkAtom; _property:TGdkAtom; time:guint32); cdecl; external gdklib;
{$endif}
{ GDK_MULTIHEAD_SAFE  }

procedure gdk_selection_send_notify_for_display(display:PGdkDisplay; requestor:guint32; selection:TGdkAtom; target:TGdkAtom; _property:TGdkAtom;
            time:guint32); cdecl; external gdklib;
{$endif read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GDK_SELECTION_PRIMARY : TGdkAtom;
begin
  GDK_SELECTION_PRIMARY:=_GDK_MAKE_ATOM(1);
end;

function GDK_SELECTION_SECONDARY : TGdkAtom;
begin
  GDK_SELECTION_SECONDARY:=_GDK_MAKE_ATOM(2);
end;

function GDK_SELECTION_CLIPBOARD : TGdkAtom;
begin
  GDK_SELECTION_CLIPBOARD:=_GDK_MAKE_ATOM(69);
end;

function GDK_TARGET_BITMAP : TGdkAtom;
begin
  GDK_TARGET_BITMAP:=_GDK_MAKE_ATOM(5);
end;

function GDK_TARGET_COLORMAP : TGdkAtom;
begin
  GDK_TARGET_COLORMAP:=_GDK_MAKE_ATOM(7);
end;

function GDK_TARGET_DRAWABLE : TGdkAtom;
begin
  GDK_TARGET_DRAWABLE:=_GDK_MAKE_ATOM(17);
end;

function GDK_TARGET_PIXMAP : TGdkAtom;
begin
  GDK_TARGET_PIXMAP:=_GDK_MAKE_ATOM(20);
end;

function GDK_TARGET_STRING : TGdkAtom;
begin
  GDK_TARGET_STRING:=_GDK_MAKE_ATOM(31);
end;

function GDK_SELECTION_TYPE_ATOM : TGdkAtom;
begin
  GDK_SELECTION_TYPE_ATOM:=_GDK_MAKE_ATOM(4);
end;

function GDK_SELECTION_TYPE_BITMAP : TGdkAtom;
begin
  GDK_SELECTION_TYPE_BITMAP:=_GDK_MAKE_ATOM(5);
end;

function GDK_SELECTION_TYPE_COLORMAP : TGdkAtom;
begin
  GDK_SELECTION_TYPE_COLORMAP:=_GDK_MAKE_ATOM(7);
end;

function GDK_SELECTION_TYPE_DRAWABLE : TGdkAtom;
begin
  GDK_SELECTION_TYPE_DRAWABLE:=_GDK_MAKE_ATOM(17);
end;

function GDK_SELECTION_TYPE_INTEGER : TGdkAtom;
begin
  GDK_SELECTION_TYPE_INTEGER:=_GDK_MAKE_ATOM(19);
end;

function GDK_SELECTION_TYPE_PIXMAP : TGdkAtom;
begin
  GDK_SELECTION_TYPE_PIXMAP:=_GDK_MAKE_ATOM(20);
end;

function GDK_SELECTION_TYPE_WINDOW : TGdkAtom;
begin
  GDK_SELECTION_TYPE_WINDOW:=_GDK_MAKE_ATOM(33);
end;

function GDK_SELECTION_TYPE_STRING : TGdkAtom;
begin
  GDK_SELECTION_TYPE_STRING:=_GDK_MAKE_ATOM(31);
end;
{$ENDIF}

