// included by gdk2.pp

{$IFDEF read_interface_types}
   PGdkDragAction = ^TGdkDragAction;
   TGdkDragAction = longint; // see below for possible values

{ A root window with nobody claiming drags  }
{ Not a valid drag window  }
{ The simple WM_DROPFILES dnd  }
{ The complex OLE2 dnd (not implemented)  }
{ Intra-app  }
   PGdkDragProtocol = ^TGdkDragProtocol;
   TGdkDragProtocol = (GDK_DRAG_PROTO_MOTIF,GDK_DRAG_PROTO_XDND,
     GDK_DRAG_PROTO_ROOTWIN,GDK_DRAG_PROTO_NONE,
     GDK_DRAG_PROTO_WIN32_DROPFILES,GDK_DRAG_PROTO_OLE2,
     GDK_DRAG_PROTO_LOCAL);

{< public > }
{< private > }
   PGdkDragContext = ^TGdkDragContext;
   TGdkDragContext = record
        parent_instance : TGObject;
        protocol : TGdkDragProtocol;
        is_source : gboolean;
        source_window : PGdkWindow;
        dest_window : PGdkWindow;
        targets : PGList;
        actions : TGdkDragAction;
        suggested_action : TGdkDragAction;
        action : TGdkDragAction;
        start_time : guint32;
        windowing_data : gpointer;
     end;

   PGdkDragContextClass = ^TGdkDragContextClass;
   TGdkDragContextClass = record
        parent_class : TGObjectClass;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
  // values for TGdkDragAction
  GDK_ACTION_DEFAULT = 1 shl 0;
  GDK_ACTION_COPY = 1 shl 1;
  GDK_ACTION_MOVE = 1 shl 2;
  GDK_ACTION_LINK = 1 shl 3;
  GDK_ACTION_PRIVATE = 1 shl 4;
  GDK_ACTION_ASK = 1 shl 5;


{ Object that holds information about a drag in progress.
   this is used on both source and destination sides.
  }
function GDK_TYPE_DRAG_CONTEXT : GType;
function GDK_DRAG_CONTEXT(anObject : Pointer) : PGdkDragContext;
function GDK_DRAG_CONTEXT_CLASS(klass : Pointer) : PGdkDragContextClass;
function GDK_IS_DRAG_CONTEXT(anObject : Pointer) : boolean;
function GDK_IS_DRAG_CONTEXT_CLASS(klass : Pointer) : boolean;
function GDK_DRAG_CONTEXT_GET_CLASS(obj : Pointer) : PGdkDragContextClass;


{ Drag and Drop  }

function gdk_drag_context_get_type:GType; cdecl; external gdklib;
function gdk_drag_context_new:PGdkDragContext; cdecl; external gdklib;

{$ifndef GDK_DISABLE_DEPRECATED}
procedure gdk_drag_context_ref(context:PGdkDragContext); cdecl; external gdklib;
procedure gdk_drag_context_unref(context:PGdkDragContext); cdecl; external gdklib;
{$endif}


{ Destination side  }

procedure gdk_drag_status(context:PGdkDragContext; action:TGdkDragAction; time:guint32); cdecl; external gdklib;
procedure gdk_drop_reply(context:PGdkDragContext; ok:gboolean; time:guint32); cdecl; external gdklib;
procedure gdk_drop_finish(context:PGdkDragContext; success:gboolean; time:guint32); cdecl; external gdklib;
function gdk_drag_get_selection(context:PGdkDragContext):TGdkAtom; cdecl; external gdklib;


{ Source side  }

function gdk_drag_begin(window:PGdkWindow; targets:PGList):PGdkDragContext; cdecl; external gdklib;
function gdk_drag_get_protocol_for_display(display:PGdkDisplay; xid:guint32; protocol:PGdkDragProtocol):guint32; cdecl; external gdklib;

{$ifndef GDK_MULTIHEAD_SAFE}
function gdk_drag_get_protocol(xid:guint32; protocol:PGdkDragProtocol):guint32; cdecl; external gdklib;
{$endif}


{ GDK_MULTIHEAD_SAFE  }

procedure gdk_drag_find_window(context:PGdkDragContext; drag_window:PGdkWindow; x_root:gint; y_root:gint; var dest_window:PGdkWindow;
            protocol:PGdkDragProtocol); cdecl; external gdklib;
function gdk_drag_motion(context:PGdkDragContext; dest_window:PGdkWindow; protocol:TGdkDragProtocol; x_root:gint; y_root:gint;
           suggested_action:TGdkDragAction; possible_actions:TGdkDragAction; time:guint32):gboolean; cdecl; external gdklib;
procedure gdk_drag_drop(context:PGdkDragContext; time:guint32); cdecl; external gdklib;
procedure gdk_drag_abort(context:PGdkDragContext; time:guint32); cdecl; external gdklib;
{$endif read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GDK_TYPE_DRAG_CONTEXT : GType;
begin
  GDK_TYPE_DRAG_CONTEXT:=gdk_drag_context_get_type;
end;

function GDK_DRAG_CONTEXT(anObject : Pointer) : PGdkDragContext;
begin
  GDK_DRAG_CONTEXT:=PGdkDragContext(G_TYPE_CHECK_INSTANCE_CAST(anObject,
                                                        GDK_TYPE_DRAG_CONTEXT));
end;

function GDK_DRAG_CONTEXT_CLASS(klass : Pointer) : PGdkDragContextClass;
begin
  GDK_DRAG_CONTEXT_CLASS:=PGdkDragContextClass(G_TYPE_CHECK_CLASS_CAST(klass,GDK_TYPE_DRAG_CONTEXT));
end;

function GDK_IS_DRAG_CONTEXT(anObject : Pointer) : boolean;
begin
  GDK_IS_DRAG_CONTEXT:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,GDK_TYPE_DRAG_CONTEXT);
end;

function GDK_IS_DRAG_CONTEXT_CLASS(klass : Pointer) : boolean;
begin
  GDK_IS_DRAG_CONTEXT_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GDK_TYPE_DRAG_CONTEXT);
end;

function GDK_DRAG_CONTEXT_GET_CLASS(obj : Pointer) : PGdkDragContextClass;
begin
  GDK_DRAG_CONTEXT_GET_CLASS:=PGdkDragContextClass(G_TYPE_INSTANCE_GET_CLASS(
                                                    obj,GDK_TYPE_DRAG_CONTEXT));
end;

{$ENDIF}
