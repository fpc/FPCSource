// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkContainer = ^TGtkContainer;
   TGtkContainer = record
        widget : TGtkWidget;
        focus_child : PGtkWidget;
        flag0 : longint;
     end;

{ Padding for future expansion  }
   PGtkContainerClass = ^TGtkContainerClass;
   TGtkContainerClass = record
        parent_class : TGtkWidgetClass;
        add : procedure (container:PGtkContainer; widget:PGtkWidget); cdecl;
        remove : procedure (container:PGtkContainer; widget:PGtkWidget); cdecl;
        check_resize : procedure (container:PGtkContainer); cdecl;
        forall : procedure (container:PGtkContainer; include_internals:gboolean; callback:TGtkCallback; callback_data:gpointer); cdecl;
        set_focus_child : procedure (container:PGtkContainer; widget:PGtkWidget); cdecl;
        child_type : function (container:PGtkContainer):TGtkType; cdecl;
        composite_name : function (container:PGtkContainer; child:PGtkWidget):Pgchar; cdecl;
        set_child_property : procedure (container:PGtkContainer; child:PGtkWidget; property_id:guint; value:PGValue; pspec:PGParamSpec); cdecl;
        get_child_property : procedure (container:PGtkContainer; child:PGtkWidget; property_id:guint; value:PGValue; pspec:PGParamSpec); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkContainer_border_width = $FFFF;
   bp_TGtkContainer_border_width = 0;
   bm_TGtkContainer_need_resize = $10000;
   bp_TGtkContainer_need_resize = 16;
   bm_TGtkContainer_resize_mode = $60000;
   bp_TGtkContainer_resize_mode = 17;
   bm_TGtkContainer_reallocate_redraws = $80000;
   bp_TGtkContainer_reallocate_redraws = 19;
   bm_TGtkContainer_has_focus_chain = $100000;
   bp_TGtkContainer_has_focus_chain = 20;


function GTK_TYPE_CONTAINER : GType;
function GTK_CONTAINER(obj: pointer) : PGtkContainer;
function GTK_CONTAINER_CLASS(klass: pointer) : PGtkContainerClass;
function GTK_IS_CONTAINER(obj: pointer) : boolean;
function GTK_IS_CONTAINER_CLASS(klass: pointer) : boolean;
function GTK_CONTAINER_GET_CLASS(obj: pointer) : PGtkContainerClass;
function GTK_IS_RESIZE_CONTAINER(widget : pointer) : boolean;

function border_width(var a : TGtkContainer) : guint;
procedure set_border_width(var a : TGtkContainer; __border_width : guint);
function need_resize(var a : TGtkContainer) : guint;
procedure set_need_resize(var a : TGtkContainer; __need_resize : guint);
function resize_mode(a : PGtkContainer) : guint;
procedure set_resize_mode(var a : TGtkContainer; __resize_mode : guint);
function reallocate_redraws(var a : TGtkContainer) : guint;
procedure set_reallocate_redraws(var a : TGtkContainer; __reallocate_redraws : guint);
function has_focus_chain(var a : TGtkContainer) : guint;
procedure set_has_focus_chain(var a : TGtkContainer; __has_focus_chain : guint);

{ Application-level methods  }

function gtk_container_get_type:TGtkType; cdecl; external gtklib;
procedure gtk_container_set_border_width(container:PGtkContainer; border_width:guint); cdecl; external gtklib;
function gtk_container_get_border_width(container:PGtkContainer):guint; cdecl; external gtklib;
procedure gtk_container_add(container:PGtkContainer; widget:PGtkWidget); cdecl; external gtklib;
procedure gtk_container_remove(container:PGtkContainer; widget:PGtkWidget); cdecl; external gtklib;
procedure gtk_container_set_resize_mode(container:PGtkContainer; resize_mode:TGtkResizeMode); cdecl; external gtklib;
function gtk_container_get_resize_mode(container:PGtkContainer):TGtkResizeMode; cdecl; external gtklib;
procedure gtk_container_check_resize(container:PGtkContainer); cdecl; external gtklib;
procedure gtk_container_foreach(container:PGtkContainer; callback:TGtkCallback; callback_data:gpointer); cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_container_foreach_full(container:PGtkContainer; callback:TGtkCallback; marshal:TGtkCallbackMarshal; callback_data:gpointer; notify:TGtkDestroyNotify); cdecl; external gtklib;
{$endif}

{  GTK_DISABLE_DEPRECATED  }
function gtk_container_get_children(container:PGtkContainer):PGList; cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
function gtk_container_children(container:PGtkContainer):PGList;
{$endif}

procedure gtk_container_propagate_expose(container:PGtkContainer; child:PGtkWidget; event:PGdkEventExpose); cdecl; external gtklib;
procedure gtk_container_set_focus_chain(container:PGtkContainer; focusable_widgets:PGList); cdecl; external gtklib;
function gtk_container_get_focus_chain(container:PGtkContainer; var focusable_widgets:PGList):gboolean; cdecl; external gtklib;
procedure gtk_container_unset_focus_chain(container:PGtkContainer); cdecl; external gtklib;
{ Widget-level methods  }
procedure gtk_container_set_reallocate_redraws(container:PGtkContainer; needs_redraws:gboolean); cdecl; external gtklib;
procedure gtk_container_set_focus_child(container:PGtkContainer; child:PGtkWidget); cdecl; external gtklib;
procedure gtk_container_set_focus_vadjustment(container:PGtkContainer; adjustment:PGtkAdjustment); cdecl; external gtklib;
function gtk_container_get_focus_vadjustment(container:PGtkContainer):PGtkAdjustment; cdecl; external gtklib;
procedure gtk_container_set_focus_hadjustment(container:PGtkContainer; adjustment:PGtkAdjustment); cdecl; external gtklib;
function gtk_container_get_focus_hadjustment(container:PGtkContainer):PGtkAdjustment; cdecl; external gtklib;
procedure gtk_container_resize_children(container:PGtkContainer); cdecl; external gtklib;
function gtk_container_child_type(container:PGtkContainer):TGtkType; cdecl; external gtklib;
procedure gtk_container_class_install_child_property(cclass:PGtkContainerClass; property_id:guint; pspec:PGParamSpec); cdecl; external gtklib;
function gtk_container_class_find_child_property(cclass:PGObjectClass; property_name:Pgchar):PGParamSpec; cdecl; external gtklib;
function gtk_container_class_list_child_properties(cclass:PGObjectClass; n_properties:Pguint):PPGParamSpec; cdecl; external gtklib;
{$IFNDEF KYLIX}
procedure gtk_container_add_with_properties(container:PGtkContainer; widget:PGtkWidget; first_prop_name:Pgchar; args:array of const); cdecl; overload; external gtklib;
procedure gtk_container_add_with_properties(container:PGtkContainer; widget:PGtkWidget; first_prop_name:Pgchar); cdecl; overload; varargs; external gtklib;
procedure gtk_container_child_set(container:PGtkContainer; child:PGtkWidget; first_prop_name:Pgchar; args:array of const); cdecl; overload; external gtklib;
procedure gtk_container_child_set(container:PGtkContainer; child:PGtkWidget; first_prop_name:Pgchar); cdecl; overload; varargs; external gtklib;
procedure gtk_container_child_get(container:PGtkContainer; child:PGtkWidget; first_prop_name:Pgchar; args:array of const); cdecl; overload; external gtklib;
procedure gtk_container_child_get(container:PGtkContainer; child:PGtkWidget; first_prop_name:Pgchar); overload; cdecl; varargs; external gtklib;
procedure gtk_container_child_set_valist(container:PGtkContainer; child:PGtkWidget; first_property_name:Pgchar; var_args: array of const); cdecl; external gtklib;
procedure gtk_container_child_get_valist(container:PGtkContainer; child:PGtkWidget; first_property_name:Pgchar; var_args: array of const); cdecl; external gtklib;
{$ELSE}
procedure gtk_container_add_with_properties(container:PGtkContainer; widget:PGtkWidget; first_prop_name:Pgchar); varargs; cdecl; external gtklib;
procedure gtk_container_child_set(container:PGtkContainer; child:PGtkWidget; first_prop_name:Pgchar); varargs; cdecl; external gtklib;
procedure gtk_container_child_get(container:PGtkContainer; child:PGtkWidget; first_prop_name:Pgchar); varargs; cdecl; external gtklib;
procedure gtk_container_child_set_valist(container:PGtkContainer; child:PGtkWidget; first_property_name:Pgchar); varargs; cdecl; external gtklib;
procedure gtk_container_child_get_valist(container:PGtkContainer; child:PGtkWidget; first_property_name:Pgchar); varargs; cdecl; external gtklib;
{$ENDIF}
procedure gtk_container_child_set_property(container:PGtkContainer; child:PGtkWidget; property_name:Pgchar; value:PGValue); cdecl; external gtklib;
procedure gtk_container_child_get_property(container:PGtkContainer; child:PGtkWidget; property_name:Pgchar; value:PGValue); cdecl; external gtklib;
procedure GTK_CONTAINER_WARN_INVALID_CHILD_PROPERTY_ID(anObject: pointer; property_id: guint; pspec : pointer);

procedure gtk_container_forall(container:PGtkContainer; callback:TGtkCallback; callback_data:gpointer); cdecl; external gtklib;
{ Non-public methods  }
procedure _gtk_container_queue_resize(container:PGtkContainer); cdecl; external gtklib;
procedure _gtk_container_clear_resize_widgets(container:PGtkContainer); cdecl; external gtklib;
function _gtk_container_child_composite_name(container:PGtkContainer; child:PGtkWidget):Pgchar; cdecl; external gtklib;
procedure _gtk_container_dequeue_resize_handler(container:PGtkContainer); cdecl; external gtklib;
function _gtk_container_focus_sort(container:PGtkContainer; children:PGList; direction:TGtkDirectionType; old_focus:PGtkWidget):PGList; cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_container_border_width(container:PGtkContainer; border_width:guint);
{$endif}
{ GTK_DISABLE_DEPRECATED  }

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_CONTAINER : GType;
begin
   GTK_TYPE_CONTAINER:=gtk_container_get_type;
end;

function GTK_CONTAINER(obj: pointer) : PGtkContainer;
begin
   GTK_CONTAINER:=PGtkContainer(GTK_CHECK_CAST(obj,GTK_TYPE_CONTAINER));
end;

function GTK_CONTAINER_CLASS(klass: pointer) : PGtkContainerClass;
begin
   GTK_CONTAINER_CLASS:=PGtkContainerClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_CONTAINER));
end;

function GTK_IS_CONTAINER(obj: pointer) : boolean;
begin
   GTK_IS_CONTAINER:=GTK_CHECK_TYPE(obj,GTK_TYPE_CONTAINER);
end;

function GTK_IS_CONTAINER_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_CONTAINER_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_CONTAINER);
end;

function GTK_CONTAINER_GET_CLASS(obj: pointer) : PGtkContainerClass;
begin
   GTK_CONTAINER_GET_CLASS:=PGtkContainerClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_CONTAINER));
end;

function GTK_IS_RESIZE_CONTAINER(widget : pointer) : boolean;
begin
   GTK_IS_RESIZE_CONTAINER:=(GTK_IS_CONTAINER(widget))
                             and ((resize_mode(PGtkContainer(widget)))
                            <> ord(GTK_RESIZE_PARENT));
end;

function border_width(var a : TGtkContainer) : guint;
begin
   border_width:=(a.flag0 and bm_TGtkContainer_border_width) shr bp_TGtkContainer_border_width;
end;

procedure set_border_width(var a : TGtkContainer; __border_width : guint);
begin
   a.flag0:=a.flag0 or ((__border_width shl bp_TGtkContainer_border_width) and bm_TGtkContainer_border_width);
end;

function need_resize(var a : TGtkContainer) : guint;
begin
   need_resize:=(a.flag0 and bm_TGtkContainer_need_resize) shr bp_TGtkContainer_need_resize;
end;

procedure set_need_resize(var a : TGtkContainer; __need_resize : guint);
begin
   a.flag0:=a.flag0 or ((__need_resize shl bp_TGtkContainer_need_resize) and bm_TGtkContainer_need_resize);
end;

function resize_mode(a : PGtkContainer) : guint;
begin
   resize_mode:=(a^.flag0 and bm_TGtkContainer_resize_mode) shr bp_TGtkContainer_resize_mode;
end;

procedure set_resize_mode(var a : TGtkContainer; __resize_mode : guint);
begin
   a.flag0:=a.flag0 or ((__resize_mode shl bp_TGtkContainer_resize_mode) and bm_TGtkContainer_resize_mode);
end;

function reallocate_redraws(var a : TGtkContainer) : guint;
begin
   reallocate_redraws:=(a.flag0 and bm_TGtkContainer_reallocate_redraws) shr bp_TGtkContainer_reallocate_redraws;
end;

procedure set_reallocate_redraws(var a : TGtkContainer; __reallocate_redraws : guint);
begin
   a.flag0:=a.flag0 or ((__reallocate_redraws shl bp_TGtkContainer_reallocate_redraws) and bm_TGtkContainer_reallocate_redraws);
end;

function has_focus_chain(var a : TGtkContainer) : guint;
begin
   has_focus_chain:=(a.flag0 and bm_TGtkContainer_has_focus_chain) shr bp_TGtkContainer_has_focus_chain;
end;

procedure set_has_focus_chain(var a : TGtkContainer; __has_focus_chain : guint);
begin
   a.flag0:=a.flag0 or ((__has_focus_chain shl bp_TGtkContainer_has_focus_chain) and bm_TGtkContainer_has_focus_chain);
end;

{$IFDEF KYLIX}
function HexStr(Value, Bits: integer): string;
begin
  Str(Value, Result);
end;
{$ENDIF}

procedure GTK_CONTAINER_WARN_INVALID_CHILD_PROPERTY_ID(
  anObject: pointer; property_id: guint; pspec : pointer);
begin
   writeln('invalid child property id ',property_id,' ',
     HexStr(PtrInt(anObject),8),' ',HexStr(PtrInt(pspec),8));
   //  G_OBJECT_WARN_INVALID_PSPEC(anObject,'child property id',property_id,pspec);
end;

{$ifndef GTK_DISABLE_DEPRECATED}
function gtk_container_children(container:PGtkContainer):PGList;
begin
   gtk_container_children:=gtk_container_get_children(container);
end;

procedure gtk_container_border_width(container:PGtkContainer; border_width:guint);
begin
   gtk_container_set_border_width(container,border_width);
end;
{$endif}
{ GTK_DISABLE_DEPRECATED  }

{$ENDIF read_implementation}
// included by gtk2.pas

