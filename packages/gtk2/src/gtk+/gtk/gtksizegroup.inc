// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkSizeGroup = ^TGtkSizeGroup;
   TGtkSizeGroup = record
        parent_instance : TGObject;
        widgets : PGSList;
        mode : guint8;
        flag0 : word;
        requisition : TGtkRequisition;
     end;

{ Padding for future expansion  }
   PGtkSizeGroupClass = ^TGtkSizeGroupClass;
   TGtkSizeGroupClass = record
        parent_class : TGObjectClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{
   GtkSizeGroupMode:
   @GTK_SIZE_GROUP_NONE: group has no effect
   @GTK_SIZE_GROUP_HORIZONTAL: group effects horizontal requisition
   @GTK_SIZE_GROUP_VERTICAL: group effects vertical requisition
   @GTK_SIZE_GROUP_BOTH: group effects both horizontal and vertical requisition

   The mode of the size group determines the directions in which the size
   group effects the requested sizes of its component widgets.
   }

   PGtkSizeGroupMode = ^TGtkSizeGroupMode;
   TGtkSizeGroupMode = (
     GTK_SIZE_GROUP_NONE,
     GTK_SIZE_GROUP_HORIZONTAL,
     GTK_SIZE_GROUP_VERTICAL,
     GTK_SIZE_GROUP_BOTH
   );

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkSizeGroup_have_width = $1;
   bp_TGtkSizeGroup_have_width = 0;
   bm_TGtkSizeGroup_have_height = $2;
   bp_TGtkSizeGroup_have_height = 1;


function GTK_TYPE_SIZE_GROUP : GType;
function GTK_SIZE_GROUP(obj: pointer) : PGtkSizeGroup;
function GTK_SIZE_GROUP_CLASS(klass: pointer) : PGtkSizeGroupClass;
function GTK_IS_SIZE_GROUP(obj: pointer) : boolean;
function GTK_IS_SIZE_GROUP_CLASS(klass: pointer) : boolean;
function GTK_SIZE_GROUP_GET_CLASS(obj: pointer) : PGtkSizeGroupClass;

function have_width(var a : TGtkSizeGroup) : guint;
procedure set_have_width(var a : TGtkSizeGroup; __have_width : guint);
function have_height(var a : TGtkSizeGroup) : guint;
procedure set_have_height(var a : TGtkSizeGroup; __have_height : guint);

function gtk_size_group_get_type:GType; cdecl; external gtklib;
function gtk_size_group_new(mode:TGtkSizeGroupMode):PGtkSizeGroup; cdecl; external gtklib;
procedure gtk_size_group_set_mode(size_group:PGtkSizeGroup; mode:TGtkSizeGroupMode); cdecl; external gtklib;
function gtk_size_group_get_mode(size_group:PGtkSizeGroup):TGtkSizeGroupMode; cdecl; external gtklib;
procedure gtk_size_group_add_widget(size_group:PGtkSizeGroup; widget:PGtkWidget); cdecl; external gtklib;
procedure gtk_size_group_remove_widget(size_group:PGtkSizeGroup; widget:PGtkWidget); cdecl; external gtklib;
procedure _gtk_size_group_get_child_requisition(widget:PGtkWidget; requisition:PGtkRequisition); cdecl; external gtklib;
procedure _gtk_size_group_compute_requisition(widget:PGtkWidget; requisition:PGtkRequisition); cdecl; external gtklib;
procedure _gtk_size_group_queue_resize(widget:PGtkWidget); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_SIZE_GROUP : GType;
begin
   GTK_TYPE_SIZE_GROUP:=gtk_size_group_get_type;
end;

function GTK_SIZE_GROUP(obj: pointer) : PGtkSizeGroup;
begin
   GTK_SIZE_GROUP:=PGtkSizeGroup(GTK_CHECK_CAST(obj,GTK_TYPE_SIZE_GROUP));
end;

function GTK_SIZE_GROUP_CLASS(klass: pointer) : PGtkSizeGroupClass;
begin
   GTK_SIZE_GROUP_CLASS:=PGtkSizeGroupClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_SIZE_GROUP));
end;

function GTK_IS_SIZE_GROUP(obj: pointer) : boolean;
begin
   GTK_IS_SIZE_GROUP:=GTK_CHECK_TYPE(obj,GTK_TYPE_SIZE_GROUP);
end;

function GTK_IS_SIZE_GROUP_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_SIZE_GROUP_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_SIZE_GROUP);
end;

function GTK_SIZE_GROUP_GET_CLASS(obj: pointer) : PGtkSizeGroupClass;
begin
   GTK_SIZE_GROUP_GET_CLASS:=PGtkSizeGroupClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_SIZE_GROUP));
end;

function have_width(var a : TGtkSizeGroup) : guint;
begin
   have_width:=(a.flag0 and bm_TGtkSizeGroup_have_width) shr bp_TGtkSizeGroup_have_width;
end;

procedure set_have_width(var a : TGtkSizeGroup; __have_width : guint);
begin
   a.flag0:=a.flag0 or ((__have_width shl bp_TGtkSizeGroup_have_width) and bm_TGtkSizeGroup_have_width);
end;

function have_height(var a : TGtkSizeGroup) : guint;
begin
   have_height:=(a.flag0 and bm_TGtkSizeGroup_have_height) shr bp_TGtkSizeGroup_have_height;
end;

procedure set_have_height(var a : TGtkSizeGroup; __have_height : guint);
begin
   a.flag0:=a.flag0 or ((__have_height shl bp_TGtkSizeGroup_have_height) and bm_TGtkSizeGroup_have_height);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

