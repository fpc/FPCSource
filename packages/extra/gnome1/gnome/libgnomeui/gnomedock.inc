{$IfDef read_interface}
type
   PGnomeDockPlacement = ^TGnomeDockPlacement;
   TGnomeDockPlacement = (GNOME_DOCK_TOP,GNOME_DOCK_RIGHT,GNOME_DOCK_BOTTOM,
     GNOME_DOCK_LEFT,GNOME_DOCK_FLOATING);

type
   TGnomeDockItemBehavior = (GNOME_DOCK_ITEM_BEH_NORMAL := 0,GNOME_DOCK_ITEM_BEH_EXCLUSIVE := 1 shl 0,
     GNOME_DOCK_ITEM_BEH_NEVER_FLOATING := 1 shl 1,
     GNOME_DOCK_ITEM_BEH_NEVER_VERTICAL := 1 shl 2,
     GNOME_DOCK_ITEM_BEH_NEVER_HORIZONTAL := 1 shl 3,
     GNOME_DOCK_ITEM_BEH_LOCKED := 1 shl 4);

const
   GNOME_DOCK_ITEM_BEH_NEVER_DETACH = GNOME_DOCK_ITEM_BEH_NEVER_FLOATING;

type
   PGnomeDockItem = ^TGnomeDockItem;
   TGnomeDockItem = record
        bin : TGtkBin;
        name : Pgchar;
        bin_window : PGdkWindow;
        float_window : PGdkWindow;
        shadow_type : TGtkShadowType;
        orientation : TGtkOrientation;
        behavior : TGnomeDockItemBehavior;
        flag0 : word;
        dragoff_x : gint;
        dragoff_y : gint;
        float_x : gint;
        float_y : gint;
        grab_on_map_event : guint;
     end;
   GNOME_DOCK_ITEM = PGnomeDockItem;

function GNOME_DOCK_ITEM_NOT_LOCKED(x : PGnomeDockItem) : gboolean;

const
   bm__GnomeDockItem_float_window_mapped = $1;
   bp__GnomeDockItem_float_window_mapped = 0;
   bm__GnomeDockItem_is_floating = $2;
   bp__GnomeDockItem_is_floating = 1;
   bm__GnomeDockItem_in_drag = $4;
   bp__GnomeDockItem_in_drag = 2;

function float_window_mapped(var a : TGnomeDockItem) : gboolean;
procedure set_float_window_mapped(var a : TGnomeDockItem; __float_window_mapped : gboolean);
function is_floating(var a : TGnomeDockItem) : gboolean;
procedure set_is_floating(var a : TGnomeDockItem; __is_floating : gboolean);
function in_drag(var a : TGnomeDockItem) : gboolean;
procedure set_in_drag(var a : TGnomeDockItem; __in_drag : gboolean);

type
   PGnomeDockItemClass = ^TGnomeDockItemClass;
   TGnomeDockItemClass = record
        parent_class : TGtkBinClass;
        dock_drag_begin : procedure (item:PGnomeDockItem);cdecl;
        dock_drag_motion : procedure (item:PGnomeDockItem; x:gint; y:gint);cdecl;
        dock_drag_end : procedure (item:PGnomeDockItem);cdecl;
        dock_detach : procedure (item:PGnomeDockItem);cdecl;
     end;
   GNOME_DOCK_ITEM_CLASS = PGnomeDockItemClass;

type
   PGnomeDockLayoutItem = ^TGnomeDockLayoutItem;
   TGnomeDockLayoutItem = record
        item : PGnomeDockItem;
        placement : TGnomeDockPlacement;
        position : record
            case longint of
               0 : ( floating : record
                    x : gint;
                    y : gint;
                    orientation : TGtkOrientation;
                 end );
               1 : ( docked : record
                    band_num : gint;
                    band_position : gint;
                    offset : gint;
                 end );
            end;
     end;
   GNOME_DOCK_LAYOUT_ITEM = PGnomeDockLayoutItem;

   PGnomeDockLayout = ^TGnomeDockLayout;
   TGnomeDockLayout = record
        theobject : TGtkObject;
        items : PGList;
     end;
   GNOME_DOCK_LAYOUT = PGnomeDockLayout;

   PGnomeDockLayoutClass = ^TGnomeDockLayoutClass;
   TGnomeDockLayoutClass = record
        parent_class : TGtkObjectClass;
     end;
   GNOME_DOCK_LAYOUT_CLASS = PGnomeDockLayoutClass;

type
   PGnomeDockBand = ^TGnomeDockBand;
   TGnomeDockBand = record
        container : TGtkContainer;
        orientation : TGtkOrientation;
        children : PGList;
        num_children : guint;
        floating_child : PGList;
        doing_drag : gboolean;
        max_space_requisition : guint;
        tot_offsets : guint;
        drag_allocation : TGtkAllocation;
        flag0 : word;
     end;
   GNOME_DOCK_BAND = PGnomeDockBand;

const
   bm__GnomeDockBand_new_for_drag = $1;
   bp__GnomeDockBand_new_for_drag = 0;

function new_for_drag(var a : TGnomeDockBand) : Boolean;
procedure set_new_for_drag(var a : TGnomeDockBand; __new_for_drag : Boolean);

type
   PGnomeDockBandClass = ^TGnomeDockBandClass;
   TGnomeDockBandClass = record
        parent_class : TGtkContainerClass;
     end;
   GNOME_DOCK_BAND_CLASS = PGnomeDockBandClass;

   PGnomeDockBandChild = ^TGnomeDockBandChild;
   TGnomeDockBandChild = record
        widget : PGtkWidget;
        offset : guint;
        real_offset : guint;
        drag_offset : guint;
        drag_allocation : TGtkAllocation;
        prev_space : guint;
        foll_space : guint;
        drag_prev_space : guint;
        drag_foll_space : guint;
        max_space_requisition : guint;
     end;
   GNOME_DOCK_BAND_CHILD = PGnomeDockBandChild;

type
   PGnomeDock = ^TGnomeDock;
   TGnomeDock = record
        container : TGtkContainer;
        client_area : PGtkWidget;
        top_bands : PGList;
        bottom_bands : PGList;
        right_bands : PGList;
        left_bands : PGList;
        floating_children : PGList;
        client_rect : TGtkAllocation;
        flag0 : word;
     end;
   GNOME_DOCK = PGnomeDock;

const
   bm__GnomeDock_floating_items_allowed = $1;
   bp__GnomeDock_floating_items_allowed = 0;

function floating_items_allowed(var a : TGnomeDock) : gboolean;
procedure set_floating_items_allowed(var a : TGnomeDock; __floating_items_allowed : gboolean);

type
   PGnomeDockClass = ^TGnomeDockClass;
   TGnomeDockClass = record
        parent_class : TGtkContainerClass;
        layout_changed : procedure (dock:PGnomeDock);cdecl;
     end;
   GNOME_DOCK_CLASS = PGnomeDockClass;

function GNOME_TYPE_DOCK_ITEM : TGTKType;
function GNOME_IS_DOCK_ITEM(obj : Pointer) : Boolean;
function GNOME_IS_DOCK_ITEM_CLASS(klass : Pointer) : Boolean;

function gnome_dock_item_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_dock_item_get_type';
function gnome_dock_item_new(name:Pgchar; behavior:TGnomeDockItemBehavior):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_dock_item_new';
procedure gnome_dock_item_construct(new_dock_item:PGnomeDockItem; name:Pgchar; behavior:TGnomeDockItemBehavior);cdecl;external libgnomeuidll name 'gnome_dock_item_construct';
function gnome_dock_item_get_child(dock_item:PGnomeDockItem):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_dock_item_get_child';
function gnome_dock_item_get_name(dock_item:PGnomeDockItem):Pchar;cdecl;external libgnomeuidll name 'gnome_dock_item_get_name';
procedure gnome_dock_item_set_shadow_type(dock_item:PGnomeDockItem; thetype:TGtkShadowType);cdecl;external libgnomeuidll name 'gnome_dock_item_set_shadow_type';
function gnome_dock_item_get_shadow_type(dock_item:PGnomeDockItem):TGtkShadowType;cdecl;external libgnomeuidll name 'gnome_dock_item_get_shadow_type';
function gnome_dock_item_set_orientation(dock_item:PGnomeDockItem; orientation:TGtkOrientation):gboolean;cdecl;external libgnomeuidll name 'gnome_dock_item_set_orientation';
function gnome_dock_item_get_orientation(dock_item:PGnomeDockItem):TGtkOrientation;cdecl;external libgnomeuidll name 'gnome_dock_item_get_orientation';
function gnome_dock_item_get_behavior(dock_item:PGnomeDockItem):TGnomeDockItemBehavior;cdecl;external libgnomeuidll name 'gnome_dock_item_get_behavior';
function gnome_dock_item_detach(item:PGnomeDockItem; x:gint; y:gint):gboolean;cdecl;external libgnomeuidll name 'gnome_dock_item_detach';
procedure gnome_dock_item_attach(item:PGnomeDockItem; parent:PGtkWidget; x:gint; y:gint);cdecl;external libgnomeuidll name 'gnome_dock_item_attach';
procedure gnome_dock_item_grab_pointer(item:PGnomeDockItem);cdecl;external libgnomeuidll name 'gnome_dock_item_grab_pointer';
procedure gnome_dock_item_drag_floating(item:PGnomeDockItem; x:gint; y:gint);cdecl;external libgnomeuidll name 'gnome_dock_item_drag_floating';
procedure gnome_dock_item_handle_size_request(item:PGnomeDockItem; requisition:PGtkRequisition);cdecl;external libgnomeuidll name 'gnome_dock_item_handle_size_request';
procedure gnome_dock_item_get_floating_position(item:PGnomeDockItem; x:Pgint; y:Pgint);cdecl;external libgnomeuidll name 'gnome_dock_item_get_floating_position';

function GNOME_TYPE_DOCK_LAYOUT : TGTKType;
function GNOME_IS_DOCK_LAYOUT(obj : pointer) : Boolean;
function GNOME_IS_DOCK_LAYOUT_CLASS(klass : pointer) : boolean;

function gnome_dock_layout_new:PGnomeDockLayout;cdecl;external libgnomeuidll name 'gnome_dock_layout_new';
function gnome_dock_layout_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_dock_layout_get_type';
function gnome_dock_layout_add_item(layout:PGnomeDockLayout; item:PGnomeDockItem; placement:TGnomeDockPlacement; band_num:gint; band_position:gint;
           offset:gint):gboolean;cdecl;external libgnomeuidll name 'gnome_dock_layout_add_item';
function gnome_dock_layout_add_floating_item(layout:PGnomeDockLayout; item:PGnomeDockItem; x:gint; y:gint; orientation:TGtkOrientation):gboolean;cdecl;external libgnomeuidll name 'gnome_dock_layout_add_floating_item';
function gnome_dock_layout_get_item(layout:PGnomeDockLayout; item:PGnomeDockItem):PGnomeDockLayoutItem;cdecl;external libgnomeuidll name 'gnome_dock_layout_get_item';
function gnome_dock_layout_get_item_by_name(layout:PGnomeDockLayout; name:Pgchar):PGnomeDockLayoutItem;cdecl;external libgnomeuidll name 'gnome_dock_layout_get_item_by_name';
function gnome_dock_layout_remove_item(layout:PGnomeDockLayout; item:PGnomeDockItem):gboolean;cdecl;external libgnomeuidll name 'gnome_dock_layout_remove_item';
function gnome_dock_layout_remove_item_by_name(layout:PGnomeDockLayout; name:Pgchar):gboolean;cdecl;external libgnomeuidll name 'gnome_dock_layout_remove_item_by_name';
function gnome_dock_layout_create_string(layout:PGnomeDockLayout):Pgchar;cdecl;external libgnomeuidll name 'gnome_dock_layout_create_string';
function gnome_dock_layout_parse_string(layout:PGnomeDockLayout; _string:Pgchar):gboolean;cdecl;external libgnomeuidll name 'gnome_dock_layout_parse_string';
function gnome_dock_layout_add_to_dock(layout:PGnomeDockLayout; dock:PGnomeDock):gboolean;cdecl;external libgnomeuidll name 'gnome_dock_layout_add_to_dock';

function GNOME_TYPE_DOCK_BAND : TGTKType;
function GNOME_IS_DOCK_BAND(obj : Pointer) : Boolean;
function GNOME_IS_DOCK_BAND_CLASS(klass : Pointer) : Boolean;

function gnome_dock_band_new:PGtkWidget;cdecl;external libgnomeuidll name 'gnome_dock_band_new';
function gnome_dock_band_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_dock_band_get_type';
procedure gnome_dock_band_set_orientation(band:PGnomeDockBand; orientation:TGtkOrientation);cdecl;external libgnomeuidll name 'gnome_dock_band_set_orientation';
function gnome_dock_band_get_orientation(band:PGnomeDockBand):TGtkOrientation;cdecl;external libgnomeuidll name 'gnome_dock_band_get_orientation';
function gnome_dock_band_insert(band:PGnomeDockBand; child:PGtkWidget; offset:guint; position:gint):gboolean;cdecl;external libgnomeuidll name 'gnome_dock_band_insert';
function gnome_dock_band_prepend(band:PGnomeDockBand; child:PGtkWidget; offset:guint):gboolean;cdecl;external libgnomeuidll name 'gnome_dock_band_prepend';
function gnome_dock_band_append(band:PGnomeDockBand; child:PGtkWidget; offset:guint):gboolean;cdecl;external libgnomeuidll name 'gnome_dock_band_append';
procedure gnome_dock_band_set_child_offset(band:PGnomeDockBand; child:PGtkWidget; offset:guint);cdecl;external libgnomeuidll name 'gnome_dock_band_set_child_offset';
function gnome_dock_band_get_child_offset(band:PGnomeDockBand; child:PGtkWidget):guint;cdecl;external libgnomeuidll name 'gnome_dock_band_get_child_offset';
procedure gnome_dock_band_move_child(band:PGnomeDockBand; old_child:PGList; new_num:guint);cdecl;external libgnomeuidll name 'gnome_dock_band_move_child';
function gnome_dock_band_get_num_children(band:PGnomeDockBand):guint;cdecl;external libgnomeuidll name 'gnome_dock_band_get_num_children';
procedure gnome_dock_band_drag_begin(band:PGnomeDockBand; item:PGnomeDockItem);cdecl;external libgnomeuidll name 'gnome_dock_band_drag_begin';
function gnome_dock_band_drag_to(band:PGnomeDockBand; item:PGnomeDockItem; x:gint; y:gint):gboolean;cdecl;external libgnomeuidll name 'gnome_dock_band_drag_to';
procedure gnome_dock_band_drag_end(band:PGnomeDockBand; item:PGnomeDockItem);cdecl;external libgnomeuidll name 'gnome_dock_band_drag_end';
function gnome_dock_band_get_item_by_name(band:PGnomeDockBand; name:Pchar; position_return:Pguint; offset_return:Pguint):PGnomeDockItem;cdecl;external libgnomeuidll name 'gnome_dock_band_get_item_by_name';
procedure gnome_dock_band_layout_add(band:PGnomeDockBand; layout:PGnomeDockLayout; placement:TGnomeDockPlacement; band_num:guint);cdecl;external libgnomeuidll name 'gnome_dock_band_layout_add';

function GNOME_TYPE_DOCK : TGTKType;
function GNOME_IS_DOCK(obj : Pointer) : Boolean;
function GNOME_IS_DOCK_CLASS(klass : Pointer) : Boolean;

function gnome_dock_new:PGtkWidget;cdecl;external libgnomeuidll name 'gnome_dock_new';
function gnome_dock_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_dock_get_type';
procedure gnome_dock_allow_floating_items(dock:PGnomeDock; enable:gboolean);cdecl;external libgnomeuidll name 'gnome_dock_allow_floating_items';
procedure gnome_dock_add_item(dock:PGnomeDock; item:PGnomeDockItem; placement:TGnomeDockPlacement; band_num:guint; position:gint;
            offset:guint; in_new_band:gboolean);cdecl;external libgnomeuidll name 'gnome_dock_add_item';
procedure gnome_dock_add_floating_item(dock:PGnomeDock; widget:PGnomeDockItem; x:gint; y:gint; orientation:TGtkOrientation);cdecl;external libgnomeuidll name 'gnome_dock_add_floating_item';
procedure gnome_dock_set_client_area(dock:PGnomeDock; widget:PGtkWidget);cdecl;external libgnomeuidll name 'gnome_dock_set_client_area';
function gnome_dock_get_client_area(dock:PGnomeDock):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_dock_get_client_area';
function gnome_dock_get_item_by_name(dock:PGnomeDock; name:Pgchar; placement_return:PGnomeDockPlacement; num_band_return:Pguint; band_position_return:Pguint;
           offset_return:Pguint):PGnomeDockItem;cdecl;external libgnomeuidll name 'gnome_dock_get_item_by_name';
function gnome_dock_get_layout(dock:PGnomeDock):PGnomeDockLayout;cdecl;external libgnomeuidll name 'gnome_dock_get_layout';
function gnome_dock_add_from_layout(dock:PGnomeDock; layout:PGnomeDockLayout):gboolean;cdecl;external libgnomeuidll name 'gnome_dock_add_from_layout';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_DOCK_ITEM : TGTKType;
begin
  GNOME_TYPE_DOCK_ITEM:=gnome_dock_band_get_type;
end;

function GNOME_IS_DOCK_ITEM(obj : pointer) : Boolean;
begin
   GNOME_IS_DOCK_ITEM:=(obj<>nil) and GNOME_IS_DOCK_ITEM_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_DOCK_ITEM_CLASS(klass : pointer) : boolean;
begin
   GNOME_IS_DOCK_ITEM_CLASS:= (klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_DOCK_ITEM);
end;

function GNOME_DOCK_ITEM_NOT_LOCKED(x : PGnomeDockItem) : gboolean;
begin
   GNOME_DOCK_ITEM_NOT_LOCKED:= not gboolean(gint(x^.behavior) and gint(GNOME_DOCK_ITEM_BEH_LOCKED));
end;

function float_window_mapped(var a : TGnomeDockItem) : gboolean;
begin
   float_window_mapped:=gboolean((a.flag0 and bm__GnomeDockItem_float_window_mapped) shr bp__GnomeDockItem_float_window_mapped);
end;

procedure set_float_window_mapped(var a : TGnomeDockItem; __float_window_mapped : gboolean);
begin
   a.flag0:=a.flag0 or ((gint(__float_window_mapped) shl bp__GnomeDockItem_float_window_mapped) and bm__GnomeDockItem_float_window_mapped);
end;

function is_floating(var a : TGnomeDockItem) : gboolean;
begin
   is_floating:=gboolean((a.flag0 and bm__GnomeDockItem_is_floating) shr bp__GnomeDockItem_is_floating);
end;

procedure set_is_floating(var a : TGnomeDockItem; __is_floating : gboolean);
begin
   a.flag0:=a.flag0 or ((gint(__is_floating) shl bp__GnomeDockItem_is_floating) and bm__GnomeDockItem_is_floating);
end;

function in_drag(var a : TGnomeDockItem) : gboolean;
begin
   in_drag:=gboolean((a.flag0 and bm__GnomeDockItem_in_drag) shr bp__GnomeDockItem_in_drag);
end;

procedure set_in_drag(var a : TGnomeDockItem; __in_drag : gboolean);
begin
   a.flag0:=a.flag0 or ((gint(__in_drag) shl bp__GnomeDockItem_in_drag) and bm__GnomeDockItem_in_drag);
end;

function GNOME_TYPE_DOCK_LAYOUT : TGTKType;
begin
  GNOME_TYPE_DOCK_LAYOUT:=gnome_dock_layout_get_type;
end;

function GNOME_IS_DOCK_LAYOUT(obj : pointer) : Boolean;
begin
   GNOME_IS_DOCK_LAYOUT:=(obj<>nil) and GNOME_IS_DOCK_LAYOUT_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_DOCK_LAYOUT_CLASS(klass : pointer) : boolean;
begin
   GNOME_IS_DOCK_LAYOUT_CLASS:= (klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_DOCK_LAYOUT);
end;

function GNOME_TYPE_DOCK_BAND : TGTKType;
begin
  GNOME_TYPE_DOCK_BAND:=gnome_dock_band_get_type;
end;

function GNOME_IS_DOCK_BAND(obj : pointer) : Boolean;
begin
   GNOME_IS_DOCK_BAND:=(obj<>nil) and GNOME_IS_DOCK_BAND_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_DOCK_BAND_CLASS(klass : pointer) : boolean;
begin
   GNOME_IS_DOCK_BAND_CLASS:= (klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_DOCK_BAND);
end;

function new_for_drag(var a : TGnomeDockBand) : boolean;
begin
   new_for_drag:=boolean((a.flag0 and bm__GnomeDockBand_new_for_drag) shr bp__GnomeDockBand_new_for_drag);
end;

procedure set_new_for_drag(var a : TGnomeDockBand; __new_for_drag : boolean);
begin
   a.flag0:=a.flag0 or ((longint(__new_for_drag) shl bp__GnomeDockBand_new_for_drag) and bm__GnomeDockBand_new_for_drag);
end;

function GNOME_TYPE_DOCK : TGTKType;
begin
  GNOME_TYPE_DOCK:=gnome_dock_get_type;
end;

function GNOME_IS_DOCK(obj : pointer) : Boolean;
begin
   GNOME_IS_DOCK:=(obj<>nil) and GNOME_IS_DOCK_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_DOCK_CLASS(klass : pointer) : boolean;
begin
   GNOME_IS_DOCK_CLASS:= (klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_DOCK);
end;

function floating_items_allowed(var a : TGnomeDock) : gboolean;
begin
   floating_items_allowed:=gBoolean((a.flag0 and bm__GnomeDock_floating_items_allowed) shr bp__GnomeDock_floating_items_allowed);
end;

procedure set_floating_items_allowed(var a : TGnomeDock; __floating_items_allowed : gboolean);
begin
   a.flag0:=a.flag0 or ((gint(__floating_items_allowed) shl bp__GnomeDock_floating_items_allowed) and bm__GnomeDock_floating_items_allowed);
end;

{$Endif read_implementation}
