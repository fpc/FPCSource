{$IfDef read_interface}
type
   TGnomeIconListMode = (GNOME_ICON_LIST_ICONS,GNOME_ICON_LIST_TEXT_BELOW,GNOME_ICON_LIST_TEXT_RIGHT);

   PGnomeIconList = ^TGnomeIconList;
   TGnomeIconList = record
        canvas : TGnomeCanvas;
        adj : PGtkAdjustment;
        hadj : PGtkAdjustment;
        icons : longint;
        priv : gpointer;
        pad3 : longint;
        pad4 : longint;
        pad5 : longint;
        pad6 : longint;
        pad7 : longint;
        pad8 : longint;
        pad9 : gpointer;
        pad10 : TGnomeIconListMode;
        pad11 : TGtkSelectionMode;
        selection : PGList;
        pad12 : gpointer;
        pad13 : longint;
        flag0 : word;
        pad16 : longint;
        pad17 : gpointer;
        pad18 : longint;
        pad19 : longint;
        pad20 : gdouble;
        pad21 : gdouble;
        pad22 : gpointer;
        pad23 : longint;
        pad24 : double;
        pad25 : double;
        pad26 : gpointer;
     end;
   GNOME_ICON_LIST = ^TGnomeIconList;

const
   bm_GnomeIconList_pad14 = $1;
   bp_GnomeIconList_pad14 = 0;
   bm_GnomeIconList_pad15 = $2;
   bp_GnomeIconList_pad15 = 1;

function pad14(var a : TGnomeIconList) : dword;
procedure set_pad14(var a : TGnomeIconList; __pad14 : dword);
function pad15(var a : TGnomeIconList) : dword;
procedure set_pad15(var a : TGnomeIconList; __pad15 : dword);

type
   PGnomeIconListClass = ^TGnomeIconListClass;
   TGnomeIconListClass = record
        parent_class : TGnomeCanvasClass;
        select_icon : procedure (gil:PGnomeIconList; num:gint; event:PGdkEvent);cdecl;
        unselect_icon : procedure (gil:PGnomeIconList; num:gint; event:PGdkEvent);cdecl;
        text_changed : function (gil:PGnomeIconList; num:gint; new_text:Pchar):gboolean;cdecl;
     end;

const
   GNOME_ICON_LIST_IS_EDITABLE = 1;
   GNOME_ICON_LIST_STATIC_TEXT = 2;

function GNOME_TYPE_ICON_LIST : TGTKType;
function GNOME_IS_ICON_LIST(obj : Pointer) : Boolean;
function GNOME_IS_ICON_LIST_CLASS(klass : Pointer) : Boolean;

function gnome_icon_list_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_icon_list_get_type';
function gnome_icon_list_new(icon_width:guint; adj:PGtkAdjustment; flags:longint):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_icon_list_new';
function gnome_icon_list_new_flags(icon_width:guint; adj:PGtkAdjustment; flags:longint):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_icon_list_new_flags';
procedure gnome_icon_list_construct(gil:PGnomeIconList; icon_width:guint; adj:PGtkAdjustment; flags:longint);cdecl;external libgnomeuidll name 'gnome_icon_list_construct';
procedure gnome_icon_list_set_hadjustment(gil:PGnomeIconList; hadj:PGtkAdjustment);cdecl;external libgnomeuidll name 'gnome_icon_list_set_hadjustment';
procedure gnome_icon_list_set_vadjustment(gil:PGnomeIconList; vadj:PGtkAdjustment);cdecl;external libgnomeuidll name 'gnome_icon_list_set_vadjustment';
procedure gnome_icon_list_freeze(gil:PGnomeIconList);cdecl;external libgnomeuidll name 'gnome_icon_list_freeze';
procedure gnome_icon_list_thaw(gil:PGnomeIconList);cdecl;external libgnomeuidll name 'gnome_icon_list_thaw';
procedure gnome_icon_list_insert(gil:PGnomeIconList; pos:longint; icon_filename:Pchar; thetext:Pchar);cdecl;external libgnomeuidll name 'gnome_icon_list_insert';
procedure gnome_icon_list_insert_imlib(gil:PGnomeIconList; pos:longint; image:PGdkImlibImage; thetext:Pchar);cdecl;external libgnomeuidll name 'gnome_icon_list_insert_imlib';
function gnome_icon_list_append(gil:PGnomeIconList; icon_filename:Pchar; thetext:Pchar):longint;cdecl;external libgnomeuidll name 'gnome_icon_list_append';
function gnome_icon_list_append_imlib(gil:PGnomeIconList; image:PGdkImlibImage; thetext:Pchar):longint;cdecl;external libgnomeuidll name 'gnome_icon_list_append_imlib';
procedure gnome_icon_list_clear(gil:PGnomeIconList);cdecl;external libgnomeuidll name 'gnome_icon_list_clear';
procedure gnome_icon_list_remove(gil:PGnomeIconList; pos:longint);cdecl;external libgnomeuidll name 'gnome_icon_list_remove';
procedure gnome_icon_list_set_selection_mode(gil:PGnomeIconList; mode:TGtkSelectionMode);cdecl;external libgnomeuidll name 'gnome_icon_list_set_selection_mode';
procedure gnome_icon_list_select_icon(gil:PGnomeIconList; idx:longint);cdecl;external libgnomeuidll name 'gnome_icon_list_select_icon';
procedure gnome_icon_list_unselect_icon(gil:PGnomeIconList; pos:longint);cdecl;external libgnomeuidll name 'gnome_icon_list_unselect_icon';
function gnome_icon_list_unselect_all(gil:PGnomeIconList; event:PGdkEvent; keep:gpointer):longint;cdecl;external libgnomeuidll name 'gnome_icon_list_unselect_all';
procedure gnome_icon_list_set_icon_width(gil:PGnomeIconList; w:longint);cdecl;external libgnomeuidll name 'gnome_icon_list_set_icon_width';
procedure gnome_icon_list_set_row_spacing(gil:PGnomeIconList; pixels:longint);cdecl;external libgnomeuidll name 'gnome_icon_list_set_row_spacing';
procedure gnome_icon_list_set_col_spacing(gil:PGnomeIconList; pixels:longint);cdecl;external libgnomeuidll name 'gnome_icon_list_set_col_spacing';
procedure gnome_icon_list_set_text_spacing(gil:PGnomeIconList; pixels:longint);cdecl;external libgnomeuidll name 'gnome_icon_list_set_text_spacing';
procedure gnome_icon_list_set_icon_border(gil:PGnomeIconList; pixels:longint);cdecl;external libgnomeuidll name 'gnome_icon_list_set_icon_border';
procedure gnome_icon_list_set_separators(gil:PGnomeIconList; sep:Pchar);cdecl;external libgnomeuidll name 'gnome_icon_list_set_separators';
procedure gnome_icon_list_set_icon_data(gil:PGnomeIconList; pos:longint; data:gpointer);cdecl;external libgnomeuidll name 'gnome_icon_list_set_icon_data';
procedure gnome_icon_list_set_icon_data_full(gil:PGnomeIconList; pos:longint; data:gpointer; destroy:TGtkDestroyNotify);cdecl;external libgnomeuidll name 'gnome_icon_list_set_icon_data_full';
function gnome_icon_list_find_icon_from_data(gil:PGnomeIconList; data:gpointer):longint;cdecl;external libgnomeuidll name 'gnome_icon_list_find_icon_from_data';
function gnome_icon_list_get_icon_data(gil:PGnomeIconList; pos:longint):gpointer;cdecl;external libgnomeuidll name 'gnome_icon_list_get_icon_data';
procedure gnome_icon_list_moveto(gil:PGnomeIconList; pos:longint; yalign:double);cdecl;external libgnomeuidll name 'gnome_icon_list_moveto';
function gnome_icon_list_icon_is_visible(gil:PGnomeIconList; pos:longint):TGtkVisibility;cdecl;external libgnomeuidll name 'gnome_icon_list_icon_is_visible';
function gnome_icon_list_get_icon_at(gil:PGnomeIconList; x:longint; y:longint):longint;cdecl;external libgnomeuidll name 'gnome_icon_list_get_icon_at';
function gnome_icon_list_get_items_per_line(gil:PGnomeIconList):longint;cdecl;external libgnomeuidll name 'gnome_icon_list_get_items_per_line';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_ICON_LIST : TGTKType;
begin
  GNOME_TYPE_ICON_LIST:=gnome_icon_list_get_type;
end;

function GNOME_IS_ICON_LIST(obj : Pointer) : Boolean;
begin
   GNOME_IS_ICON_LIST:=(obj<>nil) and GNOME_IS_ICON_LIST_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_ICON_LIST_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_ICON_LIST_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_ICON_LIST);
end;

function pad14(var a : TGnomeIconList) : dword;
begin
   pad14:=(a.flag0 and bm_GnomeIconList_pad14) shr bp_GnomeIconList_pad14;
end;

procedure set_pad14(var a : TGnomeIconList; __pad14 : dword);
begin
   a.flag0:=a.flag0 or ((__pad14 shl bp_GnomeIconList_pad14) and bm_GnomeIconList_pad14);
end;

function pad15(var a : TGnomeIconList) : dword;
begin
   pad15:=(a.flag0 and bm_GnomeIconList_pad15) shr bp_GnomeIconList_pad15;
end;

procedure set_pad15(var a : TGnomeIconList; __pad15 : dword);
begin
   a.flag0:=a.flag0 or ((__pad15 shl bp_GnomeIconList_pad15) and bm_GnomeIconList_pad15);
end;

{$Endif read_implementation}
