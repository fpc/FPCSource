{$IfDef read_interface}

type
   PGnomeIconTextItem = ^TGnomeIconTextItem;
   TGnomeIconTextItem = record
        canvas_item : TGnomeCanvasItem;
        x : longint;
        y : longint;
        width : longint;
        fontname : pchar;
        priv : gpointer;
        pad1 : gpointer;
        pad2 : gpointer;
        thetext : pchar;
        ti : PGnomeIconTextInfo;
        flag0 : word;
        pad3 : dword;
        flag1 : word;
     end;
   GNOME_ICON_TEXT_ITEM = PGnomeIconTextItem;

const
   bm_GnomeIconTextItem_editing = $1;
   bp_GnomeIconTextItem_editing = 0;
   bm_GnomeIconTextItem_selected = $2;
   bp_GnomeIconTextItem_selected = 1;
   bm_GnomeIconTextItem_selecting = $1;
   bp_GnomeIconTextItem_selecting = 0;
   bm_GnomeIconTextItem_is_editable = $2;
   bp_GnomeIconTextItem_is_editable = 1;
   bm_GnomeIconTextItem_is_text_allocated = $4;
   bp_GnomeIconTextItem_is_text_allocated = 2;

function editing(var a : TGnomeIconTextItem) : dword;
procedure set_editing(var a : TGnomeIconTextItem; __editing : dword);
function selected(var a : TGnomeIconTextItem) : dword;
procedure set_selected(var a : TGnomeIconTextItem; __selected : dword);
function selecting(var a : TGnomeIconTextItem) : dword;
procedure set_selecting(var a : TGnomeIconTextItem; __selecting : dword);
function is_editable(var a : TGnomeIconTextItem) : dword;
procedure set_is_editable(var a : TGnomeIconTextItem; __is_editable : dword);
function is_text_allocated(var a : TGnomeIconTextItem) : dword;
procedure set_is_text_allocated(var a : TGnomeIconTextItem; __is_text_allocated : dword);

type
   PGnomeIconTextItemClass = ^TGnomeIconTextItemClass;
   TGnomeIconTextItemClass = record
        parent_class : TGnomeCanvasItemClass;
        text_changed : function (iti:PGnomeIconTextItem):longint;cdecl;
        height_changed : procedure (iti:PGnomeIconTextItem);cdecl;
        width_changed : procedure (iti:PGnomeIconTextItem);cdecl;
        editing_started : procedure (iti:PGnomeIconTextItem);cdecl;
        editing_stopped : procedure (iti:PGnomeIconTextItem);cdecl;
        selection_started : procedure (iti:PGnomeIconTextItem);cdecl;
        selection_stopped : procedure (iti:PGnomeIconTextItem);cdecl;
     end;
   GNOME_ICON_TEXT_ITEM_CLASS = PGnomeIconTextItemClass;

function GNOME_TYPE_ICON_TEXT_ITEM : TGTKType;
function GNOME_IS_ICON_TEXT_ITEM(obj : Pointer) : Boolean;
function GNOME_IS_ICON_TEXT_ITEM_CLASS(klass : Pointer) : Boolean;

function gnome_icon_text_item_get_type:TGtkType;cdecl;external libgnomeuidll name 'gnome_icon_text_item_get_type';
procedure gnome_icon_text_item_configure(iti:PGnomeIconTextItem; x:longint; y:longint; width:longint; fontname:Pchar;
            thetext:Pchar; is_editable:gboolean; is_static:gboolean);cdecl;external libgnomeuidll name 'gnome_icon_text_item_configure';
procedure gnome_icon_text_item_setxy(iti:PGnomeIconTextItem; x:longint; y:longint);cdecl;external libgnomeuidll name 'gnome_icon_text_item_setxy';
procedure gnome_icon_text_item_select(iti:PGnomeIconTextItem; sel:longint);cdecl;external libgnomeuidll name 'gnome_icon_text_item_select';
function gnome_icon_text_item_get_text(iti:PGnomeIconTextItem):Pchar;cdecl;external libgnomeuidll name 'gnome_icon_text_item_get_text';
procedure gnome_icon_text_item_start_editing(iti:PGnomeIconTextItem);cdecl;external libgnomeuidll name 'gnome_icon_text_item_start_editing';
procedure gnome_icon_text_item_stop_editing(iti:PGnomeIconTextItem; accept:gboolean);cdecl;external libgnomeuidll name 'gnome_icon_text_item_stop_editing';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_ICON_TEXT_ITEM : TGTKType;
begin
  GNOME_TYPE_ICON_TEXT_ITEM:=gnome_icon_text_item_get_type;
end;

function GNOME_IS_ICON_TEXT_ITEM(obj : Pointer) : Boolean;
begin
   GNOME_IS_ICON_TEXT_ITEM:=(obj<>nil) and GNOME_IS_ICON_TEXT_ITEM_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_ICON_TEXT_ITEM_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_ICON_TEXT_ITEM_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_ICON_TEXT_ITEM);
end;

function editing(var a : TGnomeIconTextItem) : dword;
begin
   editing:=(a.flag0 and bm_GnomeIconTextItem_editing) shr bp_GnomeIconTextItem_editing;
end;

procedure set_editing(var a : TGnomeIconTextItem; __editing : dword);
begin
   a.flag0:=a.flag0 or ((__editing shl bp_GnomeIconTextItem_editing) and bm_GnomeIconTextItem_editing);
end;

function selected(var a : TGnomeIconTextItem) : dword;
begin
   selected:=(a.flag0 and bm_GnomeIconTextItem_selected) shr bp_GnomeIconTextItem_selected;
end;

procedure set_selected(var a : TGnomeIconTextItem; __selected : dword);
begin
   a.flag0:=a.flag0 or ((__selected shl bp_GnomeIconTextItem_selected) and bm_GnomeIconTextItem_selected);
end;

function selecting(var a : TGnomeIconTextItem) : dword;
begin
   selecting:=(a.flag1 and bm_GnomeIconTextItem_selecting) shr bp_GnomeIconTextItem_selecting;
end;

procedure set_selecting(var a : TGnomeIconTextItem; __selecting : dword);
begin
   a.flag1:=a.flag1 or ((__selecting shl bp_GnomeIconTextItem_selecting) and bm_GnomeIconTextItem_selecting);
end;

function is_editable(var a : TGnomeIconTextItem) : dword;
begin
   is_editable:=(a.flag1 and bm_GnomeIconTextItem_is_editable) shr bp_GnomeIconTextItem_is_editable;
end;

procedure set_is_editable(var a : TGnomeIconTextItem; __is_editable : dword);
begin
   a.flag1:=a.flag1 or ((__is_editable shl bp_GnomeIconTextItem_is_editable) and bm_GnomeIconTextItem_is_editable);
end;

function is_text_allocated(var a : TGnomeIconTextItem) : dword;
begin
   is_text_allocated:=(a.flag1 and bm_GnomeIconTextItem_is_text_allocated) shr bp_GnomeIconTextItem_is_text_allocated;
end;

procedure set_is_text_allocated(var a : TGnomeIconTextItem; __is_text_allocated : dword);
begin
   a.flag1:=a.flag1 or ((__is_text_allocated shl bp_GnomeIconTextItem_is_text_allocated) and bm_GnomeIconTextItem_is_text_allocated);
end;

{$Endif read_implementation}
