// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkItem = ^TGtkItem;
   TGtkItem = record
        bin : TGtkBin;
     end;

{ Padding for future expansion  }
   PGtkItemClass = ^TGtkItemClass;
   TGtkItemClass = record
        parent_class : TGtkBinClass;
        select : procedure (item:PGtkItem); cdecl;
        deselect : procedure (item:PGtkItem); cdecl;
        toggle : procedure (item:PGtkItem); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_ITEM : GType;
function GTK_ITEM(obj: pointer) : PGtkItem;
function GTK_ITEM_CLASS(klass: pointer) : PGtkItemClass;
function GTK_IS_ITEM(obj: pointer) : boolean;
function GTK_IS_ITEM_CLASS(klass: pointer) : boolean;
function GTK_ITEM_GET_CLASS(obj: pointer) : PGtkItemClass;


function gtk_item_get_type:TGtkType; cdecl; external gtklib;
procedure gtk_item_select(item:PGtkItem); cdecl; external gtklib;
procedure gtk_item_deselect(item:PGtkItem); cdecl; external gtklib;
procedure gtk_item_toggle(item:PGtkItem); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_ITEM : GType;
begin
   GTK_TYPE_ITEM:=gtk_item_get_type;
end;

function GTK_ITEM(obj: pointer) : PGtkItem;
begin
   GTK_ITEM:=PGtkItem(GTK_CHECK_CAST(obj,GTK_TYPE_ITEM));
end;

function GTK_ITEM_CLASS(klass: pointer) : PGtkItemClass;
begin
   GTK_ITEM_CLASS:=PGtkItemClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_ITEM));
end;

function GTK_IS_ITEM(obj: pointer) : boolean;
begin
   GTK_IS_ITEM:=GTK_CHECK_TYPE(obj,GTK_TYPE_ITEM);
end;

function GTK_IS_ITEM_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_ITEM_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_ITEM);
end;

function GTK_ITEM_GET_CLASS(obj: pointer) : PGtkItemClass;
begin
   GTK_ITEM_GET_CLASS:=PGtkItemClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_ITEM));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

