{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkItem = ^TGtkItem;
       TGtkItem = record
            bin : TGtkBin;
         end;

       PGtkItemClass = ^TGtkItemClass;
       TGtkItemClass = record
            parent_class : TGtkBinClass;
            select : procedure (item:PGtkItem); cdecl;
            deselect : procedure (item:PGtkItem); cdecl;
            toggle : procedure (item:PGtkItem); cdecl;
         end;

Type
  GTK_ITEM=PGtkItem;
  GTK_ITEM_CLASS=PGtkItemClass;

function  GTK_ITEM_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_item_get_type';
function  GTK_IS_ITEM(obj:pointer):boolean;
function  GTK_IS_ITEM_CLASS(klass:pointer):boolean;

function  gtk_item_get_type:TGtkType;cdecl;external gtkdll name 'gtk_item_get_type';
procedure gtk_item_select(item:PGtkItem);cdecl;external gtkdll name 'gtk_item_select';
procedure gtk_item_deselect(item:PGtkItem);cdecl;external gtkdll name 'gtk_item_deselect';
procedure gtk_item_toggle(item:PGtkItem);cdecl;external gtkdll name 'gtk_item_toggle';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_ITEM(obj:pointer):boolean;
begin
  GTK_IS_ITEM:=(obj<>nil) and GTK_IS_ITEM_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_ITEM_CLASS(klass:pointer):boolean;
begin
  GTK_IS_ITEM_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_ITEM_TYPE);
end;

{$endif read_implementation}


