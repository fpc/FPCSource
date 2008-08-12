// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkIconFactory = ^TGtkIconFactory;
   TGtkIconFactory = record
        parent_instance : TGObject;
        icons : PGHashTable;
     end;

{ Padding for future expansion  }
   PGtkIconFactoryClass = ^TGtkIconFactoryClass;
   TGtkIconFactoryClass = record
        parent_class : TGObjectClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_ICON_FACTORY : GType;
function GTK_ICON_FACTORY(anObject: pointer) : PGtkIconFactory;
function GTK_ICON_FACTORY_CLASS(klass: pointer) : PGtkIconFactoryClass;
function GTK_IS_ICON_FACTORY(anObject: pointer) : boolean;
function GTK_IS_ICON_FACTORY_CLASS(klass: pointer) : boolean;
function GTK_ICON_FACTORY_GET_CLASS(obj: pointer) : PGtkIconFactoryClass;

function GTK_TYPE_ICON_SET : GType;
function GTK_TYPE_ICON_SOURCE : GType;

function gtk_icon_factory_get_type:GType; cdecl; external gtklib;
function gtk_icon_factory_new:PGtkIconFactory; cdecl; external gtklib;
procedure gtk_icon_factory_add(factory:PGtkIconFactory; stock_id:Pgchar; icon_set:PGtkIconSet); cdecl; external gtklib;
function gtk_icon_factory_lookup(factory:PGtkIconFactory; stock_id:Pgchar):PGtkIconSet; cdecl; external gtklib;
{ Manage the default icon factory stack  }
procedure gtk_icon_factory_add_default(factory:PGtkIconFactory); cdecl; external gtklib;
procedure gtk_icon_factory_remove_default(factory:PGtkIconFactory); cdecl; external gtklib;
function gtk_icon_factory_lookup_default(stock_id:Pgchar):PGtkIconSet; cdecl; external gtklib;
{ Get preferred real size from registered semantic size.  Note that
   themes SHOULD use this size, but they aren't required to; for size
   requests and such, you should get the actual pixbuf from the icon
   set and see what size was rendered.

   This function is intended for people who are scaling icons,
   rather than for people who are displaying already-scaled icons.
   That is, if you are displaying an icon, you should get the
   size from the rendered pixbuf, not from here.
  }
function gtk_icon_size_lookup(size:TGtkIconSize; width:Pgint; height:Pgint):gboolean; cdecl; external gtklib;
function gtk_icon_size_register(name:Pgchar; width:gint; height:gint):TGtkIconSize; cdecl; external gtklib;
procedure gtk_icon_size_register_alias(alias:Pgchar; target:TGtkIconSize); cdecl; external gtklib;
function gtk_icon_size_from_name(name:Pgchar):TGtkIconSize; cdecl; external gtklib;
function gtk_icon_size_get_name(size:TGtkIconSize):Pgchar; cdecl; external gtklib;
{ Icon sets  }
function gtk_icon_set_get_type:GType; cdecl; external gtklib;
function gtk_icon_set_new:PGtkIconSet; cdecl; external gtklib;
function gtk_icon_set_new_from_pixbuf(pixbuf:PGdkPixbuf):PGtkIconSet; cdecl; external gtklib;
function gtk_icon_set_ref(icon_set:PGtkIconSet):PGtkIconSet; cdecl; external gtklib;
procedure gtk_icon_set_unref(icon_set:PGtkIconSet); cdecl; external gtklib;
function gtk_icon_set_copy(icon_set:PGtkIconSet):PGtkIconSet; cdecl; external gtklib;
{ Get one of the icon variants in the set, creating the variant if
   necessary.
  }
function gtk_icon_set_render_icon(icon_set:PGtkIconSet; style:PGtkStyle; direction:TGtkTextDirection; state:TGtkStateType; size:TGtkIconSize;
           widget:PGtkWidget; detail:Pchar):PGdkPixbuf; cdecl; external gtklib;
procedure gtk_icon_set_add_source(icon_set:PGtkIconSet; source:PGtkIconSource); cdecl; external gtklib;
procedure gtk_icon_set_get_sizes(icon_set:PGtkIconSet; sizes:PPGtkIconSize; n_sizes:pgint); cdecl; external gtklib;
function gtk_icon_source_get_type:GType; cdecl; external gtklib;
function gtk_icon_source_new:PGtkIconSource; cdecl; external gtklib;
function gtk_icon_source_copy(source:PGtkIconSource):PGtkIconSource; cdecl; external gtklib;
procedure gtk_icon_source_free(source:PGtkIconSource); cdecl; external gtklib;
procedure gtk_icon_source_set_filename(source:PGtkIconSource; filename:Pgchar); cdecl; external gtklib;
procedure gtk_icon_source_set_pixbuf(source:PGtkIconSource; pixbuf:PGdkPixbuf); cdecl; external gtklib;
function gtk_icon_source_get_filename(source:PGtkIconSource):Pgchar; cdecl; external gtklib;
function gtk_icon_source_get_pixbuf(source:PGtkIconSource):PGdkPixbuf; cdecl; external gtklib;
procedure gtk_icon_source_set_direction_wildcarded(source:PGtkIconSource; setting:gboolean); cdecl; external gtklib;
procedure gtk_icon_source_set_state_wildcarded(source:PGtkIconSource; setting:gboolean); cdecl; external gtklib;
procedure gtk_icon_source_set_size_wildcarded(source:PGtkIconSource; setting:gboolean); cdecl; external gtklib;
function gtk_icon_source_get_size_wildcarded(source:PGtkIconSource):gboolean; cdecl; external gtklib;
function gtk_icon_source_get_state_wildcarded(source:PGtkIconSource):gboolean; cdecl; external gtklib;
function gtk_icon_source_get_direction_wildcarded(source:PGtkIconSource):gboolean; cdecl; external gtklib;
procedure gtk_icon_source_set_direction(source:PGtkIconSource; direction:TGtkTextDirection); cdecl; external gtklib;
procedure gtk_icon_source_set_state(source:PGtkIconSource; state:TGtkStateType); cdecl; external gtklib;
procedure gtk_icon_source_set_size(source:PGtkIconSource; size:TGtkIconSize); cdecl; external gtklib;
function gtk_icon_source_get_direction(source:PGtkIconSource):TGtkTextDirection; cdecl; external gtklib;
function gtk_icon_source_get_state(source:PGtkIconSource):TGtkStateType; cdecl; external gtklib;
function gtk_icon_source_get_size(source:PGtkIconSource):TGtkIconSize; cdecl; external gtklib;
{ ignore this  }
procedure _gtk_icon_set_invalidate_caches; cdecl; external gtklib;
function _gtk_icon_factory_list_ids:PGSList; cdecl; external gtklib;

{$IFDEF HasGTK2_2}
function gtk_icon_size_lookup_for_settings(settings: PGtkSettings; size: TGtkIconSize; width: Pgint; height: Pgint): gboolean; cdecl; external gtklib;
{$ENDIF}

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_ICON_FACTORY : GType;
begin
   GTK_TYPE_ICON_FACTORY:=gtk_icon_factory_get_type;
end;

function GTK_ICON_FACTORY(anObject: pointer) : PGtkIconFactory;
begin
   GTK_ICON_FACTORY:=PGtkIconFactory(G_TYPE_CHECK_INSTANCE_CAST(anObject,GTK_TYPE_ICON_FACTORY));
end;

function GTK_ICON_FACTORY_CLASS(klass: pointer) : PGtkIconFactoryClass;
begin
   GTK_ICON_FACTORY_CLASS:=PGtkIconFactoryClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_ICON_FACTORY));
end;

function GTK_IS_ICON_FACTORY(anObject: pointer) : boolean;
begin
   GTK_IS_ICON_FACTORY:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,GTK_TYPE_ICON_FACTORY);
end;

function GTK_IS_ICON_FACTORY_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_ICON_FACTORY_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_ICON_FACTORY);
end;

function GTK_ICON_FACTORY_GET_CLASS(obj: pointer) : PGtkIconFactoryClass;
begin
   GTK_ICON_FACTORY_GET_CLASS:=PGtkIconFactoryClass(G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_ICON_FACTORY));
end;

function GTK_TYPE_ICON_SET : GType;
begin
   GTK_TYPE_ICON_SET:=gtk_icon_set_get_type;
end;

function GTK_TYPE_ICON_SOURCE : GType;
begin
   GTK_TYPE_ICON_SOURCE:=gtk_icon_source_get_type;
end;

{$ENDIF read_implementation}
// included by gtk2.pas

