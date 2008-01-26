// included by gtk2.pas

{$IFDEF read_forward_definitions}
   PGtkTextTagTable = ^TGtkTextTagTable;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   TGtkTextTagTableForeach = procedure (tag:PGtkTextTag; data:gpointer); cdecl;

   TGtkTextTagTable = record
        parent_instance : TGObject;
        hash : PGHashTable;
        anonymous : PGSList;
        anon_count : gint;
        buffers : PGSList;
     end;

{ Padding for future expansion  }
   PGtkTextTagTableClass = ^TGtkTextTagTableClass;
   TGtkTextTagTableClass = record
        parent_class : TGObjectClass;
        tag_changed : procedure (table:PGtkTextTagTable; tag:PGtkTextTag; size_changed:gboolean); cdecl;
        tag_added : procedure (table:PGtkTextTagTable; tag:PGtkTextTag); cdecl;
        tag_removed : procedure (table:PGtkTextTagTable; tag:PGtkTextTag); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_TEXT_TAG_TABLE : GType;
function GTK_TEXT_TAG_TABLE(obj: pointer) : PGtkTextTagTable;
function GTK_TEXT_TAG_TABLE_CLASS(klass: pointer) : PGtkTextTagTableClass;
function GTK_IS_TEXT_TAG_TABLE(obj: pointer) : boolean;
function GTK_IS_TEXT_TAG_TABLE_CLASS(klass: pointer) : boolean;
function GTK_TEXT_TAG_TABLE_GET_CLASS(obj: pointer) : PGtkTextTagTableClass;

function gtk_text_tag_table_get_type:GType; cdecl; external gtklib;
function gtk_text_tag_table_new:PGtkTextTagTable; cdecl; external gtklib;
procedure gtk_text_tag_table_add(table:PGtkTextTagTable; tag:PGtkTextTag); cdecl; external gtklib;
procedure gtk_text_tag_table_remove(table:PGtkTextTagTable; tag:PGtkTextTag); cdecl; external gtklib;

function gtk_text_tag_table_lookup(table:PGtkTextTagTable; name:Pgchar):PGtkTextTag; cdecl; external gtklib;
procedure gtk_text_tag_table_foreach(table:PGtkTextTagTable; func:TGtkTextTagTableForeach; data:gpointer); cdecl; external gtklib;
function gtk_text_tag_table_get_size(table:PGtkTextTagTable):gint; cdecl; external gtklib;
{ INTERNAL private stuff - not even exported from the library on
   many platforms
  }
procedure _gtk_text_tag_table_add_buffer(table:PGtkTextTagTable; buffer:gpointer); cdecl; external gtklib;
procedure _gtk_text_tag_table_remove_buffer(table:PGtkTextTagTable; buffer:gpointer); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_TEXT_TAG_TABLE : GType;
begin
   GTK_TYPE_TEXT_TAG_TABLE:=gtk_text_tag_table_get_type;
end;

function GTK_TEXT_TAG_TABLE(obj: pointer) : PGtkTextTagTable;
begin
   GTK_TEXT_TAG_TABLE:=PGtkTextTagTable(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_TEXT_TAG_TABLE));
end;

function GTK_TEXT_TAG_TABLE_CLASS(klass: pointer) : PGtkTextTagTableClass;
begin
   GTK_TEXT_TAG_TABLE_CLASS:=PGtkTextTagTableClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_TEXT_TAG_TABLE));
end;

function GTK_IS_TEXT_TAG_TABLE(obj: pointer) : boolean;
begin
   GTK_IS_TEXT_TAG_TABLE:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_TEXT_TAG_TABLE);
end;

function GTK_IS_TEXT_TAG_TABLE_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_TEXT_TAG_TABLE_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_TEXT_TAG_TABLE);
end;

function GTK_TEXT_TAG_TABLE_GET_CLASS(obj: pointer) : PGtkTextTagTableClass;
begin
   GTK_TEXT_TAG_TABLE_GET_CLASS:=PGtkTextTagTableClass(G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_TEXT_TAG_TABLE));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

