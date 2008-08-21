{$IFDEF read_forward_definitions}
{$ENDIF}

{$ifdef read_interface_types}
type
   PGtkFileFilter = pointer;
   PPGtkFileFilter = ^PGtkFileFilter;

   PGtkFileFilterFlags = ^TGtkFileFilterFlags;
   TGtkFileFilterFlags = (GTK_FILE_FILTER_FILENAME := 1 shl 0,
                          GTK_FILE_FILTER_URI := 1 shl 1,
                          GTK_FILE_FILTER_DISPLAY_NAME := 1 shl 2,
                          GTK_FILE_FILTER_MIME_TYPE := 1 shl 3);

   PGtkFileFilterInfo = ^TGtkFileFilterInfo;
   TGtkFileFilterInfo = record
        contains : TGtkFileFilterFlags;
        filename : Pgchar;
        uri      : Pgchar;
        display_name : Pgchar;
        mime_type : Pgchar;
     end;

   TGtkFileFilterFunc = function (filter_info:PGtkFileFilterInfo; data:gpointer):gboolean;cdecl;

{$endif} {read_interface_types}

{$ifdef read_interface_rest}

function GTK_TYPE_FILE_FILTER : GType;
function GTK_FILE_FILTER(obj : pointer) : PGtkFileFilter;
function GTK_IS_FILE_FILTER(obj : pointer) : gboolean;

function gtk_file_filter_get_type:GType;cdecl;external gtklib name 'gtk_file_filter_get_type';
function gtk_file_filter_new:PGtkFileFilter;cdecl;external gtklib name 'gtk_file_filter_new';

procedure gtk_file_filter_set_name(filter:PGtkFileFilter; name:Pgchar);cdecl;external gtklib name 'gtk_file_filter_set_name';

function gtk_file_filter_get_name(filter:PGtkFileFilter):Pgchar;cdecl;external gtklib name 'gtk_file_filter_get_name';

procedure gtk_file_filter_add_mime_type(filter:PGtkFileFilter; mime_type:Pgchar);cdecl;external gtklib name 'gtk_file_filter_add_mime_type';

procedure gtk_file_filter_add_pattern(filter:PGtkFileFilter; pattern:Pgchar);cdecl;external gtklib name 'gtk_file_filter_add_pattern';
procedure gtk_file_filter_add_custom(filter:PGtkFileFilter; needed:TGtkFileFilterFlags; func:TGtkFileFilterFunc; data:gpointer; notify:TGDestroyNotify);cdecl;external gtklib name 'gtk_file_filter_add_custom';
function gtk_file_filter_get_needed(filter:PGtkFileFilter):TGtkFileFilterFlags;cdecl;external gtklib name 'gtk_file_filter_get_needed';

function gtk_file_filter_filter(filter:PGtkFileFilter; filter_info:PGtkFileFilterInfo):gboolean;cdecl;external gtklib name 'gtk_file_filter_filter';

{$ifdef HasGTK2_6}
procedure gtk_file_filter_add_pixbuf_formats(filter:PGtkFileFilter);cdecl;external gtklib;
{$endif}

{$endif}

{$ifdef read_implementation}

function GTK_TYPE_FILE_FILTER : GType;
begin
   GTK_TYPE_FILE_FILTER:=gtk_file_filter_get_type;
end;

function GTK_FILE_FILTER(obj : pointer) : PGtkFileFilter;
begin
   GTK_FILE_FILTER:=G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_FILE_FILTER);
end;

function GTK_IS_FILE_FILTER(obj : pointer) : gboolean;
begin
   GTK_IS_FILE_FILTER:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_FILE_FILTER);
end;

{$endif} {read_implementation}
