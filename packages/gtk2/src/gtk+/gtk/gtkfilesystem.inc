
{ This is a "semi-private" header; it is meant only for
   alternate GtkFileChooser backend modules; no stability guarantees
   are made at this point
  }


{$IFDEF read_forward_definitions}

{$ENDIF}

{$IFDEF read_interface_types}
type

   PGtkFileTime = ^TGtkFileTime;
   TGtkFileTime = gint64;

   PGtkFileSystem = pointer;
   PPGtkFileSystem = ^PGtkFileSystem;

   PGtkFilePath   = Pgchar;     { Seems to be implemented in GTK 2.4 by now }
   PPGtkFilePath  = ^PGtkFilePath;

   PGtkFileSystemVolume = pointer;
   PPGtkFileSystemVolume = ^PGtkFileSystemVolume;

   PGtkFileFolder = pointer;
   PPGtkFileFolder = ^PGtkFileFolder;

   PGtkFileInfo   = pointer;
   PPGtkFileInfo  = ^PGtkFileInfo;

{ Mask of information about a file, for monitoring and
   gtk_file_system_get_info()
  }

   PGtkFileInfoType = ^TGtkFileInfoType;
   TGtkFileInfoType = (GTK_FILE_INFO_DISPLAY_NAME := 1 shl 0,
                       GTK_FILE_INFO_IS_FOLDER := 1 shl 1,
                       GTK_FILE_INFO_IS_HIDDEN := 1 shl 2,
                       GTK_FILE_INFO_MIME_TYPE := 1 shl 3,
                       GTK_FILE_INFO_MODIFICATION_TIME := 1 shl 4,
                       GTK_FILE_INFO_SIZE := 1 shl 5,
                       GTK_FILE_INFO_ALL := (1 shl 6) - 1 );

type

   PGtkFileSystemError = ^TGtkFileSystemError;
   TGtkFileSystemError = (GTK_FILE_SYSTEM_ERROR_NONEXISTENT,
                          GTK_FILE_SYSTEM_ERROR_NOT_FOLDER,
                          GTK_FILE_SYSTEM_ERROR_INVALID_URI,
                          GTK_FILE_SYSTEM_ERROR_BAD_FILENAME,
                          GTK_FILE_SYSTEM_ERROR_FAILED,
                          GTK_FILE_SYSTEM_ERROR_ALREADY_EXISTS);


type
   PGtkFileSystemIface = ^TGtkFileSystemIface;
   TGtkFileSystemIface = record
        base_iface : TGTypeInterface;
        { Methods  }

        { Volumes  }
        list_volumes : function (file_system:PGtkFileSystem):PGSList;cdecl;
        get_volume_for_path : function (file_system:PGtkFileSystem; path:PGtkFilePath):PGtkFileSystemVolume;
        get_folder : function (file_system:PGtkFileSystem; path:PGtkFilePath; types:TGtkFileInfoType; error:PPGError):PGtkFileFolder;
        create_folder : function (file_system:PGtkFileSystem; path:PGtkFilePath; error:PPGError):gboolean;
        volume_free : procedure (file_system:PGtkFileSystem; volume:PGtkFileSystemVolume);
        volume_get_base_path : function (file_system:PGtkFileSystem; volume:PGtkFileSystemVolume):PGtkFilePath;
        volume_get_is_mounted : function (file_system:PGtkFileSystem; volume:PGtkFileSystemVolume):gboolean;
        volume_mount : function (file_system:PGtkFileSystem; volume:PGtkFileSystemVolume; error:PPGError):gboolean;
        volume_get_display_name : function (file_system:PGtkFileSystem; volume:PGtkFileSystemVolume):Pchar;
        volume_render_icon : function (file_system:PGtkFileSystem; volume:PGtkFileSystemVolume; widget:PGtkWidget; pixel_size:gint; error:PPGError):PGdkPixbuf;
        get_parent : function (file_system:PGtkFileSystem; path:PGtkFilePath; parent:PPGtkFilePath; error:PPGError):gboolean;
        make_path : function (file_system:PGtkFileSystem; base_path:PGtkFilePath; display_name:Pgchar; error:PPGError):PGtkFilePath;
        parse : function (file_system:PGtkFileSystem; base_path:PGtkFilePath; str:Pgchar; folder:PPGtkFilePath; file_part:PPgchar;
                     error:PPGError):gboolean;
        { Path Manipulation      }
        path_to_uri : function (file_system:PGtkFileSystem; path:PGtkFilePath):Pgchar;
        path_to_filename : function (file_system:PGtkFileSystem; path:PGtkFilePath):Pgchar;
        uri_to_path : function (file_system:PGtkFileSystem; uri:Pgchar):PGtkFilePath;
        filename_to_path : function (file_system:PGtkFileSystem; path:Pgchar):PGtkFilePath;

        { Icons  }
        render_icon : function (file_system:PGtkFileSystem; path:PGtkFilePath; widget:PGtkWidget; pixel_size:gint; error:PPGError):PGdkPixbuf;

        { Bookmarks  }
        insert_bookmark : function (file_system:PGtkFileSystem; path:PGtkFilePath; position:gint; error:PPGError):gboolean;
        remove_bookmark : function (file_system:PGtkFileSystem; path:PGtkFilePath; error:PPGError):gboolean;
        list_bookmarks : function (file_system:PGtkFileSystem):PGSList;

        { Signals   }
        volumes_changed : procedure (file_system:PGtkFileSystem);
        bookmarks_changed : procedure (file_system:PGtkFileSystem);
     end;


type
   PGtkFileFolderIface = ^TGtkFileFolderIface;
   TGtkFileFolderIface = record
        base_iface : TGTypeInterface;
        { Methods  }
        get_info : function (folder:PGtkFileFolder; path:PGtkFilePath; error:PPGError):PGtkFileInfo;cdecl;
        list_children : function (folder:PGtkFileFolder; children:PPGSList; error:PPGError):gboolean;
        { ??? refresh() ???  }

        { Signals     }
        deleted : procedure (monitor:PGtkFileFolder);
        files_added : procedure (monitor:PGtkFileFolder; paths:PGSList);
        files_changed : procedure (monitor:PGtkFileFolder; paths:PGSList);
        files_removed : procedure (monitor:PGtkFileFolder; paths:PGSList);
     end;

{$ENDIF} {types }

{$IFDEF read_interface_rest}


{ GError enumeration for GtkFileSystem  }

function GTK_FILE_SYSTEM_ERROR : TGQuark;



function gtk_file_system_error_quark:TGQuark;cdecl;external gtklib name 'gtk_file_system_error_quark';



{ Boxed-type for gtk_file_folder_get_info() results  }

function GTK_TYPE_FILE_INFO : GType;

function gtk_file_info_get_type:GType;cdecl;external gtklib name 'gtk_file_info_get_type';
function gtk_file_info_new:PGtkFileInfo;cdecl;external gtklib name 'gtk_file_info_new';
function gtk_file_info_copy(info:PGtkFileInfo):PGtkFileInfo;cdecl;external gtklib name 'gtk_file_info_copy';
procedure gtk_file_info_free(info:PGtkFileInfo);cdecl;external gtklib name 'gtk_file_info_free';

function gtk_file_info_get_display_name(info:PGtkFileInfo):Pgchar;cdecl;external gtklib name 'gtk_file_info_get_display_name';
function gtk_file_info_get_display_key(info:PGtkFileInfo):Pgchar;cdecl;external gtklib name 'gtk_file_info_get_display_key';

procedure gtk_file_info_set_display_name(info:PGtkFileInfo; display_name:Pgchar);cdecl;external gtklib name 'gtk_file_info_set_display_name';

function gtk_file_info_get_is_folder(info:PGtkFileInfo):gboolean;cdecl;external gtklib name 'gtk_file_info_get_is_folder';
procedure gtk_file_info_set_is_folder(info:PGtkFileInfo; is_folder:gboolean);cdecl;external gtklib name 'gtk_file_info_set_is_folder';

function gtk_file_info_get_is_hidden(info:PGtkFileInfo):gboolean;cdecl;external gtklib name 'gtk_file_info_get_is_hidden';
procedure gtk_file_info_set_is_hidden(info:PGtkFileInfo; is_hidden:gboolean);cdecl;external gtklib name 'gtk_file_info_set_is_hidden';

function gtk_file_info_get_mime_type(info:PGtkFileInfo):Pgchar;cdecl;external gtklib name 'gtk_file_info_get_mime_type';
procedure gtk_file_info_set_mime_type(info:PGtkFileInfo; mime_type:Pgchar);cdecl;external gtklib name 'gtk_file_info_set_mime_type';

function gtk_file_info_get_modification_time(info:PGtkFileInfo):TGtkFileTime;cdecl;external gtklib name 'gtk_file_info_get_modification_time';
procedure gtk_file_info_set_modification_time(info:PGtkFileInfo; modification_time:TGtkFileTime);cdecl;external gtklib name 'gtk_file_info_set_modification_time';

function gtk_file_info_get_size(info:PGtkFileInfo):gint64;cdecl;external gtklib name 'gtk_file_info_get_size';
procedure gtk_file_info_set_size(info:PGtkFileInfo; size:gint64);cdecl;external gtklib name 'gtk_file_info_set_size';

{ The base GtkFileSystem interface  }

function GTK_TYPE_FILE_SYSTEM : GType;

function GTK_FILE_SYSTEM(obj : pointer) : PGtkFileSystem;

function GTK_IS_FILE_SYSTEM(obj : pointer) : gboolean;

function GTK_FILE_SYSTEM_GET_IFACE(inst : pointer) : PGtkFileSystemIface;




function gtk_file_system_get_type:GType;cdecl;external gtklib name 'gtk_file_system_get_type';

function gtk_file_system_list_volumes(file_system:PGtkFileSystem):PGSList;cdecl;external gtklib name 'gtk_file_system_list_volumes';
function gtk_file_system_get_volume_for_path(file_system:PGtkFileSystem; path:PGtkFilePath):PGtkFileSystemVolume;cdecl;external gtklib name 'gtk_file_system_get_volume_for_path';

procedure gtk_file_system_volume_free(file_system:PGtkFileSystem; volume:PGtkFileSystemVolume);cdecl;external gtklib name 'gtk_file_system_volume_free';

function gtk_file_system_volume_get_base_path(file_system:PGtkFileSystem; volume:PGtkFileSystemVolume):PGtkFilePath;cdecl;external gtklib name 'gtk_file_system_volume_get_base_path';

function gtk_file_system_volume_get_is_mounted(file_system:PGtkFileSystem; volume:PGtkFileSystemVolume):gboolean;cdecl;external gtklib name 'gtk_file_system_volume_get_is_mounted';
function gtk_file_system_volume_mount(file_system:PGtkFileSystem; volume:PGtkFileSystemVolume; error:PPGError):gboolean;cdecl;external gtklib name 'gtk_file_system_volume_mount';

function gtk_file_system_volume_get_display_name(file_system:PGtkFileSystem; volume:PGtkFileSystemVolume):Pchar;cdecl;external gtklib name 'gtk_file_system_volume_get_display_name';

function gtk_file_system_volume_render_icon(file_system:PGtkFileSystem; volume:PGtkFileSystemVolume; widget:PGtkWidget; pixel_size:gint; error:PPGError):PGdkPixbuf;cdecl;external gtklib name 'gtk_file_system_volume_render_icon';

function gtk_file_system_get_parent(file_system:PGtkFileSystem;  path:PGtkFilePath; parent:PPGtkFilePath; error:PPGError):gboolean;cdecl;external gtklib name 'gtk_file_system_get_parent';
function gtk_file_system_get_folder(file_system:PGtkFileSystem; path:PGtkFilePath; types:TGtkFileInfoType; error:PPGError):PGtkFileFolder;cdecl;external gtklib name 'gtk_file_system_get_folder';

function gtk_file_system_create_folder(file_system:PGtkFileSystem; path:PGtkFilePath; error:PPGError):gboolean;cdecl;external gtklib name 'gtk_file_system_create_folder';

function gtk_file_system_make_path(file_system:PGtkFileSystem; base_path:PGtkFilePath; display_name:Pgchar; error:PPGError):PGtkFilePath;cdecl;external gtklib name 'gtk_file_system_make_path';
function gtk_file_system_parse(file_system:PGtkFileSystem; base_path:PGtkFilePath; str:Pgchar; folder:PPGtkFilePath; file_part:PPgchar;
           error:PPGError):gboolean;cdecl;external gtklib name 'gtk_file_system_parse';

function gtk_file_system_path_to_uri(file_system:PGtkFileSystem; path:PGtkFilePath):Pgchar;cdecl;external gtklib name 'gtk_file_system_path_to_uri';
function gtk_file_system_path_to_filename(file_system:PGtkFileSystem; path:PGtkFilePath):Pgchar;cdecl;external gtklib name 'gtk_file_system_path_to_filename';

function gtk_file_system_uri_to_path(file_system:PGtkFileSystem; uri:Pgchar):PGtkFilePath;cdecl;external gtklib name 'gtk_file_system_uri_to_path';
function gtk_file_system_filename_to_path(file_system:PGtkFileSystem; filename:Pgchar):PGtkFilePath;cdecl;external gtklib name 'gtk_file_system_filename_to_path';

function gtk_file_system_render_icon(file_system:PGtkFileSystem; path:PGtkFilePath; widget:PGtkWidget; pixel_size:gint; error:PPGError):PGdkPixbuf;cdecl;external gtklib name 'gtk_file_system_render_icon';

function gtk_file_system_insert_bookmark(file_system:PGtkFileSystem; path:PGtkFilePath; position:gint; error:PPGError):gboolean;cdecl;external gtklib name 'gtk_file_system_insert_bookmark';

function gtk_file_system_remove_bookmark(file_system:PGtkFileSystem; path:PGtkFilePath; error:PPGError):gboolean;cdecl;external gtklib name 'gtk_file_system_remove_bookmark';
function gtk_file_system_list_bookmarks(file_system:PGtkFileSystem):PGSList;cdecl;external gtklib name 'gtk_file_system_list_bookmarks';

{ Detailed information about a particular folder }

function GTK_TYPE_FILE_FOLDER : GType;


function GTK_FILE_FOLDER(obj : pointer) : PGtkFileFolder;

function GTK_IS_FILE_FOLDER(obj : pointer) : gboolean;


function GTK_FILE_FOLDER_GET_IFACE(inst : pointer) : PGtkFileSystemIface;



function gtk_file_folder_get_type:GType;cdecl;external gtklib name 'gtk_file_folder_get_type';
function gtk_file_folder_list_children(folder:PGtkFileFolder; children:PPGSList; error:PPGError):gboolean;cdecl;external gtklib name 'gtk_file_folder_list_children';

function gtk_file_folder_get_info(folder:PGtkFileFolder; path:PGtkFilePath; error:PPGError):PGtkFileInfo;cdecl;external gtklib name 'gtk_file_folder_get_info';
function gtk_file_path_new_dup(str : Pgchar) : PGtkFilePath;
function gtk_file_path_new_steal(str : Pgchar) : PGtkFilePath;

function gtk_file_path_get_string(str : PGtkFilePath) : Pgchar;

procedure gtk_file_path_free(path : PGtkFilePath);

function gtk_file_path_copy(path : PGtkFilePath) : PGtkFilePath;


function gtk_file_path_compare(path1,path2 : PGtkFilePath) : longint;

function gtk_file_paths_sort(paths:PGSList):PGSList;cdecl;external gtklib name 'gtk_file_paths_sort';
function gtk_file_paths_copy(paths:PGSList):PGSList;cdecl;external gtklib name 'gtk_file_paths_copy';
procedure gtk_file_paths_free(paths:PGSList);cdecl;external gtklib name 'gtk_file_paths_free';


{ GtkFileSystem modules support  }

function _gtk_file_system_create(file_system_name:Pchar):PGtkFileSystem;cdecl;external gtklib name '_gtk_file_system_create';
{$endif}  {read_interface_rest}


{$ifdef read_implementation}

function GTK_FILE_SYSTEM_ERROR : TGQuark;
  begin
     GTK_FILE_SYSTEM_ERROR:=gtk_file_system_error_quark;
  end;


function GTK_TYPE_FILE_INFO : GType;

  begin
     GTK_TYPE_FILE_INFO:=gtk_file_info_get_type;
  end;


function GTK_TYPE_FILE_SYSTEM : GType;
  begin
     GTK_TYPE_FILE_SYSTEM:=gtk_file_system_get_type;
  end;

function GTK_FILE_SYSTEM(obj : pointer) : PGtkFileSystem;
begin
   GTK_FILE_SYSTEM:=G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_FILE_SYSTEM);
end;

function GTK_IS_FILE_SYSTEM(obj : pointer) : gboolean;
begin
   GTK_IS_FILE_SYSTEM:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_FILE_SYSTEM);
end;

function GTK_FILE_SYSTEM_GET_IFACE(inst : pointer) : PGtkFileSystemIface;
begin
   GTK_FILE_SYSTEM_GET_IFACE:=G_TYPE_INSTANCE_GET_INTERFACE(inst,GTK_TYPE_FILE_SYSTEM);
end;


function GTK_TYPE_FILE_FOLDER : GType;
  begin
     GTK_TYPE_FILE_FOLDER:=gtk_file_folder_get_type;
  end;


function GTK_FILE_FOLDER(obj : pointer) : PGtkFileFolder;
begin
   GTK_FILE_FOLDER:=G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_FILE_FOLDER);
end;


function GTK_IS_FILE_FOLDER(obj : pointer) : gboolean;
begin
   GTK_IS_FILE_FOLDER:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_FILE_FOLDER);
end;



function GTK_FILE_FOLDER_GET_IFACE(inst : pointer) : PGtkFileSystemIface;
begin
   GTK_FILE_FOLDER_GET_IFACE:=G_TYPE_INSTANCE_GET_INTERFACE(inst,GTK_TYPE_FILE_FOLDER);
end;

function gtk_file_path_new_dup(str : Pgchar) : PGtkFilePath;
begin
   gtk_file_path_new_dup:=PGtkFilePath(g_strdup(str));
end;

function gtk_file_path_new_steal(str : Pgchar) : PGtkFilePath;
begin
   gtk_file_path_new_steal:=PGtkFilePath(str);
end;

function gtk_file_path_get_string(str : PGtkFilePath) : Pgchar;
begin
   gtk_file_path_get_string:=Pgchar(str);
end;

procedure gtk_file_path_free(path : PGtkFilePath);
begin
   g_free( Pgchar(path));
end;

function gtk_file_path_copy(path : PGtkFilePath) : PGtkFilePath;
begin
   gtk_file_path_copy:=gtk_file_path_new_dup(gtk_file_path_get_string(path));
end;

function gtk_file_path_compare(path1,path2 : PGtkFilePath) : longint;
begin
   gtk_file_path_compare:=StrComp(gtk_file_path_get_string(path1),gtk_file_path_get_string(path2));
end;


{$endif} {read_implementation}
