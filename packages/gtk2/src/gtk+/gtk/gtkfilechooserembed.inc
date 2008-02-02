
{$IFDEF read_forward_definitions}
{$ENDIF}

{$IFDEF read_interface_types}
type
   PGtkFileChooserEmbed = pointer;
   PPGtkFileChooserEmbed = ^PGtkFileChooserEmbed;

   PGtkFileChooserEmbedIface = ^TGtkFileChooserEmbedIface;
   TGtkFileChooserEmbedIface = record
        base_iface : TGTypeInterface;
        { Methods }
        get_default_size : procedure (chooser_embed:PGtkFileChooserEmbed; default_width:Pgint; default_height:Pgint);cdecl;
        get_resizable_hints : procedure (chooser_embed:PGtkFileChooserEmbed; resize_horizontally:Pgboolean; resize_vertically:Pgboolean);
        { Signals }
        should_respond : function (chooser_embed:PGtkFileChooserEmbed):gboolean;
        default_size_changed : procedure (chooser_embed:PGtkFileChooserEmbed);
     end;

{$ENDIF} {types}

{$IFDEF read_interface_rest}

function GTK_TYPE_FILE_CHOOSER_EMBED : GType;

function GTK_FILE_CHOOSER_EMBED(obj : pointer) : PGtkFileChooserEmbed;

function GTK_IS_FILE_CHOOSER_EMBED(obj : pointer) : gboolean;

function GTK_FILE_CHOOSER_EMBED_GET_IFACE(obj : pointer) : PGtkFileChooserEmbedIface;



function _gtk_file_chooser_embed_get_type:GType;cdecl;external gtklib name '_gtk_file_chooser_embed_get_type';
procedure _gtk_file_chooser_embed_get_default_size(chooser_embed:PGtkFileChooserEmbed; default_width:Pgint; default_height:Pgint);cdecl;external gtklib name '_gtk_file_chooser_embed_get_default_size';
procedure _gtk_file_chooser_embed_get_resizable_hints(chooser_embed:PGtkFileChooserEmbed; resize_horizontally:Pgboolean; resize_vertically:Pgboolean);cdecl;external gtklib name '_gtk_file_chooser_embed_get_resizable_hints';
function _gtk_file_chooser_embed_should_respond(chooser_embed:PGtkFileChooserEmbed):gboolean;cdecl;external gtklib name '_gtk_file_chooser_embed_should_respond';
procedure _gtk_file_chooser_embed_delegate_iface_init(iface:PGtkFileChooserEmbedIface);cdecl;external gtklib name '_gtk_file_chooser_embed_delegate_iface_init';
procedure _gtk_file_chooser_embed_set_delegate(receiver:PGtkFileChooserEmbed; delegate:PGtkFileChooserEmbed);cdecl;external gtklib name '_gtk_file_chooser_embed_set_delegate';
{$endif} {read_interface_rest}

{************************************************}

{$IFDEF read_implementation}

function GTK_TYPE_FILE_CHOOSER_EMBED : GType;
  begin
     GTK_TYPE_FILE_CHOOSER_EMBED:=_gtk_file_chooser_embed_get_type;
  end;


function GTK_FILE_CHOOSER_EMBED(obj : pointer) : PGtkFileChooserEmbed;
begin
   GTK_FILE_CHOOSER_EMBED:=G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_FILE_CHOOSER_EMBED);
end;

function GTK_IS_FILE_CHOOSER_EMBED(obj : pointer) : gboolean;
begin
   GTK_IS_FILE_CHOOSER_EMBED:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_FILE_CHOOSER_EMBED);
end;

function GTK_FILE_CHOOSER_EMBED_GET_IFACE(obj : pointer) : PGtkFileChooserEmbedIface;
begin
   GTK_FILE_CHOOSER_EMBED_GET_IFACE:=G_TYPE_INSTANCE_GET_INTERFACE(obj,GTK_TYPE_FILE_CHOOSER_EMBED);
end;

{$ENDIF} {read_implementation}
