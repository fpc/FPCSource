// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkPreview = ^TGtkPreview;
   TGtkPreview = record
        widget : TGtkWidget;
        buffer : Pguchar;
        buffer_width : guint16;
        buffer_height : guint16;
        bpp : guint16;
        rowstride : guint16;
        dither : TGdkRgbDither;
        flag0 : word;
     end;

   PGtkPreviewInfo = ^TGtkPreviewInfo;
   TGtkPreviewInfo = record
        lookup : Pguchar;
        gamma : gdouble;
     end;

   PGtkDitherInfo = ^TGtkDitherInfo;
   TGtkDitherInfo = record
       case longint of
          0 : ( s : array[0..1] of gushort );
          1 : ( c : array[0..3] of guchar );
       end;

   PGtkPreviewClass = ^TGtkPreviewClass;
   TGtkPreviewClass = record
        parent_class : TGtkWidgetClass;
        info : TGtkPreviewInfo;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkPreview_type = $1;
   bp_TGtkPreview_type = 0;
   bm_TGtkPreview_expand = $2;
   bp_TGtkPreview_expand = 1;


function GTK_TYPE_PREVIEW : GType;
function GTK_PREVIEW(obj: pointer) : PGtkPreview;
function GTK_PREVIEW_CLASS(klass: pointer) : PGtkPreviewClass;
function GTK_IS_PREVIEW(obj: pointer) : boolean;
function GTK_IS_PREVIEW_CLASS(klass: pointer) : boolean;
function GTK_PREVIEW_GET_CLASS(obj: pointer) : PGtkPreviewClass;

function get_type(var a : TGtkPreview) : guint; overload;
procedure set_type(var a : TGtkPreview; __type : guint); overload;
function get_expand(var a : TGtkPreview) : guint; overload;
procedure set_expand(var a : TGtkPreview; __expand : guint); overload;

function gtk_preview_get_type:TGtkType; cdecl; external gtklib;
procedure gtk_preview_uninit; cdecl; external gtklib;
function gtk_preview_new(_type:TGtkPreviewType):PGtkWidget; cdecl; external gtklib;
procedure gtk_preview_size(preview:PGtkPreview; width:gint; height:gint); cdecl; external gtklib;
procedure gtk_preview_put(preview:PGtkPreview; window:PGdkWindow; gc:PGdkGC; srcx:gint; srcy:gint;
            destx:gint; desty:gint; width:gint; height:gint); cdecl; external gtklib;
procedure gtk_preview_draw_row(preview:PGtkPreview; data:Pguchar; x:gint; y:gint; w:gint); cdecl; external gtklib;
procedure gtk_preview_set_expand(preview:PGtkPreview; expand:gboolean); cdecl; external gtklib;
procedure gtk_preview_set_gamma(gamma:double); cdecl; external gtklib;
procedure gtk_preview_set_color_cube(nred_shades:guint; ngreen_shades:guint; nblue_shades:guint; ngray_shades:guint); cdecl; external gtklib;
procedure gtk_preview_set_install_cmap(install_cmap:gint); cdecl; external gtklib;
procedure gtk_preview_set_reserved(nreserved:gint); cdecl; external gtklib;
procedure gtk_preview_set_dither(preview:PGtkPreview; dither:TGdkRgbDither); cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
function gtk_preview_get_visual:PGdkVisual; cdecl; external gtklib;
function gtk_preview_get_cmap:PGdkColormap; cdecl; external gtklib;
{$endif}

function gtk_preview_get_info:PGtkPreviewInfo; cdecl; external gtklib;
{ This function reinitializes the preview colormap and visual from
   the current gamma/color_cube/install_cmap settings. It must only
   be called if there are no previews or users's of the preview
   colormap in existence.
  }
procedure gtk_preview_reset; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_PREVIEW : GType;
begin
   GTK_TYPE_PREVIEW:=gtk_preview_get_type;
end;

function GTK_PREVIEW(obj: pointer) : PGtkPreview;
begin
   GTK_PREVIEW:=PGtkPreview(GTK_CHECK_CAST(obj,GTK_TYPE_PREVIEW));
end;

function GTK_PREVIEW_CLASS(klass: pointer) : PGtkPreviewClass;
begin
   GTK_PREVIEW_CLASS:=PGtkPreviewClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_PREVIEW));
end;

function GTK_IS_PREVIEW(obj: pointer) : boolean;
begin
   GTK_IS_PREVIEW:=GTK_CHECK_TYPE(obj,GTK_TYPE_PREVIEW);
end;

function GTK_IS_PREVIEW_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_PREVIEW_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_PREVIEW);
end;

function GTK_PREVIEW_GET_CLASS(obj: pointer) : PGtkPreviewClass;
begin
   GTK_PREVIEW_GET_CLASS:=PGtkPreviewClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_PREVIEW));
end;

function get_type(var a : TGtkPreview) : guint;
begin
   get_type:=(a.flag0 and bm_TGtkPreview_type) shr bp_TGtkPreview_type;
end;

procedure set_type(var a : TGtkPreview; __type : guint);
begin
   a.flag0:=a.flag0 or ((__type shl bp_TGtkPreview_type) and bm_TGtkPreview_type);
end;

function get_expand(var a : TGtkPreview) : guint;
begin
   get_expand:=(a.flag0 and bm_TGtkPreview_expand) shr bp_TGtkPreview_expand;
end;

procedure set_expand(var a : TGtkPreview; __expand : guint);
begin
   a.flag0:=a.flag0 or ((__expand shl bp_TGtkPreview_expand) and bm_TGtkPreview_expand);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

