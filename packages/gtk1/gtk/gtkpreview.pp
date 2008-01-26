{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkPreview = ^TGtkPreview;
     TGtkPreview = record
          widget : TGtkWidget;
          buffer : Pguchar;
          buffer_width : guint16;
          buffer_height : guint16;
          bpp : guint16;
          rowstride : guint16;
          dither : TGdkRgbDither;
          flag0 : {$ifdef win32}longint{$else}word{$endif};
       end;
  const
     bm_TGtkPreview_type = $1;
     bp_TGtkPreview_type = 0;
     bm_TGtkPreview_expand = $2;
     bp_TGtkPreview_expand = 1;
function  thetype(var a : TGtkPreview) : guint;
procedure set_thetype(var a : TGtkPreview; __type : guint);
function  expand(var a : TGtkPreview) : guint;
procedure set_expand(var a : TGtkPreview; __expand : guint);

  type
     PGtkPreviewInfo = ^TGtkPreviewInfo;
     PGtkDitherInfo = ^TGtkDitherInfo;
     TGtkPreviewInfo = record
          visual : PGdkVisual;
          cmap : PGdkColormap;
          lookup : Pguchar;
          gamma : gdouble;
       end;

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

Type
  GTK_PREVIEW=PGtkPreview;
  GTK_PREVIEW_CLASS=PGtkPreviewClass;

function  GTK_PREVIEW_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_preview_get_type';
function  GTK_IS_PREVIEW(obj:pointer):boolean;
function  GTK_IS_PREVIEW_CLASS(klass:pointer):boolean;

function  gtk_preview_get_type:TGtkType;cdecl;external gtkdll name 'gtk_preview_get_type';
procedure gtk_preview_uninit;cdecl;external gtkdll name 'gtk_preview_uninit';
function  gtk_preview_new(thetype:TGtkPreviewType):PGtkWidget;cdecl;external gtkdll name 'gtk_preview_new';
procedure gtk_preview_size(preview:PGtkPreview; width:gint; height:gint);cdecl;external gtkdll name 'gtk_preview_size';
procedure gtk_preview_put(preview:PGtkPreview; window:PGdkWindow; gc:PGdkGC; srcx:gint; srcy:gint;destx:gint; desty:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_preview_put';
procedure gtk_preview_draw_row(preview:PGtkPreview; data:Pguchar; x:gint; y:gint; w:gint);cdecl;external gtkdll name 'gtk_preview_draw_row';
procedure gtk_preview_set_expand(preview:PGtkPreview; expand:gint);cdecl;external gtkdll name 'gtk_preview_set_expand';
procedure gtk_preview_set_gamma(gamma:double);cdecl;external gtkdll name 'gtk_preview_set_gamma';
procedure gtk_preview_set_color_cube(nred_shades:guint; ngreen_shades:guint; nblue_shades:guint; ngray_shades:guint);cdecl;external gtkdll name 'gtk_preview_set_color_cube';
procedure gtk_preview_set_install_cmap(install_cmap:gint);cdecl;external gtkdll name 'gtk_preview_set_install_cmap';
procedure gtk_preview_set_reserved(nreserved:gint);cdecl;external gtkdll name 'gtk_preview_set_reserved';
procedure gtk_preview_set_dither(preview:PGtkPreview; dither:TGdkRgbDither);cdecl;external gtkdll name 'gtk_preview_set_dither';
function  gtk_preview_get_visual:PGdkVisual;cdecl;external gtkdll name 'gtk_preview_get_visual';
function  gtk_preview_get_cmap:PGdkColormap;cdecl;external gtkdll name 'gtk_preview_get_cmap';
function  gtk_preview_get_info:PGtkPreviewInfo;cdecl;external gtkdll name 'gtk_preview_get_info';
procedure gtk_preview_reset;cdecl;external gtkdll name 'gtk_preview_reset';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  thetype(var a : TGtkPreview) : guint;
    begin
       thetype:=(a.flag0 and bm_TGtkPreview_type) shr bp_TGtkPreview_type;
    end;

procedure set_thetype(var a : TGtkPreview; __type : guint);
    begin
       a.flag0:=a.flag0 or ((__type shl bp_TGtkPreview_type) and bm_TGtkPreview_type);
    end;

function  expand(var a : TGtkPreview) : guint;
    begin
       expand:=(a.flag0 and bm_TGtkPreview_expand) shr bp_TGtkPreview_expand;
    end;

procedure set_expand(var a : TGtkPreview; __expand : guint);
    begin
       a.flag0:=a.flag0 or ((__expand shl bp_TGtkPreview_expand) and bm_TGtkPreview_expand);
    end;

function  GTK_IS_PREVIEW(obj:pointer):boolean;
begin
  GTK_IS_PREVIEW:=(obj<>nil) and GTK_IS_PREVIEW_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_PREVIEW_CLASS(klass:pointer):boolean;
begin
  GTK_IS_PREVIEW_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_PREVIEW_TYPE);
end;

{$endif read_implementation}


