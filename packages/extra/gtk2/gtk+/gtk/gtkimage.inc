// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   // internal type
   PGtkIconSet = pointer;


   PGtkImagePixmapData = ^TGtkImagePixmapData;
   TGtkImagePixmapData = record
        pixmap : PGdkPixmap;
     end;

   PGtkImageImageData = ^TGtkImageImageData;
   TGtkImageImageData = record
        image : PGdkImage;
     end;

   PGtkImagePixbufData = ^TGtkImagePixbufData;
   TGtkImagePixbufData = record
        pixbuf : PGdkPixbuf;
     end;

   PGtkImageStockData = ^TGtkImageStockData;
   TGtkImageStockData = record
        stock_id : Pgchar;
     end;

   PGtkImageIconSetData = ^TGtkImageIconSetData;
   TGtkImageIconSetData = record
        icon_set : PGtkIconSet;
     end;

   PGtkImageAnimationData = ^TGtkImageAnimationData;
   TGtkImageAnimationData = record
        anim : PGdkPixbufAnimation;
        iter : PGdkPixbufAnimationIter;
        frame_timeout : guint;
     end;


   PGtkImageType = ^TGtkImageType;
   TGtkImageType = (
     GTK_IMAGE_EMPTY,
     GTK_IMAGE_PIXMAP,
     GTK_IMAGE_IMAGE,
     GTK_IMAGE_PIXBUF,
     GTK_IMAGE_STOCK,
     GTK_IMAGE_ICON_SET,
     GTK_IMAGE_ANIMATION
   );

{ Only used with GTK_IMAGE_PIXMAP, GTK_IMAGE_IMAGE  }
{ Only used with GTK_IMAGE_STOCK, GTK_IMAGE_ICON_SET  }
   PGtkImage = ^TGtkImage;
   TGtkImage = record
        misc : TGtkMisc;
        storage_type : TGtkImageType;
        data : record
            case longint of
               0 : ( pixmap : TGtkImagePixmapData );
               1 : ( image : TGtkImageImageData );
               2 : ( pixbuf : TGtkImagePixbufData );
               3 : ( stock : TGtkImageStockData );
               4 : ( icon_set : TGtkImageIconSetData );
               5 : ( anim : TGtkImageAnimationData );
            end;
        mask : PGdkBitmap;
        icon_size : TGtkIconSize;
     end;

{ Padding for future expansion  }
   PGtkImageClass = ^TGtkImageClass;
   TGtkImageClass = record
        parent_class : TGtkMiscClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_IMAGE : GType;
function GTK_IMAGE(obj: pointer) : PGtkImage;
function GTK_IMAGE_CLASS(klass: pointer) : PGtkImageClass;
function GTK_IS_IMAGE(obj: pointer) : boolean;
function GTK_IS_IMAGE_CLASS(klass: pointer) : boolean;
function GTK_IMAGE_GET_CLASS(obj: pointer) : PGtkImageClass;


function gtk_image_get_type:TGtkType; cdecl; external gtklib;
function gtk_image_new:PGtkWidget; cdecl; external gtklib;
function gtk_image_new_from_pixmap(pixmap:PGdkPixmap; mask:PGdkBitmap):PGtkWidget; cdecl; external gtklib;
function gtk_image_new_from_image(image:PGdkImage; mask:PGdkBitmap):PGtkWidget; cdecl; external gtklib;
function gtk_image_new_from_file(filename:Pgchar):PGtkWidget; cdecl; external gtklib;
function gtk_image_new_from_pixbuf(pixbuf:PGdkPixbuf):PGtkWidget; cdecl; external gtklib;
function gtk_image_new_from_stock(stock_id:Pgchar; size:TGtkIconSize):PGtkWidget; cdecl; external gtklib;
function gtk_image_new_from_icon_set(icon_set:PGtkIconSet; size:TGtkIconSize):PGtkWidget; cdecl; external gtklib;
function gtk_image_new_from_animation(animation:PGdkPixbufAnimation):PGtkWidget; cdecl; external gtklib;
procedure gtk_image_set_from_pixmap(image:PGtkImage; pixmap:PGdkPixmap; mask:PGdkBitmap); cdecl; external gtklib;
procedure gtk_image_set_from_image(image:PGtkImage; gdk_image:PGdkImage; mask:PGdkBitmap); cdecl; external gtklib;
procedure gtk_image_set_from_file(image:PGtkImage; filename:Pgchar); cdecl; external gtklib;
procedure gtk_image_set_from_pixbuf(image:PGtkImage; pixbuf:PGdkPixbuf); cdecl; external gtklib;
procedure gtk_image_set_from_stock(image:PGtkImage; stock_id:Pgchar; size:TGtkIconSize); cdecl; external gtklib;
procedure gtk_image_set_from_icon_set(image:PGtkImage; icon_set:PGtkIconSet; size:TGtkIconSize); cdecl; external gtklib;
procedure gtk_image_set_from_animation(image:PGtkImage; animation:PGdkPixbufAnimation); cdecl; external gtklib;
function gtk_image_get_storage_type(image:PGtkImage):TGtkImageType; cdecl; external gtklib;
procedure gtk_image_get_pixmap(image:PGtkImage; var pixmap:PGdkPixmap; var mask:PGdkBitmap); cdecl; external gtklib;
procedure gtk_image_get_image(image:PGtkImage; var gdk_image:PGdkImage; var mask:PGdkBitmap); cdecl; external gtklib;
function gtk_image_get_pixbuf(image:PGtkImage):PGdkPixbuf; cdecl; external gtklib;
procedure gtk_image_get_stock(image:PGtkImage; stock_id:PPgchar; size:PGtkIconSize); cdecl; external gtklib;
procedure gtk_image_get_icon_set(image:PGtkImage; var icon_set:PGtkIconSet; size:PGtkIconSize); cdecl; external gtklib;
function gtk_image_get_animation(image:PGtkImage):PGdkPixbufAnimation; cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
{ These three are deprecated  }
procedure gtk_image_set(image:PGtkImage; val:PGdkImage; mask:PGdkBitmap); cdecl; external gtklib;
procedure gtk_image_get(image:PGtkImage; var val:PGdkImage; var mask:PGdkBitmap); cdecl; external gtklib;
{$endif}
{ GTK_DISABLE_DEPRECATED  }
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_IMAGE : GType;
begin
   GTK_TYPE_IMAGE:=gtk_image_get_type;
end;

function GTK_IMAGE(obj: pointer) : PGtkImage;
begin
   GTK_IMAGE:=PGtkImage(GTK_CHECK_CAST(obj,GTK_TYPE_IMAGE));
end;

function GTK_IMAGE_CLASS(klass: pointer) : PGtkImageClass;
begin
   GTK_IMAGE_CLASS:=PGtkImageClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_IMAGE));
end;

function GTK_IS_IMAGE(obj: pointer) : boolean;
begin
   GTK_IS_IMAGE:=GTK_CHECK_TYPE(obj,GTK_TYPE_IMAGE);
end;

function GTK_IS_IMAGE_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_IMAGE_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_IMAGE);
end;

function GTK_IMAGE_GET_CLASS(obj: pointer) : PGtkImageClass;
begin
   GTK_IMAGE_GET_CLASS:=PGtkImageClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_IMAGE));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

