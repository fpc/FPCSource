// included by gdk2.pp
{$IFDEF read_forward_definitions}
{ Types of images.
     Normal: Normal X image type. These are slow as they involve passing
             the entire image through the X connection each time a draw
             request is required. On Win32, a bitmap.
     Shared: Shared memory X image type. These are fast as the X server
             and the program actually use the same piece of memory. They
             should be used with care though as there is the possibility
             for both the X server and the program to be reading/writing
             the image simultaneously and producing undesired results.
             On Win32, also a bitmap.
  }
   PGdkImageType = ^TGdkImageType;
   TGdkImageType = (
     GDK_IMAGE_NORMAL,
     GDK_IMAGE_SHARED,
     GDK_IMAGE_FASTEST
   );

   PGdkImage = ^TGdkImage;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}

   PGdkImageClass = ^TGdkImageClass;
   TGdkImageClass = record
        parent_class : TGObjectClass;
     end;

{ visual used to create the image  }
{ bytes per pixel  }
{ bytes per line  }
{ bits per pixel  }
   TGdkImage = record
        parent_instance : TGObject;
        _type : TGdkImageType;
        visual : PGdkVisual;
        byte_order : TGdkByteOrder;
        width : gint;
        height : gint;
        depth : guint16;
        bpp : guint16;
        bpl : guint16;
        bits_per_pixel : guint16;
        mem : gpointer;
        colormap : PGdkColormap;
        windowing_data : gpointer;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GDK_TYPE_IMAGE : GType;
function GDK_IMAGE(anObject : Pointer) : PGdkImage;
function GDK_IMAGE_CLASS(klass : Pointer) : PGdkImageClass;
function GDK_IS_IMAGE(anObject : Pointer) : boolean;
function GDK_IS_IMAGE_CLASS(klass : Pointer) : boolean;
function GDK_IMAGE_GET_CLASS(obj : Pointer) : PGdkImageClass;


function gdk_image_get_type:GType; cdecl; external gdklib;
function gdk_image_new(_type:TGdkImageType; visual:PGdkVisual; width:gint; height:gint):PGdkImage; cdecl; external gdklib;

{$ifndef GDK_DISABLE_DEPRECATED}
function gdk_image_get(drawable:PGdkDrawable; x:gint; y:gint; width:gint; height:gint):PGdkImage; cdecl; external gdklib;
function gdk_image_ref(image:PGdkImage):PGdkImage; cdecl; external gdklib;
procedure gdk_image_unref(image:PGdkImage); cdecl; external gdklib;
{$endif}

procedure gdk_image_put_pixel(image:PGdkImage; x:gint; y:gint; pixel:guint32); cdecl; external gdklib;
function gdk_image_get_pixel(image:PGdkImage; x:gint; y:gint):guint32; cdecl; external gdklib;
procedure gdk_image_set_colormap(image:PGdkImage; colormap:PGdkColormap); cdecl; external gdklib;
function gdk_image_get_colormap(image:PGdkImage):PGdkColormap; cdecl; external gdklib;

{$ifdef GDK_ENABLE_BROKEN}
function gdk_image_new_bitmap(visual:PGdkVisual; data:gpointer; width:gint; height:gint):PGdkImage; cdecl; external gdklib;
{$endif}
{ GDK_ENABLE_BROKEN  }

{$ifndef GDK_DISABLE_DEPRECATED}
procedure gdk_image_destroy(image:PGdkImage);
{$endif}
{ GDK_DISABLE_DEPRECATED  }
{$endif read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GDK_TYPE_IMAGE : GType;
begin
  GDK_TYPE_IMAGE:=gdk_image_get_type;
end;

function GDK_IMAGE(anObject : Pointer) : PGdkImage;
begin
  GDK_IMAGE:=PGdkImage(G_TYPE_CHECK_INSTANCE_CAST(anObject,GDK_TYPE_IMAGE));
end;

function GDK_IMAGE_CLASS(klass : Pointer) : PGdkImageClass;
begin
  GDK_IMAGE_CLASS:=PGdkImageClass(G_TYPE_CHECK_CLASS_CAST(klass,GDK_TYPE_IMAGE));
end;

function GDK_IS_IMAGE(anObject : Pointer) : boolean;
begin
  GDK_IS_IMAGE:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,GDK_TYPE_IMAGE);
end;

function GDK_IS_IMAGE_CLASS(klass : Pointer) : boolean;
begin
  GDK_IS_IMAGE_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GDK_TYPE_IMAGE);
end;

function GDK_IMAGE_GET_CLASS(obj : Pointer) : PGdkImageClass;
begin
  GDK_IMAGE_GET_CLASS:=PGdkImageClass(G_TYPE_INSTANCE_GET_CLASS(obj,GDK_TYPE_IMAGE));
end;

procedure gdk_image_destroy(image:PGdkImage);
begin
  g_object_unref(G_OBJECT(image));
end;
{$ENDIF}

