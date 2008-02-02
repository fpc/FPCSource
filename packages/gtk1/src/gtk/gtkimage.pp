{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkImage = ^TGtkImage;
     TGtkImage = record
          misc : TGtkMisc;
          image : PGdkImage;
          mask : PGdkBitmap;
       end;

     PGtkImageClass = ^TGtkImageClass;
     TGtkImageClass = record
          parent_class : TGtkMiscClass;
       end;

Type
  GTK_IMAGE=PGtkImage;
  GTK_IMAGE_CLASS=PGtkImageClass;

function  GTK_IMAGE_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_image_get_type';
function  GTK_IS_IMAGE(obj:pointer):boolean;
function  GTK_IS_IMAGE_CLASS(klass:pointer):boolean;

function  gtk_image_get_type:TGtkType;cdecl;external gtkdll name 'gtk_image_get_type';
function  gtk_image_new(val:PGdkImage; mask:PGdkBitmap):PGtkWidget;cdecl;external gtkdll name 'gtk_image_new';
procedure gtk_image_set(image:PGtkImage; val:PGdkImage; mask:PGdkBitmap);cdecl;external gtkdll name 'gtk_image_set';
procedure gtk_image_get(image:PGtkImage; val:PPGdkImage; mask:PPGdkBitmap);cdecl;external gtkdll name 'gtk_image_get';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_IMAGE(obj:pointer):boolean;
begin
  GTK_IS_IMAGE:=(obj<>nil) and GTK_IS_IMAGE_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_IMAGE_CLASS(klass:pointer):boolean;
begin
  GTK_IS_IMAGE_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_IMAGE_TYPE);
end;

{$endif read_implementation}


