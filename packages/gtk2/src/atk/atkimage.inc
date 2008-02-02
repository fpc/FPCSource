// included by atk.pp

{
   The AtkImage interface should be supported by any anObject that has an
   associated image. This interface provides the standard mechanism for
   an assistive technology to get descriptive information about images.
  }

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PAtkImageIface = ^TAtkImageIface;
   TAtkImageIface = record
        parent : TGTypeInterface;
        get_image_position : procedure (image:PAtkImage; x:Pgint; y:Pgint; coord_type:TAtkCoordType); cdecl;
        get_image_description : function (image:PAtkImage):Pgchar; cdecl;
        get_image_size : procedure (image:PAtkImage; width:Pgint; height:Pgint); cdecl;
        set_image_description : function (image:PAtkImage; description:Pgchar):gboolean; cdecl;
        pad1 : TAtkFunction;
        pad2 : TAtkFunction;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function ATK_TYPE_IMAGE : GType;
function ATK_IS_IMAGE(obj: pointer) : boolean;
function ATK_IMAGE(obj: pointer) : PAtkImage;
function ATK_IMAGE_GET_IFACE(obj: pointer) : PAtkImageIface;


function atk_image_get_type:GType; cdecl; external atklib;
function atk_image_get_image_description(image:PAtkImage):Pgchar; cdecl; external atklib;
procedure atk_image_get_image_size(image:PAtkImage; width:Pgint; height:Pgint); cdecl; external atklib;
function atk_image_set_image_description(image:PAtkImage; description:Pgchar):gboolean; cdecl; external atklib;
procedure atk_image_get_image_position(image:PAtkImage; x:Pgint; y:Pgint; coord_type:TAtkCoordType); cdecl; external atklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function ATK_TYPE_IMAGE : GType;
begin
   ATK_TYPE_IMAGE:=atk_image_get_type;
end;

function ATK_IS_IMAGE(obj: pointer) : boolean;
begin
   ATK_IS_IMAGE:=G_TYPE_CHECK_INSTANCE_TYPE(obj,ATK_TYPE_IMAGE);
end;

function ATK_IMAGE(obj: pointer) : PAtkImage;
begin
   ATK_IMAGE:=PAtkImage(G_TYPE_CHECK_INSTANCE_CAST(obj,ATK_TYPE_IMAGE));
end;

function ATK_IMAGE_GET_IFACE(obj: pointer) : PAtkImageIface;
begin
   ATK_IMAGE_GET_IFACE:=PAtkImageIface(G_TYPE_INSTANCE_GET_INTERFACE(obj,ATK_TYPE_IMAGE));
end;

{$ENDIF read_implementation}

