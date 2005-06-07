{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

type
     TGtkPackerOptions = longint;
const
     GTK_PACK_EXPAND = 1 shl 0;
     GTK_FILL_X = 1 shl 1;
     GTK_FILL_Y = 1 shl 2;

type
     TGtkSideType = longint;
const
     GTK_SIDE_TOP = 0;
     GTK_SIDE_BOTTOM = 1;
     GTK_SIDE_LEFT = 2;
     GTK_SIDE_RIGHT = 3;

type
     TGtkAnchorType = longint;
const
       GTK_ANCHOR_CENTER = 0;
       GTK_ANCHOR_NORTH = 1;
       GTK_ANCHOR_NORTH_WEST = 2;
       GTK_ANCHOR_NORTH_EAST = 3;
       GTK_ANCHOR_SOUTH = 4;
       GTK_ANCHOR_SOUTH_WEST = 5;
       GTK_ANCHOR_SOUTH_EAST = 6;
       GTK_ANCHOR_WEST = 7;
       GTK_ANCHOR_EAST = 8;
       GTK_ANCHOR_N = GTK_ANCHOR_NORTH;
       GTK_ANCHOR_NW = GTK_ANCHOR_NORTH_WEST;
       GTK_ANCHOR_NE = GTK_ANCHOR_NORTH_EAST;
       GTK_ANCHOR_S = GTK_ANCHOR_SOUTH;
       GTK_ANCHOR_SW = GTK_ANCHOR_SOUTH_WEST;
       GTK_ANCHOR_SE = GTK_ANCHOR_SOUTH_EAST;
       GTK_ANCHOR_W = GTK_ANCHOR_WEST;
       GTK_ANCHOR_E = GTK_ANCHOR_EAST;

type
     PGtkPackerChild = ^TGtkPackerChild;
     TGtkPackerChild = record
          widget : PGtkWidget;
          anchor : TGtkAnchorType;
          side : TGtkSideType;
          options : TGtkPackerOptions;
          flag0 : longint;
          flag1 : longint;
          flag2 : {$ifdef win32}longint{$else}word{$endif};
       end;

     PGtkPacker = ^TGtkPacker;
     TGtkPacker = record
          parent : TGtkContainer;
          children : PGList;
          spacing : guint;
          flag0 : longint;
          flag1 : longint;
          flag2 : {$ifdef win32}longint{$else}word{$endif};
      end;

     PGtkPackerClass = ^TGtkPackerClass;
     TGtkPackerClass = record
          parent_class : TGtkContainerClass;
       end;

type
  GTK_PACKER=PGtkPacker;
  GTK_PACKER_CLASS=PGtkPackerClass;

function  GTK_PACKER_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_packer_get_type';
function  GTK_IS_PACKER(obj:pointer):boolean;
function  GTK_IS_PACKER_CLASS(klass:pointer):boolean;

function  gtk_packer_get_type:TGtkType;cdecl;external gtkdll name 'gtk_packer_get_type';
function  gtk_packer_new:PGtkWidget;cdecl;external gtkdll name 'gtk_packer_new';
procedure gtk_packer_add_defaults(packer:PGtkPacker; child:PGtkWidget; side:TGtkSideType; anchor:TGtkAnchorType; options:TGtkPackerOptions);cdecl;external gtkdll name 'gtk_packer_add_defaults';
procedure gtk_packer_add(packer:PGtkPacker; child:PGtkWidget; side:TGtkSideType; anchor:TGtkAnchorType; options:TGtkPackerOptions; border_width:guint; pad_x:guint; pad_y:guint; i_pad_x:guint; i_pad_y:guint);cdecl;external gtkdll name 'gtk_packer_add';
procedure gtk_packer_set_child_packing(packer:PGtkPacker; child:PGtkWidget; side:TGtkSideType; anchor:TGtkAnchorType; options:TGtkPackerOptions; border_width:guint; pad_x:guint; pad_y:guint; i_pad_x:guint; i_pad_y:guint);cdecl;external gtkdll name 'gtk_packer_set_child_packing';
procedure gtk_packer_reorder_child(packer:PGtkPacker; child:PGtkWidget; position:gint);cdecl;external gtkdll name 'gtk_packer_reorder_child';
procedure gtk_packer_set_spacing(packer:PGtkPacker; spacing:guint);cdecl;external gtkdll name 'gtk_packer_set_spacing';
procedure gtk_packer_set_default_border_width(packer:PGtkPacker; border:guint);cdecl;external gtkdll name 'gtk_packer_set_default_border_width';
procedure gtk_packer_set_default_pad(packer:PGtkPacker; pad_x:guint; pad_y:guint);cdecl;external gtkdll name 'gtk_packer_set_default_pad';
procedure gtk_packer_set_default_ipad(packer:PGtkPacker; i_pad_x:guint; i_pad_y:guint);cdecl;external gtkdll name 'gtk_packer_set_default_ipad';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_PACKER(obj:pointer):boolean;
begin
  GTK_IS_PACKER:=(obj<>nil) and GTK_IS_PACKER_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_PACKER_CLASS(klass:pointer):boolean;
begin
  GTK_IS_PACKER_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_PACKER_TYPE);
end;

{$endif read_implementation}


