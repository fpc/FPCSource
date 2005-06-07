{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkBox = ^TGtkBox;
       TGtkBox = record
            container : TGtkContainer;
            children : PGList;
            spacing : gint16;
            flag0 : {$ifdef win32}longint{$else}word{$endif};
         end;

    const
       bm_box_homogeneous = 1;
       bp_box_homogeneous = 0;
function  homogeneous(var a : TGtkBox) : guint;
procedure set_homogeneous(var a : TGtkBox; __homogeneous : guint);

    type
       PGtkBoxClass = ^TGtkBoxClass;
       TGtkBoxClass = record
            parent_class : TGtkContainerClass;
         end;

       PGtkBoxChild = ^TGtkBoxChild;
       TGtkBoxChild = record
            widget : PGtkWidget;
            padding : guint16;
            flag0 : {$ifdef win32}longint{$else}word{$endif};
         end;

    const
       bm_box_expand = 1;
       bp_box_expand = 0;
       bm_box_fill = 2;
       bp_box_fill = 1;
       bm_box_pack = 4;
       bp_box_pack = 2;

function  expand(var a : TGtkBoxChild) : guint;
procedure set_expand(var a : TGtkBoxChild; __expand : guint);
function  fill(var a : TGtkBoxChild) : guint;
procedure set_fill(var a : TGtkBoxChild; __fill : guint);
function  pack(var a : TGtkBoxChild) : guint;
procedure set_pack(var a : TGtkBoxChild; __pack : guint);

Type
  GTK_BOX=PGtkBox;
  GTK_BOX_CLASS=PGtkBoxClass;

function  GTK_BOX_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_box_get_type';
function  GTK_IS_BOX(obj:pointer):boolean;
function  GTK_IS_BOX_CLASS(klass:pointer):boolean;

function  gtk_box_get_type:TGtkType;cdecl;external gtkdll name 'gtk_box_get_type';
procedure gtk_box_pack_start(box:PGtkBox; child:PGtkWidget; expand:gboolean; fill:gboolean; padding:guint);cdecl;external gtkdll name 'gtk_box_pack_start';
procedure gtk_box_pack_end(box:PGtkBox; child:PGtkWidget; expand:gboolean; fill:gboolean; padding:guint);cdecl;external gtkdll name 'gtk_box_pack_end';
procedure gtk_box_pack_start_defaults(box:PGtkBox; widget:PGtkWidget);cdecl;external gtkdll name 'gtk_box_pack_start_defaults';
procedure gtk_box_pack_end_defaults(box:PGtkBox; widget:PGtkWidget);cdecl;external gtkdll name 'gtk_box_pack_end_defaults';
procedure gtk_box_set_homogeneous(box:PGtkBox; homogeneous:gboolean);cdecl;external gtkdll name 'gtk_box_set_homogeneous';
procedure gtk_box_set_spacing(box:PGtkBox; spacing:gint);cdecl;external gtkdll name 'gtk_box_set_spacing';
procedure gtk_box_reorder_child(box:PGtkBox; child:PGtkWidget; position:gint);cdecl;external gtkdll name 'gtk_box_reorder_child';
procedure gtk_box_query_child_packing(box:PGtkBox; child:PGtkWidget; expand:Pgboolean; fill:Pgboolean; padding:Pguint;pack_type:PGtkPackType);cdecl;external gtkdll name 'gtk_box_query_child_packing';
procedure gtk_box_set_child_packing(box:PGtkBox; child:PGtkWidget; expand:gboolean; fill:gboolean; padding:guint; pack_type:TGtkPackType);cdecl;external gtkdll name 'gtk_box_set_child_packing';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  homogeneous(var a : TGtkBox) : guint;
      begin
         homogeneous:=(a.flag0 and bm_box_homogeneous) shr bp_box_homogeneous;
      end;

procedure set_homogeneous(var a : TGtkBox; __homogeneous : guint);
      begin
         a.flag0:=a.flag0 or ((__homogeneous shl bp_box_homogeneous) and bm_box_homogeneous);
      end;

function  expand(var a : TGtkBoxChild) : guint;
      begin
         expand:=(a.flag0 and bm_box_expand) shr bp_box_expand;
      end;

procedure set_expand(var a : TGtkBoxChild; __expand : guint);
      begin
         a.flag0:=a.flag0 or ((__expand shl bp_box_expand) and bm_box_expand);
      end;

function  fill(var a : TGtkBoxChild) : guint;
      begin
         fill:=(a.flag0 and bm_box_fill) shr bp_box_fill;
      end;

procedure set_fill(var a : TGtkBoxChild; __fill : guint);
      begin
         a.flag0:=a.flag0 or ((__fill shl bp_box_fill) and bm_box_fill);
      end;

function  pack(var a : TGtkBoxChild) : guint;
      begin
         pack:=(a.flag0 and bm_box_pack) shr bp_box_pack;
      end;

procedure set_pack(var a : TGtkBoxChild; __pack : guint);
      begin
         a.flag0:=a.flag0 or ((__pack shl bp_box_pack) and bm_box_pack);
      end;

function  GTK_IS_BOX(obj:pointer):boolean;
begin
  GTK_IS_BOX:=(obj<>nil) and GTK_IS_BOX_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_BOX_CLASS(klass:pointer):boolean;
begin
  GTK_IS_BOX_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_BOX_TYPE);
end;

{$endif read_implementation}


