{
   $Id$
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


{
  $Log$
  Revision 1.1.2.1  2000-09-09 18:42:52  peter
    * gtk win32 fixes

  Revision 1.1  2000/07/13 06:34:03  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:35  peter
    * moved to packages dir

  Revision 1.11  1999/10/06 17:42:48  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.10  1999/07/23 16:12:00  peter
    * use packrecords C

  Revision 1.9  1999/05/11 00:38:12  peter
    * win32 fixes

  Revision 1.8  1999/05/10 15:18:59  peter
    * cdecl fixes

  Revision 1.7  1999/05/10 09:02:57  peter
    * gtk 1.2 port working

  Revision 1.6  1999/05/07 10:40:30  peter
    * first things for 1.2

  Revision 1.5  1998/11/09 10:09:36  peter
    + C type casts are now correctly handled

  Revision 1.4  1998/10/21 22:25:16  peter
    * fixed some wrong cdecls

  Revision 1.3  1998/10/21 20:22:11  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

