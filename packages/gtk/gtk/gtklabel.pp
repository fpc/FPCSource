{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkLabelWord = pointer;

       PGtkLabel = ^TGtkLabel;
       TGtkLabel = record
          misc : TGtkMisc;
          thelabel : Pgchar;
          label_wc : PGdkWChar;
          pattern : Pgchar;
          words : PGtkLabelWord;
          flag0 : longint;
          wrap : gboolean;
        end;

  const
     bm_TGtkLabel_max_width = $FFFF;
     bp_TGtkLabel_max_width = 0;
     bm_TGtkLabel_jtype = $30000;
     bp_TGtkLabel_jtype = 16;
function  max_width(var a : TGtkLabel) : guint;
procedure set_max_width(var a : TGtkLabel; __max_width : guint);
function  jtype(var a : TGtkLabel) : guint;
procedure set_jtype(var a : TGtkLabel; __jtype : guint);

    type
       PGtkLabelClass = ^TGtkLabelClass;
       TGtkLabelClass = record
            parent_class : TGtkMiscClass;
         end;

Type
  GTK_LABEL=PGtkLabel;
  GTK_LABEL_CLASS=PGtkLabelClass;

function  GTK_LABEL_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_label_get_type';
function  GTK_IS_LABEL(obj:pointer):boolean;
function  GTK_IS_LABEL_CLASS(klass:pointer):boolean;

function  gtk_label_get_type:TGtkType;cdecl;external gtkdll name 'gtk_label_get_type';
function  gtk_label_new (str:pchar):PGtkWidget;cdecl;external gtkdll name 'gtk_label_new';
procedure gtk_label_set_text(theLabel: PGtkLabel; str:pchar);cdecl;external gtkdll name 'gtk_label_set_text';
procedure gtk_label_set_justify(thelabel:PGtkLabel; jtype:TGtkJustification);cdecl;external gtkdll name 'gtk_label_set_justify';
procedure gtk_label_set_pattern(thelabel:PGtkLabel; pattern:Pgchar);cdecl;external gtkdll name 'gtk_label_set_pattern';
procedure gtk_label_set_line_wrap(thelabel:PGtkLabel; wrap:gboolean);cdecl;external gtkdll name 'gtk_label_set_line_wrap';
procedure gtk_label_get(thelabel:PGtkLabel; str:ppchar);cdecl;external gtkdll name 'gtk_label_get';
function  gtk_label_parse_uline(thelabel:PGtkLabel;thestring:Pgchar):guint;cdecl;external gtkdll name 'gtk_label_parse_uline';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  max_width(var a : TGtkLabel) : guint;
    begin
       max_width:=(a.flag0 and bm_TGtkLabel_max_width) shr bp_TGtkLabel_max_width;
    end;

procedure set_max_width(var a : TGtkLabel; __max_width : guint);
    begin
       a.flag0:=a.flag0 or ((__max_width shl bp_TGtkLabel_max_width) and bm_TGtkLabel_max_width);
    end;

function  jtype(var a : TGtkLabel) : guint;
    begin
       jtype:=(a.flag0 and bm_TGtkLabel_jtype) shr bp_TGtkLabel_jtype;
    end;

procedure set_jtype(var a : TGtkLabel; __jtype : guint);
    begin
       a.flag0:=a.flag0 or ((__jtype shl bp_TGtkLabel_jtype) and bm_TGtkLabel_jtype);
    end;

function  GTK_IS_LABEL(obj:pointer):boolean;
begin
  GTK_IS_LABEL:=(obj<>nil) and GTK_IS_LABEL_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_LABEL_CLASS(klass:pointer):boolean;
begin
  GTK_IS_LABEL_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_LABEL_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:05  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:36  peter
    * moved to packages dir

  Revision 1.13  1999/10/21 08:42:01  florian
    * some changes to get it work with gtk 1.3 under Windows 98:
      - removed some trailing space after the import name
      - In gtkbindings.h is
        #define  gtk_binding_entry_add          gtk_binding_entry_clear
        so in the pascal headers the import name of gtk_bindings_entry_add should be
        gtk_binding_entry_clear!
      - removed the declaration of
        gtk_drag_source_unset in gtkdnd.pp it isn't in gtk-1.3.dll!
      - in gdk.pp glibdll must be set to gdk-1.3:
        const
           gdkdll='gdk-1.3';
           glibdll='gdk-1.3';
        else the whole gdk_* calls are imported from glib-1.3.dll which is wrong!

  Revision 1.12  1999/10/06 17:42:49  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.11  1999/07/23 16:12:37  peter
    * use packrecords C

  Revision 1.10  1999/05/16 17:07:32  peter
    * removed gtk_label_set which has been changed to gtk_label_set_text

  Revision 1.9  1999/05/15 21:34:20  peter
    + gtk_label_set_text

  Revision 1.8  1999/05/11 00:38:51  peter
    * win32 fixes

  Revision 1.7  1999/05/10 15:19:42  peter
    * cdecl fixes

  Revision 1.6  1999/05/10 09:03:18  peter
    * gtk 1.2 port working

  Revision 1.5  1999/05/07 17:40:27  peter
    * more updates

  Revision 1.4  1998/11/09 10:10:06  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:22:44  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

