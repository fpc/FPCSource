{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkAccelLabel = ^TGtkAccelLabel;
     TGtkAccelLabel = record
          _label : TGtkLabel;
          queue_id : guint;
          accel_padding : guint;
          accel_widget : PGtkWidget;
          accel_string : Pgchar;
          accel_string_width : guint16;
       end;

     PGtkAccelLabelClass = ^TGtkAccelLabelClass;
     TGtkAccelLabelClass = record
          parent_class : TGtkLabelClass;
          signal_quote1 : Pgchar;
          signal_quote2 : Pgchar;
          mod_name_shift : Pgchar;
          mod_name_control : Pgchar;
          mod_name_alt : Pgchar;
          mod_separator : Pgchar;
          accel_seperator : Pgchar;
          flag0 : {$ifdef win32}longint{$else}word{$endif};
       end;

const
  bm_TGtkAccelLabelClass_latin1_to_char = $1;
  bp_TGtkAccelLabelClass_latin1_to_char = 0;

type
  GTK_ACCEL_LABEL=PGtkAccelLabel;
  GTK_ACCEL_LABEL_CLASS=PGtkAccelLabelClass;


function  latin1_to_char(var a : TGtkAccelLabelClass) : guint;
procedure set_latin1_to_char(var a : TGtkAccelLabelClass; __latin1_to_char : guint);

function  GTK_ACCEL_LABEL_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_accel_label_get_type';
function  GTK_IS_ACCEL_LABEL(obj:pointer):boolean;
function  GTK_IS_ACCEL_LABEL_CLASS(klass:pointer):boolean;

function  gtk_accel_label_get_type:TGtkType;cdecl;external gtkdll name 'gtk_accel_label_get_type';
function  gtk_accel_label_new(thestring:Pgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_accel_label_new';
function  gtk_accel_label_get_accel_width(accel_label:PGtkAccelLabel):guint;cdecl;external gtkdll name 'gtk_accel_label_get_accel_width';
procedure gtk_accel_label_set_accel_widget(accel_label:PGtkAccelLabel; accel_widget:PGtkWidget);cdecl;external gtkdll name 'gtk_accel_label_set_accel_widget';
function  gtk_accel_label_refetch(accel_label:PGtkAccelLabel):gboolean;cdecl;external gtkdll name 'gtk_accel_label_refetch';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  latin1_to_char(var a : TGtkAccelLabelClass) : guint;
    begin
       latin1_to_char:=(a.flag0 and bm_TGtkAccelLabelClass_latin1_to_char) shr bp_TGtkAccelLabelClass_latin1_to_char;
    end;

procedure set_latin1_to_char(var a : TGtkAccelLabelClass; __latin1_to_char : guint);
    begin
       a.flag0:=a.flag0 or ((__latin1_to_char shl bp_TGtkAccelLabelClass_latin1_to_char) and bm_TGtkAccelLabelClass_latin1_to_char);
    end;

function  GTK_IS_ACCEL_LABEL(obj:pointer):boolean;
begin
  GTK_IS_ACCEL_LABEL:=(obj<>nil) and GTK_IS_ACCEL_LABEL_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_ACCEL_LABEL_CLASS(klass:pointer):boolean;
begin
  GTK_IS_ACCEL_LABEL_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_ACCEL_LABEL_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1.2.1  2000-09-09 18:42:52  peter
    * gtk win32 fixes

  Revision 1.1  2000/07/13 06:34:02  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:35  peter
    * moved to packages dir

  Revision 1.6  1999/10/06 17:42:48  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.5  1999/07/23 16:11:50  peter
    * use packrecords C

  Revision 1.4  1999/05/11 00:38:02  peter
    * win32 fixes

  Revision 1.3  1999/05/10 15:18:49  peter
    * cdecl fixes

  Revision 1.2  1999/05/10 09:02:53  peter
    * gtk 1.2 port working

  Revision 1.1  1999/05/07 10:40:23  peter
    * first things for 1.2

}

