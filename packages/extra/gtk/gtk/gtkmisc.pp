{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkMisc = ^TGtkMisc;
       TGtkMisc = record
            widget : TGtkWidget;
            xalign : gfloat;
            yalign : gfloat;
            xpad : guint16;
            ypad : guint16;
         end;

       PGtkMiscClass = ^TGtkMiscClass;
       TGtkMiscClass = record
            parent_class : TGtkWidgetClass;
         end;

Type
  GTK_MISC=PGtkMisc;
  GTK_MISC_CLASS=PGtkMiscClass;

function  GTK_MISC_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_misc_get_type';
function  GTK_IS_MISC(obj:pointer):boolean;
function  GTK_IS_MISC_CLASS(klass:pointer):boolean;

function  gtk_misc_get_type:TGtkType;cdecl;external gtkdll name 'gtk_misc_get_type';
procedure gtk_misc_set_alignment(misc:PGtkMisc; xalign:gfloat; yalign:gfloat);cdecl;external gtkdll name 'gtk_misc_set_alignment';
procedure gtk_misc_set_padding(misc:PGtkMisc; xpad:gint; ypad:gint);cdecl;external gtkdll name 'gtk_misc_set_padding';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_MISC(obj:pointer):boolean;
begin
  GTK_IS_MISC:=(obj<>nil) and GTK_IS_MISC_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_MISC_CLASS(klass:pointer):boolean;
begin
  GTK_IS_MISC_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_MISC_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2002-01-29 17:55:12  peter
    * splitted to base and extra

  Revision 1.2  2000/07/13 11:33:22  michael
  + removed logs
 
}
