{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkVRuler = ^TGtkVRuler;
     TGtkVRuler = record
          ruler : TGtkRuler;
       end;

     PGtkVRulerClass = ^TGtkVRulerClass;
     TGtkVRulerClass = record
          parent_class : TGtkRulerClass;
       end;

Type
  GTK_VRULER=PGtkVRuler;
  GTK_VRULER_CLASS=PGtkVRulerClass;

function  GTK_VRULER_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_vruler_get_type';
function  GTK_IS_VRULER(obj:pointer):boolean;
function  GTK_IS_VRULER_CLASS(klass:pointer):boolean;

function  gtk_vruler_get_type:TGtkType;cdecl;external gtkdll name 'gtk_vruler_get_type';
function  gtk_vruler_new:PGtkWidget;cdecl;external gtkdll name 'gtk_vruler_new';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_VRULER(obj:pointer):boolean;
begin
  GTK_IS_VRULER:=(obj<>nil) and GTK_IS_VRULER_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_VRULER_CLASS(klass:pointer):boolean;
begin
  GTK_IS_VRULER_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_VRULER_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.2  2002-09-07 15:43:00  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/01/29 17:55:14  peter
    * splitted to base and extra

}
