{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkVSeparator = ^TGtkVSeparator;
     TGtkVSeparator = record
          separator : TGtkSeparator;
       end;

     PGtkVSeparatorClass = ^TGtkVSeparatorClass;
     TGtkVSeparatorClass = record
          parent_class : TGtkSeparatorClass;
       end;

Type
  GTK_VSEPARATOR=PGtkVSeparator;
  GTK_VSEPARATOR_CLASS=PGtkVSeparatorClass;

function  GTK_VSEPARATOR_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_vseparator_get_type';
function  GTK_IS_VSEPARATOR(obj:pointer):boolean;
function  GTK_IS_VSEPARATOR_CLASS(klass:pointer):boolean;

function  gtk_vseparator_get_type:TGtkType;cdecl;external gtkdll name 'gtk_vseparator_get_type';
function  gtk_vseparator_new:PGtkWidget;cdecl;external gtkdll name 'gtk_vseparator_new';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_VSEPARATOR(obj:pointer):boolean;
begin
  GTK_IS_VSEPARATOR:=(obj<>nil) and GTK_IS_VSEPARATOR_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_VSEPARATOR_CLASS(klass:pointer):boolean;
begin
  GTK_IS_VSEPARATOR_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_VSEPARATOR_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2002-01-29 17:55:14  peter
    * splitted to base and extra

  Revision 1.2  2000/07/13 11:33:25  michael
  + removed logs
 
}
