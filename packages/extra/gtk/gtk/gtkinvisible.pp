{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkInvisible = ^TGtkInvisible;
     TGtkInvisible = record
          bin : TGtkBin;
       end;

     PGtkInvisibleClass = ^TGtkInvisibleClass;
     TGtkInvisibleClass = record
          parent_class : TGtkBinClass;
       end;

type
  GTK_INVISIBLE=PGtkInvisible;
  GTK_INVISIBLE_CLASS=PGtkInvisibleClass;

function  GTK_INVISIBLE_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_invisible_get_type';
function  GTK_IS_INVISIBLE(obj:pointer):boolean;
function  GTK_IS_INVISIBLE_CLASS(klass:pointer):boolean;

function  gtk_invisible_get_type:TGtkType;cdecl;external gtkdll name 'gtk_invisible_get_type';
function  gtk_invisible_new:PGtkWidget;cdecl;external gtkdll name 'gtk_invisible_new';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_INVISIBLE(obj:pointer):boolean;
begin
  GTK_IS_INVISIBLE:=(obj<>nil) and GTK_IS_INVISIBLE_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_INVISIBLE_CLASS(klass:pointer):boolean;
begin
  GTK_IS_INVISIBLE_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_INVISIBLE_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.2  2002-09-07 15:42:59  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/01/29 17:55:11  peter
    * splitted to base and extra

}
