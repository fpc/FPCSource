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
  Revision 1.1  2000-07-13 06:34:04  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:36  peter
    * moved to packages dir

  Revision 1.5  1999/10/06 17:42:49  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.4  1999/07/23 16:12:33  peter
    * use packrecords C

  Revision 1.3  1999/05/11 00:38:48  peter
    * win32 fixes

  Revision 1.2  1999/05/10 15:19:39  peter
    * cdecl fixes

  Revision 1.1  1999/05/07 17:40:26  peter
    * more updates

}

