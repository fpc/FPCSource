{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

function  gtk_gc_get (depth:gint; colormap:PGdkColormap; values:PGdkGCValues; values_mask:TGdkGCValuesMask): PGdkGC;cdecl;external gtkdll name 'gtk_gc_get';
procedure gtk_gc_release(gc:PGdkGC);cdecl;external gtkdll name 'gtk_gc_release';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}
{$endif read_implementation}


{
  $Log$
  Revision 1.2  2002-09-07 15:42:59  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/01/29 17:55:11  peter
    * splitted to base and extra

}
