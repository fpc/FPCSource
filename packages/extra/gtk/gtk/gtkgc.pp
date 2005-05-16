{
   $Id: gtkgc.pp,v 1.3 2005/02/14 17:13:20 peter Exp $
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
  $Log: gtkgc.pp,v $
  Revision 1.3  2005/02/14 17:13:20  peter
    * truncate log

}
