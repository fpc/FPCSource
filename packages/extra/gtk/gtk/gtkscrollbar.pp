{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkScrollbar = ^TGtkScrollbar;
       TGtkScrollbar = record
            range : TGtkRange;
         end;

       PGtkScrollbarClass = ^TGtkScrollbarClass;
       TGtkScrollbarClass = record
            parent_class : TGtkRangeClass;
         end;

Type
  GTK_SCROLLBAR=PGtkScrollbar;
  GTK_SCROLLBAR_CLASS=PGtkScrollbarClass;

function  GTK_SCROLLBAR_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_scrollbar_get_type';
function  GTK_IS_SCROLLBAR(obj:pointer):boolean;
function  GTK_IS_SCROLLBAR_CLASS(klass:pointer):boolean;

function  gtk_scrollbar_get_type:TGtkType;cdecl;external gtkdll name 'gtk_scrollbar_get_type';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_SCROLLBAR(obj:pointer):boolean;
begin
  GTK_IS_SCROLLBAR:=(obj<>nil) and GTK_IS_SCROLLBAR_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_SCROLLBAR_CLASS(klass:pointer):boolean;
begin
  GTK_IS_SCROLLBAR_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_SCROLLBAR_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.2  2002-09-07 15:43:00  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/01/29 17:55:13  peter
    * splitted to base and extra

}
