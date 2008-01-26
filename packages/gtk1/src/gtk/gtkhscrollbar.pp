{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkHScrollbar = ^TGtkHScrollbar;
       TGtkHScrollbar = record
            scrollbar : TGtkScrollbar;
         end;

       PGtkHScrollbarClass = ^TGtkHScrollbarClass;
       TGtkHScrollbarClass = record
            parent_class : TGtkScrollbarClass;
         end;

Type
  GTK_HSCROLLBAR=PGtkHScrollbar;
  GTK_HSCROLLBAR_CLASS=PGtkHScrollbarClass;

function  GTK_HSCROLLBAR_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_hscrollbar_get_type';
function  GTK_IS_HSCROLLBAR(obj:pointer):boolean;
function  GTK_IS_HSCROLLBAR_CLASS(klass:pointer):boolean;

function  gtk_hscrollbar_get_type:TGtkType;cdecl;external gtkdll name 'gtk_hscrollbar_get_type';
function  gtk_hscrollbar_new (adjustment:PGtkAdjustment):PGtkWidget;cdecl;external gtkdll name 'gtk_hscrollbar_new';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_HSCROLLBAR(obj:pointer):boolean;
begin
  GTK_IS_HSCROLLBAR:=(obj<>nil) and GTK_IS_HSCROLLBAR_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_HSCROLLBAR_CLASS(klass:pointer):boolean;
begin
  GTK_IS_HSCROLLBAR_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_HSCROLLBAR_TYPE);
end;

{$endif read_implementation}


