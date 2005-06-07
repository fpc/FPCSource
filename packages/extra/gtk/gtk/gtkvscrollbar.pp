{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkVScrollbar = ^TGtkVScrollbar;
       TGtkVScrollbar = record
            scrollbar : TGtkScrollbar;
         end;

       PGtkVScrollbarClass = ^TGtkVScrollbarClass;
       TGtkVScrollbarClass = record
            parent_class : TGtkScrollbarClass;
         end;

Type
  GTK_VSCROLLBAR=PGtkVScrollbar;
  GTK_VSCROLLBAR_CLASS=PGtkVScrollbarClass;

function  GTK_VSCROLLBAR_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_vscrollbar_get_type';
function  GTK_IS_VSCROLLBAR(obj:pointer):boolean;
function  GTK_IS_VSCROLLBAR_CLASS(klass:pointer):boolean;

function  gtk_vscrollbar_get_type:TGtkType;cdecl;external gtkdll name 'gtk_vscrollbar_get_type';
function  gtk_vscrollbar_new (adjustment:PGtkAdjustment):PGtkWidget;cdecl;external gtkdll name 'gtk_vscrollbar_new';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_VSCROLLBAR(obj:pointer):boolean;
begin
  GTK_IS_VSCROLLBAR:=(obj<>nil) and GTK_IS_VSCROLLBAR_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_VSCROLLBAR_CLASS(klass:pointer):boolean;
begin
  GTK_IS_VSCROLLBAR_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_VSCROLLBAR_TYPE);
end;

{$endif read_implementation}


