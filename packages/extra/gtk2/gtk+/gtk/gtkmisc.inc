// included by gtk2.pas

{$IFDEF read_forward_definitions}
   PGtkMisc = ^TGtkMisc;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
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
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_MISC : GType;
function GTK_MISC(obj: pointer) : PGtkMisc;
function GTK_MISC_CLASS(klass: pointer) : PGtkMiscClass;
function GTK_IS_MISC(obj: pointer) : boolean;
function GTK_IS_MISC_CLASS(klass: pointer) : boolean;
function GTK_MISC_GET_CLASS(obj: pointer) : PGtkMiscClass;

function gtk_misc_get_type:TGtkType; cdecl; external gtklib;
procedure gtk_misc_set_alignment(misc:PGtkMisc; xalign:gfloat; yalign:gfloat); cdecl; external gtklib;
procedure gtk_misc_get_alignment(misc:PGtkMisc; var xalign:Pgfloat; var yalign:Pgfloat); cdecl; external gtklib;
procedure gtk_misc_set_padding(misc:PGtkMisc; xpad:gint; ypad:gint); cdecl; external gtklib;
procedure gtk_misc_get_padding(misc:PGtkMisc; var xpad:Pgint; var ypad:Pgint); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_MISC : GType;
begin
   GTK_TYPE_MISC:=gtk_misc_get_type;
end;

function GTK_MISC(obj: pointer) : PGtkMisc;
begin
   GTK_MISC:=PGtkMisc(GTK_CHECK_CAST(obj,GTK_TYPE_MISC));
end;

function GTK_MISC_CLASS(klass: pointer) : PGtkMiscClass;
begin
   GTK_MISC_CLASS:=PGtkMiscClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_MISC));
end;

function GTK_IS_MISC(obj: pointer) : boolean;
begin
   GTK_IS_MISC:=GTK_CHECK_TYPE(obj,GTK_TYPE_MISC);
end;

function GTK_IS_MISC_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_MISC_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_MISC);
end;

function GTK_MISC_GET_CLASS(obj: pointer) : PGtkMiscClass;
begin
   GTK_MISC_GET_CLASS:=PGtkMiscClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_MISC));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

