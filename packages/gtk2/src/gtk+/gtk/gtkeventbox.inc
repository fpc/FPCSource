// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkEventBox = ^TGtkEventBox;
   TGtkEventBox = record
        bin : TGtkBin;
     end;

   PGtkEventBoxClass = ^TGtkEventBoxClass;
   TGtkEventBoxClass = record
        parent_class : TGtkBinClass;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_EVENT_BOX : GType;
function GTK_EVENT_BOX(obj: pointer) : PGtkEventBox;
function GTK_EVENT_BOX_CLASS(klass: pointer) : PGtkEventBoxClass;
function GTK_IS_EVENT_BOX(obj: pointer) : boolean;
function GTK_IS_EVENT_BOX_CLASS(klass: pointer) : boolean;
function GTK_EVENT_BOX_GET_CLASS(obj: pointer) : PGtkEventBoxClass;


function gtk_event_box_get_type:TGtkType; cdecl; external gtklib;
function gtk_event_box_new:PGtkWidget; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_EVENT_BOX : GType;
begin
   GTK_TYPE_EVENT_BOX:=gtk_event_box_get_type;
end;

function GTK_EVENT_BOX(obj: pointer) : PGtkEventBox;
begin
   GTK_EVENT_BOX:=PGtkEventBox(GTK_CHECK_CAST(obj,GTK_TYPE_EVENT_BOX));
end;

function GTK_EVENT_BOX_CLASS(klass: pointer) : PGtkEventBoxClass;
begin
   GTK_EVENT_BOX_CLASS:=PGtkEventBoxClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_EVENT_BOX));
end;

function GTK_IS_EVENT_BOX(obj: pointer) : boolean;
begin
   GTK_IS_EVENT_BOX:=GTK_CHECK_TYPE(obj,GTK_TYPE_EVENT_BOX);
end;

function GTK_IS_EVENT_BOX_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_EVENT_BOX_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_EVENT_BOX);
end;

function GTK_EVENT_BOX_GET_CLASS(obj: pointer) : PGtkEventBoxClass;
begin
   GTK_EVENT_BOX_GET_CLASS:=PGtkEventBoxClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_EVENT_BOX));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

