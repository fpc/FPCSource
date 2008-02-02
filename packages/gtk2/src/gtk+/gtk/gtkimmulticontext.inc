// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkIMMulticontext = ^TGtkIMMulticontext;
   TGtkIMMulticontext = record
        anObject : TGtkIMContext;
        slave : PGtkIMContext;
        client_window : PGdkWindow;
        context_id : Pgchar;
     end;

{ Padding for future expansion  }
   PGtkIMMulticontextClass = ^TGtkIMMulticontextClass;
   TGtkIMMulticontextClass = record
        parent_class : TGtkIMContextClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_IM_MULTICONTEXT : GType;
function GTK_IM_MULTICONTEXT(obj: pointer) : PGtkIMMulticontext;
function GTK_IM_MULTICONTEXT_CLASS(klass: pointer) : PGtkIMMulticontextClass;
function GTK_IS_IM_MULTICONTEXT(obj: pointer) : boolean;
function GTK_IS_IM_MULTICONTEXT_CLASS(klass: pointer) : boolean;
function GTK_IM_MULTICONTEXT_GET_CLASS(obj: pointer) : PGtkIMMulticontextClass;


function gtk_im_multicontext_get_type:TGtkType; cdecl; external gtklib;
function gtk_im_multicontext_new:PGtkIMContext; cdecl; external gtklib;
procedure gtk_im_multicontext_append_menuitems(context:PGtkIMMulticontext; menushell:PGtkMenuShell); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_IM_MULTICONTEXT : GType;
begin
   GTK_TYPE_IM_MULTICONTEXT:=gtk_im_multicontext_get_type;
end;

function GTK_IM_MULTICONTEXT(obj: pointer) : PGtkIMMulticontext;
begin
   GTK_IM_MULTICONTEXT:=PGtkIMMulticontext(GTK_CHECK_CAST(obj,GTK_TYPE_IM_MULTICONTEXT));
end;

function GTK_IM_MULTICONTEXT_CLASS(klass: pointer) : PGtkIMMulticontextClass;
begin
   GTK_IM_MULTICONTEXT_CLASS:=PGtkIMMulticontextClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_IM_MULTICONTEXT));
end;

function GTK_IS_IM_MULTICONTEXT(obj: pointer) : boolean;
begin
   GTK_IS_IM_MULTICONTEXT:=GTK_CHECK_TYPE(obj,GTK_TYPE_IM_MULTICONTEXT);
end;

function GTK_IS_IM_MULTICONTEXT_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_IM_MULTICONTEXT_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_IM_MULTICONTEXT);
end;

function GTK_IM_MULTICONTEXT_GET_CLASS(obj: pointer) : PGtkIMMulticontextClass;
begin
   GTK_IM_MULTICONTEXT_GET_CLASS:=PGtkIMMulticontextClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_IM_MULTICONTEXT));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

