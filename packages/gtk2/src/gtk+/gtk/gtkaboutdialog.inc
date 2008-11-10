// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  PGtkAboutDialog = ^TGtkAboutDialog;
  TGtkAboutDialog = record
       parent_instance : TGtkDialog;
       private_data : gpointer;
    end;


  PGtkAboutDialogClass = ^TGtkAboutDialogClass;
  TGtkAboutDialogClass = record
       parent_class : TGtkDialogClass;
       _gtk_reserved1 : procedure ;cdecl;
       _gtk_reserved2 : procedure ;cdecl;
       _gtk_reserved3 : procedure ;cdecl;
       _gtk_reserved4 : procedure ;cdecl;
    end;

    TGtkAboutDialogActivateLinkFunc = procedure (about:PGtkAboutDialog; link_:Pgchar; data:gpointer);cdecl;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_ABOUT_DIALOG : GType;
function GTK_ABOUT_DIALOG(anObject : pointer) : PGtkAboutDialog;
function GTK_ABOUT_DIALOG_CLASS(klass : pointer) : PGtkAboutDialogClass;
function GTK_IS_ABOUT_DIALOG(anObject: pointer) : boolean;
function GTK_IS_ABOUT_DIALOG_CLASS(klass : pointer) : boolean;
function GTK_ABOUT_DIALOG_GET_CLASS(obj : pointer) : PGtkAboutDialogClass;

function gtk_about_dialog_get_type: GType; cdecl; external gtklib;
function gtk_about_dialog_new:PGtkWidget;cdecl; external gtklib;
procedure gtk_show_about_dialog(parent:PGtkWindow; first_property_name:Pgchar);varargs;cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
function gtk_about_dialog_get_name(about:PGtkAboutDialog): Pgchar;cdecl; external gtklib;
procedure gtk_about_dialog_set_name(about:PGtkAboutDialog; name:Pgchar);cdecl; external gtklib;
{$endif}

function gtk_about_dialog_get_program_name(about:PGtkAboutDialog):Pgchar;cdecl; external gtklib;
procedure gtk_about_dialog_set_program_name(about:PGtkAboutDialog; name:Pgchar);cdecl; external gtklib;
function gtk_about_dialog_get_version(about:PGtkAboutDialog):Pgchar;cdecl; external gtklib;
procedure gtk_about_dialog_set_version(about:PGtkAboutDialog; version:Pgchar);cdecl; external gtklib;
function gtk_about_dialog_get_copyright(about:PGtkAboutDialog):Pgchar;cdecl; external gtklib;
procedure gtk_about_dialog_set_copyright(about:PGtkAboutDialog; copyright:Pgchar);cdecl;external;
function gtk_about_dialog_get_comments(about:PGtkAboutDialog):Pgchar;cdecl;external;
procedure gtk_about_dialog_set_comments(about:PGtkAboutDialog; comments:Pgchar);cdecl;external;
function gtk_about_dialog_get_license(about:PGtkAboutDialog):Pgchar;cdecl;external;
procedure gtk_about_dialog_set_license(about:PGtkAboutDialog; license:Pgchar);cdecl;external;
function gtk_about_dialog_get_wrap_license(about:PGtkAboutDialog):gboolean;cdecl;external;
procedure gtk_about_dialog_set_wrap_license(about:PGtkAboutDialog; wrap_license:gboolean);cdecl;external;
function gtk_about_dialog_get_website(about:PGtkAboutDialog):Pgchar;cdecl;external;
procedure gtk_about_dialog_set_website(about:PGtkAboutDialog; website:Pgchar);cdecl;external;
function gtk_about_dialog_get_website_label(about:PGtkAboutDialog):Pgchar;cdecl;external;
procedure gtk_about_dialog_set_website_label(about:PGtkAboutDialog; website_label:Pgchar);cdecl;external;
function gtk_about_dialog_get_authors(about:PGtkAboutDialog):PPgchar;cdecl;external;
procedure gtk_about_dialog_set_authors(about:PGtkAboutDialog; authors:PPgchar);cdecl;external;
function gtk_about_dialog_get_documenters(about:PGtkAboutDialog):PPgchar;cdecl;external;
procedure gtk_about_dialog_set_documenters(about:PGtkAboutDialog; documenters:PPgchar);cdecl;external;
function gtk_about_dialog_get_artists(about:PGtkAboutDialog):PPgchar;cdecl;external;
procedure gtk_about_dialog_set_artists(about:PGtkAboutDialog; artists:PPgchar);cdecl;external;
function gtk_about_dialog_get_translator_credits(about:PGtkAboutDialog):Pgchar;cdecl;external;
procedure gtk_about_dialog_set_translator_credits(about:PGtkAboutDialog; translator_credits:Pgchar);cdecl;external;
function gtk_about_dialog_get_logo(about:PGtkAboutDialog):PGdkPixbuf;cdecl;external;
procedure gtk_about_dialog_set_logo(about:PGtkAboutDialog; logo:PGdkPixbuf);cdecl;external;
function gtk_about_dialog_get_logo_icon_name(about:PGtkAboutDialog):Pgchar;cdecl;external;
procedure gtk_about_dialog_set_logo_icon_name(about:PGtkAboutDialog; icon_name:Pgchar);cdecl;external;

function gtk_about_dialog_set_email_hook(func:TGtkAboutDialogActivateLinkFunc; data:gpointer; destroy:TGDestroyNotify):TGtkAboutDialogActivateLinkFunc;cdecl; external gtklib;
function gtk_about_dialog_set_url_hook(func:TGtkAboutDialogActivateLinkFunc; data:gpointer; destroy:TGDestroyNotify):TGtkAboutDialogActivateLinkFunc;cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_ABOUT_DIALOG : GType;
begin
  GTK_TYPE_ABOUT_DIALOG:=gtk_about_dialog_get_type;
end;

function GTK_ABOUT_DIALOG(anObject : pointer) : PGtkAboutDialog;
begin
   GTK_ABOUT_DIALOG:=PGtkAboutDialog(G_TYPE_CHECK_INSTANCE_CAST(anObject,GTK_TYPE_ABOUT_DIALOG));
end;

function GTK_ABOUT_DIALOG_CLASS(klass : pointer) : PGtkAboutDialogClass;
begin
   GTK_ABOUT_DIALOG_CLASS:=PGtkAboutDialogClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_ABOUT_DIALOG));
end;

function GTK_IS_ABOUT_DIALOG(anObject: pointer) : boolean;
begin
   GTK_IS_ABOUT_DIALOG:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,GTK_TYPE_ABOUT_DIALOG);
end;

function GTK_IS_ABOUT_DIALOG_CLASS(klass : pointer) : boolean;
begin
   GTK_IS_ABOUT_DIALOG_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_ABOUT_DIALOG);
end;

function GTK_ABOUT_DIALOG_GET_CLASS(obj : pointer) : PGtkAboutDialogClass;
begin
   GTK_ABOUT_DIALOG_GET_CLASS:=PGtkAboutDialogClass(G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_ABOUT_DIALOG));
end;

{$ENDIF read_implementation}
// included by gtk2.pas
