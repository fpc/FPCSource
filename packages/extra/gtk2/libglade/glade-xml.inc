// included by libglade.pas

type
   PGladeXMLPrivate = pointer;

   PGladeXML = ^TGladeXML;
   TGladeXML = record
        parent : TGObject;
        filename : Pchar;
        priv : PGladeXMLPrivate;
     end;

   PGladeXMLClass = ^TGladeXMLClass;
   TGladeXMLClass = record
        parent_class : TGObjectClass;
     end;

{ if the gtk_signal_connect_object behaviour is required, connect_object
   will point to the anObject, otherwise it will be NULL.
  }
   TGladeXMLConnectFunc = procedure (handler_name:Pgchar; anObject:PGObject;
                signal_name:Pgchar; signal_data:Pgchar; connect_object:PGObject;
                after:gboolean; user_data:gpointer); cdecl;

function GLADE_TYPE_XML : GType;
function GLADE_XML(obj: pointer) : PGladeXML;
function GLADE_XML_CLASS(klass: pointer) : PGladeXMLClass;
function GLADE_IS_XML(obj: pointer) : gboolean;
function GLADE_IS_XML_CLASS(klass: pointer) : gboolean;
function GLADE_XML_GET_CLASS(obj: pointer) : PGladeXMLClass;

function glade_xml_get_type:GType; cdecl; external LibGladeLib;
function glade_xml_new(fname:Pchar; root:Pchar; domain:Pchar):PGladeXML; cdecl; external LibGladeLib;
function glade_xml_new_from_buffer(buffer:Pchar; size:longint; root:Pchar; domain:Pchar):PGladeXML; cdecl; external LibGladeLib;
function glade_xml_construct(self:PGladeXML; fname:Pchar; root:Pchar; domain:Pchar):gboolean; cdecl; external LibGladeLib;
procedure glade_xml_signal_connect(self:PGladeXML; handlername:Pchar; func:TGCallback); cdecl; external LibGladeLib;
procedure glade_xml_signal_connect_data(self:PGladeXML; handlername:Pchar; func:TGCallback; user_data:gpointer); cdecl; external LibGladeLib;

{
   use gmodule to connect signals automatically.  Basically a symbol with
   the name of the signal handler is searched for, and that is connected to
   the associated symbols.  So setting gtk_main_quit as a signal handler
   for the destroy signal of a window will do what you expect.
  }
procedure glade_xml_signal_autoconnect(self:PGladeXML); cdecl; external LibGladeLib;

{
   These two are to make it easier to use libglade with an interpreted
   language binding.
  }
procedure glade_xml_signal_connect_full(self:PGladeXML; handler_name:Pgchar; func:TGladeXMLConnectFunc; user_data:gpointer); cdecl; external LibGladeLib;
procedure glade_xml_signal_autoconnect_full(self:PGladeXML; func:TGladeXMLConnectFunc; user_data:gpointer); cdecl; external LibGladeLib;
function glade_xml_get_widget(self:PGladeXML; name:Pchar):PGtkWidget; cdecl; external LibGladeLib;
function glade_xml_get_widget_prefix(self:PGladeXML; name:Pchar):PGList; cdecl; external LibGladeLib;
function glade_xml_relative_file(self:PGladeXML; filename:Pgchar):Pgchar; cdecl; external LibGladeLib;
{ don't free the results of these two ...  }
function glade_get_widget_name(widget:PGtkWidget):Pchar; cdecl; external LibGladeLib;
function glade_get_widget_tree(widget:PGtkWidget):PGladeXML; cdecl; external LibGladeLib;

{ interface for changing the custom widget handling  }
type
   PGladeXMLCustomWidgetHandler = ^TGladeXMLCustomWidgetHandler;
   TGladeXMLCustomWidgetHandler = TGtkWidget;

procedure glade_set_custom_handler(handler:TGladeXMLCustomWidgetHandler; user_data:gpointer); cdecl; external LibGladeLib;

{$ifndef LIBGLADE_DISABLE_DEPRECATED}
function glade_xml_new_with_domain(fname:Pchar; root:Pchar; domain:Pchar):PGladeXML;
function glade_xml_new_from_memory(buffer:Pchar; size:longint; root:Pchar; domain:Pchar):PGladeXML;
{$endif}

// included by libglade.pas

