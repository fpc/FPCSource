// included by atk.pp

{
   AtkHyperlink encapsulates a link or set of links in a hypertext document.

   It implements the AtkAction interface.
  }

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PAtkHyperlink = ^TAtkHyperlink;
   TAtkHyperlink = record
        parent : TGObject;
     end;

{ Returns a string specifying the URI associated with the nth anchor
  of this link. }
{ Returns an anObject which represents the link action, as appropriate for
  that link. }
{ Gets the index with the hypertext document at which this link ends }
{ Gets the index with the hypertext document at which this link begins }
{ Since the document a link is associated with may have changed, this
  method returns whether or not this link is still valid (with respect
  to the document is references) }
{ Returns the number of anchors associated with this link }
   PAtkHyperlinkClass = ^TAtkHyperlinkClass;
   TAtkHyperlinkClass = record
        parent : TGObjectClass;
        get_uri : function (link:PAtkHyperlink; i:gint):Pgchar; cdecl;
        get_object : function (link:PAtkHyperlink; i:gint):PAtkObject; cdecl;
        get_end_index : function (link:PAtkHyperlink):gint; cdecl;
        get_start_index : function (link:PAtkHyperlink):gint; cdecl;
        is_valid : function (link:PAtkHyperlink):gboolean; cdecl;
        get_n_anchors : function (link:PAtkHyperlink):gint; cdecl;
        pad1 : TAtkFunction;
        pad2 : TAtkFunction;
        pad3 : TAtkFunction;
        pad4 : TAtkFunction;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function ATK_TYPE_HYPERLINK : GType;
function ATK_HYPERLINK(obj: pointer) : PAtkHyperlink;
function ATK_HYPERLINK_CLASS(klass: pointer) : PAtkHyperlinkClass;
function ATK_IS_HYPERLINK(obj: pointer) : boolean;
function ATK_IS_HYPERLINK_CLASS(klass: pointer) : boolean;
function ATK_HYPERLINK_GET_CLASS(obj: pointer) : PAtkHyperlinkClass;


function atk_hyperlink_get_type:GType; cdecl; external atklib;
function atk_hyperlink_get_uri(link:PAtkHyperlink; i:gint):Pgchar; cdecl; external atklib;
function atk_hyperlink_get_object(link:PAtkHyperlink; i:gint):PAtkObject; cdecl; external atklib;
function atk_hyperlink_get_end_index(link:PAtkHyperlink):gint; cdecl; external atklib;
function atk_hyperlink_get_start_index(link:PAtkHyperlink):gint; cdecl; external atklib;
function atk_hyperlink_is_valid(link:PAtkHyperlink):gboolean; cdecl; external atklib;
function atk_hyperlink_get_n_anchors(link:PAtkHyperlink):gint; cdecl; external atklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function ATK_TYPE_HYPERLINK : GType;
begin
   ATK_TYPE_HYPERLINK:=atk_hyperlink_get_type;
end;

function ATK_HYPERLINK(obj: pointer) : PAtkHyperlink;
begin
   ATK_HYPERLINK:=PAtkHyperlink(G_TYPE_CHECK_INSTANCE_CAST(obj,ATK_TYPE_HYPERLINK));
end;

function ATK_HYPERLINK_CLASS(klass: pointer) : PAtkHyperlinkClass;
begin
   ATK_HYPERLINK_CLASS:=PAtkHyperlinkClass(G_TYPE_CHECK_CLASS_CAST(klass,ATK_TYPE_HYPERLINK));
end;

function ATK_IS_HYPERLINK(obj: pointer) : boolean;
begin
   ATK_IS_HYPERLINK:=G_TYPE_CHECK_INSTANCE_TYPE(obj,ATK_TYPE_HYPERLINK);
end;

function ATK_IS_HYPERLINK_CLASS(klass: pointer) : boolean;
begin
   ATK_IS_HYPERLINK_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,ATK_TYPE_HYPERLINK);
end;

function ATK_HYPERLINK_GET_CLASS(obj: pointer) : PAtkHyperlinkClass;
begin
   ATK_HYPERLINK_GET_CLASS:=PAtkHyperlinkClass(G_TYPE_INSTANCE_GET_CLASS(obj,ATK_TYPE_HYPERLINK));
end;

{$ENDIF read_implementation}

