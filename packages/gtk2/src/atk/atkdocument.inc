// included by atk.pp

{
   The AtkDocument interface should be supported by any anObject that has an
   associated document anObject model (DOM). This interface provides the
   standard mechanism allowing an assistive technology access to the DOM.
  }


{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PAtkDocumentIface = ^TAtkDocumentIface;
   TAtkDocumentIface = record
        parent : TGTypeInterface;
        get_document_type : function (document:PAtkDocument):Pgchar; cdecl;
        get_document : function (document:PAtkDocument):gpointer; cdecl;
        pad1 : TAtkFunction;
        pad2 : TAtkFunction;
        pad3 : TAtkFunction;
        pad4 : TAtkFunction;
        pad5 : TAtkFunction;
        pad6 : TAtkFunction;
        pad7 : TAtkFunction;
        pad8 : TAtkFunction;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function ATK_TYPE_DOCUMENT : GType;
function ATK_IS_DOCUMENT(obj: pointer) : boolean;
function ATK_DOCUMENT(obj: pointer) : PAtkDocument;
function ATK_DOCUMENT_GET_IFACE(obj: pointer) : PAtkDocumentIface;


function atk_document_get_type:GType; cdecl; external atklib;
function atk_document_get_document_type(document:PAtkDocument):Pgchar; cdecl; external atklib;
function atk_document_get_document(document:PAtkDocument):gpointer; cdecl; external atklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function ATK_TYPE_DOCUMENT : GType;
begin
   ATK_TYPE_DOCUMENT:=atk_document_get_type;
end;

function ATK_IS_DOCUMENT(obj: pointer) : boolean;
begin
   ATK_IS_DOCUMENT:=G_TYPE_CHECK_INSTANCE_TYPE(obj,ATK_TYPE_DOCUMENT);
end;

function ATK_DOCUMENT(obj: pointer) : PAtkDocument;
begin
   ATK_DOCUMENT:=PAtkDocument(G_TYPE_CHECK_INSTANCE_CAST(obj,ATK_TYPE_DOCUMENT));
end;

function ATK_DOCUMENT_GET_IFACE(obj: pointer) : PAtkDocumentIface;
begin
   ATK_DOCUMENT_GET_IFACE:=PAtkDocumentIface(G_TYPE_INSTANCE_GET_INTERFACE(obj,ATK_TYPE_DOCUMENT));
end;

{$ENDIF read_implementation}

