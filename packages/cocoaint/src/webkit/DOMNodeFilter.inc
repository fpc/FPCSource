{ Parsed from Webkit.framework DOMNodeFilter.h }

{$ifdef TYPES}
{$ifndef DOMNODEFILTER_PAS_T}
{$define DOMNODEFILTER_PAS_T}

{ Constants }

const
  DOM_FILTER_ACCEPT = 1;
  DOM_FILTER_REJECT = 2;
  DOM_FILTER_SKIP = 3;
  DOM_SHOW_ALL = $FFFFFFFF;
  DOM_SHOW_ELEMENT = $00000001;
  DOM_SHOW_ATTRIBUTE = $00000002;
  DOM_SHOW_TEXT = $00000004;
  DOM_SHOW_CDATA_SECTION = $00000008;
  DOM_SHOW_ENTITY_REFERENCE = $00000010;
  DOM_SHOW_ENTITY = $00000020;
  DOM_SHOW_PROCESSING_INSTRUCTION = $00000040;
  DOM_SHOW_COMMENT = $00000080;
  DOM_SHOW_DOCUMENT = $00000100;
  DOM_SHOW_DOCUMENT_TYPE = $00000200;
  DOM_SHOW_DOCUMENT_FRAGMENT = $00000400;
  DOM_SHOW_NOTATION = $00000800;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMNODEFILTER_PAS_R}
{$define DOMNODEFILTER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMNODEFILTER_PAS_F}
{$define DOMNODEFILTER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMNODEFILTER_PAS_S}
{$define DOMNODEFILTER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMNodeFilterProtocol = objcprotocol;

{$endif}
{$ifdef PROTOCOLS}
{$ifndef DOMNODEFILTER_PAS_P}
{$define DOMNODEFILTER_PAS_P}
  
{ DOMNodeFilter Protocol }
  DOMNodeFilterProtocol = objcprotocol external name 'DOMNodeFilter'
    function acceptNode(n: DOMNode): cshort; message 'acceptNode:';
  end;
{$endif}
{$endif}
