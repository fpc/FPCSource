{ Parsed from Webkit.framework DOMException.h }

{$ifdef TYPES}
{$ifndef DOMEXCEPTION_PAS_T}
{$define DOMEXCEPTION_PAS_T}

{ Constants }

const
  DOM_INDEX_SIZE_ERR = 1;
  DOM_DOMSTRING_SIZE_ERR = 2;
  DOM_HIERARCHY_REQUEST_ERR = 3;
  DOM_WRONG_DOCUMENT_ERR = 4;
  DOM_INVALID_CHARACTER_ERR = 5;
  DOM_NO_DATA_ALLOWED_ERR = 6;
  DOM_NO_MODIFICATION_ALLOWED_ERR = 7;
  DOM_NOT_FOUND_ERR = 8;
  DOM_NOT_SUPPORTED_ERR = 9;
  DOM_INUSE_ATTRIBUTE_ERR = 10;
  DOM_INVALID_STATE_ERR = 11;
  DOM_SYNTAX_ERR = 12;
  DOM_INVALID_MODIFICATION_ERR = 13;
  DOM_NAMESPACE_ERR = 14;
  DOM_INVALID_ACCESS_ERR = 15;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMEXCEPTION_PAS_R}
{$define DOMEXCEPTION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMEXCEPTION_PAS_F}
{$define DOMEXCEPTION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMEXCEPTION_PAS_S}
{$define DOMEXCEPTION_PAS_S}

{ External string constants }
var
  DOMException: NSString; cvar; external;

{$endif}
{$endif}
