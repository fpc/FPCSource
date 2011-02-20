{ Parsed from Webkit.framework DOMHTMLParamElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLPARAMELEMENT_PAS_T}
{$define DOMHTMLPARAMELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLPARAMELEMENT_PAS_R}
{$define DOMHTMLPARAMELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLPARAMELEMENT_PAS_F}
{$define DOMHTMLPARAMELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLPARAMELEMENT_PAS_S}
{$define DOMHTMLPARAMELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLParamElement = objcclass;
  DOMHTMLParamElementPointer = ^DOMHTMLParamElement;
  DOMHTMLParamElementPtr = DOMHTMLParamElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLPARAMELEMENT_PAS_C}
{$define DOMHTMLPARAMELEMENT_PAS_C}

{ DOMHTMLParamElement }
  DOMHTMLParamElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setType (newValue: NSString); message 'setType:';
    function type_: NSString; message 'type_';
    procedure setValue (newValue: NSString); message 'setValue:';
    function value: NSString; message 'value';
    procedure setValueType (newValue: NSString); message 'setValueType:';
    function valueType: NSString; message 'valueType';
  end;

{$endif}
{$endif}
