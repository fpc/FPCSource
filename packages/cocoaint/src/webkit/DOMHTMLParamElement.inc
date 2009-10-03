{ Parsed from Webkit.framework DOMHTMLParamElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:52 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLPARAMELEMENT_PAS_C}
{$define DOMHTMLPARAMELEMENT_PAS_C}

{ DOMHTMLParamElement }
  DOMHTMLParamElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLParamElement; message 'alloc';

    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setType (newValue: NSString); message 'setType:';
    function type_: NSString; message 'type_';
    procedure setValue (newValue: NSString); message 'setValue:';
    function value: NSString; message 'value';
    procedure setValueType (newValue: NSString); message 'setValueType:';
    function valueType: NSString; message 'valueType';
  end; external;

{$endif}
{$endif}
