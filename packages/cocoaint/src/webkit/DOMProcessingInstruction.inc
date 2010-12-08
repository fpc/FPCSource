{ Parsed from Webkit.framework DOMProcessingInstruction.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


{$ifdef TYPES}
{$ifndef DOMPROCESSINGINSTRUCTION_PAS_T}
{$define DOMPROCESSINGINSTRUCTION_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMPROCESSINGINSTRUCTION_PAS_R}
{$define DOMPROCESSINGINSTRUCTION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMPROCESSINGINSTRUCTION_PAS_F}
{$define DOMPROCESSINGINSTRUCTION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMPROCESSINGINSTRUCTION_PAS_S}
{$define DOMPROCESSINGINSTRUCTION_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMProcessingInstruction = objcclass;
  DOMProcessingInstructionPointer = ^DOMProcessingInstruction;
  DOMProcessingInstructionPtr = DOMProcessingInstructionPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMPROCESSINGINSTRUCTION_PAS_C}
{$define DOMPROCESSINGINSTRUCTION_PAS_C}

{ DOMProcessingInstruction }
  DOMProcessingInstruction = objcclass(DOMNode)
    
  public
    class function alloc: DOMProcessingInstruction; message 'alloc';

    function target: NSString; message 'target';
    procedure setData (newValue: NSString); message 'setData:';
    function data: NSString; message 'data';
    function sheet: DOMStyleSheet; message 'sheet';
  end; external;

{$endif}
{$endif}
