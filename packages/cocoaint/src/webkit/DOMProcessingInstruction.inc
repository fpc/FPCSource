{ Parsed from Webkit.framework DOMProcessingInstruction.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


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
  end; external;

{$endif}
{$endif}
