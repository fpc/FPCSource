{ Parsed from WebKit.framework DOMProcessingInstruction.h }


{$ifdef TYPES}
type
  DOMProcessingInstructionPtr = ^DOMProcessingInstruction;
{$endif}

{$ifdef CLASSES}

type
  DOMProcessingInstruction = objcclass external (DOMCharacterData)
  public
    function target: NSString; message 'target';
    function sheet: DOMStyleSheet; message 'sheet';
  end;
{$endif}

