{ Parsed from Appkit.framework NSCustomImageRep.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


{$ifdef TYPES}
{$ifndef NSCUSTOMIMAGEREP_PAS_T}
{$define NSCUSTOMIMAGEREP_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCUSTOMIMAGEREP_PAS_R}
{$define NSCUSTOMIMAGEREP_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCUSTOMIMAGEREP_PAS_F}
{$define NSCUSTOMIMAGEREP_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCUSTOMIMAGEREP_PAS_S}
{$define NSCUSTOMIMAGEREP_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSCustomImageRep = objcclass;
  NSCustomImageRepPointer = ^NSCustomImageRep;
  NSCustomImageRepPtr = NSCustomImageRepPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCUSTOMIMAGEREP_PAS_C}
{$define NSCUSTOMIMAGEREP_PAS_C}

{ NSCustomImageRep }
  NSCustomImageRep = objcclass(NSImageRep)
  private
    _drawMethod: SEL;
    _drawObject: id;
    _reserved: cuint;
    
  public
    class function alloc: NSCustomImageRep; message 'alloc';

    function initWithDrawSelector_delegate(aMethod: SEL; anObject: id): id; message 'initWithDrawSelector:delegate:';
    function drawSelector: SEL; message 'drawSelector';
    function delegate: id; message 'delegate';
  end; external;

{$endif}
{$endif}
