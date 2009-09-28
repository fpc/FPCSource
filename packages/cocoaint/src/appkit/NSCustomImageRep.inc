{ Parsed from Appkit.framework NSCustomImageRep.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:52 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSCUSTOMIMAGEREP_PAS_H}
{$define NSCUSTOMIMAGEREP_PAS_H}
type
  NSCustomImageRepPointer = Pointer;

{$endif}
{$endif}

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

{$endif}

{$ifdef CLASSES}
{$ifndef NSCUSTOMIMAGEREP_PAS_C}
{$define NSCUSTOMIMAGEREP_PAS_C}

{ NSCustomImageRep }
  NSCustomImageRep = objcclass(NSImageRep)
  private
    __drawMethod: SEL;
    __drawObject: id;
    __reserved: cuint;
    
  public
    class function alloc: NSCustomImageRep; message 'alloc';

    function initWithDrawSelector_delegate(aMethod: SEL; anObject: id): id; message 'initWithDrawSelector:delegate:';
    function drawSelector: SEL; message 'drawSelector';
    function delegate: id; message 'delegate';
  end; external;

{$endif}
{$endif}
