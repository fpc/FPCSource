{ Parsed from Quartzcore.framework CATransformLayer.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:07 CET 2010 }


{$ifdef TYPES}
{$ifndef CATRANSFORMLAYER_PAS_T}
{$define CATRANSFORMLAYER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CATRANSFORMLAYER_PAS_R}
{$define CATRANSFORMLAYER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CATRANSFORMLAYER_PAS_F}
{$define CATRANSFORMLAYER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CATRANSFORMLAYER_PAS_S}
{$define CATRANSFORMLAYER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  CATransformLayer = objcclass;
  CATransformLayerPointer = ^CATransformLayer;
  CATransformLayerPtr = CATransformLayerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef CATRANSFORMLAYER_PAS_C}
{$define CATRANSFORMLAYER_PAS_C}

{ CATransformLayer }
  CATransformLayer = objcclass(CALayer)
    
  public
    class function alloc: CATransformLayer; message 'alloc';
  end; external;

{$endif}
{$endif}
