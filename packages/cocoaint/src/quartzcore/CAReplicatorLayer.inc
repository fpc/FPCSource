{ Parsed from Quartzcore.framework CAReplicatorLayer.h }

{$ifdef TYPES}
{$ifndef CAREPLICATORLAYER_PAS_T}
{$define CAREPLICATORLAYER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CAREPLICATORLAYER_PAS_R}
{$define CAREPLICATORLAYER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CAREPLICATORLAYER_PAS_F}
{$define CAREPLICATORLAYER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CAREPLICATORLAYER_PAS_S}
{$define CAREPLICATORLAYER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  CAReplicatorLayer = objcclass;
  CAReplicatorLayerPointer = ^CAReplicatorLayer;
  CAReplicatorLayerPtr = CAReplicatorLayerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef CAREPLICATORLAYER_PAS_C}
{$define CAREPLICATORLAYER_PAS_C}

{ CAReplicatorLayer }
  CAReplicatorLayer = objcclass external (CALayer)
    
  public
    procedure setInstanceCount (newValue: NSInteger); message 'setInstanceCount:';
    function instanceCount: NSInteger; message 'instanceCount';
    procedure setPreservesDepth (newValue: Boolean); message 'setPreservesDepth:';
    function preservesDepth: Boolean; message 'preservesDepth';
    procedure setInstanceDelay (newValue: CFTimeInterval); message 'setInstanceDelay:';
    function instanceDelay: CFTimeInterval; message 'instanceDelay';
    procedure setInstanceTransform (newValue: CATransform3D); message 'setInstanceTransform:';
    function instanceTransform: CATransform3D; message 'instanceTransform';
    procedure setInstanceColor (newValue: CGColorRef); message 'setInstanceColor:';
    function instanceColor: CGColorRef; message 'instanceColor';
    procedure setInstanceRedOffset (newValue: single); message 'setInstanceRedOffset:';
    function instanceRedOffset: single; message 'instanceRedOffset';
    procedure setInstanceGreenOffset (newValue: single); message 'setInstanceGreenOffset:';
    function instanceGreenOffset: single; message 'instanceGreenOffset';
    procedure setInstanceBlueOffset (newValue: single); message 'setInstanceBlueOffset:';
    function instanceBlueOffset: single; message 'instanceBlueOffset';
    procedure setInstanceAlphaOffset (newValue: single); message 'setInstanceAlphaOffset:';
    function instanceAlphaOffset: single; message 'instanceAlphaOffset';
  end;

{$endif}
{$endif}
