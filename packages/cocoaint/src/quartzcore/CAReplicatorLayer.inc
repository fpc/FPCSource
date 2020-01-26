{ Parsed from QuartzCore.framework CAReplicatorLayer.h }


{$ifdef TYPES}
type
  CAReplicatorLayerPtr = ^CAReplicatorLayer;
{$endif}

{$ifdef CLASSES}

type
  CAReplicatorLayer = objcclass external (CALayer)
  public
    procedure setInstanceCount(newValue: NSInteger); message 'setInstanceCount:';
    function instanceCount: NSInteger; message 'instanceCount';
    procedure setPreservesDepth(newValue: ObjCBOOL); message 'setPreservesDepth:';
    function preservesDepth: ObjCBOOL; message 'preservesDepth';
    procedure setInstanceDelay(newValue: CFTimeInterval); message 'setInstanceDelay:';
    function instanceDelay: CFTimeInterval; message 'instanceDelay';
    procedure setInstanceTransform(newValue: CATransform3D); message 'setInstanceTransform:';
    function instanceTransform: CATransform3D; message 'instanceTransform';
    procedure setInstanceColor(newValue: CGColorRef); message 'setInstanceColor:';
    function instanceColor: CGColorRef; message 'instanceColor';
    procedure setInstanceRedOffset(newValue: single); message 'setInstanceRedOffset:';
    function instanceRedOffset: single; message 'instanceRedOffset';
    procedure setInstanceGreenOffset(newValue: single); message 'setInstanceGreenOffset:';
    function instanceGreenOffset: single; message 'instanceGreenOffset';
    procedure setInstanceBlueOffset(newValue: single); message 'setInstanceBlueOffset:';
    function instanceBlueOffset: single; message 'instanceBlueOffset';
    procedure setInstanceAlphaOffset(newValue: single); message 'setInstanceAlphaOffset:';
    function instanceAlphaOffset: single; message 'instanceAlphaOffset';
  end;
{$endif}

