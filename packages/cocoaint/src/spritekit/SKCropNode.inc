{ Parsed from SpriteKit.framework SKCropNode.h }


{$ifdef TYPES}
type
  SKCropNodePtr = ^SKCropNode;
{$endif}

{$ifdef CLASSES}

type
  SKCropNode = objcclass external (SKNode)
  public
    procedure setMaskNode(newValue: SKNode); message 'setMaskNode:';
    function maskNode: SKNode; message 'maskNode';
  end;
{$endif}

