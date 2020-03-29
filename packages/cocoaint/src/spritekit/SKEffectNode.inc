{ Parsed from SpriteKit.framework SKEffectNode.h }


{$ifdef TYPES}
type
  SKEffectNodePtr = ^SKEffectNode;
{$endif}

{$ifdef CLASSES}

type
  SKEffectNode = objcclass external (SKNode)
  public
    procedure setFilter(newValue: CIFilter); message 'setFilter:';
    function filter: CIFilter; message 'filter';
    procedure setShouldCenterFilter(newValue: ObjCBOOL); message 'setShouldCenterFilter:';
    function shouldCenterFilter: ObjCBOOL; message 'shouldCenterFilter';
    procedure setShouldEnableEffects(newValue: ObjCBOOL); message 'setShouldEnableEffects:';
    function shouldEnableEffects: ObjCBOOL; message 'shouldEnableEffects';
    procedure setShouldRasterize(newValue: ObjCBOOL); message 'setShouldRasterize:';
    function shouldRasterize: ObjCBOOL; message 'shouldRasterize';
    procedure setBlendMode(newValue: SKBlendMode); message 'setBlendMode:';
    function blendMode: SKBlendMode; message 'blendMode';
    procedure setShader(newValue: SKShader); message 'setShader:';
    function shader: SKShader; message 'shader';
  end;
{$endif}

