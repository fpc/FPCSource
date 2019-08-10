{ Parsed from SpriteKit.framework SKLabelNode.h }


{$ifdef TYPES}
type
  SKLabelNodePtr = ^SKLabelNode;
{$endif}

{$ifdef TYPES}
type
  SKLabelVerticalAlignmentMode = NSInteger;
  SKLabelVerticalAlignmentModePtr = ^SKLabelVerticalAlignmentMode;

const
  SKLabelVerticalAlignmentModeBaseline = 0;
  SKLabelVerticalAlignmentModeCenter = 1;
  SKLabelVerticalAlignmentModeTop = 2;
  SKLabelVerticalAlignmentModeBottom = 3;

type
  SKLabelHorizontalAlignmentMode = NSInteger;
  SKLabelHorizontalAlignmentModePtr = ^SKLabelHorizontalAlignmentMode;

const
  SKLabelHorizontalAlignmentModeCenter = 0;
  SKLabelHorizontalAlignmentModeLeft = 1;
  SKLabelHorizontalAlignmentModeRight = 2;
{$endif}

{$ifdef CLASSES}

type
  SKLabelNode = objcclass external (SKNode)
  public
    class function labelNodeWithText (text: NSString): instancetype; message 'labelNodeWithText:';
    class function labelNodeWithFontNamed (fontName: NSString): instancetype; message 'labelNodeWithFontNamed:';
    function initWithFontNamed (fontName: NSString): instancetype; message 'initWithFontNamed:';
    procedure setVerticalAlignmentMode(newValue: SKLabelVerticalAlignmentMode); message 'setVerticalAlignmentMode:';
    function verticalAlignmentMode: SKLabelVerticalAlignmentMode; message 'verticalAlignmentMode';
    procedure setHorizontalAlignmentMode(newValue: SKLabelHorizontalAlignmentMode); message 'setHorizontalAlignmentMode:';
    function horizontalAlignmentMode: SKLabelHorizontalAlignmentMode; message 'horizontalAlignmentMode';
    procedure setFontName(newValue: NSString); message 'setFontName:';
    function fontName: NSString; message 'fontName';
    procedure setText(newValue: NSString); message 'setText:';
    function text: NSString; message 'text';
    procedure setFontSize(newValue: CGFloat); message 'setFontSize:';
    function fontSize: CGFloat; message 'fontSize';
    procedure setFontColor(newValue: SKColorPtr); message 'setFontColor:';
    function fontColor: SKColorPtr; message 'fontColor';
    procedure setColorBlendFactor(newValue: CGFloat); message 'setColorBlendFactor:';
    function colorBlendFactor: CGFloat; message 'colorBlendFactor';
    procedure setColor(newValue: SKColorPtr); message 'setColor:';
    function color: SKColorPtr; message 'color';
    procedure setBlendMode(newValue: SKBlendMode); message 'setBlendMode:';
    function blendMode: SKBlendMode; message 'blendMode';
  end;
{$endif}

