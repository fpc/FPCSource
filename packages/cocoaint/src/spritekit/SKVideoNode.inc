{ Parsed from SpriteKit.framework SKVideoNode.h }


{$ifdef TYPES}
type
  SKVideoNodePtr = ^SKVideoNode;
{$endif}

{$ifdef CLASSES}

type
  SKVideoNode = objcclass external (SKNode)
  public
    class function videoNodeWithAVPlayer (player: AVPlayer): SKVideoNode; message 'videoNodeWithAVPlayer:';
    class function videoNodeWithVideoFileNamed (videoFile: NSString): SKVideoNode; message 'videoNodeWithVideoFileNamed:';
    class function videoNodeWithVideoURL (videoURL: NSURL): SKVideoNode; message 'videoNodeWithVideoURL:';
    function initWithAVPlayer (player: AVPlayer): instancetype; message 'initWithAVPlayer:'; { NS_DESIGNATED_INITIALIZER }
    function initWithVideoFileNamed (videoFile: NSString): instancetype; message 'initWithVideoFileNamed:'; { NS_DESIGNATED_INITIALIZER }
    function initWithVideoURL (url: NSURL): instancetype; message 'initWithVideoURL:'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (aDecoder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }
    procedure play; message 'play';
    procedure pause; message 'pause';
    procedure setSize(newValue: CGSize); message 'setSize:';
    function size: CGSize; message 'size';
    procedure setAnchorPoint(newValue: CGPoint); message 'setAnchorPoint:';
    function anchorPoint: CGPoint; message 'anchorPoint';
  end;
{$endif}

