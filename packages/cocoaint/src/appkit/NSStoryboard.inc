{ Parsed from AppKit.framework NSStoryboard.h }


{$ifdef TYPES}
type
  NSStoryboardPtr = ^NSStoryboard;
{$endif}

{$ifdef CLASSES}

type
  NSStoryboard = objcclass external (NSObject)
  private
    _privateStoryboardData: id;
    _storyboardFlags: bitpacked record
      case byte of
        0: (_anonBitField__storyboardFlags0: cuint);
        1: (
          reserved: 0..((1 shl 32)-1);
        );
      end;
  public
    class function storyboardWithName_bundle (name: NSString; storyboardBundleOrNil: NSBundle): instancetype; message 'storyboardWithName:bundle:';
    function instantiateInitialController: id; message 'instantiateInitialController';
    function instantiateControllerWithIdentifier (identifier: NSString): id; message 'instantiateControllerWithIdentifier:';
  end;
{$endif}

