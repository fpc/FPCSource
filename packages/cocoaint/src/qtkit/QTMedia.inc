{ Parsed from QTKit.framework QTMedia.h }


{ Types from QTMedia }
{$ifdef TYPES}


{$endif}


{$ifdef TYPES}
type
  QTMediaPtr = ^QTMedia;
{$endif}

{$ifdef CLASSES}

type
  QTMedia = objcclass external (NSObject)
  private
    {$if defined(__LP64__)}
    _proxy: cint32;
    {$else}
    _quickTimeMedia: Media;
    _undoManager: NSUndoManager;
    _undoAttributes: NSDictionary;
    _quickTimeMovieWrapper: QTQuickTimeMovieWrapper;
    {$endif}
    _flags: clong;
    _track: QTTrack;
  public
    {$if  not defined(__LP64__)}
    class function mediaWithQuickTimeMedia_error (media_: Media; errorPtr: NSErrorPtr): id; message 'mediaWithQuickTimeMedia:error:';
    function initWithQuickTimeMedia_error (media_: Media; errorPtr: NSErrorPtr): id; message 'initWithQuickTimeMedia:error:';
    {$endif}
    function track: QTTrack; message 'track';
    function mediaAttributes: NSDictionary; message 'mediaAttributes';
    procedure setMediaAttributes (attributes: NSDictionary); message 'setMediaAttributes:';
    function attributeForKey (attributeKey: NSString): id; message 'attributeForKey:';
    procedure setAttribute_forKey (value: id; attributeKey: NSString); message 'setAttribute:forKey:';
    function hasCharacteristic (characteristic: NSString): ObjCBOOL; message 'hasCharacteristic:';
    {$if  not defined(__LP64__)}
    function quickTimeMedia: Media; message 'quickTimeMedia';
    {$endif}
  end;
{$endif}

