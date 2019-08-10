{ Parsed from QTKit.framework QTSampleBuffer.h }


{$ifdef TYPES}
type
  QTSampleBufferPtr = ^QTSampleBuffer;
{$endif}

{$if ((QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4))}
{$ifdef TYPES}
const
  QTSampleBufferAudioBufferListOptionAssure16ByteAlignment = 1 shl 0;

type
  QTSampleBufferAudioBufferListOptions = NSUInteger;
  QTSampleBufferAudioBufferListOptionsPtr = ^QTSampleBufferAudioBufferListOptions;
{$endif}

{$ifdef CLASSES}

type
  QTSampleBuffer = objcclass external (NSObject)
  private
    _internal: QTSampleBufferInternal;
    _reserved1: clong;
    _reserved2: clong;
    _reserved3: clong;
  public
    function bytesForAllSamples: pointer; message 'bytesForAllSamples';
    function lengthForAllSamples: NSUInteger; message 'lengthForAllSamples';
    function formatDescription: QTFormatDescription; message 'formatDescription';
    function duration: QTTime; message 'duration';
    function decodeTime: QTTime; message 'decodeTime';
    function presentationTime: QTTime; message 'presentationTime';
    function numberOfSamples: NSUInteger; message 'numberOfSamples';
    function sampleBufferAttributes: NSDictionary; message 'sampleBufferAttributes';
    function attributeForKey (key: NSString): id; message 'attributeForKey:';
  end;


type
  QTSampleBuffer_UseCount = objccategory external (QTSampleBuffer)
    function sampleUseCount: NSUInteger; message 'sampleUseCount';
    procedure incrementSampleUseCount; message 'incrementSampleUseCount';
    procedure decrementSampleUseCount; message 'decrementSampleUseCount';
  end;

type
  QTAudioSampleBuffer = objccategory external (QTSampleBuffer)
    function audioBufferListWithOptions (options: QTSampleBufferAudioBufferListOptions): AudioBufferListPtr; message 'audioBufferListWithOptions:';
    function getAudioStreamPacketDescriptions_inRange (audioStreamPacketDescriptions: AudioStreamPacketDescriptionPtr; range: NSRange): ObjCBOOL; message 'getAudioStreamPacketDescriptions:inRange:';
  end;
{$endif}

{$endif}
