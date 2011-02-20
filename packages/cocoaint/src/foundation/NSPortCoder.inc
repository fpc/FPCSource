{ Parsed from Foundation.framework NSPortCoder.h }

{$ifdef TYPES}
{$ifndef NSPORTCODER_PAS_T}
{$define NSPORTCODER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPORTCODER_PAS_R}
{$define NSPORTCODER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPORTCODER_PAS_F}
{$define NSPORTCODER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPORTCODER_PAS_S}
{$define NSPORTCODER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPortCoder = objcclass;
  NSPortCoderPointer = ^NSPortCoder;
  NSPortCoderPtr = NSPortCoderPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPORTCODER_PAS_C}
{$define NSPORTCODER_PAS_C}

{ NSPortCoder }
  NSPortCoder = objcclass external (NSCoder)
    
  public
    function isBycopy: Boolean; message 'isBycopy';
    function isByref: Boolean; message 'isByref';
    function connection: NSConnection; message 'connection';
    procedure encodePortObject(aport: NSPort); message 'encodePortObject:';
    function decodePortObject: NSPort; message 'decodePortObject';
    function initWithReceivePort_sendPort_components(rcvPort: NSPort; sndPort: NSPort; comps: NSArray): id; message 'initWithReceivePort:sendPort:components:';
    procedure dispatch; message 'dispatch';
  end;

{ NSDistributedObjectsCategory }
  NSDistributedObjectsCategory = objccategory external (NSObject)
    function classForPortCoder: Pobjc_class; message 'classForPortCoder';
    function replacementObjectForPortCoder(coder: NSPortCoder): id; message 'replacementObjectForPortCoder:';
  end;

{$endif}
{$endif}
