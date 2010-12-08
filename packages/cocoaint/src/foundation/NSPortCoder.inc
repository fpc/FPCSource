{ Parsed from Foundation.framework NSPortCoder.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:43 CET 2010 }


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
  NSPortCoder = objcclass(NSCoder)
    
  public
    class function alloc: NSPortCoder; message 'alloc';

    function isBycopy: Boolean; message 'isBycopy';
    function isByref: Boolean; message 'isByref';
    function connection: NSConnection; message 'connection';
    procedure encodePortObject(aport: NSPort); message 'encodePortObject:';
    function decodePortObject: NSPort; message 'decodePortObject';
    function initWithReceivePort_sendPort_components(rcvPort: NSPort; sndPort: NSPort; comps: NSArray): id; message 'initWithReceivePort:sendPort:components:';
    procedure dispatch; message 'dispatch';
  end; external;

{ NSDistributedObjectsCategory }
  NSDistributedObjectsCategory = objccategory(NSObject)
    function classForPortCoder: Pobjc_class; message 'classForPortCoder';
    function replacementObjectForPortCoder(coder: NSPortCoder): id; message 'replacementObjectForPortCoder:';
  end; external;

{$endif}
{$endif}
