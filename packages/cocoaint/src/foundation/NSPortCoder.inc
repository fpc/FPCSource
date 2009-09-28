{ Parsed from Foundation.framework NSPortCoder.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:51 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSPORTCODER_PAS_H}
{$define NSPORTCODER_PAS_H}
type
  NSPortCoderPointer = Pointer;

{$endif}
{$endif}

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

{$endif}
{$endif}
