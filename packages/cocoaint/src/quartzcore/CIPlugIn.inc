{ Parsed from Quartzcore.framework CIPlugIn.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:07 CET 2010 }


{$ifdef TYPES}
{$ifndef CIPLUGIN_PAS_T}
{$define CIPLUGIN_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CIPLUGIN_PAS_R}
{$define CIPLUGIN_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CIPLUGIN_PAS_F}
{$define CIPLUGIN_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CIPLUGIN_PAS_S}
{$define CIPLUGIN_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  CIPlugIn = objcclass;
  CIPlugInPointer = ^CIPlugIn;
  CIPlugInPtr = CIPlugInPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef CIPLUGIN_PAS_C}
{$define CIPLUGIN_PAS_C}

{ CIPlugIn }
  CIPlugIn = objcclass(NSObject)
  private
        _priv: array[0..7] of Pointer;
    
  public
    class function alloc: CIPlugIn; message 'alloc';

    class procedure loadAllPlugIns; message 'loadAllPlugIns';
    class procedure loadNonExecutablePlugIns; message 'loadNonExecutablePlugIns';
    class procedure loadPlugIn_allowNonExecutable(url: NSURL; allowNonExecutable: Boolean); message 'loadPlugIn:allowNonExecutable:';
  end; external;

{$endif}
{$endif}
