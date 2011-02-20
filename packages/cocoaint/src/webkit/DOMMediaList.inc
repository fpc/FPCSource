{ Parsed from Webkit.framework DOMMediaList.h }

{$ifdef TYPES}
{$ifndef DOMMEDIALIST_PAS_T}
{$define DOMMEDIALIST_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMMEDIALIST_PAS_R}
{$define DOMMEDIALIST_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMMEDIALIST_PAS_F}
{$define DOMMEDIALIST_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMMEDIALIST_PAS_S}
{$define DOMMEDIALIST_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMMediaList = objcclass;
  DOMMediaListPointer = ^DOMMediaList;
  DOMMediaListPtr = DOMMediaListPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMMEDIALIST_PAS_C}
{$define DOMMEDIALIST_PAS_C}

{ DOMMediaList }
  DOMMediaList = objcclass external (DOMObject)
    
  public
    procedure setMediaText (newValue: NSString); message 'setMediaText:';
    function mediaText: NSString; message 'mediaText';
    function length: cuint; message 'length';
    function item(index: cuint): NSString; message 'item:';
    procedure deleteMedium(oldMedium: NSString); message 'deleteMedium:';
    procedure appendMedium(newMedium: NSString); message 'appendMedium:';
  end;

{$endif}
{$endif}
