{ Parsed from Appkit.framework NSSearchField.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:53 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSEARCHFIELD_PAS_H}
{$define NSSEARCHFIELD_PAS_H}
type
  NSSearchFieldPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSSEARCHFIELD_PAS_T}
{$define NSSEARCHFIELD_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSEARCHFIELD_PAS_R}
{$define NSSEARCHFIELD_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSEARCHFIELD_PAS_F}
{$define NSSEARCHFIELD_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSEARCHFIELD_PAS_S}
{$define NSSEARCHFIELD_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSSearchField = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSEARCHFIELD_PAS_C}
{$define NSSEARCHFIELD_PAS_C}

{ NSSearchField }
  NSSearchField = objcclass(NSTextField)
  private
    __reserved1: cuint;
    __reserved2: cuint;
    __reserved3: cuint;
    __reserved4: cuint;
    
  public
    class function alloc: NSSearchField; message 'alloc';

    procedure setRecentSearches(searches: NSArray); message 'setRecentSearches:';
    function recentSearches: NSArray; message 'recentSearches';
    procedure setRecentsAutosaveName(string_: NSString); message 'setRecentsAutosaveName:';
    function recentsAutosaveName: NSString; message 'recentsAutosaveName';
  end; external;

{$endif}
{$endif}
