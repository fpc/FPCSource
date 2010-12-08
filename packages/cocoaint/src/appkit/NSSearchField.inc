{ Parsed from Appkit.framework NSSearchField.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


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
  NSSearchFieldPointer = ^NSSearchField;
  NSSearchFieldPtr = NSSearchFieldPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSEARCHFIELD_PAS_C}
{$define NSSEARCHFIELD_PAS_C}

{ NSSearchField }
  NSSearchField = objcclass(NSTextField)
  private
    _reserved1: cuint;
    _reserved2: cuint;
    _reserved3: cuint;
    _reserved4: cuint;
    
  public
    class function alloc: NSSearchField; message 'alloc';

    procedure setRecentSearches(searches: NSArray); message 'setRecentSearches:';
    function recentSearches: NSArray; message 'recentSearches';
    procedure setRecentsAutosaveName(string_: NSString); message 'setRecentsAutosaveName:';
    function recentsAutosaveName: NSString; message 'recentsAutosaveName';
  end; external;

{$endif}
{$endif}
