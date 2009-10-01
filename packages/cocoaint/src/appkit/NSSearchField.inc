{ Parsed from Appkit.framework NSSearchField.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


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
