{ Parsed from AppKit.framework NSSearchField.h }


{$ifdef TYPES}
type
  NSSearchFieldPtr = ^NSSearchField;
{$endif}

{$ifdef CLASSES}

type
  NSSearchField = objcclass external (NSTextField)
  private
    _sfFlags: bitpacked record
      case byte of
        0: (_anonBitField__sfFlags0: cuint);
        1: (
          _inTransition: 0..1;
          _reselecting: 0..1;
          _reserved: 0..((1 shl 30)-1);
        );
      end;
    _reserved2: cuint;
    _reserved3: cuint;
    _reserved4: cuint;
  public
    procedure setRecentSearches(newValue: NSArray); message 'setRecentSearches:';
    function recentSearches: NSArray; message 'recentSearches';
    procedure setRecentsAutosaveName(newValue: NSString); message 'setRecentsAutosaveName:';
    function recentsAutosaveName: NSString; message 'recentsAutosaveName';
    procedure setSearchMenuTemplate(newValue: NSMenu); message 'setSearchMenuTemplate:';
    function searchMenuTemplate: NSMenu; message 'searchMenuTemplate';
    procedure setSendsWholeSearchString(newValue: ObjCBOOL); message 'setSendsWholeSearchString:';
    function sendsWholeSearchString: ObjCBOOL; message 'sendsWholeSearchString';
    procedure setMaximumRecents(newValue: NSInteger); message 'setMaximumRecents:';
    function maximumRecents: NSInteger; message 'maximumRecents';
    procedure setSendsSearchStringImmediately(newValue: ObjCBOOL); message 'setSendsSearchStringImmediately:';
    function sendsSearchStringImmediately: ObjCBOOL; message 'sendsSearchStringImmediately';
  end;
{$endif}

