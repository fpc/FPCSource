{ Parsed from AddressBook.framework ABSearchElement.h }


{$ifdef TYPES}
type
  ABSearchElementPtr = ^ABSearchElement;
{$endif}

{$ifdef CLASSES}

type
  ABSearchElement = objcclass external (NSObject)
  public
    class function searchElementForConjunction_children (conjuction: ABSearchConjunction; children: NSArray): ABSearchElement; message 'searchElementForConjunction:children:';
    function matchesRecord (record_: ABRecord): ObjCBOOL; message 'matchesRecord:';
  end;
{$endif}

