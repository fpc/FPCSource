{ Parsed from WebKit.framework DOMHTMLOptionsCollection.h }


{$ifdef TYPES}
type
  DOMHTMLOptionsCollectionPtr = ^DOMHTMLOptionsCollection;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLOptionsCollection = objcclass external (DOMObject)
  public
    procedure setSelectedIndex(newValue: cint); message 'setSelectedIndex:';
    function selectedIndex: cint; message 'selectedIndex';
    procedure setLength(newValue: cuint); message 'setLength:';
    function length: cuint; message 'length';
    function namedItem (name: NSString): DOMNode; message 'namedItem:';
    procedure add_index (option: DOMHTMLOptionElement; index: cuint); message 'add:index:'; { available in 10_5 }
    procedure remove (index: cuint); message 'remove:'; { available in 10_6 }
    function item (index: cuint): DOMNode; message 'item:';
  end;
{$endif}

