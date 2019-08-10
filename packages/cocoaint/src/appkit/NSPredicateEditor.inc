{ Parsed from AppKit.framework NSPredicateEditor.h }


{$ifdef TYPES}
type
  NSPredicateEditorPtr = ^NSPredicateEditor;
{$endif}

{$ifdef CLASSES}

type
  NSPredicateEditor = objcclass external (NSRuleEditor)
  private
    _allTemplates: id;
    _rootTrees: NSArray;
    _rootHeaderTrees: NSArray;
    _predicateTarget: id;
    _predicateAction: SEL;
    _peFlags: bitpacked record
      case byte of
        0: (_anonBitField__peFlags0: cuint);
        1: (
          suppressAction: 0..((1 shl 4)-1);
          reserved: 0..((1 shl 28)-1);
        );
      end;
    _predicateEditorReserved:array[0..2] of id;
  public
    procedure setRowTemplates(newValue: NSArray); message 'setRowTemplates:';
    function rowTemplates: NSArray; message 'rowTemplates';
  end;
{$endif}

