{ Parsed from Appkit.framework NSPredicateEditor.h }

{$ifdef TYPES}
{$ifndef NSPREDICATEEDITOR_PAS_T}
{$define NSPREDICATEEDITOR_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPREDICATEEDITOR_PAS_R}
{$define NSPREDICATEEDITOR_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPREDICATEEDITOR_PAS_F}
{$define NSPREDICATEEDITOR_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPREDICATEEDITOR_PAS_S}
{$define NSPREDICATEEDITOR_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPredicateEditor = objcclass;
  NSPredicateEditorPointer = ^NSPredicateEditor;
  NSPredicateEditorPtr = NSPredicateEditorPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPREDICATEEDITOR_PAS_C}
{$define NSPREDICATEEDITOR_PAS_C}

{ NSPredicateEditor }
  NSPredicateEditor = objcclass external (NSRuleEditor)
  private
    _allTemplates: id;
    _rootTrees: NSArray;
    _rootHeaderTrees: NSArray;
    _predicateTarget: id;
    _predicateAction: SEL;
    __peFlags: record
      case byte of
       0: (_anonbitfield_NSPredicateEditor0: cuint);
       1: (data: bitpacked record
        suppressAction: 0..((1 shl 4)-1);
        reserved: 0..((1 shl 28)-1);
       end;
      );
      end;
    _predicateEditorReserved: array[0..(3)-1] of id;
    
  public
    procedure setRowTemplates(rowTemplates_: NSArray); message 'setRowTemplates:';
    function rowTemplates: NSArray; message 'rowTemplates';
  end;

{$endif}
{$endif}
