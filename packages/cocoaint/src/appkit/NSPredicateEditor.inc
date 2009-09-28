{ Parsed from Appkit.framework NSPredicateEditor.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:53 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSPREDICATEEDITOR_PAS_H}
{$define NSPREDICATEEDITOR_PAS_H}
type
  NSPredicateEditorPointer = Pointer;

{$endif}
{$endif}

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

{$endif}

{$ifdef CLASSES}
{$ifndef NSPREDICATEEDITOR_PAS_C}
{$define NSPREDICATEEDITOR_PAS_C}

{ NSPredicateEditor }
  NSPredicateEditor = objcclass(NSRuleEditor)
  private
    __allTemplates: id;
    __rootItems: NSArray;
    __rootHeaderItems: NSArray;
    __predicateTarget: id;
    __predicateAction: SEL;
    __peFlags: bitpacked record
        settingValue: 0..1;
        reserved: 0..((1 shl 31)-1);
      end;
        _predicateEditorReserved: array[0..2] of id;
    
  public
    class function alloc: NSPredicateEditor; message 'alloc';

    procedure setRowTemplates(rowTemplates_: NSArray); message 'setRowTemplates:';
    function rowTemplates: NSArray; message 'rowTemplates';
  end; external;

{$endif}
{$endif}
