{ Parsed from OSAKit.framework OSAScriptView.h }


{$ifdef TYPES}
type
  OSAScriptViewPtr = ^OSAScriptView;
{$endif}

{$ifdef CLASSES}

type
  OSAScriptView = objcclass external (NSTextView)
  private
    _private: OSAScriptViewPrivate;
  public
    procedure setSource(newValue: NSString); message 'setSource:';
    function source: NSString; message 'source';
    procedure setUsesScriptAssistant(newValue: ObjCBOOL); message 'setUsesScriptAssistant:';
    function usesScriptAssistant: ObjCBOOL; message 'usesScriptAssistant';
    procedure setUsesTabs(newValue: ObjCBOOL); message 'setUsesTabs:';
    function usesTabs: ObjCBOOL; message 'usesTabs';
    procedure setTabWidth(newValue: NSUInteger); message 'setTabWidth:';
    function tabWidth: NSUInteger; message 'tabWidth';
    procedure setWrapsLines(newValue: ObjCBOOL); message 'setWrapsLines:';
    function wrapsLines: ObjCBOOL; message 'wrapsLines';
    procedure setIndentsWrappedLines(newValue: ObjCBOOL); message 'setIndentsWrappedLines:';
    function indentsWrappedLines: ObjCBOOL; message 'indentsWrappedLines';
    procedure setIndentWidth(newValue: NSUInteger); message 'setIndentWidth:';
    function indentWidth: NSUInteger; message 'indentWidth';
  end;
{$endif}

