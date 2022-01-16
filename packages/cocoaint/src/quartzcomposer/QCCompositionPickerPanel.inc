{ Parsed from QuartzComposer.framework QCCompositionPickerPanel.h }


{$ifdef TYPES}
type
  QCCompositionPickerPanelPtr = ^QCCompositionPickerPanel;
{$endif}

{$if (defined(MAC_OS_X_VERSION_10_5)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5))}
{$ifdef EXTERNAL_SYMBOLS}
var
  QCCompositionPickerPanelDidSelectCompositionNotification: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  QCCompositionPickerPanel = objcclass external (NSPanel)
  private
    _private: pointer;
  public
    class function sharedCompositionPickerPanel: QCCompositionPickerPanel; message 'sharedCompositionPickerPanel';
    function compositionPickerView: QCCompositionPickerView; message 'compositionPickerView';
  end;
{$endif}

{$endif}
