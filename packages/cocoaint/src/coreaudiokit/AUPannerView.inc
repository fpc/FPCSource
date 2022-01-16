{ Parsed from CoreAudioKit.framework AUPannerView.h }


{$ifdef TYPES}
type
  AUPannerViewPtr = ^AUPannerView;
{$endif}

{$ifdef CLASSES}

type
  AUPannerView = objcclass external (NSView)
  private
  public
    function audioUnit: AudioUnit; message 'audioUnit';
    class function AUPannerViewWithAudioUnit (au: AudioUnit): AUPannerView; message 'AUPannerViewWithAudioUnit:';
  end;
{$endif}

