{ Parsed from Appkit.framework NSFontPanel.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSFONTPANEL_PAS_H}
{$define NSFONTPANEL_PAS_H}
type
  NSFontPanelPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSFONTPANEL_PAS_T}
{$define NSFONTPANEL_PAS_T}

{ Constants }

const
  NSFPPreviewButton = 131;
  NSFPRevertButton = 130;
  NSFPSetButton = 132;
  NSFPPreviewField = 128;
  NSFPSizeField = 129;
  NSFPSizeTitle = 133;
  NSFPCurrentField = 134;

const
  NSFontPanelFaceModeMask = 1  shl  0;
  NSFontPanelSizeModeMask = 1  shl  1;
  NSFontPanelCollectionModeMask = 1  shl  2;
  NSFontPanelUnderlineEffectModeMask = 1 shl 8;
  NSFontPanelStrikethroughEffectModeMask = 1 shl 9;
  NSFontPanelTextColorEffectModeMask = 1 shl  10;
  NSFontPanelDocumentColorEffectModeMask = 1 shl 11;
  NSFontPanelShadowEffectModeMask = 1 shl 12;
  NSFontPanelAllEffectsModeMask = $FFF00;
  NSFontPanelStandardModesMask = $FFFF;
  NSFontPanelAllModesMask = $FFFFFFFF;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSFONTPANEL_PAS_R}
{$define NSFONTPANEL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSFONTPANEL_PAS_F}
{$define NSFONTPANEL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSFONTPANEL_PAS_T}
{$define NSFONTPANEL_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSFontPanel = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSFONTPANEL_PAS_C}
{$define NSFONTPANEL_PAS_C}

{ NSFontPanel }
  NSFontPanel = objcclass(NSPanel)
  private
    __manager: NSFontManager;
    __collectionNames: NSArray;
    __selection: id;
    __carbonNotification: Pointer;
    __targetObject: id;
    __familyList: id;
    __faceList: id;
    __sizeList: id;
    __mainCollectionList: id;
    __sizeField: id;
    __sizeSlider: id;
    __sizeSliderBox: id;
    __preview: id;
    __previewCaption: id;
    __mainSplitView: id;
    __mmCollectionList: id;
    __mmFamilyList: id;
    __mmFaceList: id;
    __mmSizeList: id;
    __extrasPopup: id;
    __searchField: id;
    __fixedListButton: id;
    __sliderButton: id;
    __accessoryView: id;
    __fpFlags: bitpacked record
        setFontChange: 0..1;
        setFontAttributeChange: 0..1;
        _delRespFamily: 0..1;
        _delRespFace: 0..1;
        _delRespSize: 0..1;
        _delRespColl: 0..1;
        _collectionDisabled: 0..1;
        _sizeDisabled: 0..1;
        _faceDisabled: 0..1;
        showEffects: 0..1;
        _uiMode: 0..((1 shl 8)-1);
        _reserved: 0..((1 shl 14)-1);
      end;
    __regularModeBox: id;
    __miniModeBox: id;
    __modeBoxSuperview: id;
    __collectionLabel: id;
    __sizeLabel: id;
    __faceLabel: id;
    __familyLabel: id;
    __sizeStyleButton: id;
    __newSizeField: id;
    __editSizeList: id;
    __editSizeListBox: id;
    __editSizeSliderBox: id;
    __editSizeSliderMaxField: id;
    __editSizeSliderMinField: id;
    __sizeEditWindow: id;
    __availableSizes: id;
    __addCollectionButton: id;
    __removeCollectionButton: id;
    __fontPanelPreviewHeight: CGFloat;
    __typographyPanel: id;
    __actionButton: id;
    __fontEffectsBox: id;
    __sizeStyle: cint;
    {$ifndef cpu64}
    __fpUnused: id;
    {$endif}
    
  public
    class function alloc: NSFontPanel; message 'alloc';

    class function sharedFontPanel: NSFontPanel; message 'sharedFontPanel';
    class function sharedFontPanelExists: Boolean; message 'sharedFontPanelExists';
    function accessoryView: NSView; message 'accessoryView';
    procedure setAccessoryView(aView: NSView); message 'setAccessoryView:';
    procedure setPanelFont_isMultiple(fontObj: NSFont; flag: Boolean); message 'setPanelFont:isMultiple:';
    function panelConvertFont(fontObj: NSFont): NSFont; message 'panelConvertFont:';
    function worksWhenModal: Boolean; message 'worksWhenModal';
    function isEnabled: Boolean; message 'isEnabled';
    procedure setEnabled(flag: Boolean); message 'setEnabled:';
    procedure reloadDefaultFontFamilies; message 'reloadDefaultFontFamilies';
  end; external;

{$endif}
{$endif}
