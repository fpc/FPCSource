{ Parsed from AppKit.framework NSFontPanel.h }


{$ifdef TYPES}
type
  NSFontPanelPtr = ^NSFontPanel;
{$endif}

{$ifdef CLASSES}

type
  NSFontPanelValidationAdditions = objccategory external (NSObject)
    function validModesForFontPanel (fontPanel: NSFontPanel): NSUInteger; message 'validModesForFontPanel:';
  end;


type
  NSFontPanel = objcclass external (NSPanel)
  private
    _manager: NSFontManager;
    _collectionNames: NSArray;
    _selection: id;
    _carbonNotification: pointer;
    _targetObject: id;
    _familyList: id;
    _faceList: id;
    _sizeList: id;
    _mainCollectionList: id;
    _sizeField: id;
    _sizeSlider: id;
    _sizeSliderBox: id;
    _preview: id;
    _previewContainer: id;
    _mainSplitView: id;
    _mmCollectionList: id;
    _mmFamilyList: id;
    _mmFaceList: id;
    _mmSizeList: id;
    _fullSizeHeightConstraint: id;
    _searchField: id;
    _fixedListButton: id;
    _sliderButton: id;
    _accessoryView: id;
    _fpFlags: bitpacked record
      case byte of
        0: (_anonBitField__fpFlags0: cuint);
        1: (
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
          _miniMode: 0..1;
          _reserved: 0..((1 shl 13)-1);
        );
      end;
    _regularModeBox: id;
    _miniModeBox: id;
    _sizeBox: id;
    _collectionLabel: id;
    _sizeLabel: id;
    _faceDivider: id;
    _familyLabel: id;
    _sizeStyleButton: id;
    _newSizeField: id;
    _editSizeList: id;
    _editSizeListBox: id;
    _editSizeSliderBox: id;
    _editSizeSliderMaxField: id;
    _editSizeSliderMinField: id;
    _sizeEditWindow: id;
    _availableSizes: id;
    _addCollectionButton: id;
    _removeCollectionButton: id;
    _fontPanelPreviewHeight: CGFloat;
    _typographyPanel: id;
    _actionButton: id;
    _fontEffectsBox: id;
    _sizeStyle: cint;
    _mode: NSInteger;
    {$if  not defined(__LP64__)}
    _fpUnused:array[0..70] of id;
    {$endif}
  public
    class function sharedFontPanel: NSFontPanel; message 'sharedFontPanel';
    class function sharedFontPanelExists: ObjCBOOL; message 'sharedFontPanelExists';
    procedure setAccessoryView(newValue: NSView); message 'setAccessoryView:';
    function accessoryView: NSView; message 'accessoryView';
    procedure setPanelFont_isMultiple (fontObj: NSFont; flag: ObjCBOOL); message 'setPanelFont:isMultiple:';
    function panelConvertFont (fontObj: NSFont): NSFont; message 'panelConvertFont:';
    procedure setWorksWhenModal(newValue: ObjCBOOL); message 'setWorksWhenModal:';
    function worksWhenModal: ObjCBOOL; message 'worksWhenModal';
    procedure setEnabled(newValue: ObjCBOOL); message 'setEnabled:';
    function isEnabled: ObjCBOOL; message 'isEnabled';
    procedure reloadDefaultFontFamilies; message 'reloadDefaultFontFamilies';
  end;
{$endif}

{$ifdef TYPES}
const
  NSFontPanelFaceModeMask = 1 shl 0;
  NSFontPanelSizeModeMask = 1 shl 1;
  NSFontPanelCollectionModeMask = 1 shl 2;
  NSFontPanelUnderlineEffectModeMask = 1 shl 8;
  NSFontPanelStrikethroughEffectModeMask = 1 shl 9;
  NSFontPanelTextColorEffectModeMask = 1 shl 10;
  NSFontPanelDocumentColorEffectModeMask = 1 shl 11;
  NSFontPanelShadowEffectModeMask = 1 shl 12;
  NSFontPanelAllEffectsModeMask = $FFF00;
  NSFontPanelStandardModesMask = $FFFF;
  NSFontPanelAllModesMask = $FFFFFFFF;
const
  NSFPPreviewButton = 131;
  NSFPRevertButton = 130;
  NSFPSetButton = 132;
  NSFPPreviewField = 128;
  NSFPSizeField = 129;
  NSFPSizeTitle = 133;
  NSFPCurrentField = 134;
{$endif}

