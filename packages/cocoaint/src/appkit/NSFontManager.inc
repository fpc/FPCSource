{ Parsed from AppKit.framework NSFontManager.h }


{$ifdef TYPES}
type
  NSFontManagerPtr = ^NSFontManager;
{$endif}

{$ifdef TYPES}
type
  NSFontTraitMask = NSUInteger;
  NSFontTraitMaskPtr = ^NSFontTraitMask;

const
  NSItalicFontMask = $00000001;
  NSBoldFontMask = $00000002;
  NSUnboldFontMask = $00000004;
  NSNonStandardCharacterSetFontMask = $00000008;
  NSNarrowFontMask = $00000010;
  NSExpandedFontMask = $00000020;
  NSCondensedFontMask = $00000040;
  NSSmallCapsFontMask = $00000080;
  NSPosterFontMask = $00000100;
  NSCompressedFontMask = $00000200;
  NSFixedPitchFontMask = $00000400;
  NSUnitalicFontMask = $01000000;

type
  NSFontCollectionOptions = NSUInteger;
  NSFontCollectionOptionsPtr = ^NSFontCollectionOptions;

const
  NSFontCollectionApplicationOnlyMask = 1 shl 0;

type
  NSFontAction = NSUInteger;
  NSFontActionPtr = ^NSFontAction;

const
  NSNoFontChangeAction = 0;
  NSViaPanelFontAction = 1;
  NSAddTraitFontAction = 2;
  NSSizeUpFontAction = 3;
  NSSizeDownFontAction = 4;
  NSHeavierFontAction = 5;
  NSLighterFontAction = 6;
  NSRemoveTraitFontAction = 7;
{$endif}

{$ifdef CLASSES}

type
  NSFontManager = objcclass external (NSObject)
  private
    _panel: NSFontPanel;
    _fmReserved1: cuint;
    _action: SEL;
    _actionOrigin: id;
    _target: id;
    _selFont: NSFont;
    _fmFlags: bitpacked record
      case byte of
        0: (_anonBitField__fmFlags0: cuint);
        1: (
          multipleFont: 0..1;
          disabled: 0..1;
          senderTagMode: 0..((1 shl 2)-1);
          _RESERVED: 0..((1 shl 12)-1);
        );
      end;
    _fmReserved3: cushort;
    _delegate: id;
    _collections: id;
    _hiddenCollections: id;
    _fmReserved4: NSUInteger;
  public
    class procedure setFontPanelFactory (factoryId: pobjc_class); message 'setFontPanelFactory:';
    class procedure setFontManagerFactory (factoryId: pobjc_class); message 'setFontManagerFactory:';
    class function sharedFontManager: NSFontManager; message 'sharedFontManager';
    function isMultiple: ObjCBOOL; message 'isMultiple';
    function selectedFont: NSFont; message 'selectedFont';
    procedure setSelectedFont_isMultiple (fontObj: NSFont; flag: ObjCBOOL); message 'setSelectedFont:isMultiple:';
    procedure setFontMenu (newMenu: NSMenu); message 'setFontMenu:';
    function fontMenu (create_: ObjCBOOL): NSMenu; message 'fontMenu:';
    function fontPanel (create_: ObjCBOOL): NSFontPanel; message 'fontPanel:';
    function fontWithFamily_traits_weight_size (family: NSString; traits: NSFontTraitMask; weight: NSInteger; size: CGFloat): NSFont; message 'fontWithFamily:traits:weight:size:';
    function traitsOfFont (fontObj: NSFont): NSFontTraitMask; message 'traitsOfFont:';
    function weightOfFont (fontObj: NSFont): NSInteger; message 'weightOfFont:';
    function availableFonts: NSArray; message 'availableFonts';
    function availableFontFamilies: NSArray; message 'availableFontFamilies';
    function availableMembersOfFontFamily (fam: NSString): NSArray; message 'availableMembersOfFontFamily:';
    function convertFont (fontObj: NSFont): NSFont; message 'convertFont:';
    function convertFont_toSize (fontObj: NSFont; size: CGFloat): NSFont; message 'convertFont:toSize:';
    function convertFont_toFace (fontObj: NSFont; typeface: NSString): NSFont; message 'convertFont:toFace:';
    function convertFont_toFamily (fontObj: NSFont; family: NSString): NSFont; message 'convertFont:toFamily:';
    function convertFont_toHaveTrait (fontObj: NSFont; trait: NSFontTraitMask): NSFont; message 'convertFont:toHaveTrait:';
    function convertFont_toNotHaveTrait (fontObj: NSFont; trait: NSFontTraitMask): NSFont; message 'convertFont:toNotHaveTrait:';
    function convertWeight_ofFont (upFlag: ObjCBOOL; fontObj: NSFont): NSFont; message 'convertWeight:ofFont:';
    procedure setEnabled(newValue: ObjCBOOL); message 'setEnabled:';
    function isEnabled: ObjCBOOL; message 'isEnabled';
    procedure setAction(newValue: SEL); message 'setAction:';
    function action: SEL; message 'action';
    function sendAction: ObjCBOOL; message 'sendAction';
    procedure setDelegate(newValue: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    function localizedNameForFamily_face (family: NSString; faceKey: NSString): NSString; message 'localizedNameForFamily:face:';
    procedure setSelectedAttributes_isMultiple (attributes: NSDictionary; flag: ObjCBOOL); message 'setSelectedAttributes:isMultiple:';
    function convertAttributes (attributes: NSDictionary): NSDictionary; message 'convertAttributes:';
    function availableFontNamesMatchingFontDescriptor (descriptor: NSFontDescriptor): NSArray; message 'availableFontNamesMatchingFontDescriptor:';
    function collectionNames: NSArray; message 'collectionNames';
    function fontDescriptorsInCollection (collectionNames_: NSString): NSArray; message 'fontDescriptorsInCollection:';
    function addCollection_options (collectionName: NSString; collectionOptions: NSFontCollectionOptions): ObjCBOOL; message 'addCollection:options:';
    function removeCollection (collectionName: NSString): ObjCBOOL; message 'removeCollection:';
    procedure addFontDescriptors_toCollection (descriptors: NSArray; collectionName: NSString); message 'addFontDescriptors:toCollection:';
    procedure removeFontDescriptor_fromCollection (descriptor: NSFontDescriptor; collection: NSString); message 'removeFontDescriptor:fromCollection:';
    function currentFontAction: NSFontAction; message 'currentFontAction';
    function convertFontTraits (traits: NSFontTraitMask): NSFontTraitMask; message 'convertFontTraits:'; { available in 10_5 }
    procedure setTarget(newValue: id); message 'setTarget:';
    function target: id; message 'target';
  end;


type
  NSFontManagerMenuActionMethods = objccategory external (NSFontManager)
    function fontNamed_hasTraits (fName: NSString; someTraits: NSFontTraitMask): ObjCBOOL; message 'fontNamed:hasTraits:';
    function availableFontNamesWithTraits (someTraits: NSFontTraitMask): NSArray; message 'availableFontNamesWithTraits:';
    procedure addFontTrait (sender: id); message 'addFontTrait:';
    procedure removeFontTrait (sender: id); message 'removeFontTrait:';
    procedure modifyFontViaPanel (sender: id); message 'modifyFontViaPanel:';
    procedure modifyFont (sender: id); message 'modifyFont:';
    procedure orderFrontFontPanel (sender: id); message 'orderFrontFontPanel:';
    procedure orderFrontStylesPanel (sender: id); message 'orderFrontStylesPanel:';
  end;

type
  NSFontManagerDelegate = objccategory external (NSObject)
    function fontManager_willIncludeFont (sender: id; fontName: NSString): ObjCBOOL; message 'fontManager:willIncludeFont:';
  end;

type
  NSFontManagerResponderMethod = objccategory external (NSObject)
    procedure changeFont (sender: id); message 'changeFont:';
  end;
{$endif}

