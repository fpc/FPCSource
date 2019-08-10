{ Parsed from InputMethodKit.framework IMKCandidates.h }


{$ifdef TYPES}
type
  IMKCandidatesPtr = ^IMKCandidates;
{$endif}

{$ifdef TYPES}
const
  kIMKSingleColumnScrollingCandidatePanel = 1;
  kIMKScrollingGridCandidatePanel = 2;
  kIMKSingleRowSteppingCandidatePanel = 3;

type
  IMKCandidatePanelType = NSUInteger;
  IMKCandidatePanelTypePtr = ^IMKCandidatePanelType;

const
  kIMKMain = 0;
  kIMKAnnotation = 1;
  kIMKSubList = 2;

type
  IMKStyleType = NSUInteger;
  IMKStyleTypePtr = ^IMKStyleType;

const
  kIMKLocateCandidatesAboveHint = 1;
  kIMKLocateCandidatesBelowHint = 2;
  kIMKLocateCandidatesLeftHint = 3;
  kIMKLocateCandidatesRightHint = 4;

type
  IMKCandidatesLocationHint = NSUInteger;
  IMKCandidatesLocationHintPtr = ^IMKCandidatesLocationHint;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  IMKCandidatesOpacityAttributeName: NSString; cvar; external;
  IMKCandidatesSendServerKeyEventFirst: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  IMKCandidates = objcclass external (NSResponder)
  private
    _private: IMKCandidatesPrivate;
  public
    function initWithServer_panelType (server: IMKServer; panelType: IMKCandidatePanelType): id; message 'initWithServer:panelType:';
    function initWithServer_panelType_styleType (server: IMKServer; panelType: IMKCandidatePanelType; style: IMKStyleType): id; message 'initWithServer:panelType:styleType:';
    function panelType: IMKCandidatePanelType; message 'panelType';
    procedure setPanelType (panelType_: IMKCandidatePanelType); message 'setPanelType:';
    procedure show (locationHint: IMKCandidatesLocationHint); message 'show:';
    procedure hide; message 'hide';
    function isVisible: ObjCBOOL; message 'isVisible';
    procedure updateCandidates; message 'updateCandidates';
    procedure showAnnotation (annotationString: NSAttributedString); message 'showAnnotation:';
    procedure showSublist_subListDelegate (candidates: NSArray; delegate: id); message 'showSublist:subListDelegate:';
    function selectedCandidateString: NSAttributedString; message 'selectedCandidateString';
    procedure setCandidateFrameTopLeft (point: NSPoint); message 'setCandidateFrameTopLeft:';
    function candidateFrame: NSRect; message 'candidateFrame';
    procedure setSelectionKeys (keyCodes: NSArray); message 'setSelectionKeys:';
    function selectionKeys: NSArray; message 'selectionKeys';
    procedure setSelectionKeysKeylayout (layout: TISInputSourceRef); message 'setSelectionKeysKeylayout:';
    function selectionKeysKeylayout: TISInputSourceRef; message 'selectionKeysKeylayout';
    procedure setAttributes (attributes: NSDictionary); message 'setAttributes:';
    function attributes: NSDictionary; message 'attributes';
    procedure setDismissesAutomatically (flag: ObjCBOOL); message 'setDismissesAutomatically:';
    function dismissesAutomatically: ObjCBOOL; message 'dismissesAutomatically';
    function selectedCandidateString_: NSAttributedString; message 'selectedCandidateString';
    function selectedCandidate: NSInteger; message 'selectedCandidate';
    procedure setCandidateFrameTopLeft_ (point: NSPoint); message 'setCandidateFrameTopLeft:';
    procedure showChild; message 'showChild';
    procedure hideChild; message 'hideChild';
    procedure attachChild_toCandidate_type (child: IMKCandidates; candidateIdentifier: NSInteger; theType: IMKStyleType); message 'attachChild:toCandidate:type:';
    procedure detachChild (candidateIdentifier: NSInteger); message 'detachChild:';
    procedure setCandidateData (candidatesArray: NSArray); message 'setCandidateData:';
    function selectCandidateWithIdentifier (candidateIdentifier: NSInteger): ObjCBOOL; message 'selectCandidateWithIdentifier:';
    procedure selectCandidate (candidateIdentifier: NSInteger); message 'selectCandidate:';
    procedure showCandidates; message 'showCandidates';
    function candidateStringIdentifier (candidateString: id): NSInteger; message 'candidateStringIdentifier:';
    function selectedCandidateString__: NSAttributedString; message 'selectedCandidateString';
    function candidateIdentifierAtLineNumber (lineNumber: NSInteger): NSInteger; message 'candidateIdentifierAtLineNumber:';
    function lineNumberForCandidateWithIdentifier (candidateIdentifier: NSInteger): NSInteger; message 'lineNumberForCandidateWithIdentifier:';
    procedure clearSelection; message 'clearSelection';
  end;
{$endif}

