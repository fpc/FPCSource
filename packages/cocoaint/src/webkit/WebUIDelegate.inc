{ Parsed from Webkit.framework WebUIDelegate.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


{$ifdef TYPES}
{$ifndef WEBUIDELEGATE_PAS_T}
{$define WEBUIDELEGATE_PAS_T}

{ Constants }

const
  WebMenuItemTagOpenLinkInNewWindow = 1;
  WebMenuItemTagDownloadLinkToDisk = 0;
  WebMenuItemTagCopyLinkToClipboard = 1;
  WebMenuItemTagOpenImageInNewWindow = 2;
  WebMenuItemTagDownloadImageToDisk = 3;
  WebMenuItemTagCopyImageToClipboard = 4;
  WebMenuItemTagOpenFrameInNewWindow = 5;
  WebMenuItemTagCopy = 6;
  WebMenuItemTagGoBack = 7;
  WebMenuItemTagGoForward = 8;
  WebMenuItemTagStop = 9;
  WebMenuItemTagReload = 10;
  WebMenuItemTagCut = 11;
  WebMenuItemTagPaste = 12;
  WebMenuItemTagSpellingGuess = 13;
  WebMenuItemTagNoGuessesFound = 14;
  WebMenuItemTagIgnoreSpelling = 15;
  WebMenuItemTagLearnSpelling = 16;
  WebMenuItemTagOther = 17;
  WebMenuItemTagSearchInSpotlight = 18;
  WebMenuItemTagSearchWeb = 19;
  WebMenuItemTagLookUpInDictionary = 20;
  WebMenuItemTagOpenWithDefaultApplication = 21;
  WebMenuItemPDFActualSize = 22;
  WebMenuItemPDFZoomIn = 23;
  WebMenuItemPDFZoomOut = 24;
  WebMenuItemPDFAutoSize = 25;
  WebMenuItemPDFSinglePage = 26;
  WebMenuItemPDFFacingPages = 27;
  WebMenuItemPDFContinuous = 28;
  WebMenuItemPDFNextPage = 29;
  WebMenuItemPDFPreviousPage = 30;

const
  WebDragDestinationActionNone = 0;
  WebDragDestinationActionDHTML = 1;
  WebDragDestinationActionEdit = 2;
  WebDragDestinationActionLoad = 4;
  WebDragDestinationActionAny = UINT_MAX;

const
  WebDragSourceActionNone = 0;
  WebDragSourceActionDHTML = 1;
  WebDragSourceActionImage = 2;
  WebDragSourceActionLink = 4;
  WebDragSourceActionSelection = 8;
  WebDragSourceActionAny = UINT_MAX;

{ Types }
type
  WebDragDestinationAction = culong;
  WebDragSourceAction = culong;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef WEBUIDELEGATE_PAS_R}
{$define WEBUIDELEGATE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef WEBUIDELEGATE_PAS_F}
{$define WEBUIDELEGATE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef WEBUIDELEGATE_PAS_S}
{$define WEBUIDELEGATE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  WebOpenPanelResultListenerProtocol = objcprotocol;

{$endif}
{$ifdef PROTOCOLS}
{$ifndef WEBUIDELEGATE_PAS_P}
{$define WEBUIDELEGATE_PAS_P}
  
{ WebOpenPanelResultListener Protocol }
  WebOpenPanelResultListenerProtocol = objcprotocol
    procedure chooseFilename(fileName: NSString); message 'chooseFilename:';
    procedure cancel; message 'cancel';
  end; external name 'WebOpenPanelResultListener';
{$endif}
{$endif}
