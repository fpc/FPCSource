{$mode objfpc}
{$modeswitch objectivec1}
program IvarSize;
uses
 objp,objcrtl,objcrtlmacosx;
type
 TDerivedNSAffineTransform = objcclass (NSAffineTransform)
 extrabyte: byte;
end;
type
 TDerivedNSAppleEventDescriptor = objcclass (NSAppleEventDescriptor)
 extrabyte: byte;
end;
type
 TDerivedNSAppleEventManager = objcclass (NSAppleEventManager)
 extrabyte: byte;
end;
type
 TDerivedNSAppleScript = objcclass (NSAppleScript)
 extrabyte: byte;
end;
type
 TDerivedNSArchiver = objcclass (NSArchiver)
 extrabyte: byte;
end;
type
 TDerivedNSUnarchiver = objcclass (NSUnarchiver)
 extrabyte: byte;
end;
type
 TDerivedNSArray = objcclass (NSArray)
 extrabyte: byte;
end;
type
 TDerivedNSMutableArray = objcclass (NSMutableArray)
 extrabyte: byte;
end;
type
 TDerivedNSAttributedString = objcclass (NSAttributedString)
 extrabyte: byte;
end;
type
 TDerivedNSMutableAttributedString = objcclass (NSMutableAttributedString)
 extrabyte: byte;
end;
type
 TDerivedNSAutoreleasePool = objcclass (NSAutoreleasePool)
 extrabyte: byte;
end;
type
 TDerivedNSBundle = objcclass (NSBundle)
 extrabyte: byte;
end;
type
 TDerivedNSCalendar = objcclass (NSCalendar)
 extrabyte: byte;
end;
type
 TDerivedNSDateComponents = objcclass (NSDateComponents)
 extrabyte: byte;
end;
type
 TDerivedNSCalendarDate = objcclass (NSCalendarDate)
 extrabyte: byte;
end;
type
 TDerivedNSCharacterSet = objcclass (NSCharacterSet)
 extrabyte: byte;
end;
type
 TDerivedNSMutableCharacterSet = objcclass (NSMutableCharacterSet)
 extrabyte: byte;
end;
type
 TDerivedNSClassDescription = objcclass (NSClassDescription)
 extrabyte: byte;
end;
type
 TDerivedNSCoder = objcclass (NSCoder)
 extrabyte: byte;
end;
type
 TDerivedNSComparisonPredicate = objcclass (NSComparisonPredicate)
 extrabyte: byte;
end;
type
 TDerivedNSCompoundPredicate = objcclass (NSCompoundPredicate)
 extrabyte: byte;
end;
type
 TDerivedNSConnection = objcclass (NSConnection)
 extrabyte: byte;
end;
type
 TDerivedNSDistantObjectRequest = objcclass (NSDistantObjectRequest)
 extrabyte: byte;
end;
type
 TDerivedNSData = objcclass (NSData)
 extrabyte: byte;
end;
type
 TDerivedNSMutableData = objcclass (NSMutableData)
 extrabyte: byte;
end;
type
 TDerivedNSDate = objcclass (NSDate)
 extrabyte: byte;
end;
type
 TDerivedNSDateFormatter = objcclass (NSDateFormatter)
 extrabyte: byte;
end;
type
 TDerivedNSDecimalNumber = objcclass (NSDecimalNumber)
 extrabyte: byte;
end;
type
 TDerivedNSDecimalNumberHandler = objcclass (NSDecimalNumberHandler)
 extrabyte: byte;
end;
type
 TDerivedNSDictionary = objcclass (NSDictionary)
 extrabyte: byte;
end;
type
 TDerivedNSMutableDictionary = objcclass (NSMutableDictionary)
 extrabyte: byte;
end;
type
 TDerivedNSDistantObject = objcclass (NSDistantObject)
 extrabyte: byte;
end;
type
 TDerivedNSDistributedLock = objcclass (NSDistributedLock)
 extrabyte: byte;
end;
type
 TDerivedNSDistributedNotificationCenter = objcclass (NSDistributedNotificationCenter)
 extrabyte: byte;
end;
type
 TDerivedNSEnumerator = objcclass (NSEnumerator)
 extrabyte: byte;
end;
type
 TDerivedNSError = objcclass (NSError)
 extrabyte: byte;
end;
type
 TDerivedNSException = objcclass (NSException)
 extrabyte: byte;
end;
type
 TDerivedNSAssertionHandler = objcclass (NSAssertionHandler)
 extrabyte: byte;
end;
type
 TDerivedNSExpression = objcclass (NSExpression)
 extrabyte: byte;
end;
type
 TDerivedNSFileHandle = objcclass (NSFileHandle)
 extrabyte: byte;
end;
type
 TDerivedNSPipe = objcclass (NSPipe)
 extrabyte: byte;
end;
type
 TDerivedNSFileManager = objcclass (NSFileManager)
 extrabyte: byte;
end;
type
 TDerivedNSDirectoryEnumerator = objcclass (NSDirectoryEnumerator)
 extrabyte: byte;
end;
type
 TDerivedNSFormatter = objcclass (NSFormatter)
 extrabyte: byte;
end;
type
 TDerivedNSGarbageCollector = objcclass (NSGarbageCollector)
 extrabyte: byte;
end;
type
 TDerivedNSHashTable = objcclass (NSHashTable)
 extrabyte: byte;
end;
type
 TDerivedNSHost = objcclass (NSHost)
 extrabyte: byte;
end;
type
 TDerivedNSHTTPCookie = objcclass (NSHTTPCookie)
 extrabyte: byte;
end;
type
 TDerivedNSHTTPCookieStorage = objcclass (NSHTTPCookieStorage)
 extrabyte: byte;
end;
type
 TDerivedNSIndexPath = objcclass (NSIndexPath)
 extrabyte: byte;
end;
type
 TDerivedNSIndexSet = objcclass (NSIndexSet)
 extrabyte: byte;
end;
type
 TDerivedNSMutableIndexSet = objcclass (NSMutableIndexSet)
 extrabyte: byte;
end;
type
 TDerivedNSKeyedArchiver = objcclass (NSKeyedArchiver)
 extrabyte: byte;
end;
type
 TDerivedNSKeyedUnarchiver = objcclass (NSKeyedUnarchiver)
 extrabyte: byte;
end;
type
 TDerivedNSLocale = objcclass (NSLocale)
 extrabyte: byte;
end;
type
 TDerivedNSLock = objcclass (NSLock)
 extrabyte: byte;
end;
type
 TDerivedNSConditionLock = objcclass (NSConditionLock)
 extrabyte: byte;
end;
type
 TDerivedNSRecursiveLock = objcclass (NSRecursiveLock)
 extrabyte: byte;
end;
type
 TDerivedNSCondition = objcclass (NSCondition)
 extrabyte: byte;
end;
type
 TDerivedNSMapTable = objcclass (NSMapTable)
 extrabyte: byte;
end;
type
 TDerivedNSMetadataQuery = objcclass (NSMetadataQuery)
 extrabyte: byte;
end;
type
 TDerivedNSMetadataItem = objcclass (NSMetadataItem)
 extrabyte: byte;
end;
type
 TDerivedNSMetadataQueryAttributeValueTuple = objcclass (NSMetadataQueryAttributeValueTuple)
 extrabyte: byte;
end;
type
 TDerivedNSMetadataQueryResultGroup = objcclass (NSMetadataQueryResultGroup)
 extrabyte: byte;
end;
type
 TDerivedNSMethodSignature = objcclass (NSMethodSignature)
 extrabyte: byte;
end;
type
 TDerivedNSNetService = objcclass (NSNetService)
 extrabyte: byte;
end;
type
 TDerivedNSNetServiceBrowser = objcclass (NSNetServiceBrowser)
 extrabyte: byte;
end;
type
 TDerivedNSNotification = objcclass (NSNotification)
 extrabyte: byte;
end;
type
 TDerivedNSNotificationCenter = objcclass (NSNotificationCenter)
 extrabyte: byte;
end;
type
 TDerivedNSNotificationQueue = objcclass (NSNotificationQueue)
 extrabyte: byte;
end;
type
 TDerivedNSNull = objcclass (NSNull)
 extrabyte: byte;
end;
type
 TDerivedNSNumberFormatter = objcclass (NSNumberFormatter)
 extrabyte: byte;
end;
type
 TDerivedNSObject = objcclass (NSObject)
 extrabyte: byte;
end;
type
 TDerivedNSOperation = objcclass (NSOperation)
 extrabyte: byte;
end;
type
 TDerivedNSInvocationOperation = objcclass (NSInvocationOperation)
 extrabyte: byte;
end;
type
 TDerivedNSOperationQueue = objcclass (NSOperationQueue)
 extrabyte: byte;
end;
type
 TDerivedNSPointerArray = objcclass (NSPointerArray)
 extrabyte: byte;
end;
type
 TDerivedNSPort = objcclass (NSPort)
 extrabyte: byte;
end;
type
 TDerivedNSMachPort = objcclass (NSMachPort)
 extrabyte: byte;
end;
type
 TDerivedNSMessagePort = objcclass (NSMessagePort)
 extrabyte: byte;
end;
type
 TDerivedNSSocketPort = objcclass (NSSocketPort)
 extrabyte: byte;
end;
type
 TDerivedNSPortCoder = objcclass (NSPortCoder)
 extrabyte: byte;
end;
type
 TDerivedNSPortMessage = objcclass (NSPortMessage)
 extrabyte: byte;
end;
type
 TDerivedNSPortNameServer = objcclass (NSPortNameServer)
 extrabyte: byte;
end;
type
 TDerivedNSMachBootstrapServer = objcclass (NSMachBootstrapServer)
 extrabyte: byte;
end;
type
 TDerivedNSMessagePortNameServer = objcclass (NSMessagePortNameServer)
 extrabyte: byte;
end;
type
 TDerivedNSSocketPortNameServer = objcclass (NSSocketPortNameServer)
 extrabyte: byte;
end;
type
 TDerivedNSPredicate = objcclass (NSPredicate)
 extrabyte: byte;
end;
type
 TDerivedNSProcessInfo = objcclass (NSProcessInfo)
 extrabyte: byte;
end;
type
 TDerivedNSPropertyListSerialization = objcclass (NSPropertyListSerialization)
 extrabyte: byte;
end;
type
 TDerivedNSProtocolChecker = objcclass (NSProtocolChecker)
 extrabyte: byte;
end;
type
 TDerivedNSProxy = objcclass (NSProxy)
 extrabyte: byte;
end;
type
 TDerivedNSRunLoop = objcclass (NSRunLoop)
 extrabyte: byte;
end;
type
 TDerivedNSScanner = objcclass (NSScanner)
 extrabyte: byte;
end;
type
 TDerivedNSScriptClassDescription = objcclass (NSScriptClassDescription)
 extrabyte: byte;
end;
type
 TDerivedNSScriptCoercionHandler = objcclass (NSScriptCoercionHandler)
 extrabyte: byte;
end;
type
 TDerivedNSScriptCommand = objcclass (NSScriptCommand)
 extrabyte: byte;
end;
type
 TDerivedNSScriptCommandDescription = objcclass (NSScriptCommandDescription)
 extrabyte: byte;
end;
type
 TDerivedNSScriptExecutionContext = objcclass (NSScriptExecutionContext)
 extrabyte: byte;
end;
type
 TDerivedNSScriptObjectSpecifier = objcclass (NSScriptObjectSpecifier)
 extrabyte: byte;
end;
type
 TDerivedNSIndexSpecifier = objcclass (NSIndexSpecifier)
 extrabyte: byte;
end;
type
 TDerivedNSMiddleSpecifier = objcclass (NSMiddleSpecifier)
 extrabyte: byte;
end;
type
 TDerivedNSNameSpecifier = objcclass (NSNameSpecifier)
 extrabyte: byte;
end;
type
 TDerivedNSPositionalSpecifier = objcclass (NSPositionalSpecifier)
 extrabyte: byte;
end;
type
 TDerivedNSPropertySpecifier = objcclass (NSPropertySpecifier)
 extrabyte: byte;
end;
type
 TDerivedNSRandomSpecifier = objcclass (NSRandomSpecifier)
 extrabyte: byte;
end;
type
 TDerivedNSRangeSpecifier = objcclass (NSRangeSpecifier)
 extrabyte: byte;
end;
type
 TDerivedNSRelativeSpecifier = objcclass (NSRelativeSpecifier)
 extrabyte: byte;
end;
type
 TDerivedNSUniqueIDSpecifier = objcclass (NSUniqueIDSpecifier)
 extrabyte: byte;
end;
type
 TDerivedNSWhoseSpecifier = objcclass (NSWhoseSpecifier)
 extrabyte: byte;
end;
type
 TDerivedNSCloneCommand = objcclass (NSCloneCommand)
 extrabyte: byte;
end;
type
 TDerivedNSCloseCommand = objcclass (NSCloseCommand)
 extrabyte: byte;
end;
type
 TDerivedNSCountCommand = objcclass (NSCountCommand)
 extrabyte: byte;
end;
type
 TDerivedNSCreateCommand = objcclass (NSCreateCommand)
 extrabyte: byte;
end;
type
 TDerivedNSDeleteCommand = objcclass (NSDeleteCommand)
 extrabyte: byte;
end;
type
 TDerivedNSExistsCommand = objcclass (NSExistsCommand)
 extrabyte: byte;
end;
type
 TDerivedNSGetCommand = objcclass (NSGetCommand)
 extrabyte: byte;
end;
type
 TDerivedNSMoveCommand = objcclass (NSMoveCommand)
 extrabyte: byte;
end;
type
 TDerivedNSQuitCommand = objcclass (NSQuitCommand)
 extrabyte: byte;
end;
type
 TDerivedNSSetCommand = objcclass (NSSetCommand)
 extrabyte: byte;
end;
type
 TDerivedNSScriptSuiteRegistry = objcclass (NSScriptSuiteRegistry)
 extrabyte: byte;
end;
type
 TDerivedNSScriptWhoseTest = objcclass (NSScriptWhoseTest)
 extrabyte: byte;
end;
type
 TDerivedNSLogicalTest = objcclass (NSLogicalTest)
 extrabyte: byte;
end;
type
 TDerivedNSSpecifierTest = objcclass (NSSpecifierTest)
 extrabyte: byte;
end;
type
 TDerivedNSSet = objcclass (NSSet)
 extrabyte: byte;
end;
type
 TDerivedNSMutableSet = objcclass (NSMutableSet)
 extrabyte: byte;
end;
type
 TDerivedNSCountedSet = objcclass (NSCountedSet)
 extrabyte: byte;
end;
type
 TDerivedNSSortDescriptor = objcclass (NSSortDescriptor)
 extrabyte: byte;
end;
type
 TDerivedNSSpellServer = objcclass (NSSpellServer)
 extrabyte: byte;
end;
type
 TDerivedNSStream = objcclass (NSStream)
 extrabyte: byte;
end;
type
 TDerivedNSInputStream = objcclass (NSInputStream)
 extrabyte: byte;
end;
type
 TDerivedNSOutputStream = objcclass (NSOutputStream)
 extrabyte: byte;
end;
type
 TDerivedNSString = objcclass (NSString)
 extrabyte: byte;
end;
type
 TDerivedNSMutableString = objcclass (NSMutableString)
 extrabyte: byte;
end;
type
 TDerivedNSSimpleCString = objcclass (NSSimpleCString)
 extrabyte: byte;
end;
type
 TDerivedNSTask = objcclass (NSTask)
 extrabyte: byte;
end;
type
 TDerivedNSThread = objcclass (NSThread)
 extrabyte: byte;
end;
type
 TDerivedNSTimer = objcclass (NSTimer)
 extrabyte: byte;
end;
type
 TDerivedNSTimeZone = objcclass (NSTimeZone)
 extrabyte: byte;
end;
type
 TDerivedNSUndoManager = objcclass (NSUndoManager)
 extrabyte: byte;
end;
type
 TDerivedNSURL = objcclass (NSURL)
 extrabyte: byte;
end;
type
 TDerivedNSURLAuthenticationChallenge = objcclass (NSURLAuthenticationChallenge)
 extrabyte: byte;
end;
type
 TDerivedNSCachedURLResponse = objcclass (NSCachedURLResponse)
 extrabyte: byte;
end;
type
 TDerivedNSURLCache = objcclass (NSURLCache)
 extrabyte: byte;
end;
type
 TDerivedNSURLConnection = objcclass (NSURLConnection)
 extrabyte: byte;
end;
type
 TDerivedNSURLCredential = objcclass (NSURLCredential)
 extrabyte: byte;
end;
type
 TDerivedNSURLCredentialStorage = objcclass (NSURLCredentialStorage)
 extrabyte: byte;
end;
type
 TDerivedNSURLDownload = objcclass (NSURLDownload)
 extrabyte: byte;
end;
type
 TDerivedNSURLHandle = objcclass (NSURLHandle)
 extrabyte: byte;
end;
type
 TDerivedNSURLProtectionSpace = objcclass (NSURLProtectionSpace)
 extrabyte: byte;
end;
type
 TDerivedNSURLProtocol = objcclass (NSURLProtocol)
 extrabyte: byte;
end;
type
 TDerivedNSURLRequest = objcclass (NSURLRequest)
 extrabyte: byte;
end;
type
 TDerivedNSMutableURLRequest = objcclass (NSMutableURLRequest)
 extrabyte: byte;
end;
type
 TDerivedNSURLResponse = objcclass (NSURLResponse)
 extrabyte: byte;
end;
type
 TDerivedNSHTTPURLResponse = objcclass (NSHTTPURLResponse)
 extrabyte: byte;
end;
type
 TDerivedNSUserDefaults = objcclass (NSUserDefaults)
 extrabyte: byte;
end;
type
 TDerivedNSValue = objcclass (NSValue)
 extrabyte: byte;
end;
type
 TDerivedNSNumber = objcclass (NSNumber)
 extrabyte: byte;
end;
type
 TDerivedNSValueTransformer = objcclass (NSValueTransformer)
 extrabyte: byte;
end;
type
 TDerivedNSXMLDocument = objcclass (NSXMLDocument)
 extrabyte: byte;
end;
type
 TDerivedNSXMLDTD = objcclass (NSXMLDTD)
 extrabyte: byte;
end;
type
 TDerivedNSXMLDTDNode = objcclass (NSXMLDTDNode)
 extrabyte: byte;
end;
type
 TDerivedNSXMLElement = objcclass (NSXMLElement)
 extrabyte: byte;
end;
type
 TDerivedNSXMLNode = objcclass (NSXMLNode)
 extrabyte: byte;
end;
type
 TDerivedNSXMLParser = objcclass (NSXMLParser)
 extrabyte: byte;
end;
type
 TDerivedNSActionCell = objcclass (NSActionCell)
 extrabyte: byte;
end;
type
 TDerivedNSAlert = objcclass (NSAlert)
 extrabyte: byte;
end;
type
 TDerivedNSAnimation = objcclass (NSAnimation)
 extrabyte: byte;
end;
type
 TDerivedNSViewAnimation = objcclass (NSViewAnimation)
 extrabyte: byte;
end;
type
 TDerivedNSAnimationContext = objcclass (NSAnimationContext)
 extrabyte: byte;
end;
type
 TDerivedNSApplication = objcclass (NSApplication)
 extrabyte: byte;
end;
type
 TDerivedNSArrayController = objcclass (NSArrayController)
 extrabyte: byte;
end;
type
 TDerivedNSATSTypesetter = objcclass (NSATSTypesetter)
 extrabyte: byte;
end;
type
 TDerivedNSBezierPath = objcclass (NSBezierPath)
 extrabyte: byte;
end;
type
 TDerivedNSBitmapImageRep = objcclass (NSBitmapImageRep)
 extrabyte: byte;
end;
type
 TDerivedNSBox = objcclass (NSBox)
 extrabyte: byte;
end;
type
 TDerivedNSBrowser = objcclass (NSBrowser)
 extrabyte: byte;
end;
type
 TDerivedNSBrowserCell = objcclass (NSBrowserCell)
 extrabyte: byte;
end;
type
 TDerivedNSButton = objcclass (NSButton)
 extrabyte: byte;
end;
type
 TDerivedNSButtonCell = objcclass (NSButtonCell)
 extrabyte: byte;
end;
type
 TDerivedNSCachedImageRep = objcclass (NSCachedImageRep)
 extrabyte: byte;
end;
type
 TDerivedNSCell = objcclass (NSCell)
 extrabyte: byte;
end;
type
 TDerivedNSCIImageRep = objcclass (NSCIImageRep)
 extrabyte: byte;
end;
type
 TDerivedNSClipView = objcclass (NSClipView)
 extrabyte: byte;
end;
type
 TDerivedNSCollectionViewItem = objcclass (NSCollectionViewItem)
 extrabyte: byte;
end;
type
 TDerivedNSCollectionView = objcclass (NSCollectionView)
 extrabyte: byte;
end;
type
 TDerivedNSColor = objcclass (NSColor)
 extrabyte: byte;
end;
type
 TDerivedNSColorList = objcclass (NSColorList)
 extrabyte: byte;
end;
type
 TDerivedNSColorPanel = objcclass (NSColorPanel)
 extrabyte: byte;
end;
type
 TDerivedNSColorPicker = objcclass (NSColorPicker)
 extrabyte: byte;
end;
type
 TDerivedNSColorSpace = objcclass (NSColorSpace)
 extrabyte: byte;
end;
type
 TDerivedNSColorWell = objcclass (NSColorWell)
 extrabyte: byte;
end;
type
 TDerivedNSComboBox = objcclass (NSComboBox)
 extrabyte: byte;
end;
type
 TDerivedNSComboBoxCell = objcclass (NSComboBoxCell)
 extrabyte: byte;
end;
type
 TDerivedNSControl = objcclass (NSControl)
 extrabyte: byte;
end;
type
 TDerivedNSController = objcclass (NSController)
 extrabyte: byte;
end;
type
 TDerivedNSCursor = objcclass (NSCursor)
 extrabyte: byte;
end;
type
 TDerivedNSCustomImageRep = objcclass (NSCustomImageRep)
 extrabyte: byte;
end;
type
 TDerivedNSDatePicker = objcclass (NSDatePicker)
 extrabyte: byte;
end;
type
 TDerivedNSDatePickerCell = objcclass (NSDatePickerCell)
 extrabyte: byte;
end;
type
 TDerivedNSDictionaryController = objcclass (NSDictionaryController)
 extrabyte: byte;
end;
type
 TDerivedNSDockTile = objcclass (NSDockTile)
 extrabyte: byte;
end;
type
 TDerivedNSDocument = objcclass (NSDocument)
 extrabyte: byte;
end;
type
 TDerivedNSDocumentController = objcclass (NSDocumentController)
 extrabyte: byte;
end;
type
 TDerivedNSDrawer = objcclass (NSDrawer)
 extrabyte: byte;
end;
type
 TDerivedNSEPSImageRep = objcclass (NSEPSImageRep)
 extrabyte: byte;
end;
type
 TDerivedNSEvent = objcclass (NSEvent)
 extrabyte: byte;
end;
type
 TDerivedNSFileWrapper = objcclass (NSFileWrapper)
 extrabyte: byte;
end;
type
 TDerivedNSFont = objcclass (NSFont)
 extrabyte: byte;
end;
type
 TDerivedNSFontDescriptor = objcclass (NSFontDescriptor)
 extrabyte: byte;
end;
type
 TDerivedNSFontManager = objcclass (NSFontManager)
 extrabyte: byte;
end;
type
 TDerivedNSFontPanel = objcclass (NSFontPanel)
 extrabyte: byte;
end;
type
 TDerivedNSFormCell = objcclass (NSFormCell)
 extrabyte: byte;
end;
type
 TDerivedNSGlyphGenerator = objcclass (NSGlyphGenerator)
 extrabyte: byte;
end;
type
 TDerivedNSGlyphInfo = objcclass (NSGlyphInfo)
 extrabyte: byte;
end;
type
 TDerivedNSGradient = objcclass (NSGradient)
 extrabyte: byte;
end;
type
 TDerivedNSGraphicsContext = objcclass (NSGraphicsContext)
 extrabyte: byte;
end;
type
 TDerivedNSHelpManager = objcclass (NSHelpManager)
 extrabyte: byte;
end;
type
 TDerivedNSImage = objcclass (NSImage)
 extrabyte: byte;
end;
type
 TDerivedNSImageCell = objcclass (NSImageCell)
 extrabyte: byte;
end;
type
 TDerivedNSImageRep = objcclass (NSImageRep)
 extrabyte: byte;
end;
type
 TDerivedNSImageView = objcclass (NSImageView)
 extrabyte: byte;
end;
type
 TDerivedNSInputManager = objcclass (NSInputManager)
 extrabyte: byte;
end;
type
 TDerivedNSInputServer = objcclass (NSInputServer)
 extrabyte: byte;
end;
type
 TDerivedNSLayoutManager = objcclass (NSLayoutManager)
 extrabyte: byte;
end;
type
 TDerivedNSLevelIndicator = objcclass (NSLevelIndicator)
 extrabyte: byte;
end;
type
 TDerivedNSLevelIndicatorCell = objcclass (NSLevelIndicatorCell)
 extrabyte: byte;
end;
type
 TDerivedNSMatrix = objcclass (NSMatrix)
 extrabyte: byte;
end;
type
 TDerivedNSMenu = objcclass (NSMenu)
 extrabyte: byte;
end;
type
 TDerivedNSMenuItem = objcclass (NSMenuItem)
 extrabyte: byte;
end;
type
 TDerivedNSMenuItemCell = objcclass (NSMenuItemCell)
 extrabyte: byte;
end;
type
 TDerivedNSMenuView = objcclass (NSMenuView)
 extrabyte: byte;
end;
type
 TDerivedNSMovie = objcclass (NSMovie)
 extrabyte: byte;
end;
type
 TDerivedNSMovieView = objcclass (NSMovieView)
 extrabyte: byte;
end;
type
 TDerivedNSNib = objcclass (NSNib)
 extrabyte: byte;
end;
type
 TDerivedNSObjectController = objcclass (NSObjectController)
 extrabyte: byte;
end;
type
 TDerivedNSOpenGLPixelFormat = objcclass (NSOpenGLPixelFormat)
 extrabyte: byte;
end;
type
 TDerivedNSOpenGLPixelBuffer = objcclass (NSOpenGLPixelBuffer)
 extrabyte: byte;
end;
type
 TDerivedNSOpenGLContext = objcclass (NSOpenGLContext)
 extrabyte: byte;
end;
type
 TDerivedNSOpenGLView = objcclass (NSOpenGLView)
 extrabyte: byte;
end;
type
 TDerivedNSOpenPanel = objcclass (NSOpenPanel)
 extrabyte: byte;
end;
type
 TDerivedNSOutlineView = objcclass (NSOutlineView)
 extrabyte: byte;
end;
type
 TDerivedNSPageLayout = objcclass (NSPageLayout)
 extrabyte: byte;
end;
type
 TDerivedNSPanel = objcclass (NSPanel)
 extrabyte: byte;
end;
type
 TDerivedNSTextTab = objcclass (NSTextTab)
 extrabyte: byte;
end;
type
 TDerivedNSParagraphStyle = objcclass (NSParagraphStyle)
 extrabyte: byte;
end;
type
 TDerivedNSMutableParagraphStyle = objcclass (NSMutableParagraphStyle)
 extrabyte: byte;
end;
type
 TDerivedNSPasteboard = objcclass (NSPasteboard)
 extrabyte: byte;
end;
type
 TDerivedNSPathCell = objcclass (NSPathCell)
 extrabyte: byte;
end;
type
 TDerivedNSPathComponentCell = objcclass (NSPathComponentCell)
 extrabyte: byte;
end;
type
 TDerivedNSPathControl = objcclass (NSPathControl)
 extrabyte: byte;
end;
type
 TDerivedNSPDFImageRep = objcclass (NSPDFImageRep)
 extrabyte: byte;
end;
type
 TDerivedNSPersistentDocument = objcclass (NSPersistentDocument)
 extrabyte: byte;
end;
type
 TDerivedNSPICTImageRep = objcclass (NSPICTImageRep)
 extrabyte: byte;
end;
type
 TDerivedNSPopUpButton = objcclass (NSPopUpButton)
 extrabyte: byte;
end;
type
 TDerivedNSPopUpButtonCell = objcclass (NSPopUpButtonCell)
 extrabyte: byte;
end;
type
 TDerivedNSPredicateEditor = objcclass (NSPredicateEditor)
 extrabyte: byte;
end;
type
 TDerivedNSPrinter = objcclass (NSPrinter)
 extrabyte: byte;
end;
type
 TDerivedNSPrintInfo = objcclass (NSPrintInfo)
 extrabyte: byte;
end;
type
 TDerivedNSPrintOperation = objcclass (NSPrintOperation)
 extrabyte: byte;
end;
type
 TDerivedNSPrintPanel = objcclass (NSPrintPanel)
 extrabyte: byte;
end;
type
 TDerivedNSProgressIndicator = objcclass (NSProgressIndicator)
 extrabyte: byte;
end;
type
 TDerivedNSQuickDrawView = objcclass (NSQuickDrawView)
 extrabyte: byte;
end;
type
 TDerivedNSResponder = objcclass (NSResponder)
 extrabyte: byte;
end;
type
 TDerivedNSRuleEditor = objcclass (NSRuleEditor)
 extrabyte: byte;
end;
type
 TDerivedNSRulerMarker = objcclass (NSRulerMarker)
 extrabyte: byte;
end;
type
 TDerivedNSRulerView = objcclass (NSRulerView)
 extrabyte: byte;
end;
type
 TDerivedNSSavePanel = objcclass (NSSavePanel)
 extrabyte: byte;
end;
type
 TDerivedNSScreen = objcclass (NSScreen)
 extrabyte: byte;
end;
type
 TDerivedNSScroller = objcclass (NSScroller)
 extrabyte: byte;
end;
type
 TDerivedNSScrollView = objcclass (NSScrollView)
 extrabyte: byte;
end;
type
 TDerivedNSSearchField = objcclass (NSSearchField)
 extrabyte: byte;
end;
type
 TDerivedNSSearchFieldCell = objcclass (NSSearchFieldCell)
 extrabyte: byte;
end;
type
 TDerivedNSSecureTextField = objcclass (NSSecureTextField)
 extrabyte: byte;
end;
type
 TDerivedNSSecureTextFieldCell = objcclass (NSSecureTextFieldCell)
 extrabyte: byte;
end;
type
 TDerivedNSSegmentedControl = objcclass (NSSegmentedControl)
 extrabyte: byte;
end;
type
 TDerivedNSShadow = objcclass (NSShadow)
 extrabyte: byte;
end;
type
 TDerivedNSSlider = objcclass (NSSlider)
 extrabyte: byte;
end;
type
 TDerivedNSSliderCell = objcclass (NSSliderCell)
 extrabyte: byte;
end;
type
 TDerivedNSSound = objcclass (NSSound)
 extrabyte: byte;
end;
type
 TDerivedNSSpeechRecognizer = objcclass (NSSpeechRecognizer)
 extrabyte: byte;
end;
type
 TDerivedNSSpeechSynthesizer = objcclass (NSSpeechSynthesizer)
 extrabyte: byte;
end;
type
 TDerivedNSSpellChecker = objcclass (NSSpellChecker)
 extrabyte: byte;
end;
type
 TDerivedNSSplitView = objcclass (NSSplitView)
 extrabyte: byte;
end;
type
 TDerivedNSStatusBar = objcclass (NSStatusBar)
 extrabyte: byte;
end;
type
 TDerivedNSStatusItem = objcclass (NSStatusItem)
 extrabyte: byte;
end;
type
 TDerivedNSStepper = objcclass (NSStepper)
 extrabyte: byte;
end;
type
 TDerivedNSStepperCell = objcclass (NSStepperCell)
 extrabyte: byte;
end;
type
 TDerivedNSTableColumn = objcclass (NSTableColumn)
 extrabyte: byte;
end;
type
 TDerivedNSTableHeaderCell = objcclass (NSTableHeaderCell)
 extrabyte: byte;
end;
type
 TDerivedNSTableHeaderView = objcclass (NSTableHeaderView)
 extrabyte: byte;
end;
type
 TDerivedNSTableView = objcclass (NSTableView)
 extrabyte: byte;
end;
type
 TDerivedNSTabView = objcclass (NSTabView)
 extrabyte: byte;
end;
type
 TDerivedNSTabViewItem = objcclass (NSTabViewItem)
 extrabyte: byte;
end;
type
 TDerivedNSText = objcclass (NSText)
 extrabyte: byte;
end;
type
 TDerivedNSTextAttachmentCell = objcclass (NSTextAttachmentCell)
 extrabyte: byte;
end;
type
 TDerivedNSTextAttachment = objcclass (NSTextAttachment)
 extrabyte: byte;
end;
type
 TDerivedNSTextContainer = objcclass (NSTextContainer)
 extrabyte: byte;
end;
type
 TDerivedNSTextField = objcclass (NSTextField)
 extrabyte: byte;
end;
type
 TDerivedNSTextFieldCell = objcclass (NSTextFieldCell)
 extrabyte: byte;
end;
type
 TDerivedNSTextList = objcclass (NSTextList)
 extrabyte: byte;
end;
type
 TDerivedNSTextStorage = objcclass (NSTextStorage)
 extrabyte: byte;
end;
type
 TDerivedNSTextBlock = objcclass (NSTextBlock)
 extrabyte: byte;
end;
type
 TDerivedNSTextTableBlock = objcclass (NSTextTableBlock)
 extrabyte: byte;
end;
type
 TDerivedNSTextTable = objcclass (NSTextTable)
 extrabyte: byte;
end;
type
 TDerivedNSTextView = objcclass (NSTextView)
 extrabyte: byte;
end;
type
 TDerivedNSTokenField = objcclass (NSTokenField)
 extrabyte: byte;
end;
type
 TDerivedNSTokenFieldCell = objcclass (NSTokenFieldCell)
 extrabyte: byte;
end;
type
 TDerivedNSToolbar = objcclass (NSToolbar)
 extrabyte: byte;
end;
type
 TDerivedNSToolbarItem = objcclass (NSToolbarItem)
 extrabyte: byte;
end;
type
 TDerivedNSToolbarItemGroup = objcclass (NSToolbarItemGroup)
 extrabyte: byte;
end;
type
 TDerivedNSTrackingArea = objcclass (NSTrackingArea)
 extrabyte: byte;
end;
type
 TDerivedNSTreeController = objcclass (NSTreeController)
 extrabyte: byte;
end;
type
 TDerivedNSTreeNode = objcclass (NSTreeNode)
 extrabyte: byte;
end;
type
 TDerivedNSTypesetter = objcclass (NSTypesetter)
 extrabyte: byte;
end;
type
 TDerivedNSUserDefaultsController = objcclass (NSUserDefaultsController)
 extrabyte: byte;
end;
type
 TDerivedNSView = objcclass (NSView)
 extrabyte: byte;
end;
type
 TDerivedNSViewController = objcclass (NSViewController)
 extrabyte: byte;
end;
type
 TDerivedNSWindow = objcclass (NSWindow)
 extrabyte: byte;
end;
type
 TDerivedNSWindowController = objcclass (NSWindowController)
 extrabyte: byte;
end;
type
 TDerivedNSWorkspace = objcclass (NSWorkspace)
 extrabyte: byte;
end;

procedure PrintGlue1;
begin
 if class_getInstanceSize(TDerivedNSAffineTransform) <> (class_getInstanceSize(NSAffineTransform)+1) then
 writeln('size of NSAffineTransform is wrong: ',class_getInstanceSize(TDerivedNSAffineTransform),' <> ',class_getInstanceSize(NSAffineTransform)+1);
 if class_getInstanceSize(TDerivedNSAppleEventDescriptor) <> (class_getInstanceSize(NSAppleEventDescriptor)+1) then
 writeln('size of NSAppleEventDescriptor is wrong: ',class_getInstanceSize(TDerivedNSAppleEventDescriptor),' <> ',class_getInstanceSize(NSAppleEventDescriptor)+1);
 if class_getInstanceSize(TDerivedNSAppleEventManager) <> (class_getInstanceSize(NSAppleEventManager)+1) then
 writeln('size of NSAppleEventManager is wrong: ',class_getInstanceSize(TDerivedNSAppleEventManager),' <> ',class_getInstanceSize(NSAppleEventManager)+1);
 if class_getInstanceSize(TDerivedNSAppleScript) <> (class_getInstanceSize(NSAppleScript)+1) then
 writeln('size of NSAppleScript is wrong: ',class_getInstanceSize(TDerivedNSAppleScript),' <> ',class_getInstanceSize(NSAppleScript)+1);
 if class_getInstanceSize(TDerivedNSArchiver) <> (class_getInstanceSize(NSArchiver)+1) then
 writeln('size of NSArchiver is wrong: ',class_getInstanceSize(TDerivedNSArchiver),' <> ',class_getInstanceSize(NSArchiver)+1);
 if class_getInstanceSize(TDerivedNSUnarchiver) <> (class_getInstanceSize(NSUnarchiver)+1) then
 writeln('size of NSUnarchiver is wrong: ',class_getInstanceSize(TDerivedNSUnarchiver),' <> ',class_getInstanceSize(NSUnarchiver)+1);
 if class_getInstanceSize(TDerivedNSArray) <> (class_getInstanceSize(NSArray)+1) then
 writeln('size of NSArray is wrong: ',class_getInstanceSize(TDerivedNSArray),' <> ',class_getInstanceSize(NSArray)+1);
 if class_getInstanceSize(TDerivedNSMutableArray) <> (class_getInstanceSize(NSMutableArray)+1) then
 writeln('size of NSMutableArray is wrong: ',class_getInstanceSize(TDerivedNSMutableArray),' <> ',class_getInstanceSize(NSMutableArray)+1);
 if class_getInstanceSize(TDerivedNSAttributedString) <> (class_getInstanceSize(NSAttributedString)+1) then
 writeln('size of NSAttributedString is wrong: ',class_getInstanceSize(TDerivedNSAttributedString),' <> ',class_getInstanceSize(NSAttributedString)+1);
 if class_getInstanceSize(TDerivedNSMutableAttributedString) <> (class_getInstanceSize(NSMutableAttributedString)+1) then
 writeln('size of NSMutableAttributedString is wrong: ',class_getInstanceSize(TDerivedNSMutableAttributedString),' <> ',class_getInstanceSize(NSMutableAttributedString)+1);
 if class_getInstanceSize(TDerivedNSAutoreleasePool) <> (class_getInstanceSize(NSAutoreleasePool)+1) then
 writeln('size of NSAutoreleasePool is wrong: ',class_getInstanceSize(TDerivedNSAutoreleasePool),' <> ',class_getInstanceSize(NSAutoreleasePool)+1);
 if class_getInstanceSize(TDerivedNSBundle) <> (class_getInstanceSize(NSBundle)+1) then
 writeln('size of NSBundle is wrong: ',class_getInstanceSize(TDerivedNSBundle),' <> ',class_getInstanceSize(NSBundle)+1);
 if class_getInstanceSize(TDerivedNSCalendar) <> (class_getInstanceSize(NSCalendar)+1) then
 writeln('size of NSCalendar is wrong: ',class_getInstanceSize(TDerivedNSCalendar),' <> ',class_getInstanceSize(NSCalendar)+1);
 if class_getInstanceSize(TDerivedNSDateComponents) <> (class_getInstanceSize(NSDateComponents)+1) then
 writeln('size of NSDateComponents is wrong: ',class_getInstanceSize(TDerivedNSDateComponents),' <> ',class_getInstanceSize(NSDateComponents)+1);
 if class_getInstanceSize(TDerivedNSCalendarDate) <> (class_getInstanceSize(NSCalendarDate)+1) then
 writeln('size of NSCalendarDate is wrong: ',class_getInstanceSize(TDerivedNSCalendarDate),' <> ',class_getInstanceSize(NSCalendarDate)+1);
 if class_getInstanceSize(TDerivedNSCharacterSet) <> (class_getInstanceSize(NSCharacterSet)+1) then
 writeln('size of NSCharacterSet is wrong: ',class_getInstanceSize(TDerivedNSCharacterSet),' <> ',class_getInstanceSize(NSCharacterSet)+1);
 if class_getInstanceSize(TDerivedNSMutableCharacterSet) <> (class_getInstanceSize(NSMutableCharacterSet)+1) then
 writeln('size of NSMutableCharacterSet is wrong: ',class_getInstanceSize(TDerivedNSMutableCharacterSet),' <> ',class_getInstanceSize(NSMutableCharacterSet)+1);
 if class_getInstanceSize(TDerivedNSClassDescription) <> (class_getInstanceSize(NSClassDescription)+1) then
 writeln('size of NSClassDescription is wrong: ',class_getInstanceSize(TDerivedNSClassDescription),' <> ',class_getInstanceSize(NSClassDescription)+1);
 if class_getInstanceSize(TDerivedNSCoder) <> (class_getInstanceSize(NSCoder)+1) then
 writeln('size of NSCoder is wrong: ',class_getInstanceSize(TDerivedNSCoder),' <> ',class_getInstanceSize(NSCoder)+1);
 if class_getInstanceSize(TDerivedNSComparisonPredicate) <> (class_getInstanceSize(NSComparisonPredicate)+1) then
 writeln('size of NSComparisonPredicate is wrong: ',class_getInstanceSize(TDerivedNSComparisonPredicate),' <> ',class_getInstanceSize(NSComparisonPredicate)+1);
 if class_getInstanceSize(TDerivedNSCompoundPredicate) <> (class_getInstanceSize(NSCompoundPredicate)+1) then
 writeln('size of NSCompoundPredicate is wrong: ',class_getInstanceSize(TDerivedNSCompoundPredicate),' <> ',class_getInstanceSize(NSCompoundPredicate)+1);
 if class_getInstanceSize(TDerivedNSConnection) <> (class_getInstanceSize(NSConnection)+1) then
 writeln('size of NSConnection is wrong: ',class_getInstanceSize(TDerivedNSConnection),' <> ',class_getInstanceSize(NSConnection)+1);
 if class_getInstanceSize(TDerivedNSDistantObjectRequest) <> (class_getInstanceSize(NSDistantObjectRequest)+1) then
 writeln('size of NSDistantObjectRequest is wrong: ',class_getInstanceSize(TDerivedNSDistantObjectRequest),' <> ',class_getInstanceSize(NSDistantObjectRequest)+1);
 if class_getInstanceSize(TDerivedNSData) <> (class_getInstanceSize(NSData)+1) then
 writeln('size of NSData is wrong: ',class_getInstanceSize(TDerivedNSData),' <> ',class_getInstanceSize(NSData)+1);
 if class_getInstanceSize(TDerivedNSMutableData) <> (class_getInstanceSize(NSMutableData)+1) then
 writeln('size of NSMutableData is wrong: ',class_getInstanceSize(TDerivedNSMutableData),' <> ',class_getInstanceSize(NSMutableData)+1);
 if class_getInstanceSize(TDerivedNSDate) <> (class_getInstanceSize(NSDate)+1) then
 writeln('size of NSDate is wrong: ',class_getInstanceSize(TDerivedNSDate),' <> ',class_getInstanceSize(NSDate)+1);
 if class_getInstanceSize(TDerivedNSDateFormatter) <> (class_getInstanceSize(NSDateFormatter)+1) then
 writeln('size of NSDateFormatter is wrong: ',class_getInstanceSize(TDerivedNSDateFormatter),' <> ',class_getInstanceSize(NSDateFormatter)+1);
 if class_getInstanceSize(TDerivedNSDecimalNumber) <> (class_getInstanceSize(NSDecimalNumber)+1) then
 writeln('size of NSDecimalNumber is wrong: ',class_getInstanceSize(TDerivedNSDecimalNumber),' <> ',class_getInstanceSize(NSDecimalNumber)+1);
 if class_getInstanceSize(TDerivedNSDecimalNumberHandler) <> (class_getInstanceSize(NSDecimalNumberHandler)+1) then
 writeln('size of NSDecimalNumberHandler is wrong: ',class_getInstanceSize(TDerivedNSDecimalNumberHandler),' <> ',class_getInstanceSize(NSDecimalNumberHandler)+1);
 if class_getInstanceSize(TDerivedNSDictionary) <> (class_getInstanceSize(NSDictionary)+1) then
 writeln('size of NSDictionary is wrong: ',class_getInstanceSize(TDerivedNSDictionary),' <> ',class_getInstanceSize(NSDictionary)+1);
 if class_getInstanceSize(TDerivedNSMutableDictionary) <> (class_getInstanceSize(NSMutableDictionary)+1) then
 writeln('size of NSMutableDictionary is wrong: ',class_getInstanceSize(TDerivedNSMutableDictionary),' <> ',class_getInstanceSize(NSMutableDictionary)+1);
 if class_getInstanceSize(TDerivedNSDistantObject) <> (class_getInstanceSize(NSDistantObject)+1) then
 writeln('size of NSDistantObject is wrong: ',class_getInstanceSize(TDerivedNSDistantObject),' <> ',class_getInstanceSize(NSDistantObject)+1);
 if class_getInstanceSize(TDerivedNSDistributedLock) <> (class_getInstanceSize(NSDistributedLock)+1) then
 writeln('size of NSDistributedLock is wrong: ',class_getInstanceSize(TDerivedNSDistributedLock),' <> ',class_getInstanceSize(NSDistributedLock)+1);
 if class_getInstanceSize(TDerivedNSDistributedNotificationCenter) <> (class_getInstanceSize(NSDistributedNotificationCenter)+1) then
 writeln('size of NSDistributedNotificationCenter is wrong: ',class_getInstanceSize(TDerivedNSDistributedNotificationCenter),' <> ',class_getInstanceSize(NSDistributedNotificationCenter)+1);
 if class_getInstanceSize(TDerivedNSEnumerator) <> (class_getInstanceSize(NSEnumerator)+1) then
 writeln('size of NSEnumerator is wrong: ',class_getInstanceSize(TDerivedNSEnumerator),' <> ',class_getInstanceSize(NSEnumerator)+1);
 if class_getInstanceSize(TDerivedNSError) <> (class_getInstanceSize(NSError)+1) then
 writeln('size of NSError is wrong: ',class_getInstanceSize(TDerivedNSError),' <> ',class_getInstanceSize(NSError)+1);
 if class_getInstanceSize(TDerivedNSException) <> (class_getInstanceSize(NSException)+1) then
 writeln('size of NSException is wrong: ',class_getInstanceSize(TDerivedNSException),' <> ',class_getInstanceSize(NSException)+1);
 if class_getInstanceSize(TDerivedNSAssertionHandler) <> (class_getInstanceSize(NSAssertionHandler)+1) then
 writeln('size of NSAssertionHandler is wrong: ',class_getInstanceSize(TDerivedNSAssertionHandler),' <> ',class_getInstanceSize(NSAssertionHandler)+1);
 if class_getInstanceSize(TDerivedNSExpression) <> (class_getInstanceSize(NSExpression)+1) then
 writeln('size of NSExpression is wrong: ',class_getInstanceSize(TDerivedNSExpression),' <> ',class_getInstanceSize(NSExpression)+1);
 if class_getInstanceSize(TDerivedNSFileHandle) <> (class_getInstanceSize(NSFileHandle)+1) then
 writeln('size of NSFileHandle is wrong: ',class_getInstanceSize(TDerivedNSFileHandle),' <> ',class_getInstanceSize(NSFileHandle)+1);
 if class_getInstanceSize(TDerivedNSPipe) <> (class_getInstanceSize(NSPipe)+1) then
 writeln('size of NSPipe is wrong: ',class_getInstanceSize(TDerivedNSPipe),' <> ',class_getInstanceSize(NSPipe)+1);
 if class_getInstanceSize(TDerivedNSFileManager) <> (class_getInstanceSize(NSFileManager)+1) then
 writeln('size of NSFileManager is wrong: ',class_getInstanceSize(TDerivedNSFileManager),' <> ',class_getInstanceSize(NSFileManager)+1);
 if class_getInstanceSize(TDerivedNSDirectoryEnumerator) <> (class_getInstanceSize(NSDirectoryEnumerator)+1) then
 writeln('size of NSDirectoryEnumerator is wrong: ',class_getInstanceSize(TDerivedNSDirectoryEnumerator),' <> ',class_getInstanceSize(NSDirectoryEnumerator)+1);
 if class_getInstanceSize(TDerivedNSFormatter) <> (class_getInstanceSize(NSFormatter)+1) then
 writeln('size of NSFormatter is wrong: ',class_getInstanceSize(TDerivedNSFormatter),' <> ',class_getInstanceSize(NSFormatter)+1);
 if class_getInstanceSize(TDerivedNSGarbageCollector) <> (class_getInstanceSize(NSGarbageCollector)+1) then
 writeln('size of NSGarbageCollector is wrong: ',class_getInstanceSize(TDerivedNSGarbageCollector),' <> ',class_getInstanceSize(NSGarbageCollector)+1);
 if class_getInstanceSize(TDerivedNSHashTable) <> (class_getInstanceSize(NSHashTable)+1) then
 writeln('size of NSHashTable is wrong: ',class_getInstanceSize(TDerivedNSHashTable),' <> ',class_getInstanceSize(NSHashTable)+1);
 if class_getInstanceSize(TDerivedNSHost) <> (class_getInstanceSize(NSHost)+1) then
 writeln('size of NSHost is wrong: ',class_getInstanceSize(TDerivedNSHost),' <> ',class_getInstanceSize(NSHost)+1);
 if class_getInstanceSize(TDerivedNSHTTPCookie) <> (class_getInstanceSize(NSHTTPCookie)+1) then
 writeln('size of NSHTTPCookie is wrong: ',class_getInstanceSize(TDerivedNSHTTPCookie),' <> ',class_getInstanceSize(NSHTTPCookie)+1);
 if class_getInstanceSize(TDerivedNSHTTPCookieStorage) <> (class_getInstanceSize(NSHTTPCookieStorage)+1) then
 writeln('size of NSHTTPCookieStorage is wrong: ',class_getInstanceSize(TDerivedNSHTTPCookieStorage),' <> ',class_getInstanceSize(NSHTTPCookieStorage)+1);
 if class_getInstanceSize(TDerivedNSIndexPath) <> (class_getInstanceSize(NSIndexPath)+1) then
 writeln('size of NSIndexPath is wrong: ',class_getInstanceSize(TDerivedNSIndexPath),' <> ',class_getInstanceSize(NSIndexPath)+1);
 if class_getInstanceSize(TDerivedNSIndexSet) <> (class_getInstanceSize(NSIndexSet)+1) then
 writeln('size of NSIndexSet is wrong: ',class_getInstanceSize(TDerivedNSIndexSet),' <> ',class_getInstanceSize(NSIndexSet)+1);
 if class_getInstanceSize(TDerivedNSMutableIndexSet) <> (class_getInstanceSize(NSMutableIndexSet)+1) then
 writeln('size of NSMutableIndexSet is wrong: ',class_getInstanceSize(TDerivedNSMutableIndexSet),' <> ',class_getInstanceSize(NSMutableIndexSet)+1);
 if class_getInstanceSize(TDerivedNSKeyedArchiver) <> (class_getInstanceSize(NSKeyedArchiver)+1) then
 writeln('size of NSKeyedArchiver is wrong: ',class_getInstanceSize(TDerivedNSKeyedArchiver),' <> ',class_getInstanceSize(NSKeyedArchiver)+1);
 if class_getInstanceSize(TDerivedNSKeyedUnarchiver) <> (class_getInstanceSize(NSKeyedUnarchiver)+1) then
 writeln('size of NSKeyedUnarchiver is wrong: ',class_getInstanceSize(TDerivedNSKeyedUnarchiver),' <> ',class_getInstanceSize(NSKeyedUnarchiver)+1);
 if class_getInstanceSize(TDerivedNSLocale) <> (class_getInstanceSize(NSLocale)+1) then
 writeln('size of NSLocale is wrong: ',class_getInstanceSize(TDerivedNSLocale),' <> ',class_getInstanceSize(NSLocale)+1);
 if class_getInstanceSize(TDerivedNSLock) <> (class_getInstanceSize(NSLock)+1) then
 writeln('size of NSLock is wrong: ',class_getInstanceSize(TDerivedNSLock),' <> ',class_getInstanceSize(NSLock)+1);
 if class_getInstanceSize(TDerivedNSConditionLock) <> (class_getInstanceSize(NSConditionLock)+1) then
 writeln('size of NSConditionLock is wrong: ',class_getInstanceSize(TDerivedNSConditionLock),' <> ',class_getInstanceSize(NSConditionLock)+1);
 if class_getInstanceSize(TDerivedNSRecursiveLock) <> (class_getInstanceSize(NSRecursiveLock)+1) then
 writeln('size of NSRecursiveLock is wrong: ',class_getInstanceSize(TDerivedNSRecursiveLock),' <> ',class_getInstanceSize(NSRecursiveLock)+1);
 if class_getInstanceSize(TDerivedNSCondition) <> (class_getInstanceSize(NSCondition)+1) then
 writeln('size of NSCondition is wrong: ',class_getInstanceSize(TDerivedNSCondition),' <> ',class_getInstanceSize(NSCondition)+1);
 if class_getInstanceSize(TDerivedNSMapTable) <> (class_getInstanceSize(NSMapTable)+1) then
 writeln('size of NSMapTable is wrong: ',class_getInstanceSize(TDerivedNSMapTable),' <> ',class_getInstanceSize(NSMapTable)+1);
 if class_getInstanceSize(TDerivedNSMetadataQuery) <> (class_getInstanceSize(NSMetadataQuery)+1) then
 writeln('size of NSMetadataQuery is wrong: ',class_getInstanceSize(TDerivedNSMetadataQuery),' <> ',class_getInstanceSize(NSMetadataQuery)+1);
 if class_getInstanceSize(TDerivedNSMetadataItem) <> (class_getInstanceSize(NSMetadataItem)+1) then
 writeln('size of NSMetadataItem is wrong: ',class_getInstanceSize(TDerivedNSMetadataItem),' <> ',class_getInstanceSize(NSMetadataItem)+1);
 if class_getInstanceSize(TDerivedNSMetadataQueryAttributeValueTuple) <> (class_getInstanceSize(NSMetadataQueryAttributeValueTuple)+1) then
 writeln('size of NSMetadataQueryAttributeValueTuple is wrong: ',class_getInstanceSize(TDerivedNSMetadataQueryAttributeValueTuple),' <> ',class_getInstanceSize(NSMetadataQueryAttributeValueTuple)+1);
 if class_getInstanceSize(TDerivedNSMetadataQueryResultGroup) <> (class_getInstanceSize(NSMetadataQueryResultGroup)+1) then
 writeln('size of NSMetadataQueryResultGroup is wrong: ',class_getInstanceSize(TDerivedNSMetadataQueryResultGroup),' <> ',class_getInstanceSize(NSMetadataQueryResultGroup)+1);
 if class_getInstanceSize(TDerivedNSMethodSignature) <> (class_getInstanceSize(NSMethodSignature)+1) then
 writeln('size of NSMethodSignature is wrong: ',class_getInstanceSize(TDerivedNSMethodSignature),' <> ',class_getInstanceSize(NSMethodSignature)+1);
 if class_getInstanceSize(TDerivedNSNetService) <> (class_getInstanceSize(NSNetService)+1) then
 writeln('size of NSNetService is wrong: ',class_getInstanceSize(TDerivedNSNetService),' <> ',class_getInstanceSize(NSNetService)+1);
 if class_getInstanceSize(TDerivedNSNetServiceBrowser) <> (class_getInstanceSize(NSNetServiceBrowser)+1) then
 writeln('size of NSNetServiceBrowser is wrong: ',class_getInstanceSize(TDerivedNSNetServiceBrowser),' <> ',class_getInstanceSize(NSNetServiceBrowser)+1);
 if class_getInstanceSize(TDerivedNSNotification) <> (class_getInstanceSize(NSNotification)+1) then
 writeln('size of NSNotification is wrong: ',class_getInstanceSize(TDerivedNSNotification),' <> ',class_getInstanceSize(NSNotification)+1);
 if class_getInstanceSize(TDerivedNSNotificationCenter) <> (class_getInstanceSize(NSNotificationCenter)+1) then
 writeln('size of NSNotificationCenter is wrong: ',class_getInstanceSize(TDerivedNSNotificationCenter),' <> ',class_getInstanceSize(NSNotificationCenter)+1);
 if class_getInstanceSize(TDerivedNSNotificationQueue) <> (class_getInstanceSize(NSNotificationQueue)+1) then
 writeln('size of NSNotificationQueue is wrong: ',class_getInstanceSize(TDerivedNSNotificationQueue),' <> ',class_getInstanceSize(NSNotificationQueue)+1);
 if class_getInstanceSize(TDerivedNSNull) <> (class_getInstanceSize(NSNull)+1) then
 writeln('size of NSNull is wrong: ',class_getInstanceSize(TDerivedNSNull),' <> ',class_getInstanceSize(NSNull)+1);
 if class_getInstanceSize(TDerivedNSNumberFormatter) <> (class_getInstanceSize(NSNumberFormatter)+1) then
 writeln('size of NSNumberFormatter is wrong: ',class_getInstanceSize(TDerivedNSNumberFormatter),' <> ',class_getInstanceSize(NSNumberFormatter)+1);
 if class_getInstanceSize(TDerivedNSObject) <> (class_getInstanceSize(NSObject)+1) then
 writeln('size of NSObject is wrong: ',class_getInstanceSize(TDerivedNSObject),' <> ',class_getInstanceSize(NSObject)+1);
 if class_getInstanceSize(TDerivedNSOperation) <> (class_getInstanceSize(NSOperation)+1) then
 writeln('size of NSOperation is wrong: ',class_getInstanceSize(TDerivedNSOperation),' <> ',class_getInstanceSize(NSOperation)+1);
 if class_getInstanceSize(TDerivedNSInvocationOperation) <> (class_getInstanceSize(NSInvocationOperation)+1) then
 writeln('size of NSInvocationOperation is wrong: ',class_getInstanceSize(TDerivedNSInvocationOperation),' <> ',class_getInstanceSize(NSInvocationOperation)+1);
 if class_getInstanceSize(TDerivedNSOperationQueue) <> (class_getInstanceSize(NSOperationQueue)+1) then
 writeln('size of NSOperationQueue is wrong: ',class_getInstanceSize(TDerivedNSOperationQueue),' <> ',class_getInstanceSize(NSOperationQueue)+1);
 if class_getInstanceSize(TDerivedNSPointerArray) <> (class_getInstanceSize(NSPointerArray)+1) then
 writeln('size of NSPointerArray is wrong: ',class_getInstanceSize(TDerivedNSPointerArray),' <> ',class_getInstanceSize(NSPointerArray)+1);
 if class_getInstanceSize(TDerivedNSPort) <> (class_getInstanceSize(NSPort)+1) then
 writeln('size of NSPort is wrong: ',class_getInstanceSize(TDerivedNSPort),' <> ',class_getInstanceSize(NSPort)+1);
 if class_getInstanceSize(TDerivedNSMachPort) <> (class_getInstanceSize(NSMachPort)+1) then
 writeln('size of NSMachPort is wrong: ',class_getInstanceSize(TDerivedNSMachPort),' <> ',class_getInstanceSize(NSMachPort)+1);
 if class_getInstanceSize(TDerivedNSMessagePort) <> (class_getInstanceSize(NSMessagePort)+1) then
 writeln('size of NSMessagePort is wrong: ',class_getInstanceSize(TDerivedNSMessagePort),' <> ',class_getInstanceSize(NSMessagePort)+1);
 if class_getInstanceSize(TDerivedNSSocketPort) <> (class_getInstanceSize(NSSocketPort)+1) then
 writeln('size of NSSocketPort is wrong: ',class_getInstanceSize(TDerivedNSSocketPort),' <> ',class_getInstanceSize(NSSocketPort)+1);
 if class_getInstanceSize(TDerivedNSPortCoder) <> (class_getInstanceSize(NSPortCoder)+1) then
 writeln('size of NSPortCoder is wrong: ',class_getInstanceSize(TDerivedNSPortCoder),' <> ',class_getInstanceSize(NSPortCoder)+1);
 if class_getInstanceSize(TDerivedNSPortMessage) <> (class_getInstanceSize(NSPortMessage)+1) then
 writeln('size of NSPortMessage is wrong: ',class_getInstanceSize(TDerivedNSPortMessage),' <> ',class_getInstanceSize(NSPortMessage)+1);
 if class_getInstanceSize(TDerivedNSPortNameServer) <> (class_getInstanceSize(NSPortNameServer)+1) then
 writeln('size of NSPortNameServer is wrong: ',class_getInstanceSize(TDerivedNSPortNameServer),' <> ',class_getInstanceSize(NSPortNameServer)+1);
 if class_getInstanceSize(TDerivedNSMachBootstrapServer) <> (class_getInstanceSize(NSMachBootstrapServer)+1) then
 writeln('size of NSMachBootstrapServer is wrong: ',class_getInstanceSize(TDerivedNSMachBootstrapServer),' <> ',class_getInstanceSize(NSMachBootstrapServer)+1);
 if class_getInstanceSize(TDerivedNSMessagePortNameServer) <> (class_getInstanceSize(NSMessagePortNameServer)+1) then
 writeln('size of NSMessagePortNameServer is wrong: ',class_getInstanceSize(TDerivedNSMessagePortNameServer),' <> ',class_getInstanceSize(NSMessagePortNameServer)+1);
 if class_getInstanceSize(TDerivedNSSocketPortNameServer) <> (class_getInstanceSize(NSSocketPortNameServer)+1) then
 writeln('size of NSSocketPortNameServer is wrong: ',class_getInstanceSize(TDerivedNSSocketPortNameServer),' <> ',class_getInstanceSize(NSSocketPortNameServer)+1);
 if class_getInstanceSize(TDerivedNSPredicate) <> (class_getInstanceSize(NSPredicate)+1) then
 writeln('size of NSPredicate is wrong: ',class_getInstanceSize(TDerivedNSPredicate),' <> ',class_getInstanceSize(NSPredicate)+1);
 if class_getInstanceSize(TDerivedNSProcessInfo) <> (class_getInstanceSize(NSProcessInfo)+1) then
 writeln('size of NSProcessInfo is wrong: ',class_getInstanceSize(TDerivedNSProcessInfo),' <> ',class_getInstanceSize(NSProcessInfo)+1);
 if class_getInstanceSize(TDerivedNSPropertyListSerialization) <> (class_getInstanceSize(NSPropertyListSerialization)+1) then
 writeln('size of NSPropertyListSerialization is wrong: ',class_getInstanceSize(TDerivedNSPropertyListSerialization),' <> ',class_getInstanceSize(NSPropertyListSerialization)+1);
 if class_getInstanceSize(TDerivedNSProtocolChecker) <> (class_getInstanceSize(NSProtocolChecker)+1) then
 writeln('size of NSProtocolChecker is wrong: ',class_getInstanceSize(TDerivedNSProtocolChecker),' <> ',class_getInstanceSize(NSProtocolChecker)+1);
 if class_getInstanceSize(TDerivedNSProxy) <> (class_getInstanceSize(NSProxy)+1) then
 writeln('size of NSProxy is wrong: ',class_getInstanceSize(TDerivedNSProxy),' <> ',class_getInstanceSize(NSProxy)+1);
 if class_getInstanceSize(TDerivedNSRunLoop) <> (class_getInstanceSize(NSRunLoop)+1) then
 writeln('size of NSRunLoop is wrong: ',class_getInstanceSize(TDerivedNSRunLoop),' <> ',class_getInstanceSize(NSRunLoop)+1);
 if class_getInstanceSize(TDerivedNSScanner) <> (class_getInstanceSize(NSScanner)+1) then
 writeln('size of NSScanner is wrong: ',class_getInstanceSize(TDerivedNSScanner),' <> ',class_getInstanceSize(NSScanner)+1);
 if class_getInstanceSize(TDerivedNSScriptClassDescription) <> (class_getInstanceSize(NSScriptClassDescription)+1) then
 writeln('size of NSScriptClassDescription is wrong: ',class_getInstanceSize(TDerivedNSScriptClassDescription),' <> ',class_getInstanceSize(NSScriptClassDescription)+1);
 if class_getInstanceSize(TDerivedNSScriptCoercionHandler) <> (class_getInstanceSize(NSScriptCoercionHandler)+1) then
 writeln('size of NSScriptCoercionHandler is wrong: ',class_getInstanceSize(TDerivedNSScriptCoercionHandler),' <> ',class_getInstanceSize(NSScriptCoercionHandler)+1);
 if class_getInstanceSize(TDerivedNSScriptCommand) <> (class_getInstanceSize(NSScriptCommand)+1) then
 writeln('size of NSScriptCommand is wrong: ',class_getInstanceSize(TDerivedNSScriptCommand),' <> ',class_getInstanceSize(NSScriptCommand)+1);
 if class_getInstanceSize(TDerivedNSScriptCommandDescription) <> (class_getInstanceSize(NSScriptCommandDescription)+1) then
 writeln('size of NSScriptCommandDescription is wrong: ',class_getInstanceSize(TDerivedNSScriptCommandDescription),' <> ',class_getInstanceSize(NSScriptCommandDescription)+1);
 if class_getInstanceSize(TDerivedNSScriptExecutionContext) <> (class_getInstanceSize(NSScriptExecutionContext)+1) then
 writeln('size of NSScriptExecutionContext is wrong: ',class_getInstanceSize(TDerivedNSScriptExecutionContext),' <> ',class_getInstanceSize(NSScriptExecutionContext)+1);
 if class_getInstanceSize(TDerivedNSScriptObjectSpecifier) <> (class_getInstanceSize(NSScriptObjectSpecifier)+1) then
 writeln('size of NSScriptObjectSpecifier is wrong: ',class_getInstanceSize(TDerivedNSScriptObjectSpecifier),' <> ',class_getInstanceSize(NSScriptObjectSpecifier)+1);
 if class_getInstanceSize(TDerivedNSIndexSpecifier) <> (class_getInstanceSize(NSIndexSpecifier)+1) then
 writeln('size of NSIndexSpecifier is wrong: ',class_getInstanceSize(TDerivedNSIndexSpecifier),' <> ',class_getInstanceSize(NSIndexSpecifier)+1);
 if class_getInstanceSize(TDerivedNSMiddleSpecifier) <> (class_getInstanceSize(NSMiddleSpecifier)+1) then
 writeln('size of NSMiddleSpecifier is wrong: ',class_getInstanceSize(TDerivedNSMiddleSpecifier),' <> ',class_getInstanceSize(NSMiddleSpecifier)+1);
 if class_getInstanceSize(TDerivedNSNameSpecifier) <> (class_getInstanceSize(NSNameSpecifier)+1) then
 writeln('size of NSNameSpecifier is wrong: ',class_getInstanceSize(TDerivedNSNameSpecifier),' <> ',class_getInstanceSize(NSNameSpecifier)+1);
 if class_getInstanceSize(TDerivedNSPositionalSpecifier) <> (class_getInstanceSize(NSPositionalSpecifier)+1) then
 writeln('size of NSPositionalSpecifier is wrong: ',class_getInstanceSize(TDerivedNSPositionalSpecifier),' <> ',class_getInstanceSize(NSPositionalSpecifier)+1);
 if class_getInstanceSize(TDerivedNSPropertySpecifier) <> (class_getInstanceSize(NSPropertySpecifier)+1) then
 writeln('size of NSPropertySpecifier is wrong: ',class_getInstanceSize(TDerivedNSPropertySpecifier),' <> ',class_getInstanceSize(NSPropertySpecifier)+1);
 if class_getInstanceSize(TDerivedNSRandomSpecifier) <> (class_getInstanceSize(NSRandomSpecifier)+1) then
 writeln('size of NSRandomSpecifier is wrong: ',class_getInstanceSize(TDerivedNSRandomSpecifier),' <> ',class_getInstanceSize(NSRandomSpecifier)+1);
 if class_getInstanceSize(TDerivedNSRangeSpecifier) <> (class_getInstanceSize(NSRangeSpecifier)+1) then
 writeln('size of NSRangeSpecifier is wrong: ',class_getInstanceSize(TDerivedNSRangeSpecifier),' <> ',class_getInstanceSize(NSRangeSpecifier)+1);
 if class_getInstanceSize(TDerivedNSRelativeSpecifier) <> (class_getInstanceSize(NSRelativeSpecifier)+1) then
 writeln('size of NSRelativeSpecifier is wrong: ',class_getInstanceSize(TDerivedNSRelativeSpecifier),' <> ',class_getInstanceSize(NSRelativeSpecifier)+1);
 if class_getInstanceSize(TDerivedNSUniqueIDSpecifier) <> (class_getInstanceSize(NSUniqueIDSpecifier)+1) then
 writeln('size of NSUniqueIDSpecifier is wrong: ',class_getInstanceSize(TDerivedNSUniqueIDSpecifier),' <> ',class_getInstanceSize(NSUniqueIDSpecifier)+1);
 if class_getInstanceSize(TDerivedNSWhoseSpecifier) <> (class_getInstanceSize(NSWhoseSpecifier)+1) then
 writeln('size of NSWhoseSpecifier is wrong: ',class_getInstanceSize(TDerivedNSWhoseSpecifier),' <> ',class_getInstanceSize(NSWhoseSpecifier)+1);
 if class_getInstanceSize(TDerivedNSCloneCommand) <> (class_getInstanceSize(NSCloneCommand)+1) then
 writeln('size of NSCloneCommand is wrong: ',class_getInstanceSize(TDerivedNSCloneCommand),' <> ',class_getInstanceSize(NSCloneCommand)+1);
 if class_getInstanceSize(TDerivedNSCloseCommand) <> (class_getInstanceSize(NSCloseCommand)+1) then
 writeln('size of NSCloseCommand is wrong: ',class_getInstanceSize(TDerivedNSCloseCommand),' <> ',class_getInstanceSize(NSCloseCommand)+1);
 if class_getInstanceSize(TDerivedNSCountCommand) <> (class_getInstanceSize(NSCountCommand)+1) then
 writeln('size of NSCountCommand is wrong: ',class_getInstanceSize(TDerivedNSCountCommand),' <> ',class_getInstanceSize(NSCountCommand)+1);
 if class_getInstanceSize(TDerivedNSCreateCommand) <> (class_getInstanceSize(NSCreateCommand)+1) then
 writeln('size of NSCreateCommand is wrong: ',class_getInstanceSize(TDerivedNSCreateCommand),' <> ',class_getInstanceSize(NSCreateCommand)+1);
 if class_getInstanceSize(TDerivedNSDeleteCommand) <> (class_getInstanceSize(NSDeleteCommand)+1) then
 writeln('size of NSDeleteCommand is wrong: ',class_getInstanceSize(TDerivedNSDeleteCommand),' <> ',class_getInstanceSize(NSDeleteCommand)+1);
 if class_getInstanceSize(TDerivedNSExistsCommand) <> (class_getInstanceSize(NSExistsCommand)+1) then
 writeln('size of NSExistsCommand is wrong: ',class_getInstanceSize(TDerivedNSExistsCommand),' <> ',class_getInstanceSize(NSExistsCommand)+1);
 if class_getInstanceSize(TDerivedNSGetCommand) <> (class_getInstanceSize(NSGetCommand)+1) then
 writeln('size of NSGetCommand is wrong: ',class_getInstanceSize(TDerivedNSGetCommand),' <> ',class_getInstanceSize(NSGetCommand)+1);
 if class_getInstanceSize(TDerivedNSMoveCommand) <> (class_getInstanceSize(NSMoveCommand)+1) then
 writeln('size of NSMoveCommand is wrong: ',class_getInstanceSize(TDerivedNSMoveCommand),' <> ',class_getInstanceSize(NSMoveCommand)+1);
 if class_getInstanceSize(TDerivedNSQuitCommand) <> (class_getInstanceSize(NSQuitCommand)+1) then
 writeln('size of NSQuitCommand is wrong: ',class_getInstanceSize(TDerivedNSQuitCommand),' <> ',class_getInstanceSize(NSQuitCommand)+1);
 if class_getInstanceSize(TDerivedNSSetCommand) <> (class_getInstanceSize(NSSetCommand)+1) then
 writeln('size of NSSetCommand is wrong: ',class_getInstanceSize(TDerivedNSSetCommand),' <> ',class_getInstanceSize(NSSetCommand)+1);
 if class_getInstanceSize(TDerivedNSScriptSuiteRegistry) <> (class_getInstanceSize(NSScriptSuiteRegistry)+1) then
 writeln('size of NSScriptSuiteRegistry is wrong: ',class_getInstanceSize(TDerivedNSScriptSuiteRegistry),' <> ',class_getInstanceSize(NSScriptSuiteRegistry)+1);
 if class_getInstanceSize(TDerivedNSScriptWhoseTest) <> (class_getInstanceSize(NSScriptWhoseTest)+1) then
 writeln('size of NSScriptWhoseTest is wrong: ',class_getInstanceSize(TDerivedNSScriptWhoseTest),' <> ',class_getInstanceSize(NSScriptWhoseTest)+1);
 if class_getInstanceSize(TDerivedNSLogicalTest) <> (class_getInstanceSize(NSLogicalTest)+1) then
 writeln('size of NSLogicalTest is wrong: ',class_getInstanceSize(TDerivedNSLogicalTest),' <> ',class_getInstanceSize(NSLogicalTest)+1);
 if class_getInstanceSize(TDerivedNSSpecifierTest) <> (class_getInstanceSize(NSSpecifierTest)+1) then
 writeln('size of NSSpecifierTest is wrong: ',class_getInstanceSize(TDerivedNSSpecifierTest),' <> ',class_getInstanceSize(NSSpecifierTest)+1);
 if class_getInstanceSize(TDerivedNSSet) <> (class_getInstanceSize(NSSet)+1) then
 writeln('size of NSSet is wrong: ',class_getInstanceSize(TDerivedNSSet),' <> ',class_getInstanceSize(NSSet)+1);
 if class_getInstanceSize(TDerivedNSMutableSet) <> (class_getInstanceSize(NSMutableSet)+1) then
 writeln('size of NSMutableSet is wrong: ',class_getInstanceSize(TDerivedNSMutableSet),' <> ',class_getInstanceSize(NSMutableSet)+1);
 if class_getInstanceSize(TDerivedNSCountedSet) <> (class_getInstanceSize(NSCountedSet)+1) then
 writeln('size of NSCountedSet is wrong: ',class_getInstanceSize(TDerivedNSCountedSet),' <> ',class_getInstanceSize(NSCountedSet)+1);
 if class_getInstanceSize(TDerivedNSSortDescriptor) <> (class_getInstanceSize(NSSortDescriptor)+1) then
 writeln('size of NSSortDescriptor is wrong: ',class_getInstanceSize(TDerivedNSSortDescriptor),' <> ',class_getInstanceSize(NSSortDescriptor)+1);
 if class_getInstanceSize(TDerivedNSSpellServer) <> (class_getInstanceSize(NSSpellServer)+1) then
 writeln('size of NSSpellServer is wrong: ',class_getInstanceSize(TDerivedNSSpellServer),' <> ',class_getInstanceSize(NSSpellServer)+1);
 if class_getInstanceSize(TDerivedNSStream) <> (class_getInstanceSize(NSStream)+1) then
 writeln('size of NSStream is wrong: ',class_getInstanceSize(TDerivedNSStream),' <> ',class_getInstanceSize(NSStream)+1);
 if class_getInstanceSize(TDerivedNSInputStream) <> (class_getInstanceSize(NSInputStream)+1) then
 writeln('size of NSInputStream is wrong: ',class_getInstanceSize(TDerivedNSInputStream),' <> ',class_getInstanceSize(NSInputStream)+1);
 if class_getInstanceSize(TDerivedNSOutputStream) <> (class_getInstanceSize(NSOutputStream)+1) then
 writeln('size of NSOutputStream is wrong: ',class_getInstanceSize(TDerivedNSOutputStream),' <> ',class_getInstanceSize(NSOutputStream)+1);
 if class_getInstanceSize(TDerivedNSString) <> (class_getInstanceSize(NSString)+1) then
 writeln('size of NSString is wrong: ',class_getInstanceSize(TDerivedNSString),' <> ',class_getInstanceSize(NSString)+1);
 if class_getInstanceSize(TDerivedNSMutableString) <> (class_getInstanceSize(NSMutableString)+1) then
 writeln('size of NSMutableString is wrong: ',class_getInstanceSize(TDerivedNSMutableString),' <> ',class_getInstanceSize(NSMutableString)+1);
 if class_getInstanceSize(TDerivedNSSimpleCString) <> (class_getInstanceSize(NSSimpleCString)+1) then
 writeln('size of NSSimpleCString is wrong: ',class_getInstanceSize(TDerivedNSSimpleCString),' <> ',class_getInstanceSize(NSSimpleCString)+1);
 if class_getInstanceSize(TDerivedNSTask) <> (class_getInstanceSize(NSTask)+1) then
 writeln('size of NSTask is wrong: ',class_getInstanceSize(TDerivedNSTask),' <> ',class_getInstanceSize(NSTask)+1);
 if class_getInstanceSize(TDerivedNSThread) <> (class_getInstanceSize(NSThread)+1) then
 writeln('size of NSThread is wrong: ',class_getInstanceSize(TDerivedNSThread),' <> ',class_getInstanceSize(NSThread)+1);
 if class_getInstanceSize(TDerivedNSTimer) <> (class_getInstanceSize(NSTimer)+1) then
 writeln('size of NSTimer is wrong: ',class_getInstanceSize(TDerivedNSTimer),' <> ',class_getInstanceSize(NSTimer)+1);
 if class_getInstanceSize(TDerivedNSTimeZone) <> (class_getInstanceSize(NSTimeZone)+1) then
 writeln('size of NSTimeZone is wrong: ',class_getInstanceSize(TDerivedNSTimeZone),' <> ',class_getInstanceSize(NSTimeZone)+1);
 if class_getInstanceSize(TDerivedNSUndoManager) <> (class_getInstanceSize(NSUndoManager)+1) then
 writeln('size of NSUndoManager is wrong: ',class_getInstanceSize(TDerivedNSUndoManager),' <> ',class_getInstanceSize(NSUndoManager)+1);
 if class_getInstanceSize(TDerivedNSURL) <> (class_getInstanceSize(NSURL)+1) then
 writeln('size of NSURL is wrong: ',class_getInstanceSize(TDerivedNSURL),' <> ',class_getInstanceSize(NSURL)+1);
 if class_getInstanceSize(TDerivedNSURLAuthenticationChallenge) <> (class_getInstanceSize(NSURLAuthenticationChallenge)+1) then
 writeln('size of NSURLAuthenticationChallenge is wrong: ',class_getInstanceSize(TDerivedNSURLAuthenticationChallenge),' <> ',class_getInstanceSize(NSURLAuthenticationChallenge)+1);
 if class_getInstanceSize(TDerivedNSCachedURLResponse) <> (class_getInstanceSize(NSCachedURLResponse)+1) then
 writeln('size of NSCachedURLResponse is wrong: ',class_getInstanceSize(TDerivedNSCachedURLResponse),' <> ',class_getInstanceSize(NSCachedURLResponse)+1);
 if class_getInstanceSize(TDerivedNSURLCache) <> (class_getInstanceSize(NSURLCache)+1) then
 writeln('size of NSURLCache is wrong: ',class_getInstanceSize(TDerivedNSURLCache),' <> ',class_getInstanceSize(NSURLCache)+1);
 if class_getInstanceSize(TDerivedNSURLConnection) <> (class_getInstanceSize(NSURLConnection)+1) then
 writeln('size of NSURLConnection is wrong: ',class_getInstanceSize(TDerivedNSURLConnection),' <> ',class_getInstanceSize(NSURLConnection)+1);
 if class_getInstanceSize(TDerivedNSURLCredential) <> (class_getInstanceSize(NSURLCredential)+1) then
 writeln('size of NSURLCredential is wrong: ',class_getInstanceSize(TDerivedNSURLCredential),' <> ',class_getInstanceSize(NSURLCredential)+1);
 if class_getInstanceSize(TDerivedNSURLCredentialStorage) <> (class_getInstanceSize(NSURLCredentialStorage)+1) then
 writeln('size of NSURLCredentialStorage is wrong: ',class_getInstanceSize(TDerivedNSURLCredentialStorage),' <> ',class_getInstanceSize(NSURLCredentialStorage)+1);
 if class_getInstanceSize(TDerivedNSURLDownload) <> (class_getInstanceSize(NSURLDownload)+1) then
 writeln('size of NSURLDownload is wrong: ',class_getInstanceSize(TDerivedNSURLDownload),' <> ',class_getInstanceSize(NSURLDownload)+1);
 if class_getInstanceSize(TDerivedNSURLHandle) <> (class_getInstanceSize(NSURLHandle)+1) then
 writeln('size of NSURLHandle is wrong: ',class_getInstanceSize(TDerivedNSURLHandle),' <> ',class_getInstanceSize(NSURLHandle)+1);
 if class_getInstanceSize(TDerivedNSURLProtectionSpace) <> (class_getInstanceSize(NSURLProtectionSpace)+1) then
 writeln('size of NSURLProtectionSpace is wrong: ',class_getInstanceSize(TDerivedNSURLProtectionSpace),' <> ',class_getInstanceSize(NSURLProtectionSpace)+1);
 if class_getInstanceSize(TDerivedNSURLProtocol) <> (class_getInstanceSize(NSURLProtocol)+1) then
 writeln('size of NSURLProtocol is wrong: ',class_getInstanceSize(TDerivedNSURLProtocol),' <> ',class_getInstanceSize(NSURLProtocol)+1);
 if class_getInstanceSize(TDerivedNSURLRequest) <> (class_getInstanceSize(NSURLRequest)+1) then
 writeln('size of NSURLRequest is wrong: ',class_getInstanceSize(TDerivedNSURLRequest),' <> ',class_getInstanceSize(NSURLRequest)+1);
 if class_getInstanceSize(TDerivedNSMutableURLRequest) <> (class_getInstanceSize(NSMutableURLRequest)+1) then
 writeln('size of NSMutableURLRequest is wrong: ',class_getInstanceSize(TDerivedNSMutableURLRequest),' <> ',class_getInstanceSize(NSMutableURLRequest)+1);
 if class_getInstanceSize(TDerivedNSURLResponse) <> (class_getInstanceSize(NSURLResponse)+1) then
 writeln('size of NSURLResponse is wrong: ',class_getInstanceSize(TDerivedNSURLResponse),' <> ',class_getInstanceSize(NSURLResponse)+1);
 if class_getInstanceSize(TDerivedNSHTTPURLResponse) <> (class_getInstanceSize(NSHTTPURLResponse)+1) then
 writeln('size of NSHTTPURLResponse is wrong: ',class_getInstanceSize(TDerivedNSHTTPURLResponse),' <> ',class_getInstanceSize(NSHTTPURLResponse)+1);
 if class_getInstanceSize(TDerivedNSUserDefaults) <> (class_getInstanceSize(NSUserDefaults)+1) then
 writeln('size of NSUserDefaults is wrong: ',class_getInstanceSize(TDerivedNSUserDefaults),' <> ',class_getInstanceSize(NSUserDefaults)+1);
 if class_getInstanceSize(TDerivedNSValue) <> (class_getInstanceSize(NSValue)+1) then
 writeln('size of NSValue is wrong: ',class_getInstanceSize(TDerivedNSValue),' <> ',class_getInstanceSize(NSValue)+1);
 if class_getInstanceSize(TDerivedNSNumber) <> (class_getInstanceSize(NSNumber)+1) then
 writeln('size of NSNumber is wrong: ',class_getInstanceSize(TDerivedNSNumber),' <> ',class_getInstanceSize(NSNumber)+1);
 if class_getInstanceSize(TDerivedNSValueTransformer) <> (class_getInstanceSize(NSValueTransformer)+1) then
 writeln('size of NSValueTransformer is wrong: ',class_getInstanceSize(TDerivedNSValueTransformer),' <> ',class_getInstanceSize(NSValueTransformer)+1);
 if class_getInstanceSize(TDerivedNSXMLDocument) <> (class_getInstanceSize(NSXMLDocument)+1) then
 writeln('size of NSXMLDocument is wrong: ',class_getInstanceSize(TDerivedNSXMLDocument),' <> ',class_getInstanceSize(NSXMLDocument)+1);
 if class_getInstanceSize(TDerivedNSXMLDTD) <> (class_getInstanceSize(NSXMLDTD)+1) then
 writeln('size of NSXMLDTD is wrong: ',class_getInstanceSize(TDerivedNSXMLDTD),' <> ',class_getInstanceSize(NSXMLDTD)+1);
 if class_getInstanceSize(TDerivedNSXMLDTDNode) <> (class_getInstanceSize(NSXMLDTDNode)+1) then
 writeln('size of NSXMLDTDNode is wrong: ',class_getInstanceSize(TDerivedNSXMLDTDNode),' <> ',class_getInstanceSize(NSXMLDTDNode)+1);
 if class_getInstanceSize(TDerivedNSXMLElement) <> (class_getInstanceSize(NSXMLElement)+1) then
 writeln('size of NSXMLElement is wrong: ',class_getInstanceSize(TDerivedNSXMLElement),' <> ',class_getInstanceSize(NSXMLElement)+1);
 if class_getInstanceSize(TDerivedNSXMLNode) <> (class_getInstanceSize(NSXMLNode)+1) then
 writeln('size of NSXMLNode is wrong: ',class_getInstanceSize(TDerivedNSXMLNode),' <> ',class_getInstanceSize(NSXMLNode)+1);
 if class_getInstanceSize(TDerivedNSXMLParser) <> (class_getInstanceSize(NSXMLParser)+1) then
 writeln('size of NSXMLParser is wrong: ',class_getInstanceSize(TDerivedNSXMLParser),' <> ',class_getInstanceSize(NSXMLParser)+1);
 if class_getInstanceSize(TDerivedNSActionCell) <> (class_getInstanceSize(NSActionCell)+1) then
 writeln('size of NSActionCell is wrong: ',class_getInstanceSize(TDerivedNSActionCell),' <> ',class_getInstanceSize(NSActionCell)+1);
 if class_getInstanceSize(TDerivedNSAlert) <> (class_getInstanceSize(NSAlert)+1) then
 writeln('size of NSAlert is wrong: ',class_getInstanceSize(TDerivedNSAlert),' <> ',class_getInstanceSize(NSAlert)+1);
 if class_getInstanceSize(TDerivedNSAnimation) <> (class_getInstanceSize(NSAnimation)+1) then
 writeln('size of NSAnimation is wrong: ',class_getInstanceSize(TDerivedNSAnimation),' <> ',class_getInstanceSize(NSAnimation)+1);
 if class_getInstanceSize(TDerivedNSViewAnimation) <> (class_getInstanceSize(NSViewAnimation)+1) then
 writeln('size of NSViewAnimation is wrong: ',class_getInstanceSize(TDerivedNSViewAnimation),' <> ',class_getInstanceSize(NSViewAnimation)+1);
 if class_getInstanceSize(TDerivedNSAnimationContext) <> (class_getInstanceSize(NSAnimationContext)+1) then
 writeln('size of NSAnimationContext is wrong: ',class_getInstanceSize(TDerivedNSAnimationContext),' <> ',class_getInstanceSize(NSAnimationContext)+1);
 if class_getInstanceSize(TDerivedNSApplication) <> (class_getInstanceSize(NSApplication)+1) then
 writeln('size of NSApplication is wrong: ',class_getInstanceSize(TDerivedNSApplication),' <> ',class_getInstanceSize(NSApplication)+1);
 if class_getInstanceSize(TDerivedNSArrayController) <> (class_getInstanceSize(NSArrayController)+1) then
 writeln('size of NSArrayController is wrong: ',class_getInstanceSize(TDerivedNSArrayController),' <> ',class_getInstanceSize(NSArrayController)+1);
 if class_getInstanceSize(TDerivedNSATSTypesetter) <> (class_getInstanceSize(NSATSTypesetter)+1) then
 writeln('size of NSATSTypesetter is wrong: ',class_getInstanceSize(TDerivedNSATSTypesetter),' <> ',class_getInstanceSize(NSATSTypesetter)+1);
 if class_getInstanceSize(TDerivedNSBezierPath) <> (class_getInstanceSize(NSBezierPath)+1) then
 writeln('size of NSBezierPath is wrong: ',class_getInstanceSize(TDerivedNSBezierPath),' <> ',class_getInstanceSize(NSBezierPath)+1);
 if class_getInstanceSize(TDerivedNSBitmapImageRep) <> (class_getInstanceSize(NSBitmapImageRep)+1) then
 writeln('size of NSBitmapImageRep is wrong: ',class_getInstanceSize(TDerivedNSBitmapImageRep),' <> ',class_getInstanceSize(NSBitmapImageRep)+1);
 if class_getInstanceSize(TDerivedNSBox) <> (class_getInstanceSize(NSBox)+1) then
 writeln('size of NSBox is wrong: ',class_getInstanceSize(TDerivedNSBox),' <> ',class_getInstanceSize(NSBox)+1);
 if class_getInstanceSize(TDerivedNSBrowser) <> (class_getInstanceSize(NSBrowser)+1) then
 writeln('size of NSBrowser is wrong: ',class_getInstanceSize(TDerivedNSBrowser),' <> ',class_getInstanceSize(NSBrowser)+1);
 if class_getInstanceSize(TDerivedNSBrowserCell) <> (class_getInstanceSize(NSBrowserCell)+1) then
 writeln('size of NSBrowserCell is wrong: ',class_getInstanceSize(TDerivedNSBrowserCell),' <> ',class_getInstanceSize(NSBrowserCell)+1);
 if class_getInstanceSize(TDerivedNSButton) <> (class_getInstanceSize(NSButton)+1) then
 writeln('size of NSButton is wrong: ',class_getInstanceSize(TDerivedNSButton),' <> ',class_getInstanceSize(NSButton)+1);
 if class_getInstanceSize(TDerivedNSButtonCell) <> (class_getInstanceSize(NSButtonCell)+1) then
 writeln('size of NSButtonCell is wrong: ',class_getInstanceSize(TDerivedNSButtonCell),' <> ',class_getInstanceSize(NSButtonCell)+1);
 if class_getInstanceSize(TDerivedNSCachedImageRep) <> (class_getInstanceSize(NSCachedImageRep)+1) then
 writeln('size of NSCachedImageRep is wrong: ',class_getInstanceSize(TDerivedNSCachedImageRep),' <> ',class_getInstanceSize(NSCachedImageRep)+1);
 if class_getInstanceSize(TDerivedNSCell) <> (class_getInstanceSize(NSCell)+1) then
 writeln('size of NSCell is wrong: ',class_getInstanceSize(TDerivedNSCell),' <> ',class_getInstanceSize(NSCell)+1);
 if class_getInstanceSize(TDerivedNSCIImageRep) <> (class_getInstanceSize(NSCIImageRep)+1) then
 writeln('size of NSCIImageRep is wrong: ',class_getInstanceSize(TDerivedNSCIImageRep),' <> ',class_getInstanceSize(NSCIImageRep)+1);
 if class_getInstanceSize(TDerivedNSClipView) <> (class_getInstanceSize(NSClipView)+1) then
 writeln('size of NSClipView is wrong: ',class_getInstanceSize(TDerivedNSClipView),' <> ',class_getInstanceSize(NSClipView)+1);
 if class_getInstanceSize(TDerivedNSCollectionViewItem) <> (class_getInstanceSize(NSCollectionViewItem)+1) then
 writeln('size of NSCollectionViewItem is wrong: ',class_getInstanceSize(TDerivedNSCollectionViewItem),' <> ',class_getInstanceSize(NSCollectionViewItem)+1);
 if class_getInstanceSize(TDerivedNSCollectionView) <> (class_getInstanceSize(NSCollectionView)+1) then
 writeln('size of NSCollectionView is wrong: ',class_getInstanceSize(TDerivedNSCollectionView),' <> ',class_getInstanceSize(NSCollectionView)+1);
 if class_getInstanceSize(TDerivedNSColor) <> (class_getInstanceSize(NSColor)+1) then
 writeln('size of NSColor is wrong: ',class_getInstanceSize(TDerivedNSColor),' <> ',class_getInstanceSize(NSColor)+1);
 if class_getInstanceSize(TDerivedNSColorList) <> (class_getInstanceSize(NSColorList)+1) then
 writeln('size of NSColorList is wrong: ',class_getInstanceSize(TDerivedNSColorList),' <> ',class_getInstanceSize(NSColorList)+1);
 if class_getInstanceSize(TDerivedNSColorPanel) <> (class_getInstanceSize(NSColorPanel)+1) then
 writeln('size of NSColorPanel is wrong: ',class_getInstanceSize(TDerivedNSColorPanel),' <> ',class_getInstanceSize(NSColorPanel)+1);
 if class_getInstanceSize(TDerivedNSColorPicker) <> (class_getInstanceSize(NSColorPicker)+1) then
 writeln('size of NSColorPicker is wrong: ',class_getInstanceSize(TDerivedNSColorPicker),' <> ',class_getInstanceSize(NSColorPicker)+1);
 if class_getInstanceSize(TDerivedNSColorSpace) <> (class_getInstanceSize(NSColorSpace)+1) then
 writeln('size of NSColorSpace is wrong: ',class_getInstanceSize(TDerivedNSColorSpace),' <> ',class_getInstanceSize(NSColorSpace)+1);
 if class_getInstanceSize(TDerivedNSColorWell) <> (class_getInstanceSize(NSColorWell)+1) then
 writeln('size of NSColorWell is wrong: ',class_getInstanceSize(TDerivedNSColorWell),' <> ',class_getInstanceSize(NSColorWell)+1);
 if class_getInstanceSize(TDerivedNSComboBox) <> (class_getInstanceSize(NSComboBox)+1) then
 writeln('size of NSComboBox is wrong: ',class_getInstanceSize(TDerivedNSComboBox),' <> ',class_getInstanceSize(NSComboBox)+1);
 if class_getInstanceSize(TDerivedNSComboBoxCell) <> (class_getInstanceSize(NSComboBoxCell)+1) then
 writeln('size of NSComboBoxCell is wrong: ',class_getInstanceSize(TDerivedNSComboBoxCell),' <> ',class_getInstanceSize(NSComboBoxCell)+1);
 if class_getInstanceSize(TDerivedNSControl) <> (class_getInstanceSize(NSControl)+1) then
 writeln('size of NSControl is wrong: ',class_getInstanceSize(TDerivedNSControl),' <> ',class_getInstanceSize(NSControl)+1);
 if class_getInstanceSize(TDerivedNSController) <> (class_getInstanceSize(NSController)+1) then
 writeln('size of NSController is wrong: ',class_getInstanceSize(TDerivedNSController),' <> ',class_getInstanceSize(NSController)+1);
 if class_getInstanceSize(TDerivedNSCursor) <> (class_getInstanceSize(NSCursor)+1) then
 writeln('size of NSCursor is wrong: ',class_getInstanceSize(TDerivedNSCursor),' <> ',class_getInstanceSize(NSCursor)+1);
 if class_getInstanceSize(TDerivedNSCustomImageRep) <> (class_getInstanceSize(NSCustomImageRep)+1) then
 writeln('size of NSCustomImageRep is wrong: ',class_getInstanceSize(TDerivedNSCustomImageRep),' <> ',class_getInstanceSize(NSCustomImageRep)+1);
 if class_getInstanceSize(TDerivedNSDatePicker) <> (class_getInstanceSize(NSDatePicker)+1) then
 writeln('size of NSDatePicker is wrong: ',class_getInstanceSize(TDerivedNSDatePicker),' <> ',class_getInstanceSize(NSDatePicker)+1);
 if class_getInstanceSize(TDerivedNSDatePickerCell) <> (class_getInstanceSize(NSDatePickerCell)+1) then
 writeln('size of NSDatePickerCell is wrong: ',class_getInstanceSize(TDerivedNSDatePickerCell),' <> ',class_getInstanceSize(NSDatePickerCell)+1);
 if class_getInstanceSize(TDerivedNSDictionaryController) <> (class_getInstanceSize(NSDictionaryController)+1) then
 writeln('size of NSDictionaryController is wrong: ',class_getInstanceSize(TDerivedNSDictionaryController),' <> ',class_getInstanceSize(NSDictionaryController)+1);
 if class_getInstanceSize(TDerivedNSDockTile) <> (class_getInstanceSize(NSDockTile)+1) then
 writeln('size of NSDockTile is wrong: ',class_getInstanceSize(TDerivedNSDockTile),' <> ',class_getInstanceSize(NSDockTile)+1);
 if class_getInstanceSize(TDerivedNSDocument) <> (class_getInstanceSize(NSDocument)+1) then
 writeln('size of NSDocument is wrong: ',class_getInstanceSize(TDerivedNSDocument),' <> ',class_getInstanceSize(NSDocument)+1);
 if class_getInstanceSize(TDerivedNSDocumentController) <> (class_getInstanceSize(NSDocumentController)+1) then
 writeln('size of NSDocumentController is wrong: ',class_getInstanceSize(TDerivedNSDocumentController),' <> ',class_getInstanceSize(NSDocumentController)+1);
 if class_getInstanceSize(TDerivedNSDrawer) <> (class_getInstanceSize(NSDrawer)+1) then
 writeln('size of NSDrawer is wrong: ',class_getInstanceSize(TDerivedNSDrawer),' <> ',class_getInstanceSize(NSDrawer)+1);
 if class_getInstanceSize(TDerivedNSEPSImageRep) <> (class_getInstanceSize(NSEPSImageRep)+1) then
 writeln('size of NSEPSImageRep is wrong: ',class_getInstanceSize(TDerivedNSEPSImageRep),' <> ',class_getInstanceSize(NSEPSImageRep)+1);
 if class_getInstanceSize(TDerivedNSEvent) <> (class_getInstanceSize(NSEvent)+1) then
 writeln('size of NSEvent is wrong: ',class_getInstanceSize(TDerivedNSEvent),' <> ',class_getInstanceSize(NSEvent)+1);
 if class_getInstanceSize(TDerivedNSFileWrapper) <> (class_getInstanceSize(NSFileWrapper)+1) then
 writeln('size of NSFileWrapper is wrong: ',class_getInstanceSize(TDerivedNSFileWrapper),' <> ',class_getInstanceSize(NSFileWrapper)+1);
 if class_getInstanceSize(TDerivedNSFont) <> (class_getInstanceSize(NSFont)+1) then
 writeln('size of NSFont is wrong: ',class_getInstanceSize(TDerivedNSFont),' <> ',class_getInstanceSize(NSFont)+1);
 if class_getInstanceSize(TDerivedNSFontDescriptor) <> (class_getInstanceSize(NSFontDescriptor)+1) then
 writeln('size of NSFontDescriptor is wrong: ',class_getInstanceSize(TDerivedNSFontDescriptor),' <> ',class_getInstanceSize(NSFontDescriptor)+1);
 if class_getInstanceSize(TDerivedNSFontManager) <> (class_getInstanceSize(NSFontManager)+1) then
 writeln('size of NSFontManager is wrong: ',class_getInstanceSize(TDerivedNSFontManager),' <> ',class_getInstanceSize(NSFontManager)+1);
 if class_getInstanceSize(TDerivedNSFontPanel) <> (class_getInstanceSize(NSFontPanel)+1) then
 writeln('size of NSFontPanel is wrong: ',class_getInstanceSize(TDerivedNSFontPanel),' <> ',class_getInstanceSize(NSFontPanel)+1);
 if class_getInstanceSize(TDerivedNSFormCell) <> (class_getInstanceSize(NSFormCell)+1) then
 writeln('size of NSFormCell is wrong: ',class_getInstanceSize(TDerivedNSFormCell),' <> ',class_getInstanceSize(NSFormCell)+1);
 if class_getInstanceSize(TDerivedNSGlyphGenerator) <> (class_getInstanceSize(NSGlyphGenerator)+1) then
 writeln('size of NSGlyphGenerator is wrong: ',class_getInstanceSize(TDerivedNSGlyphGenerator),' <> ',class_getInstanceSize(NSGlyphGenerator)+1);
 if class_getInstanceSize(TDerivedNSGlyphInfo) <> (class_getInstanceSize(NSGlyphInfo)+1) then
 writeln('size of NSGlyphInfo is wrong: ',class_getInstanceSize(TDerivedNSGlyphInfo),' <> ',class_getInstanceSize(NSGlyphInfo)+1);
 if class_getInstanceSize(TDerivedNSGradient) <> (class_getInstanceSize(NSGradient)+1) then
 writeln('size of NSGradient is wrong: ',class_getInstanceSize(TDerivedNSGradient),' <> ',class_getInstanceSize(NSGradient)+1);
 if class_getInstanceSize(TDerivedNSGraphicsContext) <> (class_getInstanceSize(NSGraphicsContext)+1) then
 writeln('size of NSGraphicsContext is wrong: ',class_getInstanceSize(TDerivedNSGraphicsContext),' <> ',class_getInstanceSize(NSGraphicsContext)+1);
 if class_getInstanceSize(TDerivedNSHelpManager) <> (class_getInstanceSize(NSHelpManager)+1) then
 writeln('size of NSHelpManager is wrong: ',class_getInstanceSize(TDerivedNSHelpManager),' <> ',class_getInstanceSize(NSHelpManager)+1);
 if class_getInstanceSize(TDerivedNSImage) <> (class_getInstanceSize(NSImage)+1) then
 writeln('size of NSImage is wrong: ',class_getInstanceSize(TDerivedNSImage),' <> ',class_getInstanceSize(NSImage)+1);
 if class_getInstanceSize(TDerivedNSImageCell) <> (class_getInstanceSize(NSImageCell)+1) then
 writeln('size of NSImageCell is wrong: ',class_getInstanceSize(TDerivedNSImageCell),' <> ',class_getInstanceSize(NSImageCell)+1);
 if class_getInstanceSize(TDerivedNSImageRep) <> (class_getInstanceSize(NSImageRep)+1) then
 writeln('size of NSImageRep is wrong: ',class_getInstanceSize(TDerivedNSImageRep),' <> ',class_getInstanceSize(NSImageRep)+1);
 if class_getInstanceSize(TDerivedNSImageView) <> (class_getInstanceSize(NSImageView)+1) then
 writeln('size of NSImageView is wrong: ',class_getInstanceSize(TDerivedNSImageView),' <> ',class_getInstanceSize(NSImageView)+1);
 if class_getInstanceSize(TDerivedNSInputManager) <> (class_getInstanceSize(NSInputManager)+1) then
 writeln('size of NSInputManager is wrong: ',class_getInstanceSize(TDerivedNSInputManager),' <> ',class_getInstanceSize(NSInputManager)+1);
 if class_getInstanceSize(TDerivedNSInputServer) <> (class_getInstanceSize(NSInputServer)+1) then
 writeln('size of NSInputServer is wrong: ',class_getInstanceSize(TDerivedNSInputServer),' <> ',class_getInstanceSize(NSInputServer)+1);
 if class_getInstanceSize(TDerivedNSLayoutManager) <> (class_getInstanceSize(NSLayoutManager)+1) then
 writeln('size of NSLayoutManager is wrong: ',class_getInstanceSize(TDerivedNSLayoutManager),' <> ',class_getInstanceSize(NSLayoutManager)+1);
 if class_getInstanceSize(TDerivedNSLevelIndicator) <> (class_getInstanceSize(NSLevelIndicator)+1) then
 writeln('size of NSLevelIndicator is wrong: ',class_getInstanceSize(TDerivedNSLevelIndicator),' <> ',class_getInstanceSize(NSLevelIndicator)+1);
 if class_getInstanceSize(TDerivedNSLevelIndicatorCell) <> (class_getInstanceSize(NSLevelIndicatorCell)+1) then
 writeln('size of NSLevelIndicatorCell is wrong: ',class_getInstanceSize(TDerivedNSLevelIndicatorCell),' <> ',class_getInstanceSize(NSLevelIndicatorCell)+1);
 if class_getInstanceSize(TDerivedNSMatrix) <> (class_getInstanceSize(NSMatrix)+1) then
 writeln('size of NSMatrix is wrong: ',class_getInstanceSize(TDerivedNSMatrix),' <> ',class_getInstanceSize(NSMatrix)+1);
 if class_getInstanceSize(TDerivedNSMenu) <> (class_getInstanceSize(NSMenu)+1) then
 writeln('size of NSMenu is wrong: ',class_getInstanceSize(TDerivedNSMenu),' <> ',class_getInstanceSize(NSMenu)+1);
 if class_getInstanceSize(TDerivedNSMenuItem) <> (class_getInstanceSize(NSMenuItem)+1) then
 writeln('size of NSMenuItem is wrong: ',class_getInstanceSize(TDerivedNSMenuItem),' <> ',class_getInstanceSize(NSMenuItem)+1);
 if class_getInstanceSize(TDerivedNSMenuItemCell) <> (class_getInstanceSize(NSMenuItemCell)+1) then
 writeln('size of NSMenuItemCell is wrong: ',class_getInstanceSize(TDerivedNSMenuItemCell),' <> ',class_getInstanceSize(NSMenuItemCell)+1);
 if class_getInstanceSize(TDerivedNSMenuView) <> (class_getInstanceSize(NSMenuView)+1) then
 writeln('size of NSMenuView is wrong: ',class_getInstanceSize(TDerivedNSMenuView),' <> ',class_getInstanceSize(NSMenuView)+1);
 if class_getInstanceSize(TDerivedNSMovie) <> (class_getInstanceSize(NSMovie)+1) then
 writeln('size of NSMovie is wrong: ',class_getInstanceSize(TDerivedNSMovie),' <> ',class_getInstanceSize(NSMovie)+1);
 if class_getInstanceSize(TDerivedNSMovieView) <> (class_getInstanceSize(NSMovieView)+1) then
 writeln('size of NSMovieView is wrong: ',class_getInstanceSize(TDerivedNSMovieView),' <> ',class_getInstanceSize(NSMovieView)+1);
 if class_getInstanceSize(TDerivedNSNib) <> (class_getInstanceSize(NSNib)+1) then
 writeln('size of NSNib is wrong: ',class_getInstanceSize(TDerivedNSNib),' <> ',class_getInstanceSize(NSNib)+1);
 if class_getInstanceSize(TDerivedNSObjectController) <> (class_getInstanceSize(NSObjectController)+1) then
 writeln('size of NSObjectController is wrong: ',class_getInstanceSize(TDerivedNSObjectController),' <> ',class_getInstanceSize(NSObjectController)+1);
 if class_getInstanceSize(TDerivedNSOpenGLPixelFormat) <> (class_getInstanceSize(NSOpenGLPixelFormat)+1) then
 writeln('size of NSOpenGLPixelFormat is wrong: ',class_getInstanceSize(TDerivedNSOpenGLPixelFormat),' <> ',class_getInstanceSize(NSOpenGLPixelFormat)+1);
 if class_getInstanceSize(TDerivedNSOpenGLPixelBuffer) <> (class_getInstanceSize(NSOpenGLPixelBuffer)+1) then
 writeln('size of NSOpenGLPixelBuffer is wrong: ',class_getInstanceSize(TDerivedNSOpenGLPixelBuffer),' <> ',class_getInstanceSize(NSOpenGLPixelBuffer)+1);
 if class_getInstanceSize(TDerivedNSOpenGLContext) <> (class_getInstanceSize(NSOpenGLContext)+1) then
 writeln('size of NSOpenGLContext is wrong: ',class_getInstanceSize(TDerivedNSOpenGLContext),' <> ',class_getInstanceSize(NSOpenGLContext)+1);
 if class_getInstanceSize(TDerivedNSOpenGLView) <> (class_getInstanceSize(NSOpenGLView)+1) then
 writeln('size of NSOpenGLView is wrong: ',class_getInstanceSize(TDerivedNSOpenGLView),' <> ',class_getInstanceSize(NSOpenGLView)+1);
 if class_getInstanceSize(TDerivedNSOpenPanel) <> (class_getInstanceSize(NSOpenPanel)+1) then
 writeln('size of NSOpenPanel is wrong: ',class_getInstanceSize(TDerivedNSOpenPanel),' <> ',class_getInstanceSize(NSOpenPanel)+1);
 if class_getInstanceSize(TDerivedNSOutlineView) <> (class_getInstanceSize(NSOutlineView)+1) then
 writeln('size of NSOutlineView is wrong: ',class_getInstanceSize(TDerivedNSOutlineView),' <> ',class_getInstanceSize(NSOutlineView)+1);
 if class_getInstanceSize(TDerivedNSPageLayout) <> (class_getInstanceSize(NSPageLayout)+1) then
 writeln('size of NSPageLayout is wrong: ',class_getInstanceSize(TDerivedNSPageLayout),' <> ',class_getInstanceSize(NSPageLayout)+1);
 if class_getInstanceSize(TDerivedNSPanel) <> (class_getInstanceSize(NSPanel)+1) then
 writeln('size of NSPanel is wrong: ',class_getInstanceSize(TDerivedNSPanel),' <> ',class_getInstanceSize(NSPanel)+1);
 if class_getInstanceSize(TDerivedNSTextTab) <> (class_getInstanceSize(NSTextTab)+1) then
 writeln('size of NSTextTab is wrong: ',class_getInstanceSize(TDerivedNSTextTab),' <> ',class_getInstanceSize(NSTextTab)+1);
 if class_getInstanceSize(TDerivedNSParagraphStyle) <> (class_getInstanceSize(NSParagraphStyle)+1) then
 writeln('size of NSParagraphStyle is wrong: ',class_getInstanceSize(TDerivedNSParagraphStyle),' <> ',class_getInstanceSize(NSParagraphStyle)+1);
 if class_getInstanceSize(TDerivedNSMutableParagraphStyle) <> (class_getInstanceSize(NSMutableParagraphStyle)+1) then
 writeln('size of NSMutableParagraphStyle is wrong: ',class_getInstanceSize(TDerivedNSMutableParagraphStyle),' <> ',class_getInstanceSize(NSMutableParagraphStyle)+1);
 if class_getInstanceSize(TDerivedNSPasteboard) <> (class_getInstanceSize(NSPasteboard)+1) then
 writeln('size of NSPasteboard is wrong: ',class_getInstanceSize(TDerivedNSPasteboard),' <> ',class_getInstanceSize(NSPasteboard)+1);
 if class_getInstanceSize(TDerivedNSPathCell) <> (class_getInstanceSize(NSPathCell)+1) then
 writeln('size of NSPathCell is wrong: ',class_getInstanceSize(TDerivedNSPathCell),' <> ',class_getInstanceSize(NSPathCell)+1);
 if class_getInstanceSize(TDerivedNSPathComponentCell) <> (class_getInstanceSize(NSPathComponentCell)+1) then
 writeln('size of NSPathComponentCell is wrong: ',class_getInstanceSize(TDerivedNSPathComponentCell),' <> ',class_getInstanceSize(NSPathComponentCell)+1);
 if class_getInstanceSize(TDerivedNSPathControl) <> (class_getInstanceSize(NSPathControl)+1) then
 writeln('size of NSPathControl is wrong: ',class_getInstanceSize(TDerivedNSPathControl),' <> ',class_getInstanceSize(NSPathControl)+1);
 if class_getInstanceSize(TDerivedNSPDFImageRep) <> (class_getInstanceSize(NSPDFImageRep)+1) then
 writeln('size of NSPDFImageRep is wrong: ',class_getInstanceSize(TDerivedNSPDFImageRep),' <> ',class_getInstanceSize(NSPDFImageRep)+1);
 if class_getInstanceSize(TDerivedNSPersistentDocument) <> (class_getInstanceSize(NSPersistentDocument)+1) then
 writeln('size of NSPersistentDocument is wrong: ',class_getInstanceSize(TDerivedNSPersistentDocument),' <> ',class_getInstanceSize(NSPersistentDocument)+1);
 if class_getInstanceSize(TDerivedNSPICTImageRep) <> (class_getInstanceSize(NSPICTImageRep)+1) then
 writeln('size of NSPICTImageRep is wrong: ',class_getInstanceSize(TDerivedNSPICTImageRep),' <> ',class_getInstanceSize(NSPICTImageRep)+1);
 if class_getInstanceSize(TDerivedNSPopUpButton) <> (class_getInstanceSize(NSPopUpButton)+1) then
 writeln('size of NSPopUpButton is wrong: ',class_getInstanceSize(TDerivedNSPopUpButton),' <> ',class_getInstanceSize(NSPopUpButton)+1);
 if class_getInstanceSize(TDerivedNSPopUpButtonCell) <> (class_getInstanceSize(NSPopUpButtonCell)+1) then
 writeln('size of NSPopUpButtonCell is wrong: ',class_getInstanceSize(TDerivedNSPopUpButtonCell),' <> ',class_getInstanceSize(NSPopUpButtonCell)+1);
 if class_getInstanceSize(TDerivedNSPredicateEditor) <> (class_getInstanceSize(NSPredicateEditor)+1) then
 writeln('size of NSPredicateEditor is wrong: ',class_getInstanceSize(TDerivedNSPredicateEditor),' <> ',class_getInstanceSize(NSPredicateEditor)+1);
 if class_getInstanceSize(TDerivedNSPrinter) <> (class_getInstanceSize(NSPrinter)+1) then
 writeln('size of NSPrinter is wrong: ',class_getInstanceSize(TDerivedNSPrinter),' <> ',class_getInstanceSize(NSPrinter)+1);
 if class_getInstanceSize(TDerivedNSPrintInfo) <> (class_getInstanceSize(NSPrintInfo)+1) then
 writeln('size of NSPrintInfo is wrong: ',class_getInstanceSize(TDerivedNSPrintInfo),' <> ',class_getInstanceSize(NSPrintInfo)+1);
 if class_getInstanceSize(TDerivedNSPrintOperation) <> (class_getInstanceSize(NSPrintOperation)+1) then
 writeln('size of NSPrintOperation is wrong: ',class_getInstanceSize(TDerivedNSPrintOperation),' <> ',class_getInstanceSize(NSPrintOperation)+1);
 if class_getInstanceSize(TDerivedNSPrintPanel) <> (class_getInstanceSize(NSPrintPanel)+1) then
 writeln('size of NSPrintPanel is wrong: ',class_getInstanceSize(TDerivedNSPrintPanel),' <> ',class_getInstanceSize(NSPrintPanel)+1);
 if class_getInstanceSize(TDerivedNSProgressIndicator) <> (class_getInstanceSize(NSProgressIndicator)+1) then
 writeln('size of NSProgressIndicator is wrong: ',class_getInstanceSize(TDerivedNSProgressIndicator),' <> ',class_getInstanceSize(NSProgressIndicator)+1);
 if class_getInstanceSize(TDerivedNSQuickDrawView) <> (class_getInstanceSize(NSQuickDrawView)+1) then
 writeln('size of NSQuickDrawView is wrong: ',class_getInstanceSize(TDerivedNSQuickDrawView),' <> ',class_getInstanceSize(NSQuickDrawView)+1);
 if class_getInstanceSize(TDerivedNSResponder) <> (class_getInstanceSize(NSResponder)+1) then
 writeln('size of NSResponder is wrong: ',class_getInstanceSize(TDerivedNSResponder),' <> ',class_getInstanceSize(NSResponder)+1);
 if class_getInstanceSize(TDerivedNSRuleEditor) <> (class_getInstanceSize(NSRuleEditor)+1) then
 writeln('size of NSRuleEditor is wrong: ',class_getInstanceSize(TDerivedNSRuleEditor),' <> ',class_getInstanceSize(NSRuleEditor)+1);
 if class_getInstanceSize(TDerivedNSRulerMarker) <> (class_getInstanceSize(NSRulerMarker)+1) then
 writeln('size of NSRulerMarker is wrong: ',class_getInstanceSize(TDerivedNSRulerMarker),' <> ',class_getInstanceSize(NSRulerMarker)+1);
 if class_getInstanceSize(TDerivedNSRulerView) <> (class_getInstanceSize(NSRulerView)+1) then
 writeln('size of NSRulerView is wrong: ',class_getInstanceSize(TDerivedNSRulerView),' <> ',class_getInstanceSize(NSRulerView)+1);
 if class_getInstanceSize(TDerivedNSSavePanel) <> (class_getInstanceSize(NSSavePanel)+1) then
 writeln('size of NSSavePanel is wrong: ',class_getInstanceSize(TDerivedNSSavePanel),' <> ',class_getInstanceSize(NSSavePanel)+1);
 if class_getInstanceSize(TDerivedNSScreen) <> (class_getInstanceSize(NSScreen)+1) then
 writeln('size of NSScreen is wrong: ',class_getInstanceSize(TDerivedNSScreen),' <> ',class_getInstanceSize(NSScreen)+1);
 if class_getInstanceSize(TDerivedNSScroller) <> (class_getInstanceSize(NSScroller)+1) then
 writeln('size of NSScroller is wrong: ',class_getInstanceSize(TDerivedNSScroller),' <> ',class_getInstanceSize(NSScroller)+1);
 if class_getInstanceSize(TDerivedNSScrollView) <> (class_getInstanceSize(NSScrollView)+1) then
 writeln('size of NSScrollView is wrong: ',class_getInstanceSize(TDerivedNSScrollView),' <> ',class_getInstanceSize(NSScrollView)+1);
 if class_getInstanceSize(TDerivedNSSearchField) <> (class_getInstanceSize(NSSearchField)+1) then
 writeln('size of NSSearchField is wrong: ',class_getInstanceSize(TDerivedNSSearchField),' <> ',class_getInstanceSize(NSSearchField)+1);
 if class_getInstanceSize(TDerivedNSSearchFieldCell) <> (class_getInstanceSize(NSSearchFieldCell)+1) then
 writeln('size of NSSearchFieldCell is wrong: ',class_getInstanceSize(TDerivedNSSearchFieldCell),' <> ',class_getInstanceSize(NSSearchFieldCell)+1);
 if class_getInstanceSize(TDerivedNSSecureTextField) <> (class_getInstanceSize(NSSecureTextField)+1) then
 writeln('size of NSSecureTextField is wrong: ',class_getInstanceSize(TDerivedNSSecureTextField),' <> ',class_getInstanceSize(NSSecureTextField)+1);
 if class_getInstanceSize(TDerivedNSSecureTextFieldCell) <> (class_getInstanceSize(NSSecureTextFieldCell)+1) then
 writeln('size of NSSecureTextFieldCell is wrong: ',class_getInstanceSize(TDerivedNSSecureTextFieldCell),' <> ',class_getInstanceSize(NSSecureTextFieldCell)+1);
 if class_getInstanceSize(TDerivedNSSegmentedControl) <> (class_getInstanceSize(NSSegmentedControl)+1) then
 writeln('size of NSSegmentedControl is wrong: ',class_getInstanceSize(TDerivedNSSegmentedControl),' <> ',class_getInstanceSize(NSSegmentedControl)+1);
 if class_getInstanceSize(TDerivedNSShadow) <> (class_getInstanceSize(NSShadow)+1) then
 writeln('size of NSShadow is wrong: ',class_getInstanceSize(TDerivedNSShadow),' <> ',class_getInstanceSize(NSShadow)+1);
 if class_getInstanceSize(TDerivedNSSlider) <> (class_getInstanceSize(NSSlider)+1) then
 writeln('size of NSSlider is wrong: ',class_getInstanceSize(TDerivedNSSlider),' <> ',class_getInstanceSize(NSSlider)+1);
 if class_getInstanceSize(TDerivedNSSliderCell) <> (class_getInstanceSize(NSSliderCell)+1) then
 writeln('size of NSSliderCell is wrong: ',class_getInstanceSize(TDerivedNSSliderCell),' <> ',class_getInstanceSize(NSSliderCell)+1);
 if class_getInstanceSize(TDerivedNSSound) <> (class_getInstanceSize(NSSound)+1) then
 writeln('size of NSSound is wrong: ',class_getInstanceSize(TDerivedNSSound),' <> ',class_getInstanceSize(NSSound)+1);
 if class_getInstanceSize(TDerivedNSSpeechRecognizer) <> (class_getInstanceSize(NSSpeechRecognizer)+1) then
 writeln('size of NSSpeechRecognizer is wrong: ',class_getInstanceSize(TDerivedNSSpeechRecognizer),' <> ',class_getInstanceSize(NSSpeechRecognizer)+1);
 if class_getInstanceSize(TDerivedNSSpeechSynthesizer) <> (class_getInstanceSize(NSSpeechSynthesizer)+1) then
 writeln('size of NSSpeechSynthesizer is wrong: ',class_getInstanceSize(TDerivedNSSpeechSynthesizer),' <> ',class_getInstanceSize(NSSpeechSynthesizer)+1);
 if class_getInstanceSize(TDerivedNSSpellChecker) <> (class_getInstanceSize(NSSpellChecker)+1) then
 writeln('size of NSSpellChecker is wrong: ',class_getInstanceSize(TDerivedNSSpellChecker),' <> ',class_getInstanceSize(NSSpellChecker)+1);
 if class_getInstanceSize(TDerivedNSSplitView) <> (class_getInstanceSize(NSSplitView)+1) then
 writeln('size of NSSplitView is wrong: ',class_getInstanceSize(TDerivedNSSplitView),' <> ',class_getInstanceSize(NSSplitView)+1);
 if class_getInstanceSize(TDerivedNSStatusBar) <> (class_getInstanceSize(NSStatusBar)+1) then
 writeln('size of NSStatusBar is wrong: ',class_getInstanceSize(TDerivedNSStatusBar),' <> ',class_getInstanceSize(NSStatusBar)+1);
 if class_getInstanceSize(TDerivedNSStatusItem) <> (class_getInstanceSize(NSStatusItem)+1) then
 writeln('size of NSStatusItem is wrong: ',class_getInstanceSize(TDerivedNSStatusItem),' <> ',class_getInstanceSize(NSStatusItem)+1);
 if class_getInstanceSize(TDerivedNSStepper) <> (class_getInstanceSize(NSStepper)+1) then
 writeln('size of NSStepper is wrong: ',class_getInstanceSize(TDerivedNSStepper),' <> ',class_getInstanceSize(NSStepper)+1);
 if class_getInstanceSize(TDerivedNSStepperCell) <> (class_getInstanceSize(NSStepperCell)+1) then
 writeln('size of NSStepperCell is wrong: ',class_getInstanceSize(TDerivedNSStepperCell),' <> ',class_getInstanceSize(NSStepperCell)+1);
 if class_getInstanceSize(TDerivedNSTableColumn) <> (class_getInstanceSize(NSTableColumn)+1) then
 writeln('size of NSTableColumn is wrong: ',class_getInstanceSize(TDerivedNSTableColumn),' <> ',class_getInstanceSize(NSTableColumn)+1);
 if class_getInstanceSize(TDerivedNSTableHeaderCell) <> (class_getInstanceSize(NSTableHeaderCell)+1) then
 writeln('size of NSTableHeaderCell is wrong: ',class_getInstanceSize(TDerivedNSTableHeaderCell),' <> ',class_getInstanceSize(NSTableHeaderCell)+1);
 if class_getInstanceSize(TDerivedNSTableHeaderView) <> (class_getInstanceSize(NSTableHeaderView)+1) then
 writeln('size of NSTableHeaderView is wrong: ',class_getInstanceSize(TDerivedNSTableHeaderView),' <> ',class_getInstanceSize(NSTableHeaderView)+1);
 if class_getInstanceSize(TDerivedNSTableView) <> (class_getInstanceSize(NSTableView)+1) then
 writeln('size of NSTableView is wrong: ',class_getInstanceSize(TDerivedNSTableView),' <> ',class_getInstanceSize(NSTableView)+1);
 if class_getInstanceSize(TDerivedNSTabView) <> (class_getInstanceSize(NSTabView)+1) then
 writeln('size of NSTabView is wrong: ',class_getInstanceSize(TDerivedNSTabView),' <> ',class_getInstanceSize(NSTabView)+1);
 if class_getInstanceSize(TDerivedNSTabViewItem) <> (class_getInstanceSize(NSTabViewItem)+1) then
 writeln('size of NSTabViewItem is wrong: ',class_getInstanceSize(TDerivedNSTabViewItem),' <> ',class_getInstanceSize(NSTabViewItem)+1);
 if class_getInstanceSize(TDerivedNSText) <> (class_getInstanceSize(NSText)+1) then
 writeln('size of NSText is wrong: ',class_getInstanceSize(TDerivedNSText),' <> ',class_getInstanceSize(NSText)+1);
 if class_getInstanceSize(TDerivedNSTextAttachmentCell) <> (class_getInstanceSize(NSTextAttachmentCell)+1) then
 writeln('size of NSTextAttachmentCell is wrong: ',class_getInstanceSize(TDerivedNSTextAttachmentCell),' <> ',class_getInstanceSize(NSTextAttachmentCell)+1);
 if class_getInstanceSize(TDerivedNSTextAttachment) <> (class_getInstanceSize(NSTextAttachment)+1) then
 writeln('size of NSTextAttachment is wrong: ',class_getInstanceSize(TDerivedNSTextAttachment),' <> ',class_getInstanceSize(NSTextAttachment)+1);
 if class_getInstanceSize(TDerivedNSTextContainer) <> (class_getInstanceSize(NSTextContainer)+1) then
 writeln('size of NSTextContainer is wrong: ',class_getInstanceSize(TDerivedNSTextContainer),' <> ',class_getInstanceSize(NSTextContainer)+1);
 if class_getInstanceSize(TDerivedNSTextField) <> (class_getInstanceSize(NSTextField)+1) then
 writeln('size of NSTextField is wrong: ',class_getInstanceSize(TDerivedNSTextField),' <> ',class_getInstanceSize(NSTextField)+1);
 if class_getInstanceSize(TDerivedNSTextFieldCell) <> (class_getInstanceSize(NSTextFieldCell)+1) then
 writeln('size of NSTextFieldCell is wrong: ',class_getInstanceSize(TDerivedNSTextFieldCell),' <> ',class_getInstanceSize(NSTextFieldCell)+1);
 if class_getInstanceSize(TDerivedNSTextList) <> (class_getInstanceSize(NSTextList)+1) then
 writeln('size of NSTextList is wrong: ',class_getInstanceSize(TDerivedNSTextList),' <> ',class_getInstanceSize(NSTextList)+1);
 if class_getInstanceSize(TDerivedNSTextStorage) <> (class_getInstanceSize(NSTextStorage)+1) then
 writeln('size of NSTextStorage is wrong: ',class_getInstanceSize(TDerivedNSTextStorage),' <> ',class_getInstanceSize(NSTextStorage)+1);
 if class_getInstanceSize(TDerivedNSTextBlock) <> (class_getInstanceSize(NSTextBlock)+1) then
 writeln('size of NSTextBlock is wrong: ',class_getInstanceSize(TDerivedNSTextBlock),' <> ',class_getInstanceSize(NSTextBlock)+1);
 if class_getInstanceSize(TDerivedNSTextTableBlock) <> (class_getInstanceSize(NSTextTableBlock)+1) then
 writeln('size of NSTextTableBlock is wrong: ',class_getInstanceSize(TDerivedNSTextTableBlock),' <> ',class_getInstanceSize(NSTextTableBlock)+1);
 if class_getInstanceSize(TDerivedNSTextTable) <> (class_getInstanceSize(NSTextTable)+1) then
 writeln('size of NSTextTable is wrong: ',class_getInstanceSize(TDerivedNSTextTable),' <> ',class_getInstanceSize(NSTextTable)+1);
 if class_getInstanceSize(TDerivedNSTextView) <> (class_getInstanceSize(NSTextView)+1) then
 writeln('size of NSTextView is wrong: ',class_getInstanceSize(TDerivedNSTextView),' <> ',class_getInstanceSize(NSTextView)+1);
 if class_getInstanceSize(TDerivedNSTokenField) <> (class_getInstanceSize(NSTokenField)+1) then
 writeln('size of NSTokenField is wrong: ',class_getInstanceSize(TDerivedNSTokenField),' <> ',class_getInstanceSize(NSTokenField)+1);
 if class_getInstanceSize(TDerivedNSTokenFieldCell) <> (class_getInstanceSize(NSTokenFieldCell)+1) then
 writeln('size of NSTokenFieldCell is wrong: ',class_getInstanceSize(TDerivedNSTokenFieldCell),' <> ',class_getInstanceSize(NSTokenFieldCell)+1);
 if class_getInstanceSize(TDerivedNSToolbar) <> (class_getInstanceSize(NSToolbar)+1) then
 writeln('size of NSToolbar is wrong: ',class_getInstanceSize(TDerivedNSToolbar),' <> ',class_getInstanceSize(NSToolbar)+1);
 if class_getInstanceSize(TDerivedNSToolbarItem) <> (class_getInstanceSize(NSToolbarItem)+1) then
 writeln('size of NSToolbarItem is wrong: ',class_getInstanceSize(TDerivedNSToolbarItem),' <> ',class_getInstanceSize(NSToolbarItem)+1);
 if class_getInstanceSize(TDerivedNSToolbarItemGroup) <> (class_getInstanceSize(NSToolbarItemGroup)+1) then
 writeln('size of NSToolbarItemGroup is wrong: ',class_getInstanceSize(TDerivedNSToolbarItemGroup),' <> ',class_getInstanceSize(NSToolbarItemGroup)+1);
 if class_getInstanceSize(TDerivedNSTrackingArea) <> (class_getInstanceSize(NSTrackingArea)+1) then
 writeln('size of NSTrackingArea is wrong: ',class_getInstanceSize(TDerivedNSTrackingArea),' <> ',class_getInstanceSize(NSTrackingArea)+1);
 if class_getInstanceSize(TDerivedNSTreeController) <> (class_getInstanceSize(NSTreeController)+1) then
 writeln('size of NSTreeController is wrong: ',class_getInstanceSize(TDerivedNSTreeController),' <> ',class_getInstanceSize(NSTreeController)+1);
 if class_getInstanceSize(TDerivedNSTreeNode) <> (class_getInstanceSize(NSTreeNode)+1) then
 writeln('size of NSTreeNode is wrong: ',class_getInstanceSize(TDerivedNSTreeNode),' <> ',class_getInstanceSize(NSTreeNode)+1);
 if class_getInstanceSize(TDerivedNSTypesetter) <> (class_getInstanceSize(NSTypesetter)+1) then
 writeln('size of NSTypesetter is wrong: ',class_getInstanceSize(TDerivedNSTypesetter),' <> ',class_getInstanceSize(NSTypesetter)+1);
 if class_getInstanceSize(TDerivedNSUserDefaultsController) <> (class_getInstanceSize(NSUserDefaultsController)+1) then
 writeln('size of NSUserDefaultsController is wrong: ',class_getInstanceSize(TDerivedNSUserDefaultsController),' <> ',class_getInstanceSize(NSUserDefaultsController)+1);
 if class_getInstanceSize(TDerivedNSView) <> (class_getInstanceSize(NSView)+1) then
 writeln('size of NSView is wrong: ',class_getInstanceSize(TDerivedNSView),' <> ',class_getInstanceSize(NSView)+1);
 if class_getInstanceSize(TDerivedNSViewController) <> (class_getInstanceSize(NSViewController)+1) then
 writeln('size of NSViewController is wrong: ',class_getInstanceSize(TDerivedNSViewController),' <> ',class_getInstanceSize(NSViewController)+1);
 if class_getInstanceSize(TDerivedNSWindow) <> (class_getInstanceSize(NSWindow)+1) then
 writeln('size of NSWindow is wrong: ',class_getInstanceSize(TDerivedNSWindow),' <> ',class_getInstanceSize(NSWindow)+1);
 if class_getInstanceSize(TDerivedNSWindowController) <> (class_getInstanceSize(NSWindowController)+1) then
 writeln('size of NSWindowController is wrong: ',class_getInstanceSize(TDerivedNSWindowController),' <> ',class_getInstanceSize(NSWindowController)+1);
 if class_getInstanceSize(TDerivedNSWorkspace) <> (class_getInstanceSize(NSWorkspace)+1) then
 writeln('size of NSWorkspace is wrong: ',class_getInstanceSize(TDerivedNSWorkspace),' <> ',class_getInstanceSize(NSWorkspace)+1);
end;
begin
 PrintGlue1;
end.
