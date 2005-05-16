{
    $Id: constse.inc,v 1.2 2005/02/14 17:13:11 peter Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
  SAssignError = '%s can not be assigned to %s';
  SFCreateError = 'File %s can not be created';
  SFOpenError = 'File %s can not be opened';
  SReadError = 'Stream read error';
  SWriteError = 'Stream write error';
  SMemoryStreamError = 'Cannot expand memory stream';
  SCantWriteResourceStreamError = 'Can not write to read-only ResourceStream';
  SDuplicateReference = 'WriteObject was called twice for one instance';
  SClassNotFound = 'Class %s not found';
  SInvalidImage = 'Illegal stream image';
  SResNotFound = 'Resource %s not found';
  SClassMismatch = 'Resource %s has wrong class';
  SListIndexError = 'List index exceeds bounds (%d)';
  SListCapacityError = 'The maximum list capacity is reached (%d)';
  SListCountError = 'List count too large (%d)';
  SSortedListError = 'Operation not allowed on sorted StringLists';
  SDuplicateString = 'Duplicate entries not allowed in StringList';
  SInvalidTabIndex = 'Registerindex out of bounds';
  SDuplicateName = 'A Component with name %s exists already';
  SInvalidName = '"%s" is not a valid identifier name';
  SDuplicateClass = 'A class with name %s exists already';
  SNoComSupport = '%s is not registered as COM Class';
  SLineTooLong = 'Line too long';
  SRangeError = 'Range error';
  SSeekNotImplemented = '64bit Seek not implemented for class %s';
  SErrNoStreaming = 'Failed to initialize component: No streaming method available.';

  SInvalidPropertyValue = 'Invalid property value';
  SInvalidPropertyPath = 'Invalid property path';
  SUnknownProperty = 'Unknown property';
  SReadOnlyProperty = 'Read-only property';
  SUnknownPropertyType = 'Unknown property type %d';
  SPropertyException = 'Error while reading %s%s%s: %s';
  SAncestorNotFound = 'Ancestor of ''%s'' not found';
  SInvalidBitmap = 'Invalid Bitmap';
  SInvalidIcon = 'Invalid Icon';
  SInvalidMetafile = 'Invalid Metafile';
  SInvalidPixelFormat = 'Invalid Pixelformat';
  SBitmapEmpty = 'Bitmap is empty';
  SScanLine = 'Line index out of bounds';
  SChangeIconSize = 'Can not change icon size';
  SOleGraphic = 'Invalid operation for TOleGraphic';
  SUnknownExtension = 'Unknown extension (.%s)';
  SUnknownClipboardFormat = 'Unknown clipboard format';
  SOutOfResources = 'Out of system resources';
  SNoCanvasHandle = 'Canvas handle does not allow drawing';
  SInvalidImageSize = 'Invalid image size';
  STooManyImages = 'Too many images';
  SDimsDoNotMatch = 'Image size mismatch';
  SInvalidImageList = 'Invalid ImageList';
  SReplaceImage = 'Image can not be replaced';
  SImageIndexError = 'Invalid ImageList index';
  SImageReadFail = 'The ImageList data could not be read from stream';
  SImageWriteFail = 'The ImageList data could not be written to stream';
  SWindowDCError = 'Error when??';
  SClientNotSet = 'Client of TDrag was not initialized';
  SWindowClass = 'Error when initializing Window Class';
  SWindowCreate = 'Error when creating Window';
  SCannotFocus = 'A disbled or invisible Window cannot get focus';
  SParentRequired = 'Element ''%s'' has no parent Window';
  SMDIChildNotVisible = 'A MDI-Child Windows can not be hidden.';
  SVisibleChanged = 'Visible property cannot be changed in OnShow or OnHide handlers';
  SCannotShowModal = 'A visible Window can not be made modal';
  SScrollBarRange = 'Scrollbar property out of range';
  SPropertyOutOfRange = 'Property %s out of range';
  SMenuIndexError = 'Menu Index out of range';
  SMenuReinserted = 'Menu reinserted';
  SMenuNotFound = 'Menu entry not found in menu';
  SNoTimers = 'No timers available';
  SNotPrinting = 'Printer is not printing';
  SPrinting = 'Printer is busy';
  SPrinterIndexError = 'PrinterIndex out of range';
  SInvalidPrinter = 'Selected printer is invalid';
  SDeviceOnPort = '%s on %s';
  SGroupIndexTooLow = 'GroupIndex must be greater than preceding menu groupindex';
  STwoMDIForms = 'There is only one MDI window available';
  SNoMDIForm = 'No MDI form is available, none is active';
  SRegisterError = 'Invalid registry';
  SImageCanvasNeedsBitmap = 'A Canvas can only be changedif it contains a bitmap';
  SControlParentSetToSelf = 'A component can not have itself as parent';
  SOKButton = 'OK';
  SCancelButton = 'Cancel';
  SYesButton = '&Yes';
  SNoButton = '&No';
  SHelpButton = '&Help';
  SCloseButton = '&Close';
  SIgnoreButton = '&Ignore';
  SRetryButton = '&Retry';
  SAbortButton = 'Abort';
  SAllButton = '&All';

  SFB = 'VH';
  SFG = 'VG';
  SBG = 'HG';
  SOldTShape = 'Can not load older version of TShape';
  SVMetafiles = 'MetaFiles';
  SVEnhMetafiles = 'Enhanced MetaFiles';
  SVIcons = 'Icons';
  SVBitmaps = 'Bitmaps';
  SGridTooLarge = 'Grid to large for this operation';
  STooManyDeleted = 'Zu viele Zeilen oder Spalten gelöscht';
  SIndexOutOfRange = 'Grid index out of range';
  SFixedColTooBig = 'The number of fixed Columns must be less than the Column count';
  SFixedRowTooBig = 'The number of fixed Rows must be less that the Row count';
  SInvalidStringGridOp = 'Es können keine Zeilen des ''Grids'' gelöscht oder eingefügt werden';
  SParseError = '%s in Line %d';

  SIdentifierExpected = 'Identifier expected';
  SStringExpected = 'String expected';
  SNumberExpected = 'Number expected';

  SCharExpected = '%s expected';

  SSymbolExpected = '%s expected';

  SInvalidNumber = 'Invalid numerical value';
  SInvalidString = 'Invalid string constant';
  SInvalidProperty = 'Invalid property value';
  SInvalidBinary = 'Invalid binary';
  SOutlineIndexError = 'Node index not found';
  SOutlineExpandError = 'Parent node must be expanded';
  SInvalidCurrentItem = 'Invalid item';
  SMaskErr = 'Invalid mask';
  SMaskEditErr = 'Invalid mask. Use the ESC-key to undo changes.';
  SOutlineError = 'Invalid Node index';
  SOutlineBadLevel = '???';
  SOutlineSelection = 'Ungültige Auswahl';
  SOutlineFileLoad = 'Fehler beim Dateiladen';
  SOutlineLongLine = 'Zeile zu lang';
  SOutlineMaxLevels = 'Maximale Gliederungstiefe überschritten';

  SMsgDlgWarning = 'Warning';
  SMsgDlgError = 'Error';
  SMsgDlgInformation = 'Information';
  SMsgDlgConfirm = 'Confirm';
  SMsgDlgYes = '&Yes';
  SMsgDlgNo = '&No';
  SMsgDlgOK = 'OK';
  SMsgDlgCancel = 'Cancel';
  SMsgDlgHelp = '&Help';
  SMsgDlgHelpNone = 'No help available';
  SMsgDlgHelpHelp = 'Help';
  SMsgDlgAbort = '&Abort';
  SMsgDlgRetry = '&Retry';
  SMsgDlgIgnore = '&Ignore';
  SMsgDlgAll = '&All';
  SMsgDlgNoToAll = 'N&o to all';
  SMsgDlgYesToAll = 'Yes to A&lle';

  SmkcBkSp = 'Backspace';
  SmkcTab = 'Tab';
  SmkcEsc = 'Esc';
  SmkcEnter = 'Enter';
  SmkcSpace = 'Space';
  SmkcPgUp = 'Page up';
  SmkcPgDn = 'Page down';
  SmkcEnd = 'End';
  SmkcHome = 'Home';
  SmkcLeft = 'Left';
  SmkcUp = 'Up';
  SmkcRight = 'Right';
  SmkcDown = 'Down';
  SmkcIns = 'Insert';
  SmkcDel = 'Delete';
  SmkcShift = 'Shift+';
  SmkcCtrl = 'Ctrl+';
  SmkcAlt = 'Alt+';

  srUnknown = '(Ukknown)';
  srNone = '(Empty)';
  SOutOfRange = 'Value must be between %d and %d';
  SCannotCreateName = 'Cannot use standard name for and unknown component';

  SDateEncodeError = 'Ungültiges Argument für Datumskodierung';
  STimeEncodeError = 'Ungültiges Argument für Zeitkodierung';
  SInvalidDate = '''''%s'''' ist kein gültiges Datum';
  SInvalidTime = '''''%s'''' ist keine gültige Zeit';
  SInvalidDateTime = '''''%s'''' ist kein gültiges Datum und Zeit';
  SInsertLineError = 'Zeile kann nicht eingefügt werden';

  SCannotDragForm = 'Formulare können nicht gezogen werden';
  SPutObjectError = 'PutObject auf undefiniertes Element';
  SCardDLLNotLoaded = 'CARDS.DLL kann nicht geladen werden';
  SDuplicateCardId = 'Doppelte CardId gefunden';

  SDdeErr = 'Ein Fehler wurde von der DDE zurückgeliefert  ($0%x)';
  SDdeConvErr = 'DDE Fehler - Konversation wurde nicht hergestellt ($0%x)';
  SDdeMemErr = 'Fehler trat auf, da unzureichender Speicher für DDE ($0%x)';
  SDdeNoConnect = 'DDE-Konversation kann nicht eingerichtet werden';


  SDefaultFilter = 'Alle Dateien (*.*)|*.*';
  sAllFilter = 'Alle Dateien';
  SNoVolumeLabel = ': [ - Ohne Namen - ]';

  SConfirmCreateDir = 'Das angegebene Verzeichnis existiert nicht. Soll es angelegt werden?';
  SSelectDirCap = 'Verzeichnis auswählen';
  SCannotCreateDir = 'Das Verzeichnis kann nicht erstellt werden';
  SDirNameCap = 'Verzeichnis&name:';
  SDrivesCap = '&Laufwerke:';
  SDirsCap = '&Verzeichnisse:';
  SFilesCap = '&Dateien: (*.*)';
  SNetworkCap = 'Ne&tzwerk...';

  SColorPrefix = 'Farbe';
  SColorTags = 'ABCDEFGHIJKLMNOP';

  SInvalidClipFmt = 'Ungültiges Format der Zwischenablage';
  SIconToClipboard = 'Zwischenablage unterstützt keine Symbole';

  SDefault = 'Vorgabe';

  SInvalidMemoSize = 'Text überschreitet Memo-Kapazität';
  SCustomColors = 'Selbstdefinierte Farben';
  SInvalidPrinterOp = 'Operation auf ausgewähltem Drucker nicht verfügbar';
  SNoDefaultPrinter = 'Zur Zeit ist kein Standard-Drucker gewählt';

  SIniFileWriteError = 'nach %s kann nicht geschrieben werden';

  SBitsIndexError = 'Bits-Index außerhalb des zulässigen Bereichs';

  SUntitled = '(Unbenannt)';

  SInvalidRegType = 'Ungültiger Datentyp für ''%s''';
  SRegCreateFailed = 'Erzeugung von Schlüssel %s misslungen';
  SRegSetDataFailed = 'Konnte Daten für ''%s'' nicht setzen';
  SRegGetDataFailed = 'Konnte Daten für ''%s'' nicht holen';

  SUnknownConversion = 'Unbekannte Dateierweiterung für RichEdit-Konvertierung (.%s)';
  SDuplicateMenus = 'Menü ''%s'' wird bereits von einem anderen Formular benutzt';

  SPictureLabel = 'Bild:';
  SPictureDesc = ' (%dx%d)';
  SPreviewLabel = 'Vorschau';

  SCannotOpenAVI = 'AVI kann nicht geöffnet werden';

  SNotOpenErr = 'Kein MCI-Gerät geöffnet';
  SMPOpenFilter = 'Alle Dateien (*.*)|*.*|Wave-Dateien (*.WAV)|*.WAV|Midi-Dateien (*.MID)|*.MID|Video für Windows (*.avi)|*.avi';
  SMCINil = '';
  SMCIAVIVideo = 'AVIVideo';
  SMCICDAudio = 'CDAudio';
  SMCIDAT = 'DAT';
  SMCIDigitalVideo = 'DigitalVideo';
  SMCIMMMovie = 'MMMovie';
  SMCIOther = 'Andere';
  SMCIOverlay = 'Overlay';
  SMCIScanner = 'Scanner';
  SMCISequencer = 'Sequencer';
  SMCIVCR = 'VCR';
  SMCIVideodisc = 'Videodisc';
  SMCIWaveAudio = 'WaveAudio';
  SMCIUnknownError = 'Unbekannter Fehlercode';

  SBoldItalicFont = 'Fett kursiv';
  SBoldFont = 'Fett';
  SItalicFont = 'Kursiv';
  SRegularFont = 'Normal';

  SPropertiesVerb = 'Eigenschaften';

{
  $Log: constse.inc,v $
  Revision 1.2  2005/02/14 17:13:11  peter
    * truncate log

}
