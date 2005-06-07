{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
  SAssignError = '%s kann nicht zu  %s zugewiesen werden';
  SFCreateError = 'Datei %s kann nicht erstellt werden';
  SFOpenError = 'Datei %s kann nicht geöffnet werden';
  SReadError = 'Stream-Read-Fehler';
  SWriteError = 'Stream-Write-Fehler';
  SMemoryStreamError = 'Expandieren des Speicher-Stream wegen Speichermangel nicht möglich';
  SCantWriteResourceStreamError = 'In einen zum Lesen geöffneten Ressourcen-Stream kann nicht geschrieben werden';
  SDuplicateReference = 'Zweimaliger Aufruf von WriteObject für die gleiche Instanz';
  SClassNotFound = 'Klasse %s nicht gefunden';
  SInvalidImage = 'Ungültiges Stream-Format';
  SResNotFound = 'Ressource %s nicht gefunden';
  SClassMismatch = 'Ressource %s hat die falsche Klasse';
  SListIndexError = 'Der Index der Liste überschreitet das Maximum (%d)';
  SListCapacityError = 'Die Kapazität der Liste ist erschöpft (%d)';
  SListCountError = 'Zu viele Einträge in der Liste (%d)';
  SSortedListError = 'Operation bei sortierten Stringlisten nicht erlaubt';
  SDuplicateString = 'In der Stringliste sind Duplikate nicht erlaubt';
  SInvalidTabIndex = 'Registerindex außerhalb des zulässigen Bereichs';
  SDuplicateName = 'Eine Komponente mit der Bezeichnung %s existiert bereits';
  SInvalidName = '''''%s'''' ist kein gültiger Komponentenname';
  SDuplicateClass = 'Eine Klasse mit der Bezeichnung %s existiert bereits';
  SNoComSupport = '%s wurde nicht als COM-Klasse registriert';
  SInvalidInteger = '''''%s'''' ist kein gültiger Integerwert';
  SLineTooLong = 'Zeile zu lang';

  SInvalidPropertyValue = 'Ungültiger Wert der Eigenschaft';
  SInvalidPropertyPath = 'Ungültiger Pfad für Eigenschaft';
  SUnknownProperty = 'Eigenschaft existiert nicht';
  SReadOnlyProperty = 'Eigenschaft kann nur gelesen werden';
  SUnknownPropertyType = 'Unbekannter Eigenschaftstyp %d';
  SPropertyException = 'Fehler beim Lesen von %s%s: %s';
  SAncestorNotFound = 'Vorfahr für ''%s'' nicht gefunden';
  SInvalidBitmap = 'Bitmap ist ungültig';
  SInvalidIcon = 'Ungültiges Symbol';
  SInvalidMetafile = 'Metadatei ist ungültig';
  SInvalidPixelFormat = 'Ungültiges Pixelformat';
  SBitmapEmpty = 'Bitmap ist leer';
  SScanLine = 'Bereichsüberschreitung bei Zeilenindex';
  SChangeIconSize = 'Die Größe eines Symbols kann nicht geändert werden';
  SOleGraphic = 'Ungültige Operation für TOleGraphic';
  SUnknownExtension = 'Unbekannte Bilddateierweiterung (.%s)';
  SUnknownClipboardFormat = 'Format der Zwischenablage wird nicht unterstützt';
  SOutOfResources = 'Systemressourcen erschöpft.';
  SNoCanvasHandle = 'Leinwand/Bild erlaubt kein Zeichnen';
  SInvalidImageSize = 'Ungültige Bildgröße';
  STooManyImages = 'Zu viele Bilder';
  SDimsDoNotMatch = 'Bildgröße und Bildlistengröße stimmen nicht überein';
  SInvalidImageList = 'Ungültige ImageList';
  SReplaceImage = 'Bild kann nicht ersetzt werden';
  SImageIndexError = 'Ungültiger ImageList-Index';
  SImageReadFail = 'Die ImageList-Daten konnten nicht aus dem Stream gelesen werden';
  SImageWriteFail = 'Die ImageList-Daten konnten nicht in den Stream geschrieben werden';
  SWindowDCError = 'Fehler beim Erstellen des Fenster-Gerätekontexts';
  SClientNotSet = 'Client von TDrag wurde nicht initialisiert';
  SWindowClass = 'Fehler beim Erzeugen einer Fensterklasse';
  SWindowCreate = 'Fehler beim Erzeugen eines Fensters';
  SCannotFocus = 'Ein deaktiviertes oder unsichtbares Fenster kann nicht den Fokus erhalten';
  SParentRequired = 'Element ''%s'' hat kein übergeordnetes Fenster';
  SMDIChildNotVisible = 'Ein MDI-Kindformular kann nicht verborgen werden';
  SVisibleChanged = 'Eigenschaft Visible kann in OnShow oder OnHide nicht verändert werden';
  SCannotShowModal = 'Aus einem sichtbaren Fenster kann kein modales gemacht werden';
  SScrollBarRange = 'Eigenschaft Scrollbar außerhalb des zulässigen Bereichs';
  SPropertyOutOfRange = 'Eigenschaft %s außerhalb des gültigen Bereichs';
  SMenuIndexError = 'Menüindex außerhalb des zulässigen Bereichs';
  SMenuReinserted = 'Menü zweimal eingefügt';
  SMenuNotFound = 'Untermenü ist nicht im Menü';
  SNoTimers = 'Nicht genügend Timer verfügbar';
  SNotPrinting = 'Der Drucker ist nicht am Drucken';
  SPrinting = 'Das Drucken ist im Gang';
  SPrinterIndexError = 'Druckerindex außerhalb des zulässigen Bereichs';
  SInvalidPrinter = 'Ausgewählter Drucker ist ungültig';
  SDeviceOnPort = '%s an %s';
  SGroupIndexTooLow = 'GroupIndex kann nicht kleiner sein als der GroupIndex eines vorhergehenden Menüelementes';
  STwoMDIForms = 'Es ist nur ein MDI-Formular pro Anwendung möglich';
  SNoMDIForm = 'Formular kann nicht erstellt werden. Zur Zeit sind keine MDI-Formulare aktiv';
  SRegisterError = 'Ungültige Komponentenregistrierung';
  SImageCanvasNeedsBitmap = 'Ein Bild kann nur geändert werden, wenn es ein Bitmap enthält';
  SControlParentSetToSelf = 'Ein Steuerelement kann nicht sich selbst als Vorfahr haben';
  SOKButton = 'OK';
  SCancelButton = 'Abbrechen';
  SYesButton = '&Ja';
  SNoButton = '&Nein';
  SHelpButton = '&Hilfe';
  SCloseButton = '&Schließen';
  SIgnoreButton = '&Ignorieren';
  SRetryButton = '&Wiederholen';
  SAbortButton = 'Abbruch';
  SAllButton = '&Alles';

  SFB = 'VH';
  SFG = 'VG';
  SBG = 'HG';
  SOldTShape = 'Kann ältere Version von TShape nicht laden';
  SVMetafiles = 'Metadateien';
  SVEnhMetafiles = 'Erweiterte Metadateien';
  SVIcons = 'Symbole';
  SVBitmaps = 'Bitmaps';
  SGridTooLarge = 'Gitter zu groß für Operation';
  STooManyDeleted = 'Zu viele Zeilen oder Spalten gelöscht';
  SIndexOutOfRange = 'Gitterindex außerhalb des zulässigen Bereichs';
  SFixedColTooBig = 'Die Anzahl fester Spalten muß kleiner sein als die Spaltenanzahl';
  SFixedRowTooBig = 'Die Anzahl fester Zeilen muß kleiner sein als die Zeilenanzahl';
  SInvalidStringGridOp = 'Es können keine Zeilen des ''Grids'' gelöscht oder eingefügt werden';
  SParseError = '%s in Zeile %d';

  SIdentifierExpected = 'Bezeichner erwartet';
  SStringExpected = 'String erwartet';
  SNumberExpected = 'Zahl erwartet';

  SCharExpected = '%s erwartet';

  SSymbolExpected = '%s erwartet';

  SInvalidNumber = 'Ungültiger numerischer Wert';
  SInvalidString = 'Ungültige Stringkonstante';
  SInvalidProperty = 'Ungültiger Wert der Eigenschaft';
  SInvalidBinary = 'Ungültiger Binärwert';
  SOutlineIndexError = 'Gliederungsindex nicht gefunden';
  SOutlineExpandError = 'Elternknoten muß expandiert sein';
  SInvalidCurrentItem = 'Ungültiger Wert';
  SMaskErr = 'Ungültiger Eingabewert';
  SMaskEditErr = 'Ungültiger Eingabewert. Benutzen Sie die ESC-Taste, um die Änderungen rückgängig zu machen.';
  SOutlineError = 'Ungültiger Gliederungsindex';
  SOutlineBadLevel = 'Ungültige Zuweisung von Ebenen';
  SOutlineSelection = 'Ungültige Auswahl';
  SOutlineFileLoad = 'Fehler beim Dateiladen';
  SOutlineLongLine = 'Zeile zu lang';
  SOutlineMaxLevels = 'Maximale Gliederungstiefe überschritten';

  SMsgDlgWarning = 'Warnung';
  SMsgDlgError = 'Fehler';
  SMsgDlgInformation = 'Information';
  SMsgDlgConfirm = 'Bestätigen';
  SMsgDlgYes = '&Ja';
  SMsgDlgNo = '&Nein';
  SMsgDlgOK = 'OK';
  SMsgDlgCancel = 'Abbrechen';
  SMsgDlgHelp = '&Hilfe';
  SMsgDlgHelpNone = 'Keine Hilfe verfügbar';
  SMsgDlgHelpHelp = 'Hilfe';
  SMsgDlgAbort = '&Abbrechen';
  SMsgDlgRetry = '&Wiederholen';
  SMsgDlgIgnore = '&Ignorieren';
  SMsgDlgAll = '&Alles';
  SMsgDlgNoToAll = '&Alle Nein';
  SMsgDlgYesToAll = 'A&lle Ja';

  SmkcBkSp = 'Rück';
  SmkcTab = 'Tab';
  SmkcEsc = 'Esc';
  SmkcEnter = 'Enter';
  SmkcSpace = 'Leertaste';
  SmkcPgUp = 'BildAuf';
  SmkcPgDn = 'BildAb';
  SmkcEnd = 'Ende';
  SmkcHome = 'Pos1';
  SmkcLeft = 'Linksbündig';
  SmkcUp = 'Nach oben';
  SmkcRight = 'Rechts';
  SmkcDown = 'Nach unten';
  SmkcIns = 'Einfg';
  SmkcDel = 'Entf';
  SmkcShift = 'Umsch+';
  SmkcCtrl = 'Strg+';
  SmkcAlt = 'Alt+';

  srUnknown = '(Unbekannt)';
  srNone = '(Leer)';
  SOutOfRange = 'Wert muß zwischen %d und %d liegen';
  SCannotCreateName = 'Für eine unbenannte Komponente kann kein Standard-Methodennamen erstellt werden';

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

