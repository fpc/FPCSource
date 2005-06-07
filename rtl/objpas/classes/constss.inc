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
  SAssignError = '%s no puede ser assignado a  %s';
  SFCreateError = 'Fichero %s no puede ser creado';
  SFOpenError = 'Fichero %s no puede ser abierto';
  SReadError = 'Error-Lectura-Stream';
  SWriteError = 'Error-Escritura-Stream';
  SMemoryStreamError = 'No es posible expandir Memoria Stream';
  SCantWriteResourceStreamError = 'No se puede escribir en un ResourceStream de solo lectura';
  SDuplicateReference = 'WriteObject fue llamado dos veces por una sola instancia';
  SClassNotFound = 'Clase %s no encontrada';
  SInvalidImage = 'Imagen stream ilegal';
  SResNotFound = 'No se encontro el resource %s';
  SClassMismatch = 'El resource %s tiene una clase erronea';
  SListIndexError = 'El indice de lista excede los limites (%d)';
  SListCapacityError = 'La maxima capacidad de lista a sido alcanzada (%d)';
  SListCountError = 'Contador de lista demasiado grande (%d)';
  SSortedListError = 'Operacion no permitida en StringLists ordenado';
  SDuplicateString = 'Entradas duplicadas no permitidas en StringList';
  SInvalidTabIndex = 'Registerindex fuera de limites';
  SDuplicateName = 'Un componente con el nombre %s existe actualmente';
  SInvalidName = '"%s" no es un nombre identificador valido';
  SDuplicateClass = 'Una Clase con el nombre %s existe actualmente';
  SNoComSupport = '%s no esta registrado como COM-Class';
  SLineTooLong = 'Linea demasiado larga';

  SInvalidPropertyValue = 'Valor de propiedad no valido';
  SInvalidPropertyPath = 'Path de propiedad no valido';
  SUnknownProperty = 'Propiedad desconocidad';
  SReadOnlyProperty = 'Propiedad de solo lectura';
{N}  SUnknownPropertyType = 'Unknown property type %d';
  SPropertyException = 'Error leyendo %s%s: %s';
{N}  SAncestorNotFound = 'Ancestor of ''%s'' not found.';
  SInvalidBitmap = 'Bitmap no valido';
  SInvalidIcon = 'Icono no valido';
  SInvalidMetafile = 'MetaFile no valido';
  SInvalidPixelFormat = 'PixelFormat no valido';
  SBitmapEmpty = 'El bitmap esta vacio';
  SScanLine = 'Indice de linea fuera de limites';
  SChangeIconSize = 'No se puede cambiar el tama¤o del icono';
  SOleGraphic = 'Operacion no valida para TOleGraphic';
  SUnknownExtension = 'Extension desconocida (.%s)';
  SUnknownClipboardFormat = 'Formato de Portapapeles desconocido';
  SOutOfResources = 'Recursos de sistema agotados';
  SNoCanvasHandle = 'El manejador Canvas no permite dibujar';
  SInvalidImageSize = 'Tama¤o de imagen no valido';
  STooManyImages = 'Demasiadas imagenes';
  SDimsDoNotMatch = 'El tama¤o de la imagen no coincide';
  SInvalidImageList = 'ImageList no valido';
  SReplaceImage = 'La imagen no puede ser reemplazada';
  SImageIndexError = 'ImageList-Index no valido';
  SImageReadFail = 'Los datos de ImageList no pueden ser leido desde Stream';
  SImageWriteFail = 'Los datos de ImageList no pueden ser escritos en Stream';
  SWindowDCError = 'Error cuando??';
  SClientNotSet = 'El cliente de TDrag no fue iniciado';
  SWindowClass = 'Error inicializando Window Class';
  SWindowCreate = 'Error creando una Ventana';
{?}  SCannotFocus = 'Una Ventana invisible or desactivada no puede obtener el foco';
  SParentRequired = 'El elemento ''%s'' no tiene una ventana padre';
  SMDIChildNotVisible = 'Una ventana MDI-Child no puede ser ocultada.';
  SVisibleChanged = 'Una propiedad visual no puede ser cambiada en el manejador OnShow o OnHide';
{?}  SCannotShowModal = 'Una Ventana visible no puede ser hecha modal';
  SScrollBarRange = 'Propiedad de Scrollbar fuera de limites';
  SPropertyOutOfRange = 'Propiedad %s fuera de limites';
  SMenuIndexError = 'Indice de menu fuera de rango';
  SMenuReinserted = 'Menu reinsertado';
  SMenuNotFound = 'Entrada de menu no encontra en menu';
  SNoTimers = 'No hay timers disponibles';
  SNotPrinting = 'La impresora no esta imprimiendo';
  SPrinting = 'La impresora esta ocupada';
  SPrinterIndexError = 'PrinterIndex fuera de rango';
  SInvalidPrinter = 'La impresora seleccionada no es valida';
  SDeviceOnPort = '%s en %s';
  SGroupIndexTooLow = 'GroupIndex tiene que ser mayor que el goupindex del menu predecesor';
  STwoMDIForms = 'Solo hay una ventana MDI disponible';
  SNoMDIForm = 'No hay ningun MDI form disponible, none esta activado';
  SRegisterError = 'Registro invalido';
  SImageCanvasNeedsBitmap = 'Un Canvas solo puede ser cambiado si contiene un bitmap';
  SControlParentSetToSelf = 'Un componente no puede tenerse a si mismo como padre';
  SOKButton = 'Aceptar';
  SCancelButton = 'Cancelar';
  SYesButton = '&Si';
  SNoButton = '&No';
  SHelpButton = '&Ayuda';
  SCloseButton = '&Cerrar';
  SIgnoreButton = '&Ignorar';
  SRetryButton = '&Reintentar';
  SAbortButton = 'Abortar';
  SAllButton = '&Todo';

{?}  SFB = 'VH';
{?}  SFG = 'VG';
{?}  SBG = 'HG';
  SOldTShape = 'No es posible cargar versiones antiguas de TShape';
  SVMetafiles = 'MetaFiles';
  SVEnhMetafiles = 'MetaFiles ampliados';
  SVIcons = 'Iconos';
  SVBitmaps = 'Bitmaps';
  SGridTooLarge = 'Malla demasiado grande para esta operacion';
{?}  STooManyDeleted = 'Zu viele Zeilen oder Spalten gelöscht';
  SIndexOutOfRange = 'Indice de malla fuera de rango';
  SFixedColTooBig = 'El numero de columnas fijas tiene que ser menor que el contador Column';
  SFixedRowTooBig = 'El numero de filas fijas tiene que ser menor que el contador Row';
{?}  SInvalidStringGridOp = 'Es können keine Zeilen des ''Grids'' gelöscht oder eingefügt werden';
  SParseError = '%s en Linia %d';

  SIdentifierExpected = 'Falta identificador';
  SStringExpected = 'Falta string';
  SNumberExpected = 'Falta numero';

  SCharExpected = 'Falta %s';

  SSymbolExpected = 'Falta %s';

  SInvalidNumber = 'Valor numerico no valido';
  SInvalidString = 'Constante string no valida';
  SInvalidProperty = 'Valor de propiedad no valido';
  SInvalidBinary = 'Binario no valido';
  SOutlineIndexError = 'Indice de nodo no encontrado';
  SOutlineExpandError = 'El nodo padre tiene que ser expandido';
  SInvalidCurrentItem = 'Item no valido';
  SMaskErr = 'Mascara no valida';
  SMaskEditErr = 'Mascara no valida. Usa la tecla ESC para deshacer los cambios.';
  SOutlineError = 'Indice de nodo no valido';
  SOutlineBadLevel = '???';
{?}  SOutlineSelection = 'Ungültige Auswahl';
{?}  SOutlineFileLoad = 'Fehler beim Dateiladen';
{?}  SOutlineLongLine = 'Zeile zu lang';
{?}  SOutlineMaxLevels = 'Maximale Gliederungstiefe überschritten';

  SMsgDlgWarning = 'Atencion';
  SMsgDlgError = 'Error';
  SMsgDlgInformation = 'Informacion';
  SMsgDlgConfirm = 'Confirmar';
  SMsgDlgYes = '&Si';
  SMsgDlgNo = '&No';
  SMsgDlgOK = 'Aceptar';
  SMsgDlgCancel = 'Cancelar';
  SMsgDlgHelp = '&Ayuda';
  SMsgDlgHelpNone = 'No hay ayuda disponible';
  SMsgDlgHelpHelp = 'Ayuda';
  SMsgDlgAbort = 'A&bortar';
  SMsgDlgRetry = '&Reintentar';
  SMsgDlgIgnore = '&Ignorar';
  SMsgDlgAll = '&Todo';
  SMsgDlgNoToAll = 'N&o a todo';
  SMsgDlgYesToAll = 'Si a To&do';

  SmkcBkSp = 'Backspace';
  SmkcTab = 'Tabulador';
  SmkcEsc = 'Esc';
  SmkcEnter = 'Intro';
  SmkcSpace = 'Espacio';
  SmkcPgUp = 'Pagina arriva';
  SmkcPgDn = 'Pagina abajo';
  SmkcEnd = 'Fin';
  SmkcHome = 'Inicio';
  SmkcLeft = 'Izquierda';
  SmkcUp = 'Arriba';
  SmkcRight = 'Derecha';
  SmkcDown = 'Abajo';
  SmkcIns = 'Insertar';
  SmkcDel = 'Suprimir';
  SmkcShift = 'Shift+';
  SmkcCtrl = 'Ctrl+';
  SmkcAlt = 'Alt+';

  srUnknown = '(Desconocido)';
  srNone = '(Vacio)';
  SOutOfRange = 'El valor tiene que estar entre %d y %d';
  SCannotCreateName = 'No es posible use el nombre estandard para un componente desconocido';

{?}  SDateEncodeError = 'Ungültiges Argument für Datumskodierung';
{?}  STimeEncodeError = 'Ungültiges Argument für Zeitkodierung';
{?}  SInvalidDate = '''''%s'''' ist kein gültiges Datum';
{?}  SInvalidTime = '''''%s'''' ist keine gültige Zeit';
{?}  SInvalidDateTime = '''''%s'''' ist kein gültiges Datum und Zeit';
{?}  SInsertLineError = 'Zeile kann nicht eingefügt werden';

{?}  SCannotDragForm = 'Formulare können nicht gezogen werden';
{?}  SPutObjectError = 'PutObject auf undefiniertes Element';
{?}  SCardDLLNotLoaded = 'CARDS.DLL kann nicht geladen werden';
{?}  SDuplicateCardId = 'Doppelte CardId gefunden';

{?}  SDdeErr = 'Ein Fehler wurde von der DDE zurückgeliefert  ($0%x)';
{?}  SDdeConvErr = 'DDE Fehler - Konversation wurde nicht hergestellt ($0%x)';
{?}  SDdeMemErr = 'Fehler trat auf, da unzureichender Speicher für DDE ($0%x)';
{?}  SDdeNoConnect = 'DDE-Konversation kann nicht eingerichtet werden';


{?}  SDefaultFilter = 'Alle Dateien (*.*)|*.*';
{?}  sAllFilter = 'Alle Dateien';
{?}  SNoVolumeLabel = ': [ - Ohne Namen - ]';

{?}  SConfirmCreateDir = 'Das angegebene Verzeichnis existiert nicht. Soll es angelegt werden?';
{?}  SSelectDirCap = 'Verzeichnis auswählen';
{?}  SCannotCreateDir = 'Das Verzeichnis kann nicht erstellt werden';
{?}  SDirNameCap = 'Verzeichnis&name:';
{?}  SDrivesCap = '&Laufwerke:';
{?}  SDirsCap = '&Verzeichnisse:';
{?}  SFilesCap = '&Dateien: (*.*)';
{?}  SNetworkCap = 'Ne&tzwerk...';

{?}  SColorPrefix = 'Farbe';
  SColorTags = 'ABCDEFGHIJKLMNOP';

{?}  SInvalidClipFmt = 'Ungültiges Format der Zwischenablage';
{?}  SIconToClipboard = 'Zwischenablage unterstützt keine Symbole';

{?}  SDefault = 'Vorgabe';

{?}  SInvalidMemoSize = 'Text überschreitet Memo-Kapazität';
{?}  SCustomColors = 'Selbstdefinierte Farben';
{?}  SInvalidPrinterOp = 'Operation auf ausgewähltem Drucker nicht verfügbar';
{?}  SNoDefaultPrinter = 'Zur Zeit ist kein Standard-Drucker gewählt';

{?}  SIniFileWriteError = 'nach %s kann nicht geschrieben werden';

{?}  SBitsIndexError = 'Bits-Index außerhalb des zulässigen Bereichs';

{?}  SUntitled = '(Unbenannt)';

{?}  SInvalidRegType = 'Ungültiger Datentyp für ''%s''';
{?}  SRegCreateFailed = 'Erzeugung von Schlüssel %s misslungen';
{?}  SRegSetDataFailed = 'Konnte Daten für ''%s'' nicht setzen';
{?}  SRegGetDataFailed = 'Konnte Daten für ''%s'' nicht holen';

{?}  SUnknownConversion = 'Unbekannte Dateierweiterung für RichEdit-Konvertierung (.%s)';
{?}  SDuplicateMenus = 'Menü ''%s'' wird bereits von einem anderen Formular benutzt';

{?}  SPictureLabel = 'Bild:';
  SPictureDesc = ' (%dx%d)';
{?}  SPreviewLabel = 'Vorschau';

{?}  SCannotOpenAVI = 'AVI kann nicht geöffnet werden';

{?}  SNotOpenErr = 'Kein MCI-Gerät geöffnet';
{?}  SMPOpenFilter = 'Alle Dateien (*.*)|*.*|Wave-Dateien (*.WAV)|*.WAV|Midi-Dateien (*.MID)|*.MID|Video für Windows (*.avi)|*.avi';
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

  SBoldItalicFont = 'Negrita cursiva';
  SBoldFont = 'Negrita';
  SItalicFont = 'Cursiva';
  SRegularFont = 'Normal';

{?}  SPropertiesVerb = 'Eigenschaften';

