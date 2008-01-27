(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: CoreTraps.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *      Palm OS core trap numbers
 *
 * History:
 *    06/13/95 Created by Ron Marianetti
 *    06/13/95 RM    Created by Ron Marianetti
 *    ??/??/?? ???   Added Rocky changes
 *    02/04/98 srj   Added Hardware LCD Contrast Trap for Razor
 *    05/05/98 art   Reused sysTrapPsrInit, new name sysTrapIntlDispatch.
 *    06/17/98 jhl   mapped NVPrefs to FlashMgr stuff
 *    07/03/98 kwk   Added WinDrawChar, WinDrawTruncChars, and
 *                   FntWidthToOffset for Instant Karma.
 *    07/07/98 srj   Added System LCD Contrast Trap for Razor
 *    08/05/98 scl   Cross-merged Razor's SysTraps with Main's
 *    09/07/98 kwk   Added SysWantEvent, EvtPeekEvent traps for
 *                   Instant Karma/Razor.
 *    09/18/98 scl   Cross-merged Razor's SysTraps with Main's
 *    10/13/98 kwk   Removed EvtPeekEvent trap.
 *    10/28/98 scl   Cross-merged Razor's SysTraps with Main's
 *    10/29/98 Bob   Move FtrPtr* traps from 3.2 to 3.1
 *    05/21/99 kwk   Added TsmDispatch and OmDispatch traps.
 *    06/30/99 CS    Added DmOpenDBNoOverlay and ResLoadConstant traps.
 *    07/01/99 kwk   Added DmOpenDBWithLocale trap.
 *    07/09/99 kwk   Added HwrGetSilkscreenIID trap.
 *    07/12/99 kwk   Added SysFatalAlertInit trap.
 *    07/15/99 kwk   Added EvtGetSilkscreenAreaList trap.
 *    07/15/99 bob   Moved macros to PalmTypes.h, moved library stuff to LibTraps.h.
 *    07/28/99 kwk   Added DateTemplateToAscii trap.
 *    09/14/99 gap   Removed EvtGetTrapState.
 *    09/14/99 jed   Renamed NotifyMgr trap constants.
 *    09/16/99 jmp   Noted that old Floating Point traps are maintained for
 *                   for backwards compatibility only -- i.e., FloatMgr.h now specifies
 *                   the new Floating Point dispatched traps.
 *    09/22/99 jmp   Added MenuEraseMenu trap; we won't be creating any public headers
 *                   for this routine in 3.5, but we needed to syscallize the routine
 *                   to fix 3.5-specific issues.
 *    11/22/00 bob   Added FldSetMaxVisibleLines trap
 *
 *****************************************************************************)

unit coretraps;

interface

// Regular traps start here and go up by 1's
const
  sysTrapBase = $A000;

  sysTrapMemInit = $A000;
  sysTrapMemInitHeapTable = $A001;
  sysTrapMemStoreInit = $A002;
  sysTrapMemCardFormat = $A003;
  sysTrapMemCardInfo = $A004;
  sysTrapMemStoreInfo = $A005;
  sysTrapMemStoreSetInfo = $A006;
  sysTrapMemNumHeaps = $A007;
  sysTrapMemNumRAMHeaps = $A008;
  sysTrapMemHeapID = $A009;
  sysTrapMemHeapPtr = $A00A;
  sysTrapMemHeapFreeBytes = $A00B;
  sysTrapMemHeapSize = $A00C;
  sysTrapMemHeapFlags = $A00D;
  sysTrapMemHeapCompact = $A00E;
  sysTrapMemHeapInit = $A00F;
  sysTrapMemHeapFreeByOwnerID = $A010;
  sysTrapMemChunkNew = $A011;
  sysTrapMemChunkFree = $A012;
  sysTrapMemPtrNew = $A013;
  sysTrapMemPtrRecoverHandle = $A014;
  sysTrapMemPtrFlags = $A015;
  sysTrapMemPtrSize = $A016;
  sysTrapMemPtrOwner = $A017;
  sysTrapMemPtrHeapID = $A018;
  sysTrapMemPtrCardNo = $A019;
  sysTrapMemPtrToLocalID = $A01A;
  sysTrapMemPtrSetOwner = $A01B;
  sysTrapMemPtrResize = $A01C;
  sysTrapMemPtrResetLock = $A01D;
  sysTrapMemHandleNew = $A01E;
  sysTrapMemHandleLockCount = $A01F;
  sysTrapMemHandleToLocalID = $A020;
  sysTrapMemHandleLock = $A021;
  sysTrapMemHandleUnlock = $A022;
  sysTrapMemLocalIDToGlobal = $A023;
  sysTrapMemLocalIDKind = $A024;
  sysTrapMemLocalIDToPtr = $A025;
  sysTrapMemMove = $A026;
  sysTrapMemSet = $A027;
  sysTrapMemStoreSearch = $A028;
  sysTrapSysReserved10Trap1 = $A029; // "Reserved" trap in Palm OS 1.0 and later (was sysTrapMemPtrDataStorage)

  sysTrapMemKernelInit = $A02A;
  sysTrapMemHandleFree = $A02B;
  sysTrapMemHandleFlags = $A02C;
  sysTrapMemHandleSize = $A02D;
  sysTrapMemHandleOwner = $A02E;
  sysTrapMemHandleHeapID = $A02F;
  sysTrapMemHandleDataStorage = $A030;
  sysTrapMemHandleCardNo = $A031;
  sysTrapMemHandleSetOwner = $A032;
  sysTrapMemHandleResize = $A033;
  sysTrapMemHandleResetLock = $A034;
  sysTrapMemPtrUnlock = $A035;
  sysTrapMemLocalIDToLockedPtr = $A036;
  sysTrapMemSetDebugMode = $A037;
  sysTrapMemHeapScramble = $A038;
  sysTrapMemHeapCheck = $A039;
  sysTrapMemNumCards = $A03A;
  sysTrapMemDebugMode = $A03B;
  sysTrapMemSemaphoreReserve = $A03C;
  sysTrapMemSemaphoreRelease = $A03D;
  sysTrapMemHeapDynamic = $A03E;
  sysTrapMemNVParams = $A03F;

  sysTrapDmInit = $A040;
  sysTrapDmCreateDatabase = $A041;
  sysTrapDmDeleteDatabase = $A042;
  sysTrapDmNumDatabases = $A043;
  sysTrapDmGetDatabase = $A044;
  sysTrapDmFindDatabase = $A045;
  sysTrapDmDatabaseInfo = $A046;
  sysTrapDmSetDatabaseInfo = $A047;
  sysTrapDmDatabaseSize = $A048;
  sysTrapDmOpenDatabase = $A049;
  sysTrapDmCloseDatabase = $A04A;
  sysTrapDmNextOpenDatabase = $A04B;
  sysTrapDmOpenDatabaseInfo = $A04C;
  sysTrapDmResetRecordStates = $A04D;
  sysTrapDmGetLastErr = $A04E;
  sysTrapDmNumRecords = $A04F;
  sysTrapDmRecordInfo = $A050;
  sysTrapDmSetRecordInfo = $A051;
  sysTrapDmAttachRecord = $A052;
  sysTrapDmDetachRecord = $A053;
  sysTrapDmMoveRecord = $A054;
  sysTrapDmNewRecord = $A055;
  sysTrapDmRemoveRecord = $A056;
  sysTrapDmDeleteRecord = $A057;
  sysTrapDmArchiveRecord = $A058;
  sysTrapDmNewHandle = $A059;
  sysTrapDmRemoveSecretRecords = $A05A;
  sysTrapDmQueryRecord = $A05B;
  sysTrapDmGetRecord = $A05C;
  sysTrapDmResizeRecord = $A05D;
  sysTrapDmReleaseRecord = $A05E;
  sysTrapDmGetResource = $A05F;
  sysTrapDmGet1Resource = $A060;
  sysTrapDmReleaseResource = $A061;
  sysTrapDmResizeResource = $A062;
  sysTrapDmNextOpenResDatabase = $A063;
  sysTrapDmFindResourceType = $A064;
  sysTrapDmFindResource = $A065;
  sysTrapDmSearchResource = $A066;
  sysTrapDmNumResources = $A067;
  sysTrapDmResourceInfo = $A068;
  sysTrapDmSetResourceInfo = $A069;
  sysTrapDmAttachResource = $A06A;
  sysTrapDmDetachResource = $A06B;
  sysTrapDmNewResource = $A06C;
  sysTrapDmRemoveResource = $A06D;
  sysTrapDmGetResourceIndex = $A06E;
  sysTrapDmQuickSort = $A06F;
  sysTrapDmQueryNextInCategory = $A070;
  sysTrapDmNumRecordsInCategory = $A071;
  sysTrapDmPositionInCategory = $A072;
  sysTrapDmSeekRecordInCategory = $A073;
  sysTrapDmMoveCategory = $A074;
  sysTrapDmOpenDatabaseByTypeCreator = $A075;
  sysTrapDmWrite = $A076;
  sysTrapDmStrCopy = $A077;
  sysTrapDmGetNextDatabaseByTypeCreator = $A078;
  sysTrapDmWriteCheck = $A079;
  sysTrapDmMoveOpenDBContext = $A07A;
  sysTrapDmFindRecordByID = $A07B;
  sysTrapDmGetAppInfoID = $A07C;
  sysTrapDmFindSortPositionV10 = $A07D;
  sysTrapDmSet = $A07E;
  sysTrapDmCreateDatabaseFromImage = $A07F;

  sysTrapDbgSrcMessage = $A080;
  sysTrapDbgMessage = $A081;
  sysTrapDbgGetMessage = $A082;
  sysTrapDbgCommSettings = $A083;

  sysTrapErrDisplayFileLineMsg = $A084;
  sysTrapErrSetJump = $A085;
  sysTrapErrLongJump = $A086;
  sysTrapErrThrow = $A087;
  sysTrapErrExceptionList = $A088;

  sysTrapSysBroadcastActionCode = $A089;
  sysTrapSysUnimplemented = $A08A;
  sysTrapSysColdBoot = $A08B;
  sysTrapSysReset = $A08C;
  sysTrapSysDoze = $A08D;
  sysTrapSysAppLaunch = $A08E;
  sysTrapSysAppStartup = $A08F;
  sysTrapSysAppExit = $A090;
  sysTrapSysSetA5 = $A091;
  sysTrapSysSetTrapAddress = $A092;
  sysTrapSysGetTrapAddress = $A093;
  sysTrapSysTranslateKernelErr = $A094;
  sysTrapSysSemaphoreCreate = $A095;
  sysTrapSysSemaphoreDelete = $A096;
  sysTrapSysSemaphoreWait = $A097;
  sysTrapSysSemaphoreSignal = $A098;
  sysTrapSysTimerCreate = $A099;
  sysTrapSysTimerWrite = $A09A;
  sysTrapSysTaskCreate = $A09B;
  sysTrapSysTaskDelete = $A09C;
  sysTrapSysTaskTrigger = $A09D;
  sysTrapSysTaskID = $A09E;
  sysTrapSysTaskUserInfoPtr = $A09F;
  sysTrapSysTaskDelay = $A0A0;
  sysTrapSysTaskSetTermProc = $A0A1;
  sysTrapSysUILaunch = $A0A2;
  sysTrapSysNewOwnerID = $A0A3;
  sysTrapSysSemaphoreSet = $A0A4;
  sysTrapSysDisableInts = $A0A5;
  sysTrapSysRestoreStatus = $A0A6;
  sysTrapSysUIAppSwitch = $A0A7;
  sysTrapSysCurAppInfoPV20 = $A0A8;
  sysTrapSysHandleEvent = $A0A9;
  sysTrapSysInit = $A0AA;
  sysTrapSysQSort = $A0AB;
  sysTrapSysCurAppDatabase = $A0AC;
  sysTrapSysFatalAlert = $A0AD;
  sysTrapSysResSemaphoreCreate = $A0AE;
  sysTrapSysResSemaphoreDelete = $A0AF;
  sysTrapSysResSemaphoreReserve = $A0B0;
  sysTrapSysResSemaphoreRelease = $A0B1;
  sysTrapSysSleep = $A0B2;
  sysTrapSysKeyboardDialogV10 = $A0B3;
  sysTrapSysAppLauncherDialog = $A0B4;
  sysTrapSysSetPerformance = $A0B5;
  sysTrapSysBatteryInfoV20 = $A0B6;
  sysTrapSysLibInstall = $A0B7;
  sysTrapSysLibRemove = $A0B8;
  sysTrapSysLibTblEntry = $A0B9;
  sysTrapSysLibFind = $A0BA;
  sysTrapSysBatteryDialog = $A0BB;
  sysTrapSysCopyStringResource = $A0BC;
  sysTrapSysKernelInfo = $A0BD;
  sysTrapSysLaunchConsole = $A0BE;
  sysTrapSysTimerDelete = $A0BF;
  sysTrapSysSetAutoOffTime = $A0C0;
  sysTrapSysFormPointerArrayToStrings = $A0C1;
  sysTrapSysRandom = $A0C2;
  sysTrapSysTaskSwitching = $A0C3;
  sysTrapSysTimerRead = $A0C4;

  sysTrapStrCopy = $A0C5;
  sysTrapStrCat = $A0C6;
  sysTrapStrLen = $A0C7;
  sysTrapStrCompare = $A0C8;
  sysTrapStrIToA = $A0C9;
  sysTrapStrCaselessCompare = $A0CA;
  sysTrapStrIToH = $A0CB;
  sysTrapStrChr = $A0CC;
  sysTrapStrStr = $A0CD;
  sysTrapStrAToI = $A0CE;
  sysTrapStrToLower = $A0CF;

  sysTrapSerReceiveISP = $A0D0;

  sysTrapSlkOpen = $A0D1;
  sysTrapSlkClose = $A0D2;
  sysTrapSlkOpenSocket = $A0D3;
  sysTrapSlkCloseSocket = $A0D4;
  sysTrapSlkSocketRefNum = $A0D5;
  sysTrapSlkSocketSetTimeout = $A0D6;
  sysTrapSlkFlushSocket = $A0D7;
  sysTrapSlkSetSocketListener = $A0D8;
  sysTrapSlkSendPacket = $A0D9;
  sysTrapSlkReceivePacket = $A0DA;
  sysTrapSlkSysPktDefaultResponse = $A0DB;
  sysTrapSlkProcessRPC = $A0DC;

  sysTrapConPutS = $A0DD;
  sysTrapConGetS = $A0DE;

  sysTrapFplInit = $A0DF; // Obsolete, here for compatibilty only!
  sysTrapFplFree = $A0E0; // Obsolete, here for compatibilty only!
  sysTrapFplFToA = $A0E1; // Obsolete, here for compatibilty only!
  sysTrapFplAToF = $A0E2; // Obsolete, here for compatibilty only!
  sysTrapFplBase10Info = $A0E3; // Obsolete, here for compatibilty only!
  sysTrapFplLongToFloat = $A0E4; // Obsolete, here for compatibilty only!
  sysTrapFplFloatToLong = $A0E5; // Obsolete, here for compatibilty only!
  sysTrapFplFloatToULong = $A0E6; // Obsolete, here for compatibilty only!
  sysTrapFplMul = $A0E7; // Obsolete, here for compatibilty only!
  sysTrapFplAdd = $A0E8; // Obsolete, here for compatibilty only!
  sysTrapFplSub = $A0E9; // Obsolete, here for compatibilty only!
  sysTrapFplDiv = $A0EA; // Obsolete, here for compatibilty only!

  sysTrapWinScreenInit = $A0EB; // was sysTrapScrInit
  sysTrapScrCopyRectangle = $A0EC;
  sysTrapScrDrawChars = $A0ED;
  sysTrapScrLineRoutine = $A0EE;
  sysTrapScrRectangleRoutine = $A0EF;
  sysTrapScrScreenInfo = $A0F0;
  sysTrapScrDrawNotify = $A0F1;
  sysTrapScrSendUpdateArea = $A0F2;
  sysTrapScrCompressScanLine = $A0F3;
  sysTrapScrDeCompressScanLine = $A0F4;

  sysTrapTimGetSeconds = $A0F5;
  sysTrapTimSetSeconds = $A0F6;
  sysTrapTimGetTicks = $A0F7;
  sysTrapTimInit = $A0F8;
  sysTrapTimSetAlarm = $A0F9;
  sysTrapTimGetAlarm = $A0FA;
  sysTrapTimHandleInterrupt = $A0FB;
  sysTrapTimSecondsToDateTime = $A0FC;
  sysTrapTimDateTimeToSeconds = $A0FD;
  sysTrapTimAdjust = $A0FE;
  sysTrapTimSleep = $A0FF;
  sysTrapTimWake = $A100;

  sysTrapCategoryCreateListV10 = $A101;
  sysTrapCategoryFreeListV10 = $A102;
  sysTrapCategoryFind = $A103;
  sysTrapCategoryGetName = $A104;
  sysTrapCategoryEditV10 = $A105;
  sysTrapCategorySelectV10 = $A106;
  sysTrapCategoryGetNext = $A107;
  sysTrapCategorySetTriggerLabel = $A108;
  sysTrapCategoryTruncateName = $A109;

  sysTrapClipboardAddItem = $A10A;
  sysTrapClipboardCheckIfItemExist = $A10B;
  sysTrapClipboardGetItem = $A10C;

  sysTrapCtlDrawControl = $A10D;
  sysTrapCtlEraseControl = $A10E;
  sysTrapCtlHideControl = $A10F;
  sysTrapCtlShowControl = $A110;
  sysTrapCtlGetValue = $A111;
  sysTrapCtlSetValue = $A112;
  sysTrapCtlGetLabel = $A113;
  sysTrapCtlSetLabel = $A114;
  sysTrapCtlHandleEvent = $A115;
  sysTrapCtlHitControl = $A116;
  sysTrapCtlSetEnabled = $A117;
  sysTrapCtlSetUsable = $A118;
  sysTrapCtlEnabled = $A119;

  sysTrapEvtInitialize = $A11A;
  sysTrapEvtAddEventToQueue = $A11B;
  sysTrapEvtCopyEvent = $A11C;
  sysTrapEvtGetEvent = $A11D;
  sysTrapEvtGetPen = $A11E;
  sysTrapEvtSysInit = $A11F;
  sysTrapEvtGetSysEvent = $A120;
  sysTrapEvtProcessSoftKeyStroke = $A121;
  sysTrapEvtGetPenBtnList = $A122;
  sysTrapEvtSetPenQueuePtr = $A123;
  sysTrapEvtPenQueueSize = $A124;
  sysTrapEvtFlushPenQueue = $A125;
  sysTrapEvtEnqueuePenPoint = $A126;
  sysTrapEvtDequeuePenStrokeInfo = $A127;
  sysTrapEvtDequeuePenPoint = $A128;
  sysTrapEvtFlushNextPenStroke = $A129;
  sysTrapEvtSetKeyQueuePtr = $A12A;
  sysTrapEvtKeyQueueSize = $A12B;
  sysTrapEvtFlushKeyQueue = $A12C;
  sysTrapEvtEnqueueKey = $A12D;
  sysTrapEvtDequeueKeyEvent = $A12E;
  sysTrapEvtWakeup = $A12F;
  sysTrapEvtResetAutoOffTimer = $A130;
  sysTrapEvtKeyQueueEmpty = $A131;
  sysTrapEvtEnableGraffiti = $A132;

  sysTrapFldCopy = $A133;
  sysTrapFldCut = $A134;
  sysTrapFldDrawField = $A135;
  sysTrapFldEraseField = $A136;
  sysTrapFldFreeMemory = $A137;
  sysTrapFldGetBounds = $A138;
  sysTrapFldGetTextPtr = $A139;
  sysTrapFldGetSelection = $A13A;
  sysTrapFldHandleEvent = $A13B;
  sysTrapFldPaste = $A13C;
  sysTrapFldRecalculateField = $A13D;
  sysTrapFldSetBounds = $A13E;
  sysTrapFldSetText = $A13F;
  sysTrapFldGetFont = $A140;
  sysTrapFldSetFont = $A141;
  sysTrapFldSetSelection = $A142;
  sysTrapFldGrabFocus = $A143;
  sysTrapFldReleaseFocus = $A144;
  sysTrapFldGetInsPtPosition = $A145;
  sysTrapFldSetInsPtPosition = $A146;
  sysTrapFldSetScrollPosition = $A147;
  sysTrapFldGetScrollPosition = $A148;
  sysTrapFldGetTextHeight = $A149;
  sysTrapFldGetTextAllocatedSize = $A14A;
  sysTrapFldGetTextLength = $A14B;
  sysTrapFldScrollField = $A14C;
  sysTrapFldScrollable = $A14D;
  sysTrapFldGetVisibleLines = $A14E;
  sysTrapFldGetAttributes = $A14F;
  sysTrapFldSetAttributes = $A150;
  sysTrapFldSendChangeNotification = $A151;
  sysTrapFldCalcFieldHeight = $A152;
  sysTrapFldGetTextHandle = $A153;
  sysTrapFldCompactText = $A154;
  sysTrapFldDirty = $A155;
  sysTrapFldWordWrap = $A156;
  sysTrapFldSetTextAllocatedSize = $A157;
  sysTrapFldSetTextHandle = $A158;
  sysTrapFldSetTextPtr = $A159;
  sysTrapFldGetMaxChars = $A15A;
  sysTrapFldSetMaxChars = $A15B;
  sysTrapFldSetUsable = $A15C;
  sysTrapFldInsert = $A15D;
  sysTrapFldDelete = $A15E;
  sysTrapFldUndo = $A15F;
  sysTrapFldSetDirty = $A160;
  sysTrapFldSendHeightChangeNotification = $A161;
  sysTrapFldMakeFullyVisible = $A162;

  sysTrapFntGetFont = $A163;
  sysTrapFntSetFont = $A164;
  sysTrapFntGetFontPtr = $A165;
  sysTrapFntBaseLine = $A166;
  sysTrapFntCharHeight = $A167;
  sysTrapFntLineHeight = $A168;
  sysTrapFntAverageCharWidth = $A169;
  sysTrapFntCharWidth = $A16A;
  sysTrapFntCharsWidth = $A16B;
  sysTrapFntDescenderHeight = $A16C;
  sysTrapFntCharsInWidth = $A16D;
  sysTrapFntLineWidth = $A16E;

  sysTrapFrmInitForm = $A16F;
  sysTrapFrmDeleteForm = $A170;
  sysTrapFrmDrawForm = $A171;
  sysTrapFrmEraseForm = $A172;
  sysTrapFrmGetActiveForm = $A173;
  sysTrapFrmSetActiveForm = $A174;
  sysTrapFrmGetActiveFormID = $A175;
  sysTrapFrmGetUserModifiedState = $A176;
  sysTrapFrmSetNotUserModified = $A177;
  sysTrapFrmGetFocus = $A178;
  sysTrapFrmSetFocus = $A179;
  sysTrapFrmHandleEvent = $A17A;
  sysTrapFrmGetFormBounds = $A17B;
  sysTrapFrmGetWindowHandle = $A17C;
  sysTrapFrmGetFormId = $A17D;
  sysTrapFrmGetFormPtr = $A17E;
  sysTrapFrmGetNumberOfObjects = $A17F;
  sysTrapFrmGetObjectIndex = $A180;
  sysTrapFrmGetObjectId = $A181;
  sysTrapFrmGetObjectType = $A182;
  sysTrapFrmGetObjectPtr = $A183;
  sysTrapFrmHideObject = $A184;
  sysTrapFrmShowObject = $A185;
  sysTrapFrmGetObjectPosition = $A186;
  sysTrapFrmSetObjectPosition = $A187;
  sysTrapFrmGetControlValue = $A188;
  sysTrapFrmSetControlValue = $A189;
  sysTrapFrmGetControlGroupSelection = $A18A;
  sysTrapFrmSetControlGroupSelection = $A18B;
  sysTrapFrmCopyLabel = $A18C;
  sysTrapFrmSetLabel = $A18D;
  sysTrapFrmGetLabel = $A18E;
  sysTrapFrmSetCategoryLabel = $A18F;
  sysTrapFrmGetTitle = $A190;
  sysTrapFrmSetTitle = $A191;
  sysTrapFrmAlert = $A192;
  sysTrapFrmDoDialog = $A193;
  sysTrapFrmCustomAlert = $A194;
  sysTrapFrmHelp = $A195;
  sysTrapFrmUpdateScrollers = $A196;
  sysTrapFrmGetFirstForm = $A197;
  sysTrapFrmVisible = $A198;
  sysTrapFrmGetObjectBounds = $A199;
  sysTrapFrmCopyTitle = $A19A;
  sysTrapFrmGotoForm = $A19B;
  sysTrapFrmPopupForm = $A19C;
  sysTrapFrmUpdateForm = $A19D;
  sysTrapFrmReturnToForm = $A19E;
  sysTrapFrmSetEventHandler = $A19F;
  sysTrapFrmDispatchEvent = $A1A0;
  sysTrapFrmCloseAllForms = $A1A1;
  sysTrapFrmSaveAllForms = $A1A2;
  sysTrapFrmGetGadgetData = $A1A3;
  sysTrapFrmSetGadgetData = $A1A4;
  sysTrapFrmSetCategoryTrigger = $A1A5;

  sysTrapUIInitialize = $A1A6;
  sysTrapUIReset = $A1A7;

  sysTrapInsPtInitialize = $A1A8;
  sysTrapInsPtSetLocation = $A1A9;
  sysTrapInsPtGetLocation = $A1AA;
  sysTrapInsPtEnable = $A1AB;
  sysTrapInsPtEnabled = $A1AC;
  sysTrapInsPtSetHeight = $A1AD;
  sysTrapInsPtGetHeight = $A1AE;
  sysTrapInsPtCheckBlink = $A1AF;

  sysTrapLstSetDrawFunction = $A1B0;
  sysTrapLstDrawList = $A1B1;
  sysTrapLstEraseList = $A1B2;
  sysTrapLstGetSelection = $A1B3;
  sysTrapLstGetSelectionText = $A1B4;
  sysTrapLstHandleEvent = $A1B5;
  sysTrapLstSetHeight = $A1B6;
  sysTrapLstSetSelection = $A1B7;
  sysTrapLstSetListChoices = $A1B8;
  sysTrapLstMakeItemVisible = $A1B9;
  sysTrapLstGetNumberOfItems = $A1BA;
  sysTrapLstPopupList = $A1BB;
  sysTrapLstSetPosition = $A1BC;

  sysTrapMenuInit = $A1BD;
  sysTrapMenuDispose = $A1BE;
  sysTrapMenuHandleEvent = $A1BF;
  sysTrapMenuDrawMenu = $A1C0;
  sysTrapMenuEraseStatus = $A1C1;
  sysTrapMenuGetActiveMenu = $A1C2;
  sysTrapMenuSetActiveMenu = $A1C3;

  sysTrapRctSetRectangle = $A1C4;
  sysTrapRctCopyRectangle = $A1C5;
  sysTrapRctInsetRectangle = $A1C6;
  sysTrapRctOffsetRectangle = $A1C7;
  sysTrapRctPtInRectangle = $A1C8;
  sysTrapRctGetIntersection = $A1C9;

  sysTrapTblDrawTable = $A1CA;
  sysTrapTblEraseTable = $A1CB;
  sysTrapTblHandleEvent = $A1CC;
  sysTrapTblGetItemBounds = $A1CD;
  sysTrapTblSelectItem = $A1CE;
  sysTrapTblGetItemInt = $A1CF;
  sysTrapTblSetItemInt = $A1D0;
  sysTrapTblSetItemStyle = $A1D1;
  sysTrapTblUnhighlightSelection = $A1D2;
  sysTrapTblSetRowUsable = $A1D3;
  sysTrapTblGetNumberOfRows = $A1D4;
  sysTrapTblSetCustomDrawProcedure = $A1D5;
  sysTrapTblSetRowSelectable = $A1D6;
  sysTrapTblRowSelectable = $A1D7;
  sysTrapTblSetLoadDataProcedure = $A1D8;
  sysTrapTblSetSaveDataProcedure = $A1D9;
  sysTrapTblGetBounds = $A1DA;
  sysTrapTblSetRowHeight = $A1DB;
  sysTrapTblGetColumnWidth = $A1DC;
  sysTrapTblGetRowID = $A1DD;
  sysTrapTblSetRowID = $A1DE;
  sysTrapTblMarkRowInvalid = $A1DF;
  sysTrapTblMarkTableInvalid = $A1E0;
  sysTrapTblGetSelection = $A1E1;
  sysTrapTblInsertRow = $A1E2;
  sysTrapTblRemoveRow = $A1E3;
  sysTrapTblRowInvalid = $A1E4;
  sysTrapTblRedrawTable = $A1E5;
  sysTrapTblRowUsable = $A1E6;
  sysTrapTblReleaseFocus = $A1E7;
  sysTrapTblEditing = $A1E8;
  sysTrapTblGetCurrentField = $A1E9;
  sysTrapTblSetColumnUsable = $A1EA;
  sysTrapTblGetRowHeight = $A1EB;
  sysTrapTblSetColumnWidth = $A1EC;
  sysTrapTblGrabFocus = $A1ED;
  sysTrapTblSetItemPtr = $A1EE;
  sysTrapTblFindRowID = $A1EF;
  sysTrapTblGetLastUsableRow = $A1F0;
  sysTrapTblGetColumnSpacing = $A1F1;
  sysTrapTblFindRowData = $A1F2;
  sysTrapTblGetRowData = $A1F3;
  sysTrapTblSetRowData = $A1F4;
  sysTrapTblSetColumnSpacing = $A1F5;

  sysTrapWinCreateWindow = $A1F6;
  sysTrapWinCreateOffscreenWindow = $A1F7;
  sysTrapWinDeleteWindow = $A1F8;
  sysTrapWinInitializeWindow = $A1F9;
  sysTrapWinAddWindow = $A1FA;
  sysTrapWinRemoveWindow = $A1FB;
  sysTrapWinSetActiveWindow = $A1FC;
  sysTrapWinSetDrawWindow = $A1FD;
  sysTrapWinGetDrawWindow = $A1FE;
  sysTrapWinGetActiveWindow = $A1FF;
  sysTrapWinGetDisplayWindow = $A200;
  sysTrapWinGetFirstWindow = $A201;
  sysTrapWinEnableWindow = $A202;
  sysTrapWinDisableWindow = $A203;
  sysTrapWinGetWindowFrameRect = $A204;
  sysTrapWinDrawWindowFrame = $A205;
  sysTrapWinEraseWindow = $A206;
  sysTrapWinSaveBits = $A207;
  sysTrapWinRestoreBits = $A208;
  sysTrapWinCopyRectangle = $A209;
  sysTrapWinScrollRectangle = $A20A;
  sysTrapWinGetDisplayExtent = $A20B;
  sysTrapWinGetWindowExtent = $A20C;
  sysTrapWinDisplayToWindowPt = $A20D;
  sysTrapWinWindowToDisplayPt = $A20E;
  sysTrapWinGetClip = $A20F;
  sysTrapWinSetClip = $A210;
  sysTrapWinResetClip = $A211;
  sysTrapWinClipRectangle = $A212;
  sysTrapWinDrawLine = $A213;
  sysTrapWinDrawGrayLine = $A214;
  sysTrapWinEraseLine = $A215;
  sysTrapWinInvertLine = $A216;
  sysTrapWinFillLine = $A217;
  sysTrapWinDrawRectangle = $A218;
  sysTrapWinEraseRectangle = $A219;
  sysTrapWinInvertRectangle = $A21A;
  sysTrapWinDrawRectangleFrame = $A21B;
  sysTrapWinDrawGrayRectangleFrame = $A21C;
  sysTrapWinEraseRectangleFrame = $A21D;
  sysTrapWinInvertRectangleFrame = $A21E;
  sysTrapWinGetFramesRectangle = $A21F;
  sysTrapWinDrawChars = $A220;
  sysTrapWinEraseChars = $A221;
  sysTrapWinInvertChars = $A222;
  sysTrapWinGetPattern = $A223;
  sysTrapWinSetPattern = $A224;
  sysTrapWinSetUnderlineMode = $A225;
  sysTrapWinDrawBitmap = $A226;
  sysTrapWinModal = $A227;
  sysTrapWinGetDrawWindowBounds = $A228;
  sysTrapWinFillRectangle = $A229;
  sysTrapWinDrawInvertedChars = $A22A;

  sysTrapPrefOpenPreferenceDBV10 = $A22B;
  sysTrapPrefGetPreferences = $A22C;
  sysTrapPrefSetPreferences = $A22D;
  sysTrapPrefGetAppPreferencesV10 = $A22E;
  sysTrapPrefSetAppPreferencesV10 = $A22F;

  sysTrapSndInit = $A230;
  sysTrapSndSetDefaultVolume = $A231;
  sysTrapSndGetDefaultVolume = $A232;
  sysTrapSndDoCmd = $A233;
  sysTrapSndPlaySystemSound = $A234;

  sysTrapAlmInit = $A235;
  sysTrapAlmCancelAll = $A236;
  sysTrapAlmAlarmCallback = $A237;
  sysTrapAlmSetAlarm = $A238;
  sysTrapAlmGetAlarm = $A239;
  sysTrapAlmDisplayAlarm = $A23A;
  sysTrapAlmEnableNotification = $A23B;

  sysTrapHwrGetRAMMapping = $A23C;
  sysTrapHwrMemWritable = $A23D;
  sysTrapHwrMemReadable = $A23E;
  sysTrapHwrDoze = $A23F;
  sysTrapHwrSleep = $A240;
  sysTrapHwrWake = $A241;
  sysTrapHwrSetSystemClock = $A242;
  sysTrapHwrSetCPUDutyCycle = $A243;
  sysTrapHwrDisplayInit = $A244; // Before OS 3.5, this trap a.k.a. sysTrapHwrLCDInit
  sysTrapHwrDisplaySleep = $A245; // Before OS 3.5, this trap a.k.a. sysTrapHwrLCDSleep
  sysTrapHwrTimerInit = $A246;
  sysTrapHwrCursorV33 = $A247; // This trap obsoleted for OS 3.5 and later
  sysTrapHwrBatteryLevel = $A248;
  sysTrapHwrDelay = $A249;
  sysTrapHwrEnableDataWrites = $A24A;
  sysTrapHwrDisableDataWrites = $A24B;
  sysTrapHwrLCDBaseAddrV33 = $A24C; // This trap obsoleted for OS 3.5 and later
  sysTrapHwrDisplayDrawBootScreen = $A24D; // Before OS 3.5, this trap a.k.a. sysTrapHwrLCDDrawBitmap
  sysTrapHwrTimerSleep = $A24E;
  sysTrapHwrTimerWake = $A24F;
  sysTrapHwrDisplayWake = $A250; // Before OS 3.5, this trap a.k.a. sysTrapHwrLCDWake
  sysTrapHwrIRQ1Handler = $A251;
  sysTrapHwrIRQ2Handler = $A252;
  sysTrapHwrIRQ3Handler = $A253;
  sysTrapHwrIRQ4Handler = $A254;
  sysTrapHwrIRQ5Handler = $A255;
  sysTrapHwrIRQ6Handler = $A256;
  sysTrapHwrDockSignals = $A257;
  sysTrapHwrPluggedIn = $A258;

  sysTrapCrc16CalcBlock = $A259;

  sysTrapSelectDayV10 = $A25A;
  sysTrapSelectTimeV33 = $A25B;

  sysTrapDayDrawDaySelector = $A25C;
  sysTrapDayHandleEvent = $A25D;
  sysTrapDayDrawDays = $A25E;
  sysTrapDayOfWeek = $A25F;
  sysTrapDaysInMonth = $A260;
  sysTrapDayOfMonth = $A261;

  sysTrapDateDaysToDate = $A262;
  sysTrapDateToDays = $A263;
  sysTrapDateAdjust = $A264;
  sysTrapDateSecondsToDate = $A265;
  sysTrapDateToAscii = $A266;
  sysTrapDateToDOWDMFormat = $A267;
  sysTrapTimeToAscii = $A268;

  sysTrapFind = $A269;
  sysTrapFindStrInStr = $A26A;
  sysTrapFindSaveMatch = $A26B;
  sysTrapFindGetLineBounds = $A26C;
  sysTrapFindDrawHeader = $A26D;

  sysTrapPenOpen = $A26E;
  sysTrapPenClose = $A26F;
  sysTrapPenGetRawPen = $A270;
  sysTrapPenCalibrate = $A271;
  sysTrapPenRawToScreen = $A272;
  sysTrapPenScreenToRaw = $A273;
  sysTrapPenResetCalibration = $A274;
  sysTrapPenSleep = $A275;
  sysTrapPenWake = $A276;

  sysTrapResLoadForm = $A277;
  sysTrapResLoadMenu = $A278;

  sysTrapFtrInit = $A279;
  sysTrapFtrUnregister = $A27A;
  sysTrapFtrGet = $A27B;
  sysTrapFtrSet = $A27C;
  sysTrapFtrGetByIndex = $A27D;

  sysTrapGrfInit = $A27E;
  sysTrapGrfFree = $A27F;
  sysTrapGrfGetState = $A280;
  sysTrapGrfSetState = $A281;
  sysTrapGrfFlushPoints = $A282;
  sysTrapGrfAddPoint = $A283;
  sysTrapGrfInitState = $A284;
  sysTrapGrfCleanState = $A285;
  sysTrapGrfMatch = $A286;
  sysTrapGrfGetMacro = $A287;
  sysTrapGrfFilterPoints = $A288;
  sysTrapGrfGetNumPoints = $A289;
  sysTrapGrfGetPoint = $A28A;
  sysTrapGrfFindBranch = $A28B;
  sysTrapGrfMatchGlyph = $A28C;
  sysTrapGrfGetGlyphMapping = $A28D;
  sysTrapGrfGetMacroName = $A28E;
  sysTrapGrfDeleteMacro = $A28F;
  sysTrapGrfAddMacro = $A290;
  sysTrapGrfGetAndExpandMacro = $A291;
  sysTrapGrfProcessStroke = $A292;
  sysTrapGrfFieldChange = $A293;

  sysTrapGetCharSortValue = $A294;
  sysTrapGetCharAttr = $A295;
  sysTrapGetCharCaselessValue = $A296;

  sysTrapPwdExists = $A297;
  sysTrapPwdVerify = $A298;
  sysTrapPwdSet = $A299;
  sysTrapPwdRemove = $A29A;

  sysTrapGsiInitialize = $A29B;
  sysTrapGsiSetLocation = $A29C;
  sysTrapGsiEnable = $A29D;
  sysTrapGsiEnabled = $A29E;
  sysTrapGsiSetShiftState = $A29F;

  sysTrapKeyInit = $A2A0;
  sysTrapKeyHandleInterrupt = $A2A1;
  sysTrapKeyCurrentState = $A2A2;
  sysTrapKeyResetDoubleTap = $A2A3;
  sysTrapKeyRates = $A2A4;
  sysTrapKeySleep = $A2A5;
  sysTrapKeyWake = $A2A6;

  sysTrapDlkControl = $A2A7; // was sysTrapCmBroadcast

  sysTrapDlkStartServer = $A2A8;
  sysTrapDlkGetSyncInfo = $A2A9;
  sysTrapDlkSetLogEntry = $A2AA;

  sysTrapIntlDispatch = $A2AB; // REUSED IN v3.1 (was sysTrapPsrInit in 1.0, removed in 2.0)
  sysTrapSysLibLoad = $A2AC; // REUSED IN v2.0 (was sysTrapPsrClose)
  sysTrapSndPlaySmf = $A2AD; // REUSED IN v3.0 (was sysTrapPsrGetCommand in 1.0, removed in 2.0)
  sysTrapSndCreateMidiList = $A2AE; // REUSED IN v3.0 (was sysTrapPsrSendReply in 1.0, removed in 2.0)

  sysTrapAbtShowAbout = $A2AF;

  sysTrapMdmDial = $A2B0;
  sysTrapMdmHangUp = $A2B1;

  sysTrapDmSearchRecord = $A2B2;

  sysTrapSysInsertionSort = $A2B3;
  sysTrapDmInsertionSort = $A2B4;

  sysTrapLstSetTopItem = $A2B5;

// Palm OS 2.X traps     Palm Pilot and 2.0 Upgrade Card

  sysTrapSclSetScrollBar = $A2B6;
  sysTrapSclDrawScrollBar = $A2B7;
  sysTrapSclHandleEvent = $A2B8;

  sysTrapSysMailboxCreate = $A2B9;
  sysTrapSysMailboxDelete = $A2BA;
  sysTrapSysMailboxFlush = $A2BB;
  sysTrapSysMailboxSend = $A2BC;
  sysTrapSysMailboxWait = $A2BD;

  sysTrapSysTaskWait = $A2BE;
  sysTrapSysTaskWake = $A2BF;
  sysTrapSysTaskWaitClr = $A2C0;
  sysTrapSysTaskSuspend = $A2C1;
  sysTrapSysTaskResume = $A2C2;

  sysTrapCategoryCreateList = $A2C3;
  sysTrapCategoryFreeList = $A2C4;
  sysTrapCategoryEditV20 = $A2C5;
  sysTrapCategorySelect = $A2C6;

  sysTrapDmDeleteCategory = $A2C7;

  sysTrapSysEvGroupCreate = $A2C8;
  sysTrapSysEvGroupSignal = $A2C9;
  sysTrapSysEvGroupRead = $A2CA;
  sysTrapSysEvGroupWait = $A2CB;

  sysTrapEvtEventAvail = $A2CC;
  sysTrapEvtSysEventAvail = $A2CD;
  sysTrapStrNCopy = $A2CE;

  sysTrapKeySetMask = $A2CF;

  sysTrapSelectDay = $A2D0;

  sysTrapPrefGetPreference = $A2D1;
  sysTrapPrefSetPreference = $A2D2;
  sysTrapPrefGetAppPreferences = $A2D3;
  sysTrapPrefSetAppPreferences = $A2D4;

  sysTrapFrmPointInTitle = $A2D5;

  sysTrapStrNCat = $A2D6;

  sysTrapMemCmp = $A2D7;

  sysTrapTblSetColumnEditIndicator = $A2D8;

  sysTrapFntWordWrap = $A2D9;

  sysTrapFldGetScrollValues = $A2DA;

  sysTrapSysCreateDataBaseList = $A2DB;
  sysTrapSysCreatePanelList = $A2DC;

  sysTrapDlkDispatchRequest = $A2DD;

  sysTrapStrPrintF = $A2DE;
  sysTrapStrVPrintF = $A2DF;

  sysTrapPrefOpenPreferenceDB = $A2E0;

  sysTrapSysGraffitiReferenceDialog = $A2E1;

  sysTrapSysKeyboardDialog = $A2E2;

  sysTrapFntWordWrapReverseNLines = $A2E3;
  sysTrapFntGetScrollValues = $A2E4;

  sysTrapTblSetRowStaticHeight = $A2E5;
  sysTrapTblHasScrollBar = $A2E6;

  sysTrapSclGetScrollBar = $A2E7;

  sysTrapFldGetNumberOfBlankLines = $A2E8;

  sysTrapSysTicksPerSecond = $A2E9;
  sysTrapHwrBacklightV33 = $A2EA; // This trap obsoleted for OS 3.5 and later
  sysTrapDmDatabaseProtect = $A2EB;

  sysTrapTblSetBounds = $A2EC;

  sysTrapStrNCompare = $A2ED;
  sysTrapStrNCaselessCompare = $A2EE;

  sysTrapPhoneNumberLookup = $A2EF;

  sysTrapFrmSetMenu = $A2F0;

  sysTrapEncDigestMD5 = $A2F1;

  sysTrapDmFindSortPosition = $A2F2;

  sysTrapSysBinarySearch = $A2F3;
  sysTrapSysErrString = $A2F4;
  sysTrapSysStringByIndex = $A2F5;

  sysTrapEvtAddUniqueEventToQueue = $A2F6;

  sysTrapStrLocalizeNumber = $A2F7;
  sysTrapStrDelocalizeNumber = $A2F8;
  sysTrapLocGetNumberSeparators = $A2F9;

  sysTrapMenuSetActiveMenuRscID = $A2FA;

  sysTrapLstScrollList = $A2FB;

  sysTrapCategoryInitialize = $A2FC;

  sysTrapEncDigestMD4 = $A2FD;
  sysTrapEncDES = $A2FE;

  sysTrapLstGetVisibleItems = $A2FF;

  sysTrapWinSetBounds = $A300;

  sysTrapCategorySetName = $A301;

  sysTrapFldSetInsertionPoint = $A302;

  sysTrapFrmSetObjectBounds = $A303;

  sysTrapWinSetColors = $A304;

  sysTrapFlpDispatch = $A305;
  sysTrapFlpEmDispatch = $A306;

// Palm OS 3.0 traps     Palm III and 3.0 Upgrade Card

  sysTrapExgInit = $A307;
  sysTrapExgConnect = $A308;
  sysTrapExgPut = $A309;
  sysTrapExgGet = $A30A;
  sysTrapExgAccept = $A30B;
  sysTrapExgDisconnect = $A30C;
  sysTrapExgSend = $A30D;
  sysTrapExgReceive = $A30E;
  sysTrapExgRegisterData = $A30F;
  sysTrapExgNotifyReceiveV35 = $A310;
  sysTrapSysReserved30Trap2 = $A311; // "Reserved" trap in Palm OS 3.0 and later (was sysTrapExgControl)
  sysTrapPrgStartDialogV31 = $A312; // Updated in v3.2
  sysTrapPrgStopDialog = $A313;
  sysTrapPrgUpdateDialog = $A314;
  sysTrapPrgHandleEvent = $A315;

  sysTrapImcReadFieldNoSemicolon = $A316;
  sysTrapImcReadFieldQuotablePrintable = $A317;
  sysTrapImcReadPropertyParameter = $A318;
  sysTrapImcSkipAllPropertyParameters = $A319;
  sysTrapImcReadWhiteSpace = $A31A;
  sysTrapImcWriteQuotedPrintable = $A31B;
  sysTrapImcWriteNoSemicolon = $A31C;
  sysTrapImcStringIsAscii = $A31D;

  sysTrapTblGetItemFont = $A31E;
  sysTrapTblSetItemFont = $A31F;

  sysTrapFontSelect = $A320;
  sysTrapFntDefineFont = $A321;

  sysTrapCategoryEdit = $A322;

  sysTrapSysGetOSVersionString = $A323;
  sysTrapSysBatteryInfo = $A324;
  sysTrapSysUIBusy = $A325;

  sysTrapWinValidateHandle = $A326;
  sysTrapFrmValidatePtr = $A327;
  sysTrapCtlValidatePointer = $A328;
  sysTrapWinMoveWindowAddr = $A329;
  sysTrapFrmAddSpaceForObject = $A32A;
  sysTrapFrmNewForm = $A32B;
  sysTrapCtlNewControl = $A32C;
  sysTrapFldNewField = $A32D;
  sysTrapLstNewList = $A32E;
  sysTrapFrmNewLabel = $A32F;
  sysTrapFrmNewBitmap = $A330;
  sysTrapFrmNewGadget = $A331;

  sysTrapFileOpen = $A332;
  sysTrapFileClose = $A333;
  sysTrapFileDelete = $A334;
  sysTrapFileReadLow = $A335;
  sysTrapFileWrite = $A336;
  sysTrapFileSeek = $A337;
  sysTrapFileTell = $A338;
  sysTrapFileTruncate = $A339;
  sysTrapFileControl = $A33A;

  sysTrapFrmActiveState = $A33B;

  sysTrapSysGetAppInfo = $A33C;
  sysTrapSysGetStackInfo = $A33D;

  sysTrapWinScreenMode = $A33E; // was sysTrapScrDisplayMode
  sysTrapHwrLCDGetDepthV33 = $A33F; // This trap obsoleted for OS 3.5 and later
  sysTrapHwrGetROMToken = $A340;

  sysTrapDbgControl = $A341;

  sysTrapExgDBRead = $A342;
  sysTrapExgDBWrite = $A343;

  sysTrapHostControl = $A344; // Renamed from sysTrapSysGremlins, functionality generalized
  sysTrapFrmRemoveObject = $A345;

  sysTrapSysReserved30Trap1 = $A346; // "Reserved" trap in Palm OS 3.0 and later (was sysTrapSysReserved1)

// NOTE: The following two traps are reserved for future mgrs
// that may or may not be present on any particular device.
// They are NOT present by default; code must check first!
  sysTrapExpansionDispatch = $A347;  // Reserved for ExpansionMgr (was sysTrapSysReserved2)
  sysTrapFileSystemDispatch = $A348;  // Reserved for FileSystemMgr (was sysTrapSysReserved3)

  sysTrapOEMDispatch = $A349; // OEM trap in Palm OS 3.0 and later trap table (formerly sysTrapSysReserved4)

// Palm OS 3.1 traps     Palm IIIx and Palm V

  sysTrapHwrLCDContrastV33 = $A34A; // This trap obsoleted for OS 3.5 and later
  sysTrapSysLCDContrast = $A34B;
  sysTrapUIContrastAdjust = $A34C; // Renamed from sysTrapContrastAdjust
  sysTrapHwrDockStatus = $A34D;

  sysTrapFntWidthToOffset = $A34E;
  sysTrapSelectOneTime = $A34F;
  sysTrapWinDrawChar = $A350;
  sysTrapWinDrawTruncChars = $A351;

  sysTrapSysNotifyInit = $A352; // Notification Manager traps
  sysTrapSysNotifyRegister = $A353;
  sysTrapSysNotifyUnregister = $A354;
  sysTrapSysNotifyBroadcast = $A355;
  sysTrapSysNotifyBroadcastDeferred = $A356;
  sysTrapSysNotifyDatabaseAdded = $A357;
  sysTrapSysNotifyDatabaseRemoved = $A358;

  sysTrapSysWantEvent = $A359;

  sysTrapFtrPtrNew = $A35A;
  sysTrapFtrPtrFree = $A35B;
  sysTrapFtrPtrResize = $A35C;

  sysTrapSysReserved31Trap1 = $A35D; // "Reserved" trap in Palm OS 3.1 and later (was sysTrapSysReserved5)

// Palm OS 3.2 & 3.3 traps  Palm VII (3.2) and Fall '99 Palm OS Flash Update (3.3)

  sysTrapHwrNVPrefSet = $A35E; // mapped to FlashParmsWrite
  sysTrapHwrNVPrefGet = $A35F; // mapped to FlashParmsRead
  sysTrapFlashInit = $A360;
  sysTrapFlashCompress = $A361;
  sysTrapFlashErase = $A362;
  sysTrapFlashProgram = $A363;

  sysTrapAlmTimeChange = $A364;
  sysTrapErrAlertCustom = $A365;
  sysTrapPrgStartDialog = $A366; // New version of sysTrapPrgStartDialogV31

  sysTrapSerialDispatch = $A367;
  sysTrapHwrBattery = $A368;
  sysTrapDmGetDatabaseLockState = $A369;

  sysTrapCncGetProfileList = $A36A;
  sysTrapCncGetProfileInfo = $A36B;
  sysTrapCncAddProfile = $A36C;
  sysTrapCncDeleteProfile = $A36D;

  sysTrapSndPlaySmfResource = $A36E;

  sysTrapMemPtrDataStorage = $A36F; // Never actually installed until now.

  sysTrapClipboardAppendItem = $A370;

  sysTrapWiCmdV32 = $A371; // Code moved to INetLib; trap obsolete

 // Palm OS 3.5 traps          Palm IIIc and other products

// HAL Display-layer new traps
  sysTrapHwrDisplayAttributes = $A372;
  sysTrapHwrDisplayDoze = $A373;
  sysTrapHwrDisplayPalette = $A374;

// Screen driver new traps
  sysTrapBltFindIndexes = $A375;
  sysTrapBmpGetBits = $A376; // was BltGetBitsAddr
  sysTrapBltCopyRectangle = $A377;
  sysTrapBltDrawChars = $A378;
  sysTrapBltLineRoutine = $A379;
  sysTrapBltRectangleRoutine = $A37A;

// ScrUtils new traps
  sysTrapScrCompress = $A37B;
  sysTrapScrDecompress = $A37C;

// System Manager new traps
  sysTrapSysLCDBrightness = $A37D;

// WindowColor new traps
  sysTrapWinPaintChar = $A37E;
  sysTrapWinPaintChars = $A37F;
  sysTrapWinPaintBitmap = $A380;
  sysTrapWinGetPixel = $A381;
  sysTrapWinPaintPixel = $A382;
  sysTrapWinDrawPixel = $A383;
  sysTrapWinErasePixel = $A384;
  sysTrapWinInvertPixel = $A385;
  sysTrapWinPaintPixels = $A386;
  sysTrapWinPaintLines = $A387;
  sysTrapWinPaintLine = $A388;
  sysTrapWinPaintRectangle = $A389;
  sysTrapWinPaintRectangleFrame = $A38A;
  sysTrapWinPaintPolygon = $A38B;
  sysTrapWinDrawPolygon = $A38C;
  sysTrapWinErasePolygon = $A38D;
  sysTrapWinInvertPolygon = $A38E;
  sysTrapWinFillPolygon = $A38F;
  sysTrapWinPaintArc = $A390;
  sysTrapWinDrawArc = $A391;
  sysTrapWinEraseArc = $A392;
  sysTrapWinInvertArc = $A393;
  sysTrapWinFillArc = $A394;
  sysTrapWinPushDrawState = $A395;
  sysTrapWinPopDrawState = $A396;
  sysTrapWinSetDrawMode = $A397;
  sysTrapWinSetForeColor = $A398;
  sysTrapWinSetBackColor = $A399;
  sysTrapWinSetTextColor = $A39A;
  sysTrapWinGetPatternType = $A39B;
  sysTrapWinSetPatternType = $A39C;
  sysTrapWinPalette = $A39D;
  sysTrapWinRGBToIndex = $A39E;
  sysTrapWinIndexToRGB = $A39F;
  sysTrapWinScreenLock = $A3A0;
  sysTrapWinScreenUnlock = $A3A1;
  sysTrapWinGetBitmap = $A3A2;

// UIColor new traps
  sysTrapUIColorInit = $A3A3;
  sysTrapUIColorGetTableEntryIndex = $A3A4;
  sysTrapUIColorGetTableEntryRGB = $A3A5;
  sysTrapUIColorSetTableEntry = $A3A6;
  sysTrapUIColorPushTable = $A3A7;
  sysTrapUIColorPopTable = $A3A8;

// misc cleanup and API additions

  sysTrapCtlNewGraphicControl = $A3A9;

  sysTrapTblGetItemPtr = $A3AA;

  sysTrapUIBrightnessAdjust = $A3AB;
  sysTrapUIPickColor = $A3AC;

  sysTrapEvtSetAutoOffTimer = $A3AD;

// Misc int'l/overlay support.
  sysTrapTsmDispatch = $A3AE;
  sysTrapOmDispatch = $A3AF;
  sysTrapDmOpenDBNoOverlay = $A3B0;
  sysTrapDmOpenDBWithLocale = $A3B1;
  sysTrapResLoadConstant = $A3B2;

// new boot-time SmallROM HAL additions
  sysTrapHwrPreDebugInit = $A3B3;
  sysTrapHwrResetNMI = $A3B4;
  sysTrapHwrResetPWM = $A3B5;

  sysTrapKeyBootKeys = $A3B6;

  sysTrapDbgSerDrvOpen = $A3B7;
  sysTrapDbgSerDrvClose = $A3B8;
  sysTrapDbgSerDrvControl = $A3B9;
  sysTrapDbgSerDrvStatus = $A3BA;
  sysTrapDbgSerDrvWriteChar = $A3BB;
  sysTrapDbgSerDrvReadChar = $A3BC;

// new boot-time BigROM HAL additions
  sysTrapHwrPostDebugInit = $A3BD;
  sysTrapHwrIdentifyFeatures = $A3BE;
  sysTrapHwrModelSpecificInit = $A3BF;
  sysTrapHwrModelInitStage2 = $A3C0;
  sysTrapHwrInterruptsInit = $A3C1;

  sysTrapHwrSoundOn = $A3C2;
  sysTrapHwrSoundOff = $A3C3;

// Kernel clock tick routine
  sysTrapSysKernelClockTick = $A3C4;

// MenuEraseMenu is exposed as of PalmOS 3.5, but there are
// no public interfaces for it yet.  Perhaps in a later release.
  sysTrapMenuEraseMenu = $A3C5;

  sysTrapSelectTime = $A3C6;

// Menu Command Bar traps
  sysTrapMenuCmdBarAddButton = $A3C7;
  sysTrapMenuCmdBarGetButtonData = $A3C8;
  sysTrapMenuCmdBarDisplay = $A3C9;

// Silkscreen info
  sysTrapHwrGetSilkscreenID = $A3CA;
  sysTrapEvtGetSilkscreenAreaList = $A3CB;

  sysTrapSysFatalAlertInit = $A3CC;
  sysTrapDateTemplateToAscii = $A3CD;

// New traps dealing with masking private records
  sysTrapSecVerifyPW = $A3CE;
  sysTrapSecSelectViewStatus = $A3CF;
  sysTrapTblSetColumnMasked = $A3D0;
  sysTrapTblSetRowMasked = $A3D1;
  sysTrapTblRowMasked = $A3D2;

// New form trap for dialogs with text entry field
  sysTrapFrmCustomResponseAlert = $A3D3;
  sysTrapFrmNewGsi = $A3D4;

// New dynamic menu functions
  sysTrapMenuShowItem = $A3D5;
  sysTrapMenuHideItem = $A3D6;
  sysTrapMenuAddItem = $A3D7;

// New form traps for "smart gadgets"
  sysTrapFrmSetGadgetHandler = $A3D8;

// More new control functions
  sysTrapCtlSetGraphics = $A3D9;
  sysTrapCtlGetSliderValues = $A3DA;
  sysTrapCtlSetSliderValues = $A3DB;
  sysTrapCtlNewSliderControl = $A3DC;

// Bitmap manager functions
  sysTrapBmpCreate = $A3DD;
  sysTrapBmpDelete = $A3DE;
  sysTrapBmpCompress = $A3DF;
// sysTrapBmpGetBits defined in Screen driver traps
  sysTrapBmpGetColortable = $A3E0;
  sysTrapBmpSize = $A3E1;
  sysTrapBmpBitsSize = $A3E2;
  sysTrapBmpColortableSize = $A3E3;
// extra window namager
  sysTrapWinCreateBitmapWindow = $A3E4;
// Ask for a null event sooner (replaces a macro which Poser hated)
  sysTrapEvtSetNullEventTick = $A3E5;

// Exchange manager call to allow apps to select destination categories
  sysTrapExgDoDialog = $A3E6;

// this call will remove temporary UI like popup lists
  sysTrapSysUICleanup = $A3E7;

// The following 4 traps were "Reserved" traps, present only in SOME post-release builds of Palm OS 3.5
  sysTrapWinSetForeColorRGB = $A3E8;
  sysTrapWinSetBackColorRGB = $A3E9;
  sysTrapWinSetTextColorRGB = $A3EA;
  sysTrapWinGetPixelRGB = $A3EB;

// TRAPS ABOVE THIS POINT CAN NOT CHANGE BECAUSE THEY HAVE
// BEEN RELEASED TO CUSTOMERS IN SHIPPING ROMS AND SDKS.
// (MOVE THIS COMMENT DOWN WHENEVER THE "NEXT" RELEASE OCCURS.)

// WARNING!!  The following are new traps for 4.0.  If this file is merged
// with MAIN sources, new traps that are added for products that precede
// 4.0 MUST insert their traps BEFORE this section.

  sysTrapSysReserved40Trap1 = $A3EC;
  sysTrapSysReserved40Trap2 = $A3ED;
  sysTrapSysReserved40Trap3 = $A3EE;
  sysTrapSysReserved40Trap4 = $A3EF;

// DO NOT CHANGE TRAPS ABOVE THIS LINE
// THESE TRAPS HAVE BEEN RELEASED IN THE 3.5 SDK
// NEW TRAPS FOR PALM OS 4.0 CAN BE ADDED AFTER THIS
// THE ORDER IS NOT IMPORTANT AND CAN BE CHANGED.

// New Trap selector added for New Connection Mgr API
  sysTrapCncMgrDispatch = $A3F0;

// new trap for notify from interrupt, implemented in SysEvtMgr.c
  sysTrapSysNotifyBroadcastFromInterrupt = $A3F1;

// new trap for waking the UI without generating a null event
  sysTrapEvtWakeupWithoutNilEvent = $A3F2;

// new trap for doing stable, fast, 7-bit string compare
  sysTrapStrCompareAscii = $A3F3;

// New trap for accessors available thru PalmOS glue
  sysTrapAccessorDispatch = $A3F4;

  sysTrapBltGetPixel = $A3F5;
  sysTrapBltPaintPixel = $A3F6;
  sysTrapScrScreenInit = $A3F7;
  sysTrapScrUpdateScreenBitmap = $A3F8;
  sysTrapScrPalette = $A3F9;
  sysTrapScrGetColortable = $A3FA;
  sysTrapScrGetGrayPat = $A3FB;
  sysTrapScrScreenLock = $A3FC;
  sysTrapScrScreenUnlock = $A3FD;
  sysTrapFntPrvGetFontList = $A3FE;

// Exchange manager functions
  sysTrapExgRegisterDatatype = $A3FF;
  sysTrapExgNotifyReceive = $A400;
  sysTrapExgNotifyGoto = $A401;
  sysTrapExgRequest = $A402;
  sysTrapExgSetDefaultApplication = $A403;
  sysTrapExgGetDefaultApplication = $A404;
  sysTrapExgGetTargetApplication = $A405;
  sysTrapExgGetRegisteredApplications = $A406;
  sysTrapExgGetRegisteredTypes = $A407;
  sysTrapExgNotifyPreview = $A408;
  sysTrapExgControl = $A409;

// 04/30/00 CS - New Locale Manager handles access to region-specific info like date formats
  sysTrapLmDispatch = $A40A;

// 05/10/00 kwk - New Memory Manager trap for retrieving ROM NVParam values (sys use only)
  sysTrapMemGetRomNVParams = $A40B;

// 05/12/00 kwk - Safe character width Font Mgr call
  sysTrapFntWCharWidth = $A40C;

// 05/17/00 kwk - Faster DmFindDatabase
  sysTrapDmFindDatabaseWithTypeCreator = $A40D;

// New Trap selectors added for time zone picker API
  sysTrapSelectTimeZone = $A40E;
  sysTrapTimeZoneToAscii = $A40F;

// 08/18/00 kwk - trap for doing stable, fast, 7-bit string compare.
// 08/21/00 kwk - moved here in place of  sysTrapSelectDaylightSavingAdjustment.
  sysTrapStrNCompareAscii = $A410;

// New Trap selectors added for time zone conversion API
  sysTrapTimTimeZoneToUTC = $A411;
  sysTrapTimUTCToTimeZone = $A412;

// New trap implemented in PhoneLookup.c
  sysTrapPhoneNumberLookupCustom = $A413;

// new trap for selecting debugger path.
  sysTrapHwrDebugSelect = $A414;

  sysTrapBltRoundedRectangle = $A415;
  sysTrapBltRoundedRectangleFill = $A416;
  sysTrapWinPrvInitCanvas = $A417;

  sysTrapHwrCalcDynamicHeapSize = $A418;
  sysTrapHwrDebuggerEnter = $A419;
  sysTrapHwrDebuggerExit = $A41A;

  sysTrapLstGetTopItem = $A41B;

  sysTrapHwrModelInitStage3 = $A41C;

// 06/21/00 peter - New Attention Manager
  sysTrapAttnIndicatorAllow = $A41D;
  sysTrapAttnIndicatorAllowed = $A41E;
  sysTrapAttnIndicatorEnable = $A41F;
  sysTrapAttnIndicatorEnabled = $A420;
  sysTrapAttnIndicatorSetBlinkPattern = $A421;
  sysTrapAttnIndicatorGetBlinkPattern = $A422;
  sysTrapAttnIndicatorTicksTillNextBlink = $A423;
  sysTrapAttnIndicatorCheckBlink = $A424;
  sysTrapAttnInitialize = $A425;
  sysTrapAttnGetAttention = $A426;
  sysTrapAttnUpdate = $A427;
  sysTrapAttnForgetIt = $A428;
  sysTrapAttnGetCounts = $A429;
  sysTrapAttnListOpen = $A42A;
  sysTrapAttnHandleEvent = $A42B;
  sysTrapAttnEffectOfEvent = $A42C;
  sysTrapAttnIterate = $A42D;
  sysTrapAttnDoSpecialEffects = $A42E;
  sysTrapAttnDoEmergencySpecialEffects = $A42F;
  sysTrapAttnAllowClose = $A430;
  sysTrapAttnReopen = $A431;
  sysTrapAttnEnableNotification = $A432;
  sysTrapHwrLEDAttributes = $A433;
  sysTrapHwrVibrateAttributes = $A434;

// Trap for getting and setting the device password hint.
  sysTrapSecGetPwdHint = $A435;
  sysTrapSecSetPwdHint = $A436;

  sysTrapHwrFlashWrite = $A437;

  sysTrapKeyboardStatusNew = $A438;
  sysTrapKeyboardStatusFree = $A439;
  sysTrapKbdSetLayout = $A43A;
  sysTrapKbdGetLayout = $A43B;
  sysTrapKbdSetPosition = $A43C;
  sysTrapKbdGetPosition = $A43D;
  sysTrapKbdSetShiftState = $A43E;
  sysTrapKbdGetShiftState = $A43F;
  sysTrapKbdDraw = $A440;
  sysTrapKbdErase = $A441;
  sysTrapKbdHandleEvent = $A442;

  sysTrapOEMDispatch2 = $A443;
  sysTrapHwrCustom = $A444;

// 08/28/00 kwk - Trap for getting form's active field.
  sysTrapFrmGetActiveField = $A445;

// 9/18/00 rkr - Added for playing sounds regardless of interruptible flag
  sysTrapSndPlaySmfIrregardless = $A446;
  sysTrapSndPlaySmfResourceIrregardless = $A447;
  sysTrapSndInterruptSmfIrregardless = $A448;

// 10/14/00 ABa: UDA manager
  sysTrapUdaMgrDispatch = $A449;

// WK: private traps for PalmOS
  sysTrapPalmPrivate1 = $A44A;
  sysTrapPalmPrivate2 = $A44B;
  sysTrapPalmPrivate3 = $A44C;
  sysTrapPalmPrivate4 = $A44D;

// 11/07/00 tlw: Added accessors
  sysTrapBmpGetDimensions = $A44E;
  sysTrapBmpGetBitDepth = $A44F;
  sysTrapBmpGetNextBitmap = $A450;
  sysTrapTblGetNumberOfColumns = $A451;
  sysTrapTblGetTopRow = $A452;
  sysTrapTblSetSelection = $A453;
  sysTrapFrmGetObjectIndexFromPtr = $A454;

// 11/10/00 acs
  sysTrapBmpGetSizes = $A455;
  sysTrapWinGetBounds = $A456;

  sysTrapBltPaintPixels = $A457;

// 11/22/00 bob
  sysTrapFldSetMaxVisibleLines = $A458;

// 01/09/01 acs
  sysTrapScrDefaultPaletteState = $A459;

// WARNING!! LEAVE THIS AT THE END AND ALWAYS ADD NEW TRAPS TO
// THE END OF THE TRAP TABLE BUT RIGHT BEFORE THIS TRAP, AND THEN
// RENUMBER THIS ONE TO ONE MORE THAN THE ONE RIGHT BEFORE IT!!!!!!!!!

  sysTrapLastTrapNumber = $A45A;

const
  sysNumTraps = sysTrapLastTrapNumber - sysTrapBase;

implementation

end.
