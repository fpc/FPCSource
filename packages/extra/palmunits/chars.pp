(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: Chars.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *        This file defines the characters in fonts.
 *
 * History:
 *    November 3, 1994  Created by Roger Flores
 *    11/03/94 rsf   Created by Roger Flores.
 *    04/21/99 JFS   Added list of virtual command key ranges reserved
 *                   for use by licensees.
 *    09/13/99 kwk   Added vchrTsmMode.
 *    10/28/99 kwk   Defined vchrPageUp and vchrPageDown.
 *  2001-02-10 jwm   Added symbol11Help and corrected values of *ArrowDisabled
 *
 *****************************************************************************)

unit chars;

interface

uses palmos;

// Standard Unicode 2.0 names for the ascii characters. These exist in
// all of the text fonts, no matter what character encoding is being
// used by PalmOS.

const
  chrNull = $0000;
  chrStartOfHeading = $0001;
  chrStartOfText = $0002;
  chrEndOfText = $0003;
  chrEndOfTransmission = $0004;
  chrEnquiry = $0005;
  chrAcknowledge = $0006;
  chrBell = $0007;
  chrBackspace = $0008;
  chrHorizontalTabulation = $0009;
  chrLineFeed = $000A;
  chrVerticalTabulation = $000B;
  chrFormFeed = $000C;
  chrCarriageReturn = $000D;
  chrShiftOut = $000E;
  chrShiftIn = $000F;
  chrDataLinkEscape = $0010;
  chrDeviceControlOne = $0011;
  chrDeviceControlTwo = $0012;
  chrDeviceControlThree = $0013;
  chrDeviceControlFour = $0014;
  chrNegativeAcknowledge = $0015;
  chrSynchronousIdle = $0016;
  chrEndOfTransmissionBlock = $0017;
  chrCancel = $0018;
  chrEndOfMedium = $0019;
  chrSubstitute = $001A;
  chrEscape = $001B;
  chrFileSeparator = $001C;
  chrGroupSeparator = $001D;
  chrRecordSeparator = $001E;
  chrUnitSeparator = $001F;
  chrSpace = $0020;
  chrExclamationMark = $0021;
  chrQuotationMark = $0022;
  chrNumberSign = $0023;
  chrDollarSign = $0024;
  chrPercentSign = $0025;
  chrAmpersand = $0026;
  chrApostrophe = $0027;
  chrLeftParenthesis = $0028;
  chrRightParenthesis = $0029;
  chrAsterisk = $002A;
  chrPlusSign = $002B;
  chrComma = $002C;
  chrHyphenMinus = $002D;
  chrFullStop = $002E;
  chrSolidus = $002F;
  chrDigitZero = $0030;
  chrDigitOne = $0031;
  chrDigitTwo = $0032;
  chrDigitThree = $0033;
  chrDigitFour = $0034;
  chrDigitFive = $0035;
  chrDigitSix = $0036;
  chrDigitSeven = $0037;
  chrDigitEight = $0038;
  chrDigitNine = $0039;
  chrColon = $003A;
  chrSemicolon = $003B;
  chrLessThanSign = $003C;
  chrEqualsSign = $003D;
  chrGreaterThanSign = $003E;
  chrQuestionMark = $003F;
  chrCommercialAt = $0040;
  chrCapital_A = $0041;
  chrCapital_B = $0042;
  chrCapital_C = $0043;
  chrCapital_D = $0044;
  chrCapital_E = $0045;
  chrCapital_F = $0046;
  chrCapital_G = $0047;
  chrCapital_H = $0048;
  chrCapital_I = $0049;
  chrCapital_J = $004A;
  chrCapital_K = $004B;
  chrCapital_L = $004C;
  chrCapital_M = $004D;
  chrCapital_N = $004E;
  chrCapital_O = $004F;
  chrCapital_P = $0050;
  chrCapital_Q = $0051;
  chrCapital_R = $0052;
  chrCapital_S = $0053;
  chrCapital_T = $0054;
  chrCapital_U = $0055;
  chrCapital_V = $0056;
  chrCapital_W = $0057;
  chrCapital_X = $0058;
  chrCapital_Y = $0059;
  chrCapital_Z = $005A;
  chrLeftSquareBracket = $005B;
//   chrReverseSolidus $005C (not in Japanese fonts)
  chrRightSquareBracket = $005D;
  chrCircumflexAccent = $005E;
  chrLowLine = $005F;
  chrGraveAccent = $0060;
  chrSmall_A = $0061;
  chrSmall_B = $0062;
  chrSmall_C = $0063;
  chrSmall_D = $0064;
  chrSmall_E = $0065;
  chrSmall_F = $0066;
  chrSmall_G = $0067;
  chrSmall_H = $0068;
  chrSmall_I = $0069;
  chrSmall_J = $006A;
  chrSmall_K = $006B;
  chrSmall_L = $006C;
  chrSmall_M = $006D;
  chrSmall_N = $006E;
  chrSmall_O = $006F;
  chrSmall_P = $0070;
  chrSmall_Q = $0071;
  chrSmall_R = $0072;
  chrSmall_S = $0073;
  chrSmall_T = $0074;
  chrSmall_U = $0075;
  chrSmall_V = $0076;
  chrSmall_W = $0077;
  chrSmall_X = $0078;
  chrSmall_Y = $0079;
  chrSmall_Z = $007A;
  chrLeftCurlyBracket = $007B;
  chrVerticalLine = $007C;
  chrRightCurlyBracket = $007D;
  chrTilde = $007E;
  chrDelete = $007F;

// Special meanings given to characters by the PalmOS
  chrTab = chrHorizontalTabulation;              // $0009
  vchrPageUp = chrVerticalTabulation;            // $000B
  vchrPageDown = chrFormFeed;                    // $000C
  chrOtaSecure = chrDeviceControlFour;           // $0014
  chrOta = chrNegativeAcknowledge;               // $0015
  chrCommandStroke = chrSynchronousIdle;         // $0016
  chrShortcutStroke = chrEndOfTransmissionBlock; // $0017
  chrEllipsis = chrCancel;                       // $0018
  chrNumericSpace = chrEndOfMedium;              // $0019
  chrCardIcon = chrSubstitute;                   // $001A   Card Icon glyph, added in PalmOS 4.0
  chrLeftArrow = chrFileSeparator;               // $001C
  chrRightArrow = chrGroupSeparator;             // $001D
  chrUpArrow = chrRecordSeparator;               // $001E
  chrDownArrow = chrUnitSeparator;               // $001F

// The following are key codes used for virtual events, like
// low battery warnings, etc. These keyboard events MUST
// have the commandKeyMask bit set in the modifiers in order
// to be recognized.
  vchrLowBattery = $0101;                        // Display low battery dialog
  vchrEnterDebugger = $0102;                     // Enter Debugger
  vchrNextField = $0103;                         // Go to next field in form
  vchrStartConsole = $0104;                      // Startup console task
  vchrMenu = $0105;                              // Ctl-A
  vchrCommand = $0106;                           // Ctl-C
  vchrConfirm = $0107;                           // Ctl-D
  vchrLaunch = $0108;                            // Ctl-E
  vchrKeyboard = $0109;                          // Ctl-F popup the keyboard in appropriate mode
  vchrFind = $010A;
  vchrCalc = $010B;
  vchrPrevField = $010C;
  vchrAlarm = $010D;                             // sent before displaying an alarm
  vchrRonamatic = $010E;                         // stroke from graffiti area to top half of screen
  vchrGraffitiReference = $010F;                 // popup the Graffiti reference
  vchrKeyboardAlpha = $0110;                     // popup the keyboard in alpha mode
  vchrKeyboardNumeric = $0111;                   // popup the keyboard in number mode
  vchrLock = $0112;                              // switch to the Security app and lock the device
  vchrBacklight = $0113;                         // toggle state of backlight
  vchrAutoOff = $0114;                           // power off due to inactivity timer
// Added for PalmOS 3.0
  vchrExgTest = $0115;                           // put exchange Manager into test mode (&.t)
  vchrSendData = $0116;                          // Send data if possible
  vchrIrReceive = $0117;                         // Initiate an Ir receive manually (&.i)
// Added for PalmOS 3.1
  vchrTsm1 = $0118;                              // Text Services silk-screen button
  vchrTsm2 = $0119;                              // Text Services silk-screen button
  vchrTsm3 = $011A;                              // Text Services silk-screen button
  vchrTsm4 = $011B;                              // Text Services silk-screen button
// Added for PalmOS 3.2
  vchrRadioCoverageOK = $011C;                   // Radio coverage check successful
  vchrRadioCoverageFail = $011D;                 // Radio coverage check failure
  vchrPowerOff = $011E;                          // Posted after autoOffChr or hardPowerChr
 // to put system to sleep with SysSleep.
// Added for PalmOS 3.5
  vchrResumeSleep = $011F;                       // Posted by NotifyMgr clients after they
 // have deferred a sleep request in order
 // to resume it.
  vchrLateWakeup = $0120;                        // Posted by the system after waking up
 // to broadcast a late wakeup notification.
 // FOR SYSTEM USE ONLY
  vchrTsmMode = $0121;                           // Posted by TSM to trigger mode change.
  vchrBrightness = $0122;                        // Activates brightness adjust dialog
  vchrContrast = $0123;                          // Activates contrast adjust dialog

  vchrExpCardInserted = $0124;                   // ExpansionMgr card inserted & removed.
  vchrExpCardRemoved = $0125;                    // NOTE: these keys will never show up in an
                                                 // app's event loop (they are caught inside
                                                 // EvtGetEvent()), and will probably be
                                                 // deprecated soon (see comments in ExpansionMgr.c).

  vchrExgIntData = $01FF;                        // Exchange Manager wakeup event

// Added for PalmOS 4.0          NOTE: 0x1FF is used above - not in numeric order!
  vchrAttnStateChanged = $0126;                 // Posted by AttentionMgr API to open or update dialog
  vchrAttnUnsnooze = $0127;                     // Posted when AttentionMgr snooze timer expires
  vchrAttnIndicatorTapped = $0128;              // Posted when AttentionIndicator is tapped
  vchrAttnAllowClose = $0129;                   // Posted when AttnAllowClose is called
  vchrAttnReopen = $012A;                       // Posted when AttnReopen is called
  vchrCardCloseMenu = $012B;                    // Posted when a card is inserted
  vchrIrGotData = $012C;                        // Posted when IR Receive initiated
                                                // and copying of an app is imminent

// The application launching buttons generate the following
// key codes and will also set the commandKeyMask bit in the
// modifiers field
  vchrHardKeyMin = $0200;
  vchrHardKeyMax = $02FF;                        // 256 hard keys

  vchrHard1 = $0204;
  vchrHard2 = $0205;
  vchrHard3 = $0206;
  vchrHard4 = $0207;
  vchrHardPower = $0208;
  vchrHardCradle = $0209;                        // Button on cradle pressed
  vchrHardCradle2 = $020A;                       // Button on cradle pressed and hwrDockInGeneric1
 // input on dock asserted (low).
  vchrHardContrast = $020B;                      // Sumo's Contrast button
  vchrHardAntenna = $020C;                       // Eleven's Antenna switch
  vchrHardBrightness = $020D;                    // Hypothetical Brightness button
  vchrPageUpWhenOff = $020E;                     // The m100 device's page-up clock button
  vchrHardEarbud = $020F;                        // Hypothetical Earbud button

// The following keycode RANGES are reserved for use by licensees.
// All have the commandKeyMask bit set in the event's modifiers field.
// Note that ranges include the Min and Max values themselves (i.e. key
// codes >= min and <= max are assigned to the following licensees).
//
// Qualcomm
  vchrThumperMin = $0300;
  vchrThumperMax = $03FF;                        // 256 command keys

// Motorola
  vchrCessnaMin = $14CD;
  vchrCessnaMax = $14CD;                         // 1 command key

// TRG
  vchrCFlashMin = $1500;
  vchrCFlashMax = $150F;                         // 16 command keys

// Symbol
  vchrSPTMin = $15A0;
  vchrSPTMax = $15AF;                            // 16 command keys

// Handspring
  vchrSlinkyMin = $1600;
  vchrSlinkyMax = $16FF;                         // 256 command keys

// Palm
  vchrPalmMin = $1700;                           // 256 command keys
  vchrPalmMax = $17ff;

// Old names for some of the characters.
  nullChr = chrNull;                             // $0000
  backspaceChr = chrBackspace;                   // $0008
  tabChr = chrHorizontalTabulation;              // $0009
  linefeedChr = chrLineFeed;                     // $000A
  pageUpChr = vchrPageUp;                        // $000B
  chrPageUp = vchrPageUp;                        // $000B
  pageDownChr = vchrPageDown;                    // $000C
  chrPageDown = vchrPageDown;                    // $000C
  crChr = chrCarriageReturn;                     // $000D
  returnChr = chrCarriageReturn;                 // $000D
  otaSecureChr = chrOtaSecure;                   // $0014
  otaChr = chrOta;                               // $0015

  escapeChr = chrEscape;                         // $001B
  leftArrowChr = chrLeftArrow;                   // $001C
  rightArrowChr = chrRightArrow;                 // $001D
  upArrowChr = chrUpArrow;                       // $001E
  downArrowChr = chrDownArrow;                   // $001F
  spaceChr = chrSpace;                           // $0020
  quoteChr = chrQuotationMark;                   // $0022 '"'
  commaChr = chrComma;                           // $002C ','
  periodChr = chrFullStop;                       // $002E '.'
  colonChr = chrColon;                           // $003A ':'
  lowBatteryChr = vchrLowBattery;                // $0101
  enterDebuggerChr = vchrEnterDebugger;          // $0102
  nextFieldChr = vchrNextField;                  // $0103
  startConsoleChr = vchrStartConsole;            // $0104
  menuChr = vchrMenu;                            // $0105
  commandChr = vchrCommand;                      // $0106
  confirmChr = vchrConfirm;                      // $0107
  launchChr = vchrLaunch;                        // $0108
  keyboardChr = vchrKeyboard;                    // $0109
  findChr = vchrFind;                            // $010A
  calcChr = vchrCalc;                            // $010B
  prevFieldChr = vchrPrevField;                  // $010C
  alarmChr = vchrAlarm;                          // $010D
  ronamaticChr = vchrRonamatic;                  // $010E
  graffitiReferenceChr = vchrGraffitiReference;  // $010F
  keyboardAlphaChr = vchrKeyboardAlpha;          // $0110
  keyboardNumericChr = vchrKeyboardNumeric;      // $0111
  lockChr = vchrLock;                            // $0112
  backlightChr = vchrBacklight;                  // $0113
  autoOffChr = vchrAutoOff;                      // $0114
  exgTestChr = vchrExgTest;                      // $0115
  sendDataChr = vchrSendData;                    // $0116
  irReceiveChr = vchrIrReceive;                  // $0117
  radioCoverageOKChr = vchrRadioCoverageOK;      // $011C
  radioCoverageFailChr = vchrRadioCoverageFail;  // $011D
  powerOffChr = vchrPowerOff;                    // $011E
  resumeSleepChr = vchrResumeSleep;              // $011F
  lateWakeupChr = vchrLateWakeup;                // $0120
  brightnessChr = vchrBrightness;                // $0121
  contrastChr = vchrContrast;                    // $0122
  hardKeyMin = vchrHardKeyMin;                   // $0200
  hardKeyMax = vchrHardKeyMax;                   // $02FF
  hard1Chr = vchrHard1;                          // $0204
  hard2Chr = vchrHard2;                          // $0205
  hard3Chr = vchrHard3;                          // $0206
  hard4Chr = vchrHard4;                          // $0207
  hardPowerChr = vchrHardPower;                  // $0208
  hardCradleChr = vchrHardCradle;                // $0209
  hardCradle2Chr = vchrHardCradle2;              // $020A
  hardContrastChr = vchrHardContrast;            // $020B
  hardAntennaChr = vchrHardAntenna;              // $020C
  hardBrightnessChr = vchrHardBrightness;        // $020D
  hardEarbudChr = vchrHardEarbud;                // 0x020F

// Macros to determine correct character code to use for drawing numeric space
// and horizontal ellipsis.

(*
#define ChrNumericSpace(chP)
 do {
  UInt32 attribute;
  if ((FtrGet(sysFtrCreator, sysFtrNumROMVersion, &attribute) == 0)
  && (attribute >= sysMakeROMVersion(3, 1, 0, 0, 0))) {
   *(chP) = chrNumericSpace;
  } else {
   *(chP) = 0x80;
  }
 } while (0)

#define ChrHorizEllipsis(chP)
 do {
  UInt32 attribute;
  if ((FtrGet(sysFtrCreator, sysFtrNumROMVersion, &attribute) == 0)
  && (attribute >= sysMakeROMVersion(3, 1, 0, 0, 0))) {
   *(chP) = chrEllipsis;
  } else {
   *(chP) = 0x85;
  }
 } while (0)
*)

// Characters in the 9 point symbol font.  Resource ID 9003
type
  symbolChars = Enum;

const
  symbolLeftArrow = 3;
  symbolRightArrow = Succ(symbolLeftArrow);
  symbolUpArrow = Succ(symbolRightArrow);
  symbolDownArrow = Succ(symbolUpArrow);
  symbolSmallDownArrow = Succ(symbolDownArrow);
  symbolSmallUpArrow = Succ(symbolSmallDownArrow);
  symbolMemo = 9;
  symbolHelp = Succ(symbolMemo);
  symbolNote = Succ(symbolHelp);
  symbolNoteSelected = Succ(symbolNote);
  symbolCapsLock = Succ(symbolNoteSelected);
  symbolNumLock = Succ(symbolCapsLock);
  symbolShiftUpper = Succ(symbolNumLock);
  symbolShiftPunc = Succ(symbolShiftUpper);
  symbolShiftExt = Succ(symbolShiftPunc);
  symbolShiftNone = Succ(symbolShiftExt);
  symbolNoTime = Succ(symbolShiftNone);
  symbolAlarm = Succ(symbolNoTime);
  symbolRepeat = Succ(symbolAlarm);
  symbolCheckMark = Succ(symbolRepeat);
  // These next four characters were moved from the 0x8D..0x90
  // range in the main fonts to the 9pt Symbol font in PalmOS 3.1
  symbolDiamondChr = Succ(symbolCheckMark);
  symbolClubChr = Succ(symbolDiamondChr);
  symbolHeartChr = Succ(symbolClubChr);
  symbolSpadeCh = Succ(symbolHeartChr);

// Character in the 7 point symbol font.  Resource ID 9005
type
  symbol7Chars = Enum;

const
  symbol7ScrollUp = 1;
  symbol7ScrollDown = Succ(symbol7ScrollUp);
  symbol7ScrollUpDisabled = Succ(symbol7ScrollDown);
  symbol7ScrollDownDisabled = Succ(symbol7ScrollUpDisabled);

// Characters in the 11 point symbol font.  Resource ID 9004
type
  symbol11Chars = Enum;

const
  symbolCheckboxOff = 0;
  symbolCheckboxOn = Succ(symbolCheckboxOff);
  symbol11LeftArrow = Succ(symbolCheckboxOn);
  symbol11RightArrow = Succ(symbol11LeftArrow);
  symbol11Help = Succ(symbol11RightArrow);
  symbol11LeftArrowDisabled = Succ(symbol11Help); // New for Palm OS v3.2
  symbol11RightArrowDisabled = Succ(symbol11LeftArrowDisabled);  // New for Palm OS v3.2

implementation

end.
