{$MACRO ON}
{$define Rsc := }
(******************************************************************************
 *
 * Copyright (c) 2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: AttentionMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Include file for Attention Manager
 *
 * History:
 *       Name  Date     Description
 *       ----  ----     -----------
 *       peter 06/12/00 Initial Revision
 *       gap   07/21/00 Change parameter list and data structures to support
 *                      specification of card number as well as dbID.
 *
 *****************************************************************************)

unit attentionmgr;

interface

uses palmos, coretraps, rect, errorbase;

(************************************************************
 * Attention Manager result codes
 * (attnErrorClass is defined in ErrorBase)
 *************************************************************)

const
  attnErrMemory = attnErrorClass or 1; // ran out of memory

(************************************************************
 * Attention Indicator bounds
 *************************************************************)

const
  kAttnIndicatorLeft   = 0;
  kAttnIndicatorTop    = 0;
  kAttnIndicatorWidth  = 16;
  kAttnIndicatorHeight = 15;

(************************************************************
 * Constants used for list view drawing.
 *
 * Applications should use the following constants to format
 * the display of information in attention manager list view.
 *
 * The application's small icon should be drawn centered within
 * the first kAttnListMaxIconWidth pixels of the drawing bounds.
 *
 * Two lines of text information describing the attention should
 * then be drawn left justified starting at kAttnListTextOffset
 * from the left edge of the drawing bounds.
 *************************************************************)

const
  kAttnListMaxIconWidth = 15;
  kAttnListTextOffset   = 17;

(********************************************************************
 * Attention Manager Structures
 ********************************************************************)

type
  AttnFlagsType = UInt32;

const
  kAttnFlagsSoundBit        = AttnFlagsType($1);
  kAttnFlagsLEDBit          = AttnFlagsType($2);
  kAttnFlagsVibrateBit      = AttnFlagsType($4);
  kAttnFlagsCustomEffectBit = AttnFlagsType($8);
  // Note: More bits can be defined if/when hardware capability increases

  kAttnFlagsAllBits         = AttnFlagsType($FFFF);

// The following are passed to AttnGetAttention() and AttnUpdate to specify
// overrides from the user settings for an attention request.
  kAttnFlagsUseUserSettings    = AttnFlagsType($0);

  kAttnFlagsAlwaysSound        = kAttnFlagsSoundBit;
  kAttnFlagsAlwaysLED          = kAttnFlagsLEDBit;
  kAttnFlagsAlwaysVibrate      = kAttnFlagsVibrateBit;
  kAttnFlagsAlwaysCustomEffect = kAttnFlagsCustomEffectBit;
  kAttnFlagsEverything         = kAttnFlagsAllBits;

  kAttnFlagsNoSound            = kAttnFlagsSoundBit shl 16;
  kAttnFlagsNoLED              = kAttnFlagsLEDBit shl 16;
  kAttnFlagsNoVibrate          = kAttnFlagsVibrateBit shl 16;
  kAttnFlagsNoCustomEffect     = kAttnFlagsCustomEffectBit shl 16;
  kAttnFlagsNothing            = kAttnFlagsAllBits shl 16;

// The following are used to interpret the feature.
  kAttnFtrCreator              = Rsc('attn');
  kAttnFtrCapabilities         = 0; // Read to determine device capabilities and user settings.

  kAttnFlagsUserWantsSound        = kAttnFlagsSoundBit;
  kAttnFlagsUserWantsLED          = kAttnFlagsLEDBit;
  kAttnFlagsUserWantsVibrate      = kAttnFlagsVibrateBit;
  kAttnFlagsUserWantsCustomEffect = kAttnFlagsCustomEffectBit; // Always false
  kAttnFlagsUserSettingsMask      = kAttnFlagsAllBits;

  kAttnFlagsHasSound         = kAttnFlagsSoundBit shl 16;
  kAttnFlagsHasLED           = kAttnFlagsLEDBit shl 16;
  kAttnFlagsHasVibrate       = kAttnFlagsVibrateBit shl 16;
  kAttnFlagsHasCustomEffect  = kAttnFlagsCustomEffectBit shl 16; // Always true
  kAttnFlagsCapabilitiesMask = kAttnFlagsAllBits shl 16;

type
  AttnLevelType = UInt16;

const
  kAttnLevelInsistent = AttnLevelType(0);
  kAttnLevelSubtle    = AttnLevelType(1);

type
  AttnCommandType = UInt16;

const
  kAttnCommandDrawDetail   = AttnCommandType(1);
  kAttnCommandDrawList     = AttnCommandType(2);
  kAttnCommandPlaySound    = AttnCommandType(3);
  kAttnCommandCustomEffect = AttnCommandType(4);
  kAttnCommandGoThere      = AttnCommandType(5);
  kAttnCommandGotIt        = AttnCommandType(6);
  kAttnCommandSnooze       = AttnCommandType(7);
  kAttnCommandIterate      = AttnCommandType(8);

type
  AttnCommandArgsDrawDetailTag = record
    bounds: RectangleType;
    firstTime: Boolean;
    flags: AttnFlagsType;
  end;

  AttnCommandArgsDrawListTag = record
    bounds: RectangleType;
    firstTime: Boolean;
    flags: AttnFlagsType;
    selected: Boolean;
  end;

  AttnCommandArgsGotItTag = record
    dismissedByUser: Boolean;
  end;

  AttnCommandArgsIterateTag = record
    iterationData: UInt32;
  end;

  AttnCommandArgsTag = record
    case Integer of
      1: (drawDetail: AttnCommandArgsDrawDetailTag);
      2: (drawList: AttnCommandArgsDrawListTag);
      3: (gotIt: AttnCommandArgsGotItTag);
      4: (iterate: AttnCommandArgsIterateTag);
  end;
  AttnCommandArgsType = AttnCommandArgsTag;

type
  AttnLaunchCodeArgsType = record
    command: AttnCommandType;
    userData: UInt32;
    commandArgsP: ^AttnCommandArgsType;
  end;

type
  AttnCallbackProc = function(command: AttnCommandType; userData: UInt32; var commandArgsP: AttnCommandArgsType): Err;

// These details go with the sysNotifyGotUsersAttention notification.
type
  AttnNotifyDetailsType = record
   flags: AttnFlagsType;
  end;

(********************************************************************
 * Public Attention Manager Routines
 ********************************************************************)

function AttnGetAttention(cardNo: UInt16; dbID: LocalID; userData: UInt32;
   callbackFnP: AttnCallbackProc; level: AttnLevelType; flags: AttnFlagsType;
   nagRateInSeconds, nagRepeatLimit: UInt16): Err; syscall sysTrapAttnGetAttention;

function AttnUpdate(cardNo: UInt16; dbID: LocalID; userData: UInt32;
   callbackFnP: AttnCallbackProc; var flagsP: AttnFlagsType;
   var nagRateInSecondsP, nagRepeatLimitP: UInt16): Boolean; syscall sysTrapAttnUpdate;

function AttnForgetIt(cardNo: UInt16; dbID: LocalID; userData: UInt32): Boolean; syscall sysTrapAttnForgetIt;

function AttnGetCounts(cardNo: UInt16; dbID: LocalID; var insistentCountP, subtleCountP: UInt16): UInt16; syscall sysTrapAttnGetCounts;

procedure AttnListOpen; syscall sysTrapAttnListOpen;

procedure AttnIterate(cardNo: UInt16; dbID: LocalID; iterationData: UInt32); syscall sysTrapAttnIterate;

function AttnDoSpecialEffects(flags: AttnFlagsType): Err; syscall sysTrapAttnDoSpecialEffects;

procedure AttnIndicatorEnable(enableIt: Boolean); syscall sysTrapAttnIndicatorEnable;

function AttnIndicatorEnabled: Boolean; syscall sysTrapAttnIndicatorEnabled;

implementation

end.
