{$MACRO ON}

{$define Rsc := }
(******************************************************************************
 *
 * Copyright (c) 1995-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: HwrMiscFlags.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Bit constants for the hardware MiscFlags
 *
 * History:
 *     10/26/99   JED   Created by Jesse Donaldson, extracted from <HwrGlobals.h>
 *
 *****************************************************************************)

unit hwrmiscflags;

interface

(**************************************************************************
 * General Equates
 ***************************************************************************)

// ----------------------------------------------------------------------
// NOTE: In some ROMs between 3.0 and 3.3 (inclusive), OEMs may have
// ROM tokens present in the ROM that were detected by the boot code
// in order to set the various HwrMiscFlags and GHwrMiscFlagsExt
// settings.  That scheme is no longer relevant, since starting with
// version 3.5 and later, it is now the responsibility of the HAL
// to set these flags, using whatever means necessary to determine
// what "features" the device has that the higher level OS may want
// to know about.
//
// These flags are defined in this public header file since both
// of these low memory globals are published as system features
// (sysFtrNumHwrMiscFlags and sysFtrNumHwrMiscFlagsExt) in <SystemMgr.h>.
// These features are for third party software that may (for whatever
// reason) want to know about certain hardware differences without
// having to read the low memory globals directly.
//
// Palm OS v3.1 was the first OS to publish sysFtrNumHwrMiscFlags as a feature.
//  Call FtrGet first; if the feature doesn't exist, check the OS version:
// Palm OS v2.0 and 3.0 have GHwrMiscFlags defined as a low memory global.
// Palm OS v1.0 did not have GHwrMiscFlags, so its contents are unpredictable.
//      Any devices running Palm OS v1.0 devices should assume zero for all flags.
// ----------------------------------------------------------------------

// Bits in the low memory global GHwrMiscFlags (UInt16)

const
  hwrMiscFlagHasBacklight    = $0001; // set if backlight is present
  hwrMiscFlagHasMbdIrDA      = $0002; // set if IrDA is present (on main board)
  hwrMiscFlagHasCardIrDA     = $0004; // set if IrDA is present (on memory card)
  hwrMiscFlagHasBurrBrown    = $0008; // set if BurrBrown A/D is present
  hwrMiscFlagHasJerryHW      = $0010; // set if Jerry Hardware is present
  hwrMiscFlagNoRTCBug        = $0020; // set if using rev of DragonBall (3G or later)
                                      //  that doesn't require the RealTimeClock
                                      //  bug work-around (see TimeMgr68328.c).
                                      //  <chg 3-27-98 RM>
  hwrMiscFlagHas3vRef        = $0040; // set if switchable 3v reference is present
  hwrMiscFlagHasAntennaSw    = $0080; // set if viewer has an antenna raised switch
  hwrMiscFlagHasCradleDetect = $0100; // set if we have an A/D converter on hotsync port used for ID'ing the attached device
  hwrMiscFlagHasSWContrast   = $0200; // set if UI should support software contrast
  hwrMiscFlagInvertLCDForBL  = $0400; // set if we need to invert LCD w/Backlight
  hwrMiscFlagHasMiscFlagExt  = $0800; // set if we have new hwrMiscFlagsExt

  // The following bit flags are set by HwrIdentifyFeatures.
  // They allow software to read the hardware ID without poking at hardware.
  // They also provide some isolation from different ID detection schemes
  // such as if the ID detection mechanism should change with EZ...

  hwrMiscFlagID1             = $1000; // set if ID bit keyBitHard1 was set
  hwrMiscFlagID2             = $2000; // set if ID bit keyBitHard2 was set
  hwrMiscFlagID3             = $4000; // set if ID bit keyBitHard3 was set
  hwrMiscFlagID4             = $8000; // set if ID bit keyBitHard4 was set
  hwrMiscFlagIDMask          = $F000;
  hwrMiscFlagIDOffset        = 12;     // Bits to shift to get a numeric ID

// NOTE: Currently, the '328 IDs don't overlap with the 'EZ IDs.  This is NOT a requirement,
// but is convenient for the time being as it makes it one step easier to identify a device.
// If the spaces are forced to overlap, it will be necessary to first check the processor
// type (328 or EZ) and then parse the product ID code.  Fortunately, this scheme is rapidly
// becoming obsolete since it was based on reading the keyboard I/O pins, and new products
// are starting to move their keyboard I/O bits to new places.  With the introduction of
// different HAL modules, identifying the actual hardware is now something the HAL code
// will do when the device boots.  The HAL need only do whatever it needs to do to uniquely
// tell the difference between those devices on which it is capable of operating.  Once
// the hardware is identified, the appropriate hwrMiscFlag and hwrMiscFlagExt bits can be
// set to tell the OS what features are present, and the appropriate hardware ID information
// can also be set so higher level software can uniquely identify the OEM/Device/HAL info.
//
// Changes
//  3/16/99 SCL: Documented '328 and 'EZ IDs and how the space could overlap if necessary
//  3/31/99 SRJ: hwrMiscFlagIDUndetermined created, used specifically during the boot sequence
//               before we have done HwrIdentifyFeatures().
// 10/29/99 SCL: Renamed hwrMiscFlagIDOther to hwrMiscFlagIDCheckROMToken
// 10/29/99 SCL: Assigned hwrMiscFlagIDUnused1 to hwrMiscFlagIDUndetermined for Palm OS 3.5
// 10/29/99 SCL: Assigned hwrMiscFlagIDUnused2 to hwrMiscFlagIDCheckOEMFtrs for Palm OS 3.5
// 11/ 2/99 SCL: Assigned hwrMiscFlagIDUnused3 to hwrMiscFlagIDCobra2 for Palm OS 3.5

// hwrMiscFlagIDCheckROMToken indicates that the actual device ID information
// should be read from hwrROMTokenHardwareID using SysGetROMToken or HwrGetROMToken.
// Attached to this token is the OEM ID and the OEM-specific Product ID.
// This scheme was used in Palm OS releases prior to 3.5.  See <HwrROMToken.h> for details.
// This ID is also reported when booting on PalmPilot devices (aka 2.0 hardware).

  hwrMiscFlagIDCheckROMToken = 0; // used to be hwrMiscFlagIDOther
  hwrMiscFlagIDPalmPilot     = 0; // since it was never explicitly set

// hwrMiscFlagIDUndetermined is what the OS initializes the ID to when booting.
// The HAL is responsible for setting the ID to something valid (and meaningful).

  hwrMiscFlagIDUndetermined  = hwrMiscFlagID1; // used to be hwrMiscFlagIDUnused1

// hwrMiscFlagIDCheckOEMFtrs indicates that the OEM/Device/HAL identification
// information should be read from the new Palm OS 3.5 System Features
// (sysFtrNumOEMCompanyID, sysFtrNumOEMDeviceID, and sysFtrNumOEMHALID)
// or system globals (hwrOEMCompanyID, hwrOEMDeviceID, and hwrOEMHALID).
// This method of hardware device ID is for HAL-based devices starting with Palm OS
// 3.5, but some devices may continue to report valid old-style hwrMiscFlagIDxxx tags.

  hwrMiscFlagIDCheckOEMFtrs  = hwrMiscFlagID2; // used to be hwrMiscFlagIDUnused2

// Old-style Hardware IDs for DragonBall '328 based products

  hwrMiscFlagIDThumper       = hwrMiscFlagID4 or hwrMiscFlagID2;
  hwrMiscFlagIDJerry         = hwrMiscFlagID4 or hwrMiscFlagID3;
  hwrMiscFlagIDRocky         = hwrMiscFlagID4 or hwrMiscFlagID3 or hwrMiscFlagID2;
  hwrMiscFlagIDTouchdown     = hwrMiscFlagID4 or hwrMiscFlagID3 or hwrMiscFlagID2 or hwrMiscFlagID1;

// Old-style Hardware IDs for DragonBall 'EZ based products

  hwrMiscFlagIDJerryEZ       = hwrMiscFlagID3 or hwrMiscFlagID2;
  hwrMiscFlagIDSumo          = hwrMiscFlagID4 or hwrMiscFlagID2 or hwrMiscFlagID1;
  hwrMiscFlagIDBrad          = hwrMiscFlagID4 or hwrMiscFlagID3 or hwrMiscFlagID1;
  hwrMiscFlagIDAustin        = hwrMiscFlagID4 or hwrMiscFlagID1;
  hwrMiscFlagIDCobra2        = hwrMiscFlagID2 or hwrMiscFlagID1;
  hwrMiscFlagIDCalvin        = hwrMiscFlagID3 or hwrMiscFlagID1;

// Hardware SubIDs used to detect hardware type early in boot process

  hwrMiscFlagExtSubIDBrad      = $0;
  hwrMiscFlagExtSubIDSumo      = $2;
  hwrMiscFlagExtSubIDCobra     = $4;
  hwrMiscFlagExtSubIDCobra2_16 = $6;
  hwrMiscFlagExtSubIDCobra2_20 = $7;

// Old-style Hardware IDs still unused

  hwrMiscFlagIDUnused4       = hwrMiscFlagID3;
  hwrMiscFlagIDUnused5       = hwrMiscFlagID3 or hwrMiscFlagID1;
  hwrMiscFlagIDUnused7       = hwrMiscFlagID3 or hwrMiscFlagID2 or hwrMiscFlagID1;
  hwrMiscFlagIDUnused8       = hwrMiscFlagID4;

// Bits in the low memory global GHwrMiscFlagsExt (UInt32)

  hwrMiscFlagExtSubID1       = $00000001; // subtype ID (for feature select in device)
  hwrMiscFlagExtSubID2       = $00000002; // subtype ID (for feature select in device)
  hwrMiscFlagExtSubID3       = $00000004; // subtype ID (for feature select in device)
  hwrMiscFlagExtSubIDMask    = $00000007; // sybtype ID Mask

  hwrMiscFlagExtHasLiIon     = $00000010; // set if we have Lithium Ion battery rechargable in the cradle
  hwrMiscFlagExtHasRailIO    = $00000020; // set if we have Rail I/O hardware
  hwrMiscFlagExtHasFlash     = $00000040; // set (by OS or HAL) if we have Flash ROM
  hwrMiscFlagExtHasFParms    = $00000080; // set (by OS or HAL) if we have Flash parms area

  hwrMiscFlagExt115KIrOK     = $00000100; // device supports 115K IR transfers
  hwrMiscFlagExtHasExtLCD    = $00000200; // device has syscall LCD controller
  hwrMiscFlagExtHasSWBright  = $00000400; // device has software controlled brightness
  // Added by BGT, 08/01/2000
  hwrMiscFlagExtNeedsLpr     = $00000800; // DRAM needs special LP Refresh

// Assigned values for hwrOEMCompanyID (aka sysFtrNumOEMCompanyID):
// Values are assigned by the Palm Computing Platform Engineering group.
//
// Note: These values are different from the values that may be found in some
// OEM devices which used HwrROMTokens on versions of Palm OS prior to 3.5.

  hwrOEMCompanyIDUnspecified = $00000000; // hwrOEMCompanyID not specified by HAL
  hwrOEMHALIDUnspecified     = $00000000; // hwrOEMHALID not specified by HAL
  hwrOEMDeviceIDUnspecified  = $00000000; // hwrOEMDeviceID not specified by HAL

  hwrOEMCompanyIDPalmPlatform = Rsc('psys'); // Reference Platforms made by Palm Computing
  hwrOEMCompanyIDPalmDevices  = Rsc('palm'); // Devices made by Palm Computing

  hwrOEMCompanyIDSymbol       = Rsc('smbl'); // Devices made by Symbol Technologies
  hwrOEMCompanyIDQualcomm     = Rsc('qcom'); // Devices made by Qualcomm
  hwrOEMCompanyIDTRG          = Rsc('trgp'); // Devices made by TRG Products
  hwrOEMCompanyIDHandspring   = Rsc('hspr'); // Devices made by Handspring

// Hardware ID's for DragonBall EZ based products

//  hwrMiscFlagIDSumo          = hwrMiscFlagID4 or hwrMiscFlagID2 or hwrMiscFlagID1;
//  hwrMiscFlagIDBrad          = hwrMiscFlagID4 or hwrMiscFlagID3 or hwrMiscFlagID1;

// Note that values for hwrOEMDeviceID (aka sysFtrNumOEMDeviceID) and
// hwrOEMHALID (aka sysFtrNumOEMHALID) are OEM vendor-specific, and not
// necessarily tracked by this Palm OS header file, though it may be
// worthwhile to include "known" values here for third party developers.
//
// It is recommended that OEM vendors choose values for these globals that
// are four-digit human-readable ASCII values, rather than numeric codes,
// though this is not a requirement.

// HALs that belong to hwrOEMCompanyIDPalmPlatform

  hwrOEMHALIDEZRef           = Rsc('eref'); // (Mono) EZ Reference Platform (Palm Computing)
  hwrOEMHALIDEZRefColor      = Rsc('cref'); // Color EZ Reference Platform (Palm Computing)

// HALs that belong to hwrOEMCompanyIDPalmPlatform
  hwrOEMHALIDVZRef           = Rsc('vref'); // (Mono) VZ Reference Platform (Palm Computing)
  hwrOEMHALIDVZRefColor      = Rsc('cvrf'); // Color VZ Reference Platform (Palm Computing)

// HALs that belong to hwrOEMCompanyIDPalmDevices

  hwrOEMHALID328Jerry        = Rsc('jery'); // Palm VII HAL (Palm Computing)
  hwrOEMHALIDEZSumo          = Rsc('sumo'); // Palm IIIx/V/Vx HAL (Palm Computing)
  hwrOEMHALID328Rocky        = Rsc('rcky'); // Pilot, PalmPilot, Palm III HAL (Palm Computing)
  hwrOEMHALIDEZAustin        = Rsc('astn'); // Palm IIIc (Palm Computing)
  hwrOEMHALIDEZCalvin        = Rsc('cvln'); // Palm m100 (Palm Computing)

implementation

end.
