{$MACRO ON}

{$define Rsc := }

(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: PalmCompatibility.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Type & macro definitions for compile-time compatibility between
 *    old and new header files.
 *
 * History:
 *       4/26/99  BRM      Created.
 *       99-10-22 jwm      Added old-style data types.
 *       10/23/99 jmp      Added more win-to-scr-oriented aliases.
 *       99-10-26 jwm      Added old-style nWORD_INLINE macros.
 *       11/23/99 jmp      Added ScrDisplayModeOperation-to-WinScreenModeOperation
 *                         definitions by request from Tool team.
 *       05/03/00 CS       Added LanguageType..countryCount, which have been
 *                         subsumed by typedefs and #defines in new Locale Manager.
 *       05/16/00 CS       LmCountryType/LmLanguageType are now back to
 *                         CountryType/LanguageType.
 *                CS       Added #defines for deprecated countryNameLength,
 *                         currencyNameLength, and currencySymbolLength,
 *                         although nobody seems to be using them anyway.
 *       05/30/00 kwk      Added missing #ifndef __PALMCOMPATIBILITY_H__
 *       05/31/00 CS       Redefined languageFirst..countryCount so that they'll
 *                         take on their original values, rather than the new
 *                         ISO sets, which don't all correspond to entries in
 *                         either the old country or new locale settings resources.
 *       10/04/00 kwk      Fixed up countryCount macro - countryLast was countrylast.
 *       11/13/00 sjm      Added deprecated expansion manager names.
 *
 *****************************************************************************)

unit palmcompatibility;

interface

uses  palmos, localemgr, palmlocale, window, fslib;

// The data types Byte, Word, DWord and so on are now deprecated.  We
// recommend that you use the corresponding new data types: for example,
// use Int16 instead of SWord and UInt32 instead of DWord.  In particular,
// the unfortunate distinction between Handle/VoidHand has been fixed:
// use MemHandle instead.

type
  SByte = Int8;

  SWord = Int16;
  Word = UInt16;

  SDWord = Int32;
  DWord = UInt32;

// Logical data types
  SChar= Int8;
  UChar = UInt8;

  Short = Int16;
  UShort = UInt16;

  Int = Int16;
  UInt = UInt16;

  Long = Int32;
  ULong = UInt32;

// Pointer Types
  VoidPtr = MemPtr;
  VoidHand = MemHandle;

  Ptr = MemPtr;
  Handle = MemHandle;

// Because "const BytePtr" means "const pointer to Byte" rather than "pointer
// to const Byte", all these XXXXPtr types are deprecated: you're better off
// just using "Byte *" and so on.  (Even better, use "UInt8 *"!)

  SBytePtr = ^SByte;
  BytePtr = ^Byte;

  SWordPtr = ^SWord;
  WordPtr = ^Word;
  UInt16Ptr = ^UInt16;

  SDWordPtr = ^SDWord;
  DWordPtr = ^DWord;

// Logical data types
  BooleanPtr = ^Boolean;

  CharPtr = ^Char;
  SCharPtr = ^SChar;
  UCharPtr = ^UChar;

  WCharPtr = ^WChar;

  ShortPtr = ^Short;
  UShortPtr = ^UShort;

  IntPtr = ^Int;
  UIntPtr = ^UInt;

  LongPtr = ^Long;
  ULongPtr = ^ULong;

// Instead of indexing through countries and languages, developers should call
// the Locale Manager to index through known locales:
const
  languageFirst = lEnglish;                         // From Preferences.pas
  languageLast  = lDutch;                           // From Preferences.pas
  languageCount = languageLast - languageFirst + 1; // From Preferences.pas
  countryFirst  = cAustralia;                       // From Preferences.pas
  countryLast   = cTaiwan;                          // From Preferences.pas
  countryCount  = countryLast - countryFirst + 1;   // From Preferences.pas

// Incorporated into the Locale Manager:
  countryNameLength = kMaxCountryNameLen + 1;       // From Preferences.pas
  currencyNameLength = kMaxCurrencyNameLen + 1;     // From Preferences.pas
  currencySymbolLength = kMaxCurrencySymbolLen + 1; // From Preferences.pas

(********************************************************************
 *
 *              Deprecated screen stuff
 *
 ********************************************************************)

const
  scrCopy                          = winPaint;
  scrAND                           = winErase;
  scrANDNOT                        = winMask;
  scrXOR                           = winInvert;
  scrOR                            = winOverlay;
  scrCopyNOT                       = winPaintInverse;

  scrDisplayModeGetDefaults        = winScreenModeGetDefaults;
  scrDisplayModeGet                = winScreenModeGet;
  scrDisplayModeSetToDefaults      = winScreenModeSetToDefaults;
  scrDisplayModeSet                = winScreenModeSet;
  scrDisplayModeGetSupportedDepths = winScreenModeGetSupportedDepths;
  scrDisplayModeGetSupportsColor   = winScreenModeGetSupportsColor;

//  ScrOperation                     = WinDrawOperation;

//  ScrDisplayMode(op, widthP, heightP, depthP, enableColorP) WinScreenMode(op, widthP, heightP, depthP, enableColorP)

//  ScrInit() WinScreenInit()

(********************************************************************
 *
 *          Deprecated resource ids
 *
 ********************************************************************)

// Resources with system ids (>= 10000) are subject to change, and
// should _not_ be relied upon.

// System date string resources. You should use DateTemplateToAscii
// (Palm OS 3.5 or later) or DateGlueTemplateToAscii (backwards
// compatible) instead of these resources.

const
  daysOfWeekStrID                  = 10000;
  dayFullNamesStrID                = 10001;
  monthNamesStrID                  = 10002;
  monthFullNamesStrID              = 10003;

// More system date string resources, introduced in Palm OS 3.5.  If you use
// these, you are limiting yourself to running on nothing earlier than 3.5,
// so you likely might as well use DateTempalateToAscii instead.

  daysOfWeekShortStrListID         = 10200;
  daysOfWeekStdStrListID           = 10201;
  daysOfWeekLongStrListID          = 10202;
  monthNamesShortStrListID         = 10203;
  monthNamesStdStrListID           = 10204;
  monthNamesLongStrListID          = 10205;

// The country table resource has changed between versions, and is
// now completely obsolete. Use LmGetLocaleSetting (4.0 or later)
// or LmGlueGetLocaleSetting instead of this resource.

  sysResTCountries                 = Rsc('cnty');
  sysResIDCountries                = 10000;

(********************************************************************
 *
 *          Deprecated Expansion Manager names
 *
 ********************************************************************)

// Expansion Manager
{!!!
  invalidSlotRefNum           = expInvalidSlotRefNum;
  expErrInvalidSlotRefNumber  = expErrInvalidSlotRefNum;
  ExpMediaType_Any            = expMediaType_Any;
  ExpMediaType_MemoryStick    = expMediaType_MemoryStick;
  ExpMediaType_CompactFlash   = expMediaType_CompactFlash;
  ExpMediaType_SecureDigital  = expMediaType_SecureDigital;
  ExpMediaType_MultiMediaCard = expMediaType_MultiMediaCard;
  ExpMediaType_SmartMedia     = expMediaType_SmartMedia;
  ExpMediaType_RAMDisk        = expMediaType_RAMDisk;
  ExpMediaType_PoserHost      = expMediaType_PoserHost;
  ExpMediaType_MacSim         = expMediaType_MacSim;

// VFS Manager:
//  VFSMountClass_SlotDriver    = vfsMountClass_SlotDriver;
//  VFSMountClass_Simulator     = vfsMountClass_Simulator;
  fsOriginBeginning           = vfsOriginBeginning;
  fsOriginCurrent             = vfsOriginCurrent;
  fsOriginEnd                 = vfsOriginEnd;
  fsFilesystemType_VFAT       = vfsFilesystemType_VFAT;
  fsFilesystemType_FAT        = vfsFilesystemType_FAT;
  fsFilesystemType_NTFS       = vfsFilesystemType_NTFS;
  fsFilesystemType_HFSPlus    = vfsFilesystemType_HFSPlus;
  fsFilesystemType_HFS        = vfsFilesystemType_HFS;
  fsFilesystemType_MFS        = vfsFilesystemType_MFS;
  fsFilesystemType_EXT2       = vfsFilesystemType_EXT2;
  fsFilesystemType_FFS        = vfsFilesystemType_FFS;
  fsFilesystemType_NFS        = vfsFilesystemType_NFS;
  fsFilesystemType_AFS        = vfsFilesystemType_AFS;
  fsFilesystemType_Novell     = vfsFilesystemType_Novell;
  fsFilesystemType_HPFS       = vfsFilesystemType_HPFS;
  VFSFileAttributesGet        = VFSFileGetAttributes;
  VFSFileAttributesSet        = VFSFileSetAttributes;
  VFSFileDateGet              = VFSFileGetDate;
  VFSFileDateSet              = VFSFileSetDate;
  VFSVolumeLabelGet           = VFSVolumeGetLabel;
  VFSVolumeLabelSet           = VFSVolumeSetLabel;

// FSLib:
  FS_LIB_APIVersion           = fsLibAPIVersion;
  FSFileAttributesGet         = FSFileGetAttributes;
  FSFileAttributesSet         = FSFileSetAttributes;
  FSFileDateGet               = FSFileGetDate;
  FSFileDateSet               = FSFileSetDate;
  FSVolumeLabelGet            = FSVolumeGetLabel;
  FSVolumeLabelSet            = FSVolumeSetLabel;

// SlotDrvrLib:
  SlotDrvr_LIB_APIVersion     = slotDrvrAPIVersion;
  Slot_SECTOR_SIZE            = slotSectorSize;
}

implementation

end.
