{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1999-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: HAL.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    General HAL Equates. This header file contains function prototypes for
 *  HAL routines, and is used by both Palm OS and the HAL module.
 *
 * History:
 *     5/31/99 SCL      Created by Steve Lemke
 *     8/24/00 SCL      Cleanup; moved boot-related prototypes (HwrInitProcPtr,
 *                      HwrPreRAMInit, and HwrInit) to "HwrBoot.h"
 *
 *****************************************************************************)

unit hal;

interface

uses palmos, coretraps, errorbase;

(***********************************************************************
 * Hardware Manager (HAL) constants
 **********************************************************************)

// Error codes related to HwrCustom() API
const
  hwrErrHwrCustomNotImplemented = hwrErrorClass or 1;
  hwrErrCreatorNotSupported     = hwrErrorClass or 2;
  hwrErrSelectorNotSupported    = hwrErrorClass or 3;
  hwrErrParamTooSmall           = hwrErrorClass or 4;

(**************************************************************************
 * Prototypes of functions used only when running on the real hardware
 ***************************************************************************)

// HwrCustom call is new in Palm OS 4.0, and many HALs may not support it.
// This won't cause problems though, since the OS installs a default handler
// (in case the HAL doesn't install its own). The default OS handler simply
// always returns hwrErrHwrCustomNotImplemented.

function HwrCustom(creator: UInt32; opCode: UInt32; paramP: Pointer;
                   var paramSizeP: UInt16): Err; syscall sysTrapHwrCustom;

implementation

end.
