{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1995-2000 Palm Computing, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: Crc.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *      This is the header file for the CRC calculation routines for Pilot.
 *
 * History:
 *      May 10, 1995    Created by Vitaly Kruglikov
 *      05/10/95    vmk Created by Vitaly Kruglikov.
 *      09/10/99    kwk Crc16CalcBlock takes a const void *.
 *
 *****************************************************************************)

unit crc;

interface

uses palmos, coretraps;

(********************************************************************
 * CRC Calculation Routines
 * These are define as syscall calls only under emulation mode or
 *  under native mode from the module that actually installs the trap
 *  vectors
 ********************************************************************)

//-------------------------------------------------------------------
// API
//-------------------------------------------------------------------

// Crc16CalcBlock()
//
// Calculate the 16-bit CRC of a data block using the table lookup method.
//

function Crc16CalcBlock(const bufP: Pointer; count, crc: UInt16): UInt16; syscall sysTrapCrc16CalcBlock;

//UInt16  Crc16CalcBigBlock(void *bufP, UInt32 count, UInt16 crc);

implementation

end.
