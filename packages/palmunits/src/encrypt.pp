{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: Encrypt.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Equates for encryption/digestion routines in pilot
 *
 * History:
 *    7/31/96  RM - Created by Ron Marianetti
 *
 *****************************************************************************)

unit encrypt;

interface

uses palmos, coretraps;

(************************************************************
 * Function Prototypes
 *************************************************************)

// Perform reversible encryption or decryption of 8 byte string in
//  srcP using 8 byte key keyP. Place 8 byte result in dstP.

function EncDES(srcP, keyP, dstP: UInt8Ptr; encrypt: Boolean): Err; syscall sysTrapEncDES;

// Digest a string of bytes and produce a 128 bit result using
//   the MD4 algorithm.

type
  TDigestArray = array [0..15] of UInt8;

function EncDigestMD4(strP: UInt8Ptr; strLen: UInt16; var digestP: TDigestArray): Err; syscall sysTrapEncDigestMD4;

// Digest a string of bytes and produce a 128 bit result using
//   the MD5 algorithm.

function EncDigestMD5(strP: UInt8Ptr; strLen: UInt16; var digestP: TDigestArray): Err; syscall sysTrapEncDigestMD5;

implementation

end.
