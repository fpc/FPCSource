(******************************************************************************
 *
 * Copyright (c) 1995-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: M68KHwr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Pilot debugger remote hardware/system info
 *
 * History:
 *    1/18/95  RM - Created by Ron Marianetti
 *
 *****************************************************************************)

unit m68khwr;

interface

uses palmos;

(***********************************************************************
 * Breakpoint words we use
 ***********************************************************************)

const
  m68kTrapInstr      = $4E40;
  m68kTrapVectorMask = $000F;

(***********************************************************************
 * 68000 Exception Vector table
 ***********************************************************************)

type
  M68KExcTableType = record
    initStack: UInt32;                   // initial stack pointer
    initPC: UInt32;                      // initial PC

    busErr: UInt32;                      // 08
    addressErr: UInt32;                  // 0C
    illegalInstr: UInt32;                // 10
    divideByZero: UInt32;                // 14
    chk: UInt32;                         // 18
    trap: UInt32;                        // 1C
    privilege: UInt32;                   // 20
    trace: UInt32;                       // 24
    aTrap: UInt32;                       // 28
    fTrap: UInt32;                       // 2C
    reserved12: UInt32;                  // 30
    coproc: UInt32;                      // 34
    formatErr: UInt32;                   // 38
    unitializedInt: UInt32;              // 3C

    reserved: array [0..7] of UInt32;    // 40-5C

    spuriousInt: UInt32;                 // 60
    autoVec1: UInt32;                    // 64
    autoVec2: UInt32;                    // 68
    autoVec3: UInt32;                    // 6C
    autoVec4: UInt32;                    // 70
    autoVec5: UInt32;                    // 74
    autoVec6: UInt32;                    // 78
    autoVec7: UInt32;                    // 7C

    trapN: array [0..15] of UInt32;      // 80 - BC

    unassigned: array [0..15] of UInt32; // C0 - FC
  end;

(**************************************************************************************
 *  structure for the Motorolla 68000 processor registers (variables).
 *
 *  WARNING:
 *  This structure is used as the body of the 'read regs' command response
 *  packet.  Any changes to it will require changes in the nub's code.
 *
 **************************************************************************************)

  M68KRegsType = record
    d: array [0..7] of UInt32; // data registers
    a: array [0..7] of UInt32; // address registers
    usp: UInt32;               // user stack pointer
    ssp: UInt32;               // supervisor stack pointer
    pc: UInt32;                // program counter
    sr: UInt16;                // status register
  end;

(**************************************************************************************
 *  bit masks for testing M68000 status register fields
 **************************************************************************************)

// trace mode
const
  m68kSrTraceMask = $08000;
  m68kSrTraceBit  = 15;

// supervisor state
  m68kSrSupervisorMask = $02000;

// interrupt mask
  m68kSrInterruptMask   = $00700;
  m68kSrInterruptOffset = 8;      // offset for right-shifting interrupt mask

// condition codes
  m68kSrExtendMask   = $00010;
  m68kSrNegativeMask = $00008;
  m68kSrZeroMask     = $00004;
  m68kSrOverflowMask = $00002;
  m68kSrCarryMask    = $00001;

implementation

end.
