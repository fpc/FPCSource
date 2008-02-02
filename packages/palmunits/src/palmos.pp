unit palmos;

interface

(************************************************************
 * Common constants
 *************************************************************)
type
  Int8 = ShortInt;
  Int16 = Integer;
  Int32 = LongInt;

  UInt8 = Byte;
  UInt16 = Word;
  UInt32 = LongWord;

// Logical data types

  WChar = UInt16;      // 'wide' int'l character type.
  Err = UInt16;
  LocalID = UInt32;    // local (card relative) chunk ID
  Coord = Int16;       // screen/window coordinate
  MemPtr = Pointer;    // global pointer
  MemHandle = Pointer; // global handle

  ProcPtr = function: Int32;

const
  NULL = 0;
  bitsInByte = 8;

(************************************************************
 * Palm specific TRAP instruction numbers
 *************************************************************)

const
  sysDbgBreakpointTrapNum = 0;  // For soft breakpoints
  sysDbgTrapNum           = 8;  // For compiled breakpoints
  sysDispatchTrapNum      = 15; // Trap dispatcher

type
  Enum = Byte;
  WordEnum = Word;
  LongEnum = LongWord;

  Int8Ptr = ^Int8;
  Int16Ptr = ^Int16;
  Int32Ptr = ^Int32;

  UInt8Ptr = ^UInt8;
  UInt16Ptr = ^UInt16;
  UInt32Ptr = ^UInt32;
  PointerPtr = ^Pointer;

  PCharPtr = ^PChar;
  MemPtrPtr = ^MemPtr;
  WCharPtr = ^WChar;
  Smallint = Integer;

implementation

end.
