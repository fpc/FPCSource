{ Parsed from Foundation.framework NSByteOrder.h }

{$ifdef TYPES}
const
  NS_UnknownByteOrder = CFByteOrderUnknown;
  NS_LittleEndian = CFByteOrderLittleEndian;
  NS_BigEndian = CFByteOrderBigEndian;

type
  NSSwappedFloat = record
    v: cuint;
  end;
type
  NSSwappedFloatPtr = ^NSSwappedFloat;
type
  NSSwappedDouble = record
    v: culonglong;
  end;
type
  NSSwappedDoublePtr = ^NSSwappedDouble;
{$endif}

