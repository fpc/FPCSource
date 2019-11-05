program tb0655;

uses
  Variants;

var
  s8: Int8 = $12;
  u8: UInt8 = $98;
  s16: Int16 = $1234;
  u16: UInt16 = $9876;
  s32: Int32 = $12345768;
  u32: UInt32 = $98765432;
  s64: Int64 = $1234567812345678;
  u64: UInt64 = UInt64($9876543298765432);
  v: Variant;
  ov: OleVariant;
begin
  v := s8;
  if VarType(v) <> varShortInt then
    Halt(1);
  if Int8(v) <> s8 then
    Halt(2);

  v := u8;
  if VarType(v) <> varByte then
    Halt(3);
  if UInt8(v) <> u8 then
    Halt(4);

  v := s16;
  if VarType(v) <> varSmallInt then
    Halt(5);
  if Int16(v) <> s16 then
    Halt(6);

  v := u16;
  if VarType(v) <> varWord then
    Halt(7);
  if UInt16(v) <> u16 then
    Halt(8);

  v := s32;
  if VarType(v) <> varInteger then
    Halt(9);
  if Int32(v) <> s32 then
    Halt(10);

  v := u32;
  if VarType(v) <> varLongWord then
    Halt(11);
  if UInt32(v) <> u32 then
    Halt(12);

  v := s64;
  if VarType(v) <> varInt64 then
    Halt(13);
  if Int64(v) <> s64 then
    Halt(14);

  v := u64;
  if VarType(v) <> varUInt64 then
    Halt(15);
  if UInt64(v) <> u64 then
    Halt(16);

  { OleVariant has slightly different behaviour to Variant }
  ov := s8;
  if VarType(ov) <> varInteger then
    Halt(17);
  if Int8(ov) <> s8 then
    Halt(18);

  ov := u8;
  if VarType(ov) <> varInteger then
    Halt(19);
  if UInt8(ov) <> u8 then
    Halt(20);

  ov := s16;
  if VarType(ov) <> varInteger then
    Halt(21);
  if Int16(ov) <> s16 then
    Halt(22);

  ov := u16;
  if VarType(ov) <> varInteger then
    Halt(23);
  if UInt16(ov) <> u16 then
    Halt(24);

  ov := s32;
  if VarType(ov) <> varInteger then
    Halt(25);
  if Int32(ov) <> s32 then
    Halt(26);

  ov := u32;
  if VarType(ov) <> varInteger then
    Halt(27);
  { ! }
  if UInt32(Int32(ov)) <> u32 then
    Halt(28);

  ov := s64;
  if VarType(ov) <> varInt64 then
    Halt(29);
  if Int64(ov) <> s64 then
    Halt(30);

  ov := u64;
  if VarType(ov) <> varUInt64 then
    Halt(31);
  if UInt64(ov) <> u64 then
    Halt(32);
end.
