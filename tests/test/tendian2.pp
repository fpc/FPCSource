program tendian;

var
  i16: Int16;
  i32: Int32;
  i64: Int64;
  u16: UInt16;
  u32: UInt32;
  u64: UInt64;
begin
  i16 := $1234;
  i32 := $12345678;
  i64 := $1234567843218765;
  u16 := $1234;
  u32 := $12345678;
  u64 := $1234567843218765;

  if SwapEndian(i16) <> $3412 then
    Halt(1);
  if SwapEndian(i32) <> $78563412 then
    Halt(2);
  if SwapEndian(i64) <> $6587214378563412 then
    Halt(3);

  if SwapEndian(u16) <> $3412 then
    Halt(4);
  if SwapEndian(u32) <> $78563412 then
    Halt(5);
  if SwapEndian(u64) <> $6587214378563412 then
    Halt(6);
end.
