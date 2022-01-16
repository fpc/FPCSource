{ %NORUN }

program tb0676;

{$warn 4110 error}

begin
  SwapEndian(UInt16($1234));
  SwapEndian(Int16($8765));
  SwapEndian(UInt32($12345678));
  SwapEndian(Int32($87654321));
  SwapEndian(UInt64($1234567887654321));
  SwapEndian(Int64($8765432112345678));
end.
