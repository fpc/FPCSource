program llvm_procvar_const;

{$C+}
{$modeswitch pointertoprocvar}

const
  INTEGER_VALUE = qword($FFFFFFFFFFFFFFFF);
  POINTER_VALUE: TProcedure = Pointer(INTEGER_VALUE);

var
  Ptr, Int: String;

begin
  Ptr := HexStr(POINTER_VALUE);
  Int := HexStr(PtrUInt(INTEGER_VALUE), SizeOf(Pointer) * 2);
  WriteLn('Pointer(', Ptr, '), PtrUInt(', Int, ')');
  Assert(Ptr = Int);
end.

