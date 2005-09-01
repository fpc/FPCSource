{ Source provided for Free Pascal Bug Report 4308 }
{ Submitted by "Olle" on  2005-08-22 }
{ e-mail: olle.r@automagika.se }
program loop_var_assgn;

	procedure ZeroBlock (startAddr: PtrUInt; length: SizeUInt);

		var
			i: PtrUInt;

	begin
		for i := startAddr to startAddr + length - 1 do
			PByte(i)^ := 0;
	end;

begin
end.
