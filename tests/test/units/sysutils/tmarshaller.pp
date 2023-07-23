{$ifdef fpc}
	{$mode objfpc} {$longstrings on} {$codepage utf8} {$modeswitch advancedrecords}

	{$warn 4055 off : Conversion between ordinals and pointers is not portable}
{$else}
	{$define delphi}
	{$define endian_little}
	{$apptype console}
{$endif}

{-$define test_things_broken_in_fpc}
{-$define test_things_broken_in_xe3}

uses
{$ifdef unix }
cwstring,
{$endif}
	SysUtils;

var
	anythingFailed: boolean = false;

	procedure Fail(const msg: unicodestring);
	begin
		writeln(msg + sLineBreak);
		anythingFailed := true;
	end;

	procedure Expect(const got, expected, what: ansistring); overload;
	begin
		if got <> expected then
			Fail(unicodestring(what + ' failed: got ' + got + ', expected ' + expected + '.'));
	end;

	// My Delphi XE3 has a bug: its TMarshal occasionally does not append #0 to returned unicodestrings (!),
	// and its unicodestring comparison for equality compares terminators, making ExpectU(TMarshal.ReadStringThatDefinitelyWasあ, 'あ') fail.
	function UStrEq(const a, b: unicodestring): boolean;
	begin
	{$ifdef delphi}
		result := (length(a) = length(b)) and CompareMem(pointer(a), pointer(b), length(a) * sizeof(a[1]));
	{$else}
		result := a = b;
	{$endif}
	end;

	procedure ExpectU(const got, expected, what: unicodestring); overload;
	begin
		if not UStrEq(got, expected) then
			Fail(what + ' failed: got ' + got + ', expected ' + expected + '.');
	end;

	procedure Expect(got: pointer; const expected: array of byte; const what: ansistring); overload;
	var
		i: NativeInt;
		lines: array[0 .. 4] of ansistring;
		ba, bb: byte;
	begin
		if not CompareMem(got, @expected[0], length(expected)) then
		begin
			for i := 0 to High(lines) do lines[i] := '';
			for i := 0 to High(expected) do
			begin
				ba := pByte(got)[i];
				bb := expected[i];
				if (ba >= 32) and (ba <= 127) then lines[0] := lines[0] + '  ' + chr(ba) else lines[0] := lines[0] + '---';
				lines[1] := lines[1] + ' ' + IntToHex(ba, 2);
				if ba <> bb then lines[2] := lines[2] + ' !!' else lines[2] := lines[2] + '   ';
				lines[3] := lines[3] + ' ' + IntToHex(bb, 2);
				if (bb >= 32) and (bb <= 127) then lines[4] := lines[4] + '  ' + chr(bb) else lines[4] := lines[4] + '---';
			end;
			Fail(unicodestring(what + ' failed:' + sLineBreak +
				'         ' + lines[0] + sLineBreak +
				'got      ' + lines[1] + sLineBreak +
				'         ' + lines[2] + sLineBreak +
				'expected ' + lines[3] + sLineBreak +
				'         ' + lines[4]));
			exit;
		end;
	end;

{$ifdef delphi}
const
	CP_UTF8 = 65001;

type
	TUnicodeStringBuilder = TStringBuilder;

	function NtoLE(x: smallint): smallint; overload; begin result := x; end;
	function NtoLE(x: longint): longint; overload; begin result := x; end;
	function NtoLE(x: int64): int64; overload; begin result := x; end;
	function LEtoN(x: smallint): smallint; overload; begin result := x; end;
	function LEtoN(x: longint): longint; overload; begin result := x; end;
	function LEtoN(x: int64): int64; overload; begin result := x; end;
{$endif}

const
	ShiftJIS = 932;
	SjJaA: array[0 .. 1] of byte = ($82, $A0); // Bytes of あ in Shift JIS.
	SjJaKa: array[0 .. 1] of byte = ($82, $A9); // Bytes of か in Shift JIS.
	Utf8JaA: array[0 .. 2] of byte = ($E3, $81, $82); // Bytes of あ in UTF-8.
	Utf8JaKa: array[0 .. 2] of byte = ($E3, $81, $8B); // Bytes of か in UTF-8.
	Utf16JaA: array[0 .. 1] of byte = {$ifdef endian_little} ($42, $30) {$else} ($30, $42) {$endif}; // Bytes of あ in UTF-16.
	Utf16JaKa: array[0 .. 1] of byte = {$ifdef endian_little} ($4B, $30) {$else} ($30, $4B) {$endif}; // Bytes of か in UTF-16.

	function Concat(const a, b: array of byte): {$ifdef fpc} specialize {$endif} TArray<byte>;
	begin
		result := nil; // suppress warning
		SetLength(result, length(a) + length(b));
		Move(a[0], result[0], length(a));
		Move(b[0], result[length(a)], length(b));
	end;

	function MemBytes(mem: pointer; n: NativeInt): {$ifdef fpc} specialize {$endif} TArray<byte>;
	begin
		result := nil; // suppress warning
		SetLength(result, n);
		Move(mem^, result[0], n);
	end;

	procedure TestTMarshal;
	var
		pw, pw2, pfa, pfs: TPtrWrapper;
		i: NativeInt;
		ptrVal: pointer;
		i32arr: {$ifdef fpc} specialize {$endif} TArray<longint>;
		us, us2: unicodestring;
	begin
		pw := TPtrWrapper.NilValue;
		pw2 := TPtrWrapper.NilValue;
		pfa := TPtrWrapper.NilValue;
		pfs := TPtrWrapper.NilValue;

		try
			pw := TMarshal.AllocMem(19 + sizeof(pointer));
			TMarshal.WriteByte(pw, 4, $1);
			TMarshal.WriteInt16(pw, 5, NtoLE($2345));
			TMarshal.WriteInt32(pw, 7, NtoLE($6789ABCD));
			TMarshal.WriteInt64(pw, 11, NtoLE(int64($EF00112233445566)));

			// Create a nontrivial pointer value...
			NativeUint(ptrVal) := 0;
			for i := 0 to sizeof(pointer) - 1 do
				NativeUint(ptrVal) := NativeUint(ptrVal) shl 8 or NativeUint(1 + i);

			TMarshal.WritePtr(pw, 19, TPtrWrapper.Create(ptrVal));

			pw2 := TMarshal.ReallocMem(pw, 100);
			pw := TPtrWrapper.NilValue;
			Expect(pw2.ToPointer, Concat([0, 0, 0, 0, $1, $45, $23, $CD, $AB, $89, $67, $66, $55, $44, $33, $22, $11, $00, $EF], MemBytes(@ptrVal, sizeof(ptrVal))),
				'TMarshal.AllocMem/WriteByte/Int16/Int32/Int64/Ptr/ReallocMem');

			Expect(IntToHex(TMarshal.ReadByte(pw2, 4), 2), '01', 'ReadByte');
			Expect(IntToHex(LEtoN(TMarshal.ReadInt16(pw2, 5)), 4), '2345', 'ReadInt16');
			Expect(IntToHex(LEtoN(TMarshal.ReadInt32(pw2, 7)), 8), '6789ABCD', 'ReadInt32');
			Expect(IntToHex(LEtoN(TMarshal.ReadInt64(pw2, 11)), 16), 'EF00112233445566', 'ReadInt64');
			Expect(IntToHex(LEtoN(TMarshal.ReadPtr(pw2, 19).ToInteger), sizeof(pointer) * 2), IntToHex(NativeUint(ptrVal), sizeof(pointer) * 2), 'ReadPtr');

			pw := TMarshal.AllocMem(19);
			TMarshal.Move(pw2, pw, 19);
			Expect(pw.ToPointer, [0, 0, 0, 0, $1, $45, $23, $CD, $AB, $89, $67, $66, $55, $44, $33, $22, $11, $00, $EF], 'Move');

			i32arr := nil; // suppress warning
			SetLength(i32arr, 4);
			i32arr[0] := NtoLE(longint($11223344));
			i32arr[1] := NtoLE(longint($55667788));
			i32arr[2] := NtoLE(longint($99AABBCC));
			i32arr[3] := NtoLE(longint($DDEEFF00));
			TMarshal.Copy(i32arr, 1, pw, 2);
			Expect(pw.ToPointer, [$88, $77, $66, $55, $CC, $BB, $AA, $99, $AB, $89, $67, $66, $55, $44, $33, $22, $11, $00, $EF], 'Copy(Int32[] -> TPtrWrapper)');
			TMarshal.FreeMem(pw);
			pw := TPtrWrapper.NilValue;

			pw := TMarshal.AllocStringAsAnsi('あか', ShiftJIS);
			Expect(pw.ToPointer, Concat(Concat(SjJaA, SjJaKa), [0]), 'AllocStringAsAnsi');
			TMarshal.FreeMem(pw);
			pw := TPtrWrapper.NilValue;

			pw := TMarshal.AllocStringAsUnicode('あか');
			Expect(pw.ToPointer, Concat(Concat(Utf16JaA, Utf16JaKa), [0, 0]), 'AllocStringAsUnicode');
			TMarshal.FreeMem(pw);
			pw := TPtrWrapper.NilValue;

			pw := TMarshal.AllocStringAsUtf8('あか');
			Expect(pw.ToPointer, Concat(Concat(Utf8JaA, Utf8JaKa), [0]), 'AllocStringAsUtf8');
			TMarshal.FreeMem(pw);
			pw := TPtrWrapper.NilValue;

			pfa := TMarshal.{$ifdef fpc} specialize {$endif} FixArray<longint>(i32arr);
			i32arr := nil;
			Expect(pfa.ToPointer, [$44, $33, $22, $11, $88, $77, $66, $55, $CC, $BB, $AA, $99, $00, $FF, $EE, $DD], 'FixArray');
			TMarshal.{$ifdef fpc} specialize {$endif} UnfixArray<longint>(pfa);
			pfa := TPtrWrapper.NilValue;

			us := 'h' + unicodestring(IntToStr(random(0)));
			us2 := us;
			pfs := TMarshal.FixString(us2);
			PWideChar(Pointer(us))[0] := 'q'; // Doing this with us2 (which was passed to FixString) WILL alter the string pfs points to, same in Delphi.
			Expect(pfs.ToPointer, [LEtoN(smallint('h')) and $FF, LEtoN(smallint('h')) shr 8, LEtoN(smallint('0')) and $FF, LEtoN(smallint('0')) shr 8], 'FixString');
			TMarshal.UnfixString(pfs);
			pfs := TPtrWrapper.NilValue;

			FillChar(pw2.ToPointer^, 8, $EE);
			// For now, FP TMarshal does not handle multi-byte characters atomically unlike Delphi TMarshal,
			// but I think this must be addressed in the WideStringManager rather than TMarshal.
		{$if defined(delphi) or defined(test_things_broken_in_fpc)}
			TMarshal.WriteStringAsAnsi(pw2, 2, 'あか', 3, ShiftJIS);
			Expect(pw2.ToPointer, [$EE, $EE, SjJaA[0], SjJaA[1], $EE, $EE, $EE, $EE], 'WriteStringAsAnsi(max=3)');
		{$endif}
			TMarshal.WriteStringAsAnsi(pw2, 2, 'あか', 4, ShiftJIS);
			Expect(pw2.ToPointer, [$EE, $EE, SjJaA[0], SjJaA[1], SjJaKa[0], SjJaKa[1], $EE, $EE], 'WriteStringAsAnsi(max=4)');
			TMarshal.WriteStringAsAnsi(pw2, 2, 'あか', -1, ShiftJIS);
			Expect(pw2.ToPointer, [$EE, $EE, SjJaA[0], SjJaA[1], SjJaKa[0], SjJaKa[1], 0, $EE], 'WriteStringAsAnsi(max=-1)');
			TMarshal.WriteStringAsAnsi(pw2, 7, 'あか', 0, ShiftJIS);
			Expect(pw2.ToPointer, [$EE, $EE, SjJaA[0], SjJaA[1], SjJaKa[0], SjJaKa[1], 0, $EE], 'WriteStringAsAnsi(max=0)');

			ExpectU(TMarshal.ReadStringAsAnsi(ShiftJIS, TPtrWrapper.Create(pw2.ToInteger + 2), -1), 'あか', 'ReadStringAsAnsi(Len = -1)');
			ExpectU(TMarshal.ReadStringAsAnsi(ShiftJIS, TPtrWrapper.Create(pw2.ToInteger + 2), 2), 'あ', 'ReadStringAsAnsi(Len = 2)');
		{$if defined(delphi) or defined(test_things_broken_in_fpc)}
			ExpectU(TMarshal.ReadStringAsAnsi(ShiftJIS, TPtrWrapper.Create(pw2.ToInteger + 2), 3), 'あ', 'ReadStringAsAnsi(Len = 3)');
		{$endif}

			FillChar(pw2.ToPointer^, 9, $EE);
			TMarshal.WriteStringAsUnicode(pw2, 2, 'あか', 1);
			Expect(pw2.ToPointer, [$EE, $EE, Utf16JaA[0], Utf16JaA[1], $EE, $EE, $EE, $EE, $EE], 'WriteStringAsUnicode(max=1)');
			TMarshal.WriteStringAsUnicode(pw2, 2, 'あか', 2);
			Expect(pw2.ToPointer, [$EE, $EE, Utf16JaA[0], Utf16JaA[1], Utf16JaKa[0], Utf16JaKa[1], $EE, $EE, $EE], 'WriteStringAsUnicode(max=2)');
			FillChar(pw2.ToPointer^, 8, $EE);
			TMarshal.WriteStringAsUnicode(pw2, 2, 'あか', -1);
			Expect(pw2.ToPointer, [$EE, $EE, Utf16JaA[0], Utf16JaA[1], Utf16JaKa[0], Utf16JaKa[1], 0, 0, $EE], 'WriteStringAsUnicode(max=-1)');
			TMarshal.WriteStringAsUnicode(pw2, 8, 'あか', 0);
			Expect(pw2.ToPointer, [$EE, $EE, Utf16JaA[0], Utf16JaA[1], Utf16JaKa[0], Utf16JaKa[1], 0, 0, $EE], 'WriteStringAsUnicode(max=0)');

			ExpectU(TMarshal.ReadStringAsUnicode(TPtrWrapper.Create(pw2.ToInteger + 2), -1), 'あか', 'ReadStringAsUnicode(Len = -1)');
			ExpectU(TMarshal.ReadStringAsUnicode(TPtrWrapper.Create(pw2.ToInteger + 2), 1), 'あ', 'ReadStringAsUnicode(Len = 1)');

			FillChar(pw2.ToPointer^, 10, $EE);
		{$if defined(delphi) or defined(test_things_broken_in_fpc)}
			TMarshal.WriteStringAsUtf8(pw2, 2, 'あか', 4);
			Expect(pw2.ToPointer, [$EE, $EE, Utf8JaA[0], Utf8JaA[1], Utf8JaA[2], $EE, $EE, $EE, $EE, $EE], 'WriteStringAsUtf8(max=4)');
		{$endif}
			TMarshal.WriteStringAsUtf8(pw2, 2, 'あか', 6);
			Expect(pw2.ToPointer, [$EE, $EE, Utf8JaA[0], Utf8JaA[1], Utf8JaA[2], Utf8JaKa[0], Utf8JaKa[1], Utf8JaKa[2], $EE, $EE], 'WriteStringAsUtf8(max=6)');
			TMarshal.WriteStringAsUtf8(pw2, 2, 'あか', -1);
			Expect(pw2.ToPointer, [$EE, $EE, Utf8JaA[0], Utf8JaA[1], Utf8JaA[2], Utf8JaKa[0], Utf8JaKa[1], Utf8JaKa[2], 0, $EE], 'WriteStringAsUtf8(max=-1)');

			ExpectU(TMarshal.ReadStringAsUtf8(TPtrWrapper.Create(pw2.ToInteger + 2), -1), 'あか', 'ReadStringAsUtf8(Len = -1)');
			// These things are buggy in Delphi XE3 due to wrong assumptions it makes about when MultiByteToWideChar result
			// includes the null terminator and when it does not, didn't test newer versions.
			// (Shift-JIS version works for me however, probably by chance...)
		{$if defined(fpc) or defined(test_things_broken_in_xe3)}
			ExpectU(TMarshal.ReadStringAsUtf8(TPtrWrapper.Create(pw2.ToInteger + 2), 3), 'あ', 'ReadStringAsUtf8(Len = 3)');
		{$if defined(delphi) or defined(test_things_broken_in_fpc)}
			ExpectU(TMarshal.ReadStringAsUtf8(TPtrWrapper.Create(pw2.ToInteger + 2), 4), 'あ', 'ReadStringAsUtf8(Len = 4)');
		{$endif}
		{$endif}

		finally
			TMarshal.UnfixString(pfs);
			TMarshal.{$ifdef fpc} specialize {$endif} UnfixArray<longint>(pfa);
			TMarshal.FreeMem(pw2);
			TMarshal.FreeMem(pw);
		end;
	end;

{$if defined(fpc) or defined(test_things_broken_in_xe3)}
var
	OrigMgr: TMemoryManager;
	TrackedPointers: array[0 .. 1] of pointer;

	function TrackedFreeMem(ptr: pointer): {$ifdef delphi} integer {$else} PtrUint {$endif};
	var
		i: NativeInt;
	begin
		for i := 0 to High(TrackedPointers) do
			if TrackedPointers[i] = ptr then TrackedPointers[i] := nil;
		result := OrigMgr.FreeMem(ptr);
	end;
{$endif}

	procedure TestTMarshaller;

	{$if defined(fpc) or defined(test_things_broken_in_xe3)} // *SIGH*
		procedure TestRealloc;
		var
			m: TMarshaller;
			c: TPtrWrapper;
			oldcv: pointer;
			csz: NativeInt;
			newMgr: TMemoryManager;
		begin
			m.AllocMem(5);
			m.AllocMem(6);
			csz := 7;
			c := m.AllocMem(csz);
			oldcv := c.ToPointer;
			repeat
				csz := 2 * csz;
				c := m.ReallocMem(c, csz);
			until c.ToPointer <> oldcv;

			newMgr.FreeMem := nil; // suppress warning
			GetMemoryManager(newMgr);
			OrigMgr := newMgr;
			newMgr.FreeMem := @TrackedFreeMem;
			SetMemoryManager(newMgr);
			try
				TrackedPointers[0] := oldcv;
				TrackedPointers[1] := c.ToPointer;
				m.Flush;
				if TrackedPointers[0] = nil then Fail('TMarshaller freed the old pointer supplied to ReallocMem.');
				if TrackedPointers[1] <> nil then Fail('TMarshaller did not free the new pointer got from ReallocMem.');
			finally
				SetMemoryManager(OrigMgr);
			end;
		end;
	{$endif}

		procedure TestAllocString;
		var
			m: TMarshaller;
		begin
			Expect(m.AllocStringAsAnsi('あか', ShiftJIS).ToPointer, Concat(Concat(SjJaA, SjJaKa), [0]), 'AllocStringAsAnsi');
			Expect(m.AllocStringAsUnicode('あか').ToPointer, Concat(Concat(Utf16JaA, Utf16JaKa), [0, 0]), 'AllocStringAsUnicode');
		end;

		procedure TestInOutString;
		var
			m: TMarshaller;
			sba, sbb: TUnicodeStringBuilder;
		begin
			sba := nil; sbb := nil;
			try
				sba := TUnicodeStringBuilder.Create('ikai wa');
				sbb := TUnicodeStringBuilder.Create('fukashin atorakushon');

				TMarshal.WriteStringAsUnicode(m.InString(sba, 10), 'IKAI', -1);
				TMarshal.WriteStringAsUnicode(m.InOutString(sbb, 10), 'FUKASHIN', -1);

				ExpectU(sba.ToString, 'ikai wa', 'InString (pending)');
				ExpectU(sbb.ToString, 'fukashin atorakushon', 'InOutString (pending)');

				m.Flush;

				ExpectU(sba.ToString, 'IKAI', 'InString');
				ExpectU(sbb.ToString, 'FUKASHIN', 'InOutString');
			finally
				sba.Free; sbb.Free;
			end;
		end;

	var
		heapUsed: NativeUint;
	begin
		heapUsed := GetHeapStatus.TotalAllocated;
	{$if defined(fpc) or defined(test_things_broken_in_xe3)}
		TestRealloc;
	{$endif}
		TestAllocString;
		TestInOutString;
		if GetHeapStatus.TotalAllocated <> heapUsed then
			Fail(unicodestring('TMarshaller leaked: heap used before = ' + IntToStr(heapUsed) + ', after = ' + IntToStr(GetHeapStatus.TotalAllocated) + '.'));
	end;

begin
	TestTMarshal;
	if anythingFailed then halt(1);
	TestTMarshaller;
	if anythingFailed then halt(2);
	if not anythingFailed then writeln('...ok?');
end.
