{$mode objfpc}
{$warn 4055 off Conversion between ordinals and pointers is not portable}
type
	PObject1 = ^Object1;
	Object1 = record
		field: int32;
		struct: record
			field: int64;
			ary: array[2 .. 5] of record
				x, y: int32;
				bottomAry: record
					n: SizeUint;
					items: array[3 .. 6, 4 .. 6] of uint16;
				end;
			end;
		end;
	end;

	PTopLevelArray = ^TopLevelArray;
	TopLevelArray = array[1 .. 3] of Object1;

	TopLevelClass = class
		objs: array[1 .. 3] of Object1;
	end;

const
	OffsetOf3YInObject1 = PtrUint(@PObject1(nil)^.struct.ary[3].y);
	OffsetOf2345InObject1 = PtrUint(@PObject1(nil)^.struct.ary[3].bottomAry.items[4, 5]);
	OffsetOf23YInTopLevelArray = PtrUint(@PTopLevelArray(nil)^[2].struct.ary[3].y);
	OffsetOf2345InTopLevelArray = PtrUint(@PTopLevelArray(nil)^[2].struct.ary[3].bottomAry.items[4, 5]);
	OffsetOf23YInTopLevelClass = PtrUint(@TopLevelClass(nil).objs[2].struct.ary[3].y);
	OffsetOf2345InTopLevelClass = PtrUint(@TopLevelClass(nil).objs[2].struct.ary[3].bottomAry.items[4, 5]);

	function RecoverObject1From3Y(_3Y: PInt32): PTopLevelArray;
	begin
		result := pointer(_3Y) - PtrUint(@PObject1(nil)^.struct.ary[3].y);
	end;

	function RecoverObject1From345(_345: PUint16): PTopLevelArray;
	begin
		result := pointer(_345) - PtrUint(@PObject1(nil)^.struct.ary[3].bottomAry.items[4, 5]);
	end;

	function RecoverTopLevelArrayFrom23Y(_23Y: PInt32): PTopLevelArray;
	begin
		result := pointer(_23Y) - PtrUint(@PTopLevelArray(nil)^[2].struct.ary[3].y);
	end;

	function RecoverTopLevelArrayFrom2345(_2345: PUint16): PTopLevelArray;
	begin
		result := pointer(_2345) - PtrUint(@PTopLevelArray(nil)^[2].struct.ary[3].bottomAry.items[4, 5]);
	end;

	function RecoverTopLevelClassFrom23Y(_23Y: PInt32): TopLevelClass;
	begin
		result := TopLevelClass(pointer(_23Y) - PtrUint(@TopLevelClass(nil).objs[2].struct.ary[3].y));
	end;

	function RecoverTopLevelClassFrom2345(_2345: PUint16): TopLevelClass;
	begin
		result := TopLevelClass(pointer(_2345) - PtrUint(@TopLevelClass(nil).objs[2].struct.ary[3].bottomAry.items[4, 5]));
	end;

var
	o1: Object1;
	tla: TopLevelArray;
	tlc: TopLevelClass;
	fieldPtr, recovered: pointer;

begin
	tlc := nil;
	try
		fieldPtr := @o1.struct.ary[3].y;
		recovered := RecoverObject1From3Y(fieldPtr);
		if recovered <> @o1 then
		begin
			writeln('@o1 = ', HexStr(@o1), ', @o1.struct.ary[3].y = ', HexStr(fieldPtr), ', recovered = ', HexStr(recovered));
			halt(1);
		end;

		fieldPtr := @o1.struct.ary[3].bottomAry.items[4, 5];
		recovered := RecoverObject1From345(fieldPtr);
		if recovered <> @o1 then
		begin
			writeln('@o1 = ', HexStr(@o1), ', @o1.struct.ary[3].bottomAry.items[4, 5] = ', HexStr(fieldPtr), ', recovered = ', HexStr(recovered));
			halt(2);
		end;

		fieldPtr := @tla[2].struct.ary[3].y;
		recovered := RecoverTopLevelArrayFrom23Y(fieldPtr);
		if recovered <> @tla then
		begin
			writeln('@tla = ', HexStr(@tla), ', @tla[2].struct.ary[3].y = ', HexStr(fieldPtr), ', recovered = ', HexStr(recovered));
			halt(3);
		end;

		fieldPtr := @tla[2].struct.ary[3].bottomAry.items[4, 5];
		recovered := RecoverTopLevelArrayFrom2345(fieldPtr);
		if recovered <> @tla then
		begin
			writeln('@tla = ', HexStr(@tla), ', @tla[2].struct.ary[3].bottomAry.items[4, 5] = ', HexStr(fieldPtr), ', recovered = ', HexStr(recovered));
			halt(4);
		end;

		tlc := TopLevelClass.Create;
		fieldPtr := @tlc.objs[2].struct.ary[3].y;
		recovered := RecoverTopLevelClassFrom23Y(fieldPtr);
		if recovered <> pointer(tlc) then
		begin
			writeln('tlc = ', HexStr(tlc), ', @tlc.objs[2].struct.ary[3].y = ', HexStr(fieldPtr), ', recovered = ', HexStr(recovered));
			halt(5);
		end;

		fieldPtr := @tlc.objs[2].struct.ary[3].bottomAry.items[4, 5];
		recovered := RecoverTopLevelClassFrom2345(fieldPtr);
		if recovered <> pointer(tlc) then
		begin
			writeln('tlc = ', HexStr(tlc), ', @tlc.objs[2].struct.ary[3].bottomAry.items[4, 5] = ', HexStr(fieldPtr), ', recovered = ', HexStr(recovered));
			halt(6);
		end;

		writeln('ok');
	finally
		tlc.Free;
	end;
end.

