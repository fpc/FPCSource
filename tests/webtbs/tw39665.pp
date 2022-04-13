{$mode objfpc}
type
	PBContainer = ^BContainer;
	BContainer = record
		b: int32;
	end;

	PMyObj = ^MyObj;
	MyObj = record
		dummy: int32;
		a: int32;
		bCtr: BContainer;
	end;

const
	AOfs = PtrUint(@PMyObj(nil)^.a);
	BOfs = PtrUint(@PMyObj(nil)^.bCtr.b); // does not compile
	BOfs2 = PtrUint(@PMyObj(nil)^.bCtr) + PtrUint(@PBContainer(nil)^.b); // ugly workaround

	function MyObjFromA_SubOffsetOf(aPtr: PInt32): PMyObj;
	begin
		result := pointer(aPtr) - PtrUint(@PMyObj(nil)^.a);
	end;

	function MyObjFromA_SubConst(aPtr: PInt32): PMyObj;
	begin
		result := pointer(aPtr) - AOfs;
	end;

	function MyObjFromB_SubOffsetOf(bPtr: PInt32): PMyObj;
	begin
		result := pointer(bPtr) - PtrUint(@PMyObj(nil)^.bCtr.b);
	end;

	function MyObjFromB_SubConst(bPtr: PInt32): PMyObj;
	begin
		result := pointer(bPtr) - BOfs;
	end;

var
	mo: MyObj;
begin
	writeln('''a'' offset in ''MyObj'': ', AOfs);
	writeln('''b'' offset in ''MyObj'': ', BOfs);
    if BOfs<>BOfs2 then
      halt(1);
	writeln('@mo: ', HexStr(@mo));
	writeln('@mo recovered from @mo.a by subtracting "offsetof": ', HexStr(MyObjFromA_SubOffsetOf(@mo.a)));
	writeln('@mo recovered from @mo.a by subtracting constant:   ', HexStr(MyObjFromA_SubConst(@mo.a)));
	writeln('@mo recovered from @mo.b by subtracting "offsetof": ', HexStr(MyObjFromB_SubOffsetOf(@mo.bCtr.b)));
	writeln('@mo recovered from @mo.b by subtracting constant:   ', HexStr(MyObjFromB_SubConst(@mo.bCtr.b)));
end.
