{$mode objfpc} {$h+} {$typedaddress on} {$modeswitch advancedrecords} {$coperators on} {$modeswitch anonymousfunctions}

uses
	SysUtils, Generics.Collections, Generics.Defaults;

var
	anythingWrong: boolean = false;

	generic procedure Swap<Ty>(var a, b: Ty);
	var
		temp: Ty;
	begin
		temp := a; a := b; b := temp;
	end;

type
	generic SortBenchmark<Ty> = record
	type
		CreateProc = function(id: SizeUint): Ty;
		TyArray = array of Ty;

		class procedure Run(create: CreateProc; const tyPlural: string; lenMul: double); static;
		class procedure BenchSort(const src, ref: array of Ty; var prevTime: double); static;

		// Built against specific QSort implementation that uses (L + R) div 2 as a median, and won't kill any other, even one that uses (L + R + 1) div 2.
		// 'ref' must be sorted and contain no duplicates.
		class function BuildQSortKiller(const ref: array of Ty): TyArray; static;
	end;

	class procedure SortBenchmark.Run(create: CreateProc; const tyPlural: string; lenMul: double);
	type
		OrderEnum = (RandomOrder, QSortKillerOrder);
	const
		OrderNames: array[OrderEnum] of string = ('random', 'QSort killer');
	var
		ref, src: TyArray;
		i, lenBase, len: SizeInt;
		cmp: specialize IComparer<Ty>;
		srcOrder: OrderEnum;
		msg: string;
		prevTime: double;
	begin
		writeln('--- ', tyPlural, ' ---', LineEnding);

		for srcOrder in OrderEnum do
		begin
			writeln('Order: ', OrderNames[srcOrder], '.');
			prevTime := -1;
			for lenBase in specialize TArray<SizeInt>.Create(10 * 1000, 20 * 1000, 40 * 1000) do
			begin
				len := round(lenMul * lenBase);

				SetLength((@ref)^, len);
				cmp := specialize TComparer<Ty>.Default;
				for i := 0 to len - 1 do
				begin
					ref[i] := create(i);
					if (i > 0) and (cmp.Compare(ref[i - 1], ref[i]) >= 0) then
					begin
						writeln('''create'' callback must return ', tyPlural, ' in strictly ascending order.');
						anythingWrong := true;
						exit;
					end;
				end;

				case srcOrder of
					RandomOrder:
						begin
							RandSeed := 1;
							src := Copy(ref);
							for i := len - 1 downto 1 do
								specialize Swap<Ty>(src[i], src[random(int64(i))]);
						end;

					QSortKillerOrder:
						src := BuildQSortKiller(ref);
				end;

				WriteStr(msg, 'n = ', len, ': ');
				write(msg.PadRight(12));
				BenchSort(src, ref, prevTime);
			end;
			writeln;
		end;
	end;

	class function SortBenchmark.BuildQSortKiller(const ref: array of Ty): TyArray;
	var
		ris: array of SizeInt;
		i: SizeInt;
	begin
		SetLength((@ris)^, length(ref)); // Swaps that QSort would perform are tracked here, to build the worst case possible. >:3
		for i := 0 to High(ris) do ris[i] := i;

		SetLength((@result)^, length(ref));
		for i := 0 to High(ref) do
		begin
			specialize Swap<SizeInt>(ris[i], ris[i + (High(ref) - i) shr 1]);
			result[ris[i]] := ref[i];
		end;
	end;

	class procedure SortBenchmark.BenchSort(const src, ref: array of Ty; var prevTime: double);
	var
		arr: TyArray;
		startTime: TDateTime;
		time, timePassed: double;
		i: SizeInt;
		reps: cardinal;
	begin
		startTime := Now;
		reps := 0;
		repeat
			arr := Copy(src);
			specialize TArrayHelper<Ty>.Sort(arr);
			timePassed := (Now - startTime) * SecsPerDay;
			reps += 1;
		until not (timePassed < 0.5);

		time := timePassed / reps;
		write(time * 1e3:0:1, ' ms/sort');
		if prevTime > 0 then write(' (', time / prevTime:0:1, 'x from previous)');
		if time / prevTime > 3 then
		  begin
		    writeln;
		    writeln('Bad sorting algorithm behaviour');
			halt(1);
		  end;
		prevTime := time;
		write(', ');

		for i := 0 to High(ref) do
			if arr[i] <> ref[i] then
			begin
				writeln('FAIL @ ', i, ' / ', length(ref));
				anythingWrong := true;
				exit;
			end;
		writeln('OK');
	end;

begin
	specialize SortBenchmark<string>.Run(
		function(id: SizeUint): string
		begin
			SetLength((@result)^, 5);
			result[5] := char(ord('A') + id mod 26); id := id div 26;
			result[4] := char(ord('A') + id mod 26); id := id div 26;
			result[3] := char(ord('A') + id mod 26); id := id div 26;
			result[2] := char(ord('A') + id mod 26); id := id div 26;
			result[1] := char(ord('A') + id mod 26);
		end, 'strings', 0.15);

	specialize SortBenchmark<single>.Run(
		function(id: SizeUint): single
		begin
			result := -1000 + id / 1000;
		end, 'float32''s', 1.0);

	if anythingWrong then writeln(LineEnding, 'Something was wrong, see above.');
end.

