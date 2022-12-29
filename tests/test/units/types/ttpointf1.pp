{$mode objfpc} {$longstrings on}
uses
	Types, Math;

var
	anythingFailed: boolean = false;

	procedure Fail(const msg: string);
	begin
		writeln(msg);
		anythingFailed := true;
	end;

	function ToString(const pt: TPointF): string;
	begin
		WriteStr(result, '(', pt.x, ', ', pt.y, ')');
	end;

	function SamePoint(const a, b: TPointF; eps: single = 0): boolean;
	begin
		result := SameValue(a.x, b.x, eps) and SameValue(a.y, b.y, eps);
	end;

	procedure TestTPointF_Angle_AngleCosine;
	type
		TestRec = record
			a, b: TPointF;
			anBaOx, cosAnAB: single;
		end;
	const
		Tests: array[0 .. 1] of TestRec =
		(
			(a: (x: 1; y: -2); b: (x: 3; y: 4); anBaOx: -1.892546892; cosAnAB: -1 / sqrt(5)),
			(a: (x: 1; y: 2); b: (x: 3; y: 4); anBaOx: -3 * pi / 4; cosAnAB: 11 / (5 * sqrt(5)))
		);
	var
		t: TestRec;
		gotAnBaOx, gotCosAnAB: single;
		msg: string;
	begin
		for t in Tests do
		begin
			gotAnBaOx := t.a.Angle(t.b);
			if not SameValue(gotAnBaOx, t.anBaOx) then
			begin
				WriteStr(msg, 'TPointF', ToString(t.a), '.Angle', ', ', ToString(t.b), ' = ', gotAnBaOx, ', expected ', t.anBaOx, '.');
				Fail(msg);
			end;

			gotCosAnAB := t.a.AngleCosine(t.b);
			if not SameValue(gotCosAnAB, t.cosAnAB) then
			begin
				WriteStr(msg, 'TPointF', ToString(t.a), '.AngleCosine', ToString(t.b), ' = ', gotCosAnAB, ', expected ', t.cosAnAB, '.');
				Fail(msg);
			end;
		end;
	end;

	procedure TestTPointF_MidPoint;
	type
		TestRec = record
			a, b, mid: TPointF;
		end;
	const
		Tests: array[0 .. 0] of TestRec =
		(
			(a: (x: 1; y: 2); b: (x: 3; y: 4); mid: (x: 2; y: 3))
		);
	var
		t: TestRec;
		gotMid: TPointF;
		msg: string;
	begin
		for t in Tests do
		begin
			gotMid := t.a.MidPoint(t.b);
			if not SamePoint(gotMid, t.mid) then
			begin
				WriteStr(msg, 'TPointF', ToString(t.a), '.MidPoint', ToString(t.b), ' = ', ToString(gotMid), ', expected ', ToString(t.mid), '.');
				Fail(msg);
			end;
		end;
	end;

	procedure TestTPointF_PointInCircle;
	type
		TestRec = record
			center: TPointF;
			radius: float;
			point: TPointF;
			PiC: boolean;
		end;
	const
		Tests: array[0 .. 16] of TestRec =
		(
			(center: (x: 10; y: 20); radius: 0;    point: (x: 10; y: 20); PiC: false),
			(center: (x: 10; y: 20); radius: 2;    point: (x: 12; y: 20); PiC: false),
			(center: (x: 10; y: 20); radius: 2;    point: (x:  8; y: 20); PiC: false),
			(center: (x: 10; y: 20); radius: 2;    point: (x: 10; y: 22); PiC: false),
			(center: (x: 10; y: 20); radius: 2;    point: (x: 10; y: 18); PiC: false),
			(center: (x: 10; y: 20); radius: 2;    point: (x: 10 + sqrt(2.01); y: 20 + sqrt(2.01)); PiC: false),
			(center: (x: 10; y: 20); radius: 2;    point: (x: 10 - sqrt(2.01); y: 20 + sqrt(2.01)); PiC: false),
			(center: (x: 10; y: 20); radius: 2;    point: (x: 10 - sqrt(2.01); y: 20 - sqrt(2.01)); PiC: false),
			(center: (x: 10; y: 20); radius: 2;    point: (x: 10 + sqrt(2.01); y: 20 - sqrt(2.01)); PiC: false),
			(center: (x: 10; y: 20); radius: 2.02; point: (x: 12; y: 20); PiC: true),
			(center: (x: 10; y: 20); radius: 2.02; point: (x:  8; y: 20); PiC: true),
			(center: (x: 10; y: 20); radius: 2.02; point: (x: 10; y: 22); PiC: true),
			(center: (x: 10; y: 20); radius: 2.02; point: (x: 10; y: 18); PiC: true),
			(center: (x: 10; y: 20); radius: 2.02; point: (x: 10 + sqrt(2.01); y: 20 + sqrt(2.01)); PiC: true),
			(center: (x: 10; y: 20); radius: 2.02; point: (x: 10 - sqrt(2.01); y: 20 + sqrt(2.01)); PiC: true),
			(center: (x: 10; y: 20); radius: 2.02; point: (x: 10 - sqrt(2.01); y: 20 - sqrt(2.01)); PiC: true),
			(center: (x: 10; y: 20); radius: 2.02; point: (x: 10 + sqrt(2.01); y: 20 - sqrt(2.01)); PiC: true)
		);
	var
		t: TestRec;
		gotPiC: boolean;
		msg: string;
	begin
		for t in Tests do
		begin
			gotPiC := TPointF.PointInCircle(t.point, t.center, t.radius);
			if gotPiC <> t.PiC then
			begin
				WriteStr(msg, 'TPointF.PointInCircle(', ToString(t.point), ', ', ToString(t.center), ', ', t.radius, ') = ', pChar('-+')[ord(gotPiC)], ', expected ', pChar('-+')[ord(t.PiC)], '.');
				Fail(msg);
			end;
		end;
	end;

	procedure TestTPointF_Rotate;
	type
		TestRec = record
			point: TPointF;
			angle: float;
			rotated: TPointF;
		end;
	const
		Tests: array[0 .. 1] of TestRec =
		(
			(point: (x: 1; y: 2); angle: 2 * pi + 1; rotated: (x: -1.142639637; y: 1.92207551)),
			(point: (x: 1; y: 2); angle: 2 * pi - 1; rotated: (x: 2.22324419; y: 0.2391340137))
		);
	var
		t: TestRec;
		got: TPointF;
		msg: string;
	begin
		for t in Tests do
		begin
			got := t.point.Rotate(t.angle);
			if not SamePoint(got, t.rotated) then
			begin
				WriteStr(msg, 'TPointF', ToString(t.point), '.Rotate(', t.angle, ') = ', ToString(got), ', expected ', ToString(t.rotated), '.');
				Fail(msg);
			end;
		end;
	end;

	procedure TestTPointF_Reflect;
	type
		TestRec = record
			point, normal, reflected: TPointF;
		end;
	const
		Tests: array[0 .. 1] of TestRec =
		(
			(point: (x: 1; y: 2); normal: (x: sqrt(2) / 2; y: sqrt(2) / 2); reflected: (x: -2; y: -1)),
			(point: (x: 1; y: 2); normal: (x: -sqrt(2) / 2; y: sqrt(2) / 2); reflected: (x: 2; y: 1))
		);
	var
		t: TestRec;
		got: TPointF;
		msg: string;
	begin
		for t in Tests do
		begin
			got := t.point.Reflect(t.normal);
			if not SamePoint(got, t.reflected) then
			begin
				WriteStr(msg, 'TPointF', ToString(t.point), '.Reflect', ToString(t.normal), ' = ', ToString(got), ', expected ', ToString(t.reflected), '.');
				Fail(msg);
			end;
		end;
	end;

begin
	TestTPointF_Angle_AngleCosine;
	TestTPointF_MidPoint;
	TestTPointF_PointInCircle;
	TestTPointF_Rotate;
	TestTPointF_Reflect;
	if not anythingFailed then writeln('ok');
	if anythingFailed then halt(1);
end.

