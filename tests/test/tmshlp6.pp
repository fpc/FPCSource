{ %NORUN }

{$mode objfpc}
{$modeswitch multihelpers}

program tmshlp6;

type
	TMyObject = class
		m_num: integer;
		property num1: integer read m_num;
	end;
	THelperBase = class helper for TMyObject
		function GetNum: integer;
	end;
	THelper1 = class helper(THelperBase) for TMyObject
		property num2: integer read GetNum;
	end;
	THelper2 = class helper(THelperBase) for TMyObject
		property num3: integer read GetNum;
	end;

function THelperBase.GetNum: integer;
begin
	result := m_num;
end;

var
	obj: TMyObject;
	num: integer;
begin
	obj := TMyObject.Create;
	// 2^3
	obj.m_num := 2;
	num := obj.num1 * obj.num2 * obj.num3;
	writeln(num);
end.
