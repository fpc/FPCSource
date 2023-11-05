program llvmccy5ParamTest;

type

	TTestObjFail = object
		constructor Init(
			P1, P2, P3, P4: Currency; P5: Currency);
	end;

constructor TTestObjFail.Init(
			P1, P2, P3, P4: Currency; P5: Currency);
begin
  if p1<>1 then
    halt(1);
  if p2<>2 then
    halt(2);
  if p3<>3 then
    halt(3);
  if p4<>4 then
    halt(4);
  if p5<>5 then
    halt(5);
end;

var
	TestObj	: TTestObjFail;
begin
	TestObj.Init(1, 2, 3, 4, 5);
end.
