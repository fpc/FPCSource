program tanonfunc24;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test anonymous methods with extremely long symbol names }

type
  tproc = reference to procedure;

  tprocrefname_01234567890123456789 = reference to procedure(c: char; i: longint);

  tlongclassname_01234567890123456789 = class
	  procedure longmethodname_0123456789(p: tprocrefname_01234567890123456789);
  end;

procedure foo(p: tproc);
begin
  p();
end;

procedure tlongclassname_01234567890123456789.longmethodname_0123456789(
  p: tprocrefname_01234567890123456789);
begin
  foo(
	procedure
	begin
	  p('a', 123);
	end);
end;

procedure bar;
var
  cls: tlongclassname_01234567890123456789;
  val: Integer;
begin
  cls := tlongclassname_01234567890123456789.create;
  cls.longmethodname_0123456789(
    procedure(c: char; i: longint)
    begin
      if (c <> 'a') or (i <> 123) then
        halt(1);
      val := i;
    end);
  cls.free;
  if val <> 123 then
    halt(1);
end;

begin
  bar;
end.
