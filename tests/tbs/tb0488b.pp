{ Checks whether the %x format string supports qwords.
 This is a variation of tb0488a
}

uses {$ifdef unix}cwstring, {$endif}SysUtils,erroru;

procedure Check(a,b:ansistring);
begin
  if a<>b then
    begin
      writeln(a,' should be equal to ',b);
      error;
    end;
end;

begin
 check(WideFormat('%x', [$FFFFFFF]),'FFFFFFF');
end.
