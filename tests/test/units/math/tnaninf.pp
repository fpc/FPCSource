uses
  Math;

procedure DoTestSingle;
var
  val: Single;
begin
  val := nan;
  if not(isnan(val)) then
    begin
      writeln('error single 1');
      halt(1);
    end;
  val := infinity;
  if not(isinfinite(val)) then
    begin
      writeln('error single 2');
      halt(1);
    end;
  val := 12341234;
  if isnan(val) then
    begin
      writeln('error single 3');
      halt(1);
    end;
  val := 0;
  if isinfinite(val) then
    begin
      writeln('error single 4');
      halt(1);
    end;
  val := 12341234;
  if isinfinite(val) then
    begin
      writeln('error single 5');
      halt(1);
    end;
end;

{$ifdef FPC_HAS_TYPE_DOUBLE}
procedure DoTestDouble;
var
  val: Double;
begin
  val := nan;
  if not(isnan(val)) then
    begin
      writeln('error double 1');
      halt(1);
    end;
  val := infinity;
  if not(isinfinite(val)) then
    begin
      writeln('error double 2');
      halt(1);
    end;
  val := 12341234;
  if isnan(val) then
    begin
      writeln('error double 3');
      halt(1);
    end;
  val := 0;
  if isinfinite(val) then
    begin
      writeln('error double 4');
      halt(1);
    end;
  val := 12341234;
  if isinfinite(val) then
    begin
      writeln('error double 5');
      halt(1);
    end;
end;
{$endif}

{$ifdef FPC_HAS_TYPE_EXTENDED}
procedure DoTestExtended;
var
  val: Extended;
begin
  val := nan;
  if not(isnan(val)) then
    begin
      writeln('error extended 1');
      halt(1);
    end;
  val := infinity;
  if not(isinfinite(val)) then
    begin
      writeln('error extended 2');
      halt(1);
    end;
  val := 12341234;
  if isnan(val) then
    begin
      writeln('error extended 3');
      halt(1);
    end;
  val := 0;
  if isinfinite(val) then
    begin
      writeln('error extended 4');
      halt(1);
    end;
  val := 12341234;
  if isinfinite(val) then
    begin
      writeln('error extended 5');
      halt(1);
    end;
end;
{$endif}

begin
  DoTestSingle;
{$ifdef FPC_HAS_TYPE_DOUBLE}
  DoTestDouble;
{$endif}
{$ifdef FPC_HAS_TYPE_EXTENDED}
  DoTestExtended;
{$endif}
end.
