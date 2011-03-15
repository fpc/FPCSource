{ %OPT=-gh }

{ Test correct RTTI handling of open arrays with managed elements. 
  When a part (slice or range) of array is passed as an out-parameter open array
  to a procedure, the entire array should NOT be finalized, only part that is actually passed should. }

{$mode objfpc}{$h+}
uses SysUtils;


procedure test3(out arr: array of string);
var
  i: Integer;
begin
  { implicit initialization happens here }
  for i := 0 to High(arr) do
  begin
    Pointer(arr[i]):=nil;            // if array initialization was correct, this will be a no-op
                                     // otherwise, it will trigger a memory leak 
    arr[i] := 'tested'+IntToStr(i);
  end;
end;

procedure test_entire_openarray(var arr: array of string);
begin
  test3(arr);
end;

procedure test_openarray_subrange(var arr: array of string);
begin
  test3(arr[1..2]);
end;

procedure test_openarray_slice(var arr: array of string);
begin
  test3(slice(arr,2));
end;


var
  sarr: array[0..3] of string;
  darr: array of string;

procedure prepare;
var
  i: Integer;
begin
  for i := 0 to 3 do
  begin
    sarr[i] := 'static'+IntToStr(i);
    darr[i] := 'dynamic'+IntToStr(i);
  end;  
end;

begin
  HaltOnNotReleased := True;
  SetLength(darr,4);

  prepare;
  test_entire_openarray(sarr);
  if sarr[0] <> 'tested0' then Halt(1);
  if sarr[1] <> 'tested1' then Halt(2);
  if sarr[2] <> 'tested2' then Halt(3);
  if sarr[3] <> 'tested3' then Halt(4);

  prepare;
  test_openarray_subrange(sarr);            // must leave elements 0 and 3 intact
  if sarr[0] <> 'static0' then Halt(11);
  if sarr[1] <> 'tested0' then Halt(12);
  if sarr[2] <> 'tested1' then Halt(13);
  if sarr[3] <> 'static3' then Halt(14);

  prepare;
  test_openarray_slice(sarr);               // must leave elements 2 and 3 intact
  if sarr[0] <> 'tested0' then Halt(21);
  if sarr[1] <> 'tested1' then Halt(22);
  if sarr[2] <> 'static2' then Halt(23);
  if sarr[3] <> 'static3' then Halt(24);

  prepare;
  test3(sarr);           // entire static array
  if sarr[0] <> 'tested0' then Halt(31);
  if sarr[1] <> 'tested1' then Halt(32);
  if sarr[2] <> 'tested2' then Halt(33);
  if sarr[3] <> 'tested3' then Halt(34);

  prepare;
  test3(sarr[1..2]);     // static array subrange
  if sarr[0] <> 'static0' then Halt(41);
  if sarr[1] <> 'tested0' then Halt(42);
  if sarr[2] <> 'tested1' then Halt(43);
  if sarr[3] <> 'static3' then Halt(44);

  prepare;
  test3(slice(sarr,2));  // static array slice
  if sarr[0] <> 'tested0' then Halt(51);
  if sarr[1] <> 'tested1' then Halt(52);
  if sarr[2] <> 'static2' then Halt(53);
  if sarr[3] <> 'static3' then Halt(54);

  prepare;
  test3(darr);           // entire dynamic array
  if darr[0] <> 'tested0' then Halt(61);
  if darr[1] <> 'tested1' then Halt(62);
  if darr[2] <> 'tested2' then Halt(63);
  if darr[3] <> 'tested3' then Halt(64);

  prepare;
  test3(darr[1..2]);     // dynamic array subrange
  if darr[0] <> 'dynamic0' then Halt(71);
  if darr[1] <> 'tested0' then Halt(72);
  if darr[2] <> 'tested1' then Halt(73);
  if darr[3] <> 'dynamic3' then Halt(74);

  prepare;
  test3(slice(darr,2));  // dynamic array slice
  if darr[0] <> 'tested0' then Halt(81);
  if darr[1] <> 'tested1' then Halt(82);
  if darr[2] <> 'dynamic2' then Halt(83);
  if darr[3] <> 'dynamic3' then Halt(84);

end.
