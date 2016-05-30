{ Part of System unit testsuit        }
{ Carl Eric Codere Copyright (c) 2002 }
program tseg;

{$ifdef CPUI8086}
const
  seg_should_be_zero = false;
{$else}
const
  seg_should_be_zero = true;
{$endif}

const
 cst : integer = 0;
var
 variable : integer;

procedure fail;
 begin
  WriteLn('Failure!');
  halt(1);
 end;

procedure test_cseg;
 begin
   Write('Testing CSeg()...');
   if ((cseg <> 0) and seg_should_be_zero) or
      ((cseg=0) and not seg_should_be_zero) then
     fail
   else
     WriteLn('Success!');
 end;

procedure test_dseg;
 begin
   Write('Testing DSeg()...');
   if ((dseg <> 0) and seg_should_be_zero) or
      ((dseg=0) and not seg_should_be_zero) then
     fail
   else
     WriteLn('Success!');
 end;

procedure test_sseg;
 begin
   Write('Testing SSeg()...');
   if ((sseg <> 0) and seg_should_be_zero) or
      ((sseg=0) and not seg_should_be_zero) then
     fail
   else
     WriteLn('Success!');
 end;

procedure test_seg;
 var
   x : longint;
   _result : boolean;
 begin
   _result := true;
   Write('Testing Seg()...');
   if ((seg(x) <> 0) and seg_should_be_zero) or
      ((seg(x)=0) and not seg_should_be_zero) then
     _result := false;
   if ((seg(cst) <> 0) and seg_should_be_zero) or
      ((seg(cst)=0) and not seg_should_be_zero) then
     _result := false;
   if ((seg(variable) <> 0) and seg_should_be_zero) or
      ((seg(variable)=0) and not seg_should_be_zero) then
     _result := false;
   if not _result then
     fail
   else
     WriteLn('Success!');
 end;

Begin
  test_cseg;
  test_dseg;
  test_seg;
  test_sseg;
end.
