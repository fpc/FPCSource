{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{****************************************************************}
{ NODE TESTED : secondfor()                                      }
{****************************************************************}
{ PRE-REQUISITES: secondload()                                   }
{                 secondassign()                                 }
{                 secondcalln()                                  }
{                 secondinline()                                 }
{                 secondadd()                                    }
{****************************************************************}
{ DEFINES:                                                       }
{****************************************************************}
{ REMARKS:                                                       }
{****************************************************************}
program tfor;

const LOOP_NUMS = 100;

{$ifndef fpc}
  type cardinal = longint;
{$endif}

procedure fail;
begin
  WriteLn('Failure.');
  halt(1);
end;


function getupper : longint;
 begin
   getupper:=LOOP_NUMS;
 end;

function getupper0: longint;
 begin
   getupper0 := 1;
 end;

{$ifdef fpc}
function getupper64 : int64;
 begin
   getupper64:=1;
 end;

function getupper64high : int64;
 begin
   getupper64high:=LOOP_NUMS;
 end;

{$endif}


var
  index_signed: longint;
  index_unsigned : cardinal;
  count:longint;
  count1 : longint;
  loop_count : longint;
  failed : boolean;

{$ifdef fpc}
  index64 : int64;
  count64 : int64;
  count642 : int64;
{$endif}
begin
  loop_count:=0;
  count := LOOP_NUMS;
  count1 := LOOP_NUMS;
  { SIGNED INDEX }
  failed := false;
  Write('Signed index for loop testing (forward)...');
  { lowerbound : constant        }
  { upper bound :: LOC_REFERENCE }
  for index_signed:=1 to count do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REGISTER  }
  loop_count:=0;
  for index_signed:=1 to getupper do
     loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_CONSTANT }
  loop_count:=0;
  for index_signed:=1 to LOOP_NUMS do
     loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { lowerbound : LOC_REFERENCE }
  { upper bound : constant     }
  count:=1;
  loop_count:=0;
  for index_signed:=count to LOOP_NUMS do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REGISTER }
  loop_count:=0;
  for index_signed:=count to getupper do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REFERENCE }
  loop_count:=0;
  for index_signed:=count to count1 do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;


  { lowerbound : LOC_REGISTER }
  { upper bound : constant     }
  count:=0;
  loop_count:=0;
  for index_signed:=getupper0 to LOOP_NUMS do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REGISTER }
  loop_count:=0;
  for index_signed:=getupper0 to getupper do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REFERENCE }
  loop_count:=0;
  for index_signed:=getupper0 to count1 do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  if failed then
    fail
  else
    WriteLn('Passed!');


  { UNSIGNED INDEX }
  Write('Unsigned index for loop testing (forward)...');
  loop_count:=0;
  failed := false;
  count := LOOP_NUMS;
  count1 := LOOP_NUMS;
  { lowerbound : constant        }
  { upper bound :: LOC_REFERENCE }
  for index_unsigned:=1 to count do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REGISTER  }
  loop_count:=0;
  for index_unsigned:=1 to getupper do
     loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_CONSTANT }
  loop_count:=0;
  for index_unsigned:=1 to LOOP_NUMS do
     loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;

  { lowerbound : LOC_REFERENCE }
  { upper bound : constant     }
  count:=1;
  loop_count:=0;
  for index_unsigned:=count to LOOP_NUMS do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REGISTER }
  loop_count:=0;
  for index_unsigned:=count to getupper do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REFERENCE }
  loop_count:=0;
  for index_unsigned:=count to count1 do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;


  { lowerbound : LOC_REGISTER }
  { upper bound : constant     }
  count:=0;
  loop_count:=0;
  for index_unsigned:=getupper0 to LOOP_NUMS do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REGISTER }
  loop_count:=0;
  for index_unsigned:=getupper0 to getupper do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REFERENCE }
  loop_count:=0;
  for index_unsigned:=getupper0 to count1 do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  if failed then
    fail
  else
    WriteLn('Passed!');

(*  UNSUPPORTED IN FPC VERSION 1.0.x (CEC)
  { --------------------- int64 testing!------------------- }
  WriteLn('int64 testing...');
  loop_count:=0;
  count64 := LOOP_NUMS;
  count1 := LOOP_NUMS;
  { SIGNED INDEX }
  failed := false;
  { lowerbound : constant        }
  { upper bound :: LOC_REFERENCE }
  for index64:=1 to count64 do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REGISTER  }
  loop_count:=0;
  for index64:=1 to getupper64high do
     loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_CONSTANT }
  loop_count:=0;
  for index64:=1 to LOOP_NUMS do
     loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { lowerbound : LOC_REFERENCE }
  { upper bound : constant     }
  count64:=1;
  count642:=LOOP_NUMS;
  loop_count:=0;
  for index64:=count64 to LOOP_NUMS do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REGISTER }
  loop_count:=0;
  for index64:=count64 to getupper64high do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REFERENCE }
  loop_count:=0;
  for index64:=count64 to count642 do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;


  { lowerbound : LOC_REGISTER }
  { upper bound : constant     }
  count64:=LOOP_NUMS;
  loop_count:=0;
  for index64:=getupper64 to LOOP_NUMS do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REGISTER }
  loop_count:=0;
  for index64:=getupper64 to getupper64high do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REFERENCE }
  loop_count:=0;
  for index64:=getupper64 to count64 do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  if failed then
    fail
  else
    WriteLn('Passed!');
*)

  loop_count:=0;
  count := LOOP_NUMS;
  count1 := LOOP_NUMS;
  { SIGNED INDEX }
  failed := false;
  Write('Signed index for loop testing (backward)...');
  { lowerbound : constant        }
  { upper bound :: LOC_REFERENCE }
  for index_signed:=count downto 1 do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REGISTER  }
  loop_count:=0;
  for index_signed:=getupper downto 1 do
     loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_CONSTANT }
  loop_count:=0;
  for index_signed:=LOOP_NUMS  downto 1 do
     loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { lowerbound : LOC_REFERENCE }
  { upper bound : constant     }
  count:=1;
  loop_count:=0;
  for index_signed:=LOOP_NUMS downto count do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REGISTER }
  loop_count:=0;
  for index_signed:=getupper downto count do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REFERENCE }
  loop_count:=0;
  for index_signed:=count1 downto count do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;


  { lowerbound : LOC_REGISTER }
  { upper bound : constant     }
  count:=0;
  loop_count:=0;
  for index_signed:=LOOP_NUMS downto getupper0 do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REGISTER }
  loop_count:=0;
  for index_signed:=getupper downto getupper0 do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REFERENCE }
  loop_count:=0;
  for index_signed:=count1 downto getupper0 do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  if failed then
    fail
  else
    WriteLn('Passed!');


  { UNSIGNED INDEX }
  Write('Unsigned index for loop testing (backward)...');
  loop_count:=0;
  failed := false;
  count := LOOP_NUMS;
  count1 := LOOP_NUMS;
  { lowerbound : constant        }
  { upper bound :: LOC_REFERENCE }
  for index_unsigned:=count downto 1 do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REGISTER  }
  loop_count:=0;
  for index_unsigned:=getupper downto 1 do
     loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_CONSTANT }
  loop_count:=0;
  for index_unsigned:=LOOP_NUMS downto 1 do
     loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;

  { lowerbound : LOC_REFERENCE }
  { upper bound : constant     }
  count:=1;
  loop_count:=0;
  for index_unsigned:=LOOP_NUMS downto count do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REGISTER }
  loop_count:=0;
  for index_unsigned:=getupper downto count do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REFERENCE }
  loop_count:=0;
  for index_unsigned:=count1 downto count do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;


  { lowerbound : LOC_REGISTER }
  { upper bound : constant     }
  count:=0;
  loop_count:=0;
  for index_unsigned:=LOOP_NUMS downto getupper0 do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REGISTER }
  loop_count:=0;
  for index_unsigned:=getupper downto getupper0 do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  { upper bound : LOC_REFERENCE }
  loop_count:=0;
  for index_unsigned:=count1 downto getupper0 do
    loop_count:=loop_count+1;
  if loop_count <> LOOP_NUMS then
    failed := true;
  if failed then
    fail
  else
    WriteLn('Passed!');

end.
