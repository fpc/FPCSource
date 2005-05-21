{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{  By Carl Eric Codere                                           }
{****************************************************************}
{ NODE TESTED : secondfuncret()                                  }
{****************************************************************}
{ DEFINES:                                                       }
{            FPC     = Target is FreePascal compiler             }
{****************************************************************}
{ REMARKS : Tested with Delphi 3 as reference implementation     }
{****************************************************************}
program tfuncret;

{$ifdef ver70}
{$define tp}
{$endif}

const
  { adjusts the size of the bigrecord }
  MAX_INDEX = 7;


  RESULT_S64BIT = -12;
  RESULT_S32BIT = -124356;
  RESULT_U32BIT = 654321;
  RESULT_U8BIT  = $55;
type
  {
    the size of this record should *at least* be the size
    of a natural register for the target processor
  }
  tbigrecord = record
   x : cardinal;
   y : cardinal;
   z : array[0..MAX_INDEX] of byte;
  end;


    procedure fail;
    begin
      WriteLn('Failure.');
      halt(1);
    end;

{****************************************************************}
{                         SIMPLE CASE                            }
{****************************************************************}

    function getresult_simple_s64bit: int64;
      var
       s64bit : int64;
       i: integer;
      begin
        getresult_simple_s64bit := 0;
        s64bit:=RESULT_S64BIT;
        getresult_simple_s64bit := s64bit;
      end;


    function getresult_simple_s32bit: longint;
      var
       s32bit : longint;
       i: longint;
      begin
        getresult_simple_s32bit := 0;
        i:=1;
        i:=i*RESULT_S32BIT div i;
        s32bit:=i;
        getresult_simple_s32bit := s32bit;
      end;


    function getresult_simple_bigrecord : tbigrecord;
     var
      localbigrecord : tbigrecord;
      i: integer;
     begin
      localbigrecord.x := RESULT_U32BIT;
      localbigrecord.y := RESULT_U32BIT;
      for i:=0 to MAX_INDEX do
        localbigrecord.z[i] := RESULT_U8BIT;
      getresult_simple_bigrecord := localbigrecord;
     end;

{****************************************************************}
{                         WITH NESTING                           }
{****************************************************************}

    function getresult_nested_s64bit: int64;

      procedure nested_one;
      var
       s64bit : int64;
       i: longint;
      begin
        getresult_nested_s64bit := 0;
        s64bit:=RESULT_S64BIT;
        getresult_nested_s64bit := s64bit;
      end;

    begin
      nested_one;
    end;


    function getresult_nested_s32bit: longint;


      procedure nested_one;
      var
       s32bit : longint;
       i: longint;
      begin
        getresult_nested_s32bit := 0;
        i:=1;
        i:=i*RESULT_S32BIT div i;
        s32bit:=i;
        getresult_nested_s32bit := s32bit;
      end;

    begin
      nested_one;
    end;


    function getresult_nested_bigrecord : tbigrecord;

       procedure nested_one;
        var
         localbigrecord : tbigrecord;
         i: longint;
       begin
         localbigrecord.x := RESULT_U32BIT;
         localbigrecord.y := RESULT_U32BIT;
         for i:=0 to MAX_INDEX do
           localbigrecord.z[i] := RESULT_U8BIT;
         getresult_nested_bigrecord := localbigrecord;
       end;

     begin
       nested_one;
     end;


{****************************************************************}
{                     WITH COMPLEX NESTING                       }
{****************************************************************}

    function getresult_nested_complex_s64bit: int64;

      procedure nested_one;
      var
       s64bit : int64;
       i: integer;

       function nested_two: int64;
        begin
         nested_two:=0;
         getresult_nested_complex_s64bit := 0;
         s64bit:=RESULT_S64BIT;
         getresult_nested_complex_s64bit := s64bit;
        end;

      begin
        nested_two;
      end;

    begin
      nested_one;
    end;


    function getresult_nested_complex_s32bit: longint;


      procedure nested_one;
      var
       s32bit : longint;
       i: longint;

       function nested_two: longint;
         begin
           nested_two := 0;
           getresult_nested_complex_s32bit := 0;
           i:=1;
           i:=i*RESULT_S32BIT div i;
           s32bit:=i;
           getresult_nested_complex_s32bit := s32bit;
         end;

      begin
        nested_two;
      end;

    begin
      nested_one;
    end;


    function getresult_nested_complex_bigrecord : tbigrecord;

       procedure nested_one;
        var
         localbigrecord : tbigrecord;

         function nested_two : tbigrecord;
           var
            i : integer;
           begin
            nested_two := localbigrecord;
            localbigrecord.x := RESULT_U32BIT;
            localbigrecord.y := RESULT_U32BIT;
            for i:=0 to MAX_INDEX do
               localbigrecord.z[i] := RESULT_U8BIT;
            getresult_nested_complex_bigrecord := localbigrecord;
           end;

       begin
         nested_two;
       end;

     begin
       nested_one;
     end;


var
 failed : boolean;
 bigrecord : tbigrecord;
 i: integer;
Begin
  Write('secondfuncret simple case tests...');
  failed := false;
  if getresult_simple_s64bit <> RESULT_S64BIT then
    failed := true;
  if getresult_simple_s32bit <> RESULT_S32BIT then
    failed := true;
  bigrecord := getresult_simple_bigrecord;
  if bigrecord.x <> RESULT_U32BIT then
    failed := true;
  if bigrecord.y <> RESULT_U32BIT then
    failed := true;
  for i:=0 to MAX_INDEX do
    begin
       if bigrecord.z[i] <> RESULT_U8BIT then
         begin
           failed := true;
           break;
         end;
    end;


  if failed then
    fail
  else
    WriteLn('Success!');

  Write('secondfuncret simple nesting case tests...');
  failed := false;
  if getresult_nested_s64bit <> RESULT_S64BIT then
    failed := true;
  if getresult_nested_s32bit <> RESULT_S32BIT then
    failed := true;

  bigrecord := getresult_nested_bigrecord;
  if bigrecord.x <> RESULT_U32BIT then
    failed := true;
  if bigrecord.y <> RESULT_U32BIT then
    failed := true;
  for i:=0 to MAX_INDEX do
    begin
       if bigrecord.z[i] <> RESULT_U8BIT then
         begin
           failed := true;
           break;
         end;
    end;


  if failed then
    fail
  else
    WriteLn('Success!');

  Write('secondfuncret complex nesting case tests...');
  failed := false;
  if getresult_nested_complex_s64bit <> RESULT_S64BIT then
    failed := true;
  if getresult_nested_complex_s32bit <> RESULT_S32BIT then
    failed := true;

  bigrecord := getresult_nested_complex_bigrecord;
  if bigrecord.x <> RESULT_U32BIT then
    failed := true;
  if bigrecord.y <> RESULT_U32BIT then
    failed := true;
  for i:=0 to MAX_INDEX do
    begin
       if bigrecord.z[i] <> RESULT_U8BIT then
         begin
           failed := true;
           break;
         end;
    end;


  if failed then
    fail
  else
    WriteLn('Success!');

end.
