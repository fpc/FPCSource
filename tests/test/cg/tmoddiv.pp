{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{****************************************************************}
{ NODE TESTED : secondmoddiv()                                   }
{****************************************************************}
{ PRE-REQUISITES: secondload()                                   }
{                 secondassign()                                 }
{                 secondtypeconv()                               }
{****************************************************************}
{ DEFINES:                                                       }
{            FPC     = Target is FreePascal compiler             }
{****************************************************************}
{ REMARKS:                                                       }
{                                                                }
{                                                                }
{                                                                }
{****************************************************************}

{ CURRENT NODE (result):                                        }
{  LOC_REGISTER                                                 }
{ LEFT NODE (operand) (numerator)                               }
{  LOC_REFERENCE / LOC_MEM                                      }
{  LOC_REGISTER / LOC_CREGISTER                                 }
{ RIGHT NODE (operand (denominator)                             }
{  ord constant node                                            }
{  LOC_REGISTER / LOC_CREGISTER                                 }
{  LOC_REFERENCE / LOC_MEM                                      }


function getlongcnt: longint;
 begin
   getlongcnt := -10;
 end;

 {$IFDEF FPC}
function getcardinalcnt: cardinal;
 begin
   getcardinalcnt := 10;
 end;

function getint64cnt: int64;
 begin
   getint64cnt := -10;
 end;

  {$ENDIF}

procedure test(value, required: longint);
begin
  if value <> required then
    begin
      writeln('Got ',value,' instead of ',required);
      halt(1);
    end
  else
    writeln('Passed!');
end;

var
 longres : longint;
 longcnt : longint;
{$IFDEF FPC}
  cardinalres : cardinal;
  cardinalcnt : cardinal;
  int64res : int64;
  int64cnt : int64;
{$ENDIF}
begin
  WriteLn('------------------- LONGINT ------------------------');

  WriteLn('(left) : LOC_REFERENCE; (right) : ordinal constant');
  { RIGHT : power of 2 ordconstn   }
  { LEFT : LOC_REFERENCE           }
  longres := 24;
  longres := longres div 4;
  Write('Value should be 6...');
  test(longres, 6);

  { RIGHT : power of 2 ordconstn   }
  { LEFT : LOC_REFERENCE           }
  longres := 24;
  longres := longres mod 4;
  Write('Value should be 0...');
  test(longres, 0);


  WriteLn('(left) : LOC_REFERENCE; (right) : LOC_REFERENCE');
  { RIGHT : LOC_REFERENCE      }
  { LEFT : LOC_REFERENCE       }
  longres := 136;
  longcnt := -13;
  longres := longres div longcnt;
  Write('Value should be -10...');
  test(longres, -10);

  { RIGHT : LOC_REFERENCE      }
  { LEFT : LOC_REFERENCE       }
  longres := 10101010;
  longcnt := -13;
  longres := longres mod longcnt;
  Write('Value should be 10...');
  test(longres, 10);

  WriteLn('(left) : LOC_REFERENCE; (right) : LOC_REGISTER');
  { RIGHT : LOC_REGISTER       }
  { LEFT : LOC_REFERENCE       }
  longres := -11111111;
  longres := longres div getlongcnt;
  Write('Value should be 1111111...');
  test(longres, 1111111);

  { RIGHT : LOC_REGISTER       }
  { LEFT : LOC_REFERENCE       }
  longres := -1111111;
  longres := longres mod getlongcnt;
  Write('Value should be -1...');
  test(longres, -1);

  { RIGHT : LOC_REFERENCE }
  { LEFT : LOC_REGISTER   }
  longcnt := 2;
  longres := getlongcnt div longcnt;
  Write('Value should be -5...');
  test(longres, -5);

  { RIGHT : LOC_REFERENCE }
  { LEFT : LOC_REGISTER   }
  longcnt := 3;
  longres := getlongcnt mod longcnt;
  Write('Value should be -1...');
  test(longres, -1);

  { special tests for results }
  Writeln('special numeric values tests...');
  longres := $7FFFFFFF;
  longcnt := $80000000;
  longres := longres div longcnt;
  Write('Value should be 0...');
  test(longres, 0);

  Writeln('special numeric values tests...');
  longres := $7FFFFFFF;
  longcnt := $80000000;
  longres := longcnt div longres;
  Write('Value should be -1...');
  test(longres, -1);

  Writeln('special numeric values tests...');
  cardinalcnt := $80000;
  cardinalres := $12345;
  cardinalres := cardinalcnt div cardinalres;
  Write('Value should be 7...');
  test(cardinalres, 7);

{$IFDEF FPC}
  WriteLn('------------------- CARDINAL -----------------------');
  { special tests for results }
  Writeln('special numeric values tests...');
  cardinalres := $7FFFFFFF;
  cardinalcnt := $80000000;
  cardinalres := cardinalres div cardinalcnt;
  Write('Value should be 0...');
  test(cardinalres, 0);

  Writeln('special numeric values tests...');
  cardinalres := $7FFFFFFF;
  cardinalcnt := $80000000;
  cardinalres := cardinalcnt div cardinalres;
  Write('Value should be 1...');
  test(cardinalres, 1);

  Writeln('special numeric values tests...');
  cardinalcnt := $80000;
  cardinalres := $12345;
  cardinalres := cardinalcnt div cardinalres;
  test(cardinalres, 7);

  WriteLn('(left) : LOC_REFERENCE; (right) : ordinal constant');
  { RIGHT : power of 2 ordconstn   }
  { LEFT : LOC_REFERENCE           }
  cardinalres := 24;
  cardinalres := cardinalres div 4;
  Write('Value should be 6...');
  test(cardinalres, 6);

  { RIGHT : power of 2 ordconstn   }
  { LEFT : LOC_REFERENCE           }
  cardinalres := 24;
  cardinalres := cardinalres mod 4;
  Write('Value should be 0...');
  test(cardinalres, 0);


  WriteLn('(left) : LOC_REFERENCE; (right) : LOC_REFERENCE');
  { RIGHT : LOC_REFERENCE      }
  { LEFT : LOC_REFERENCE       }
  cardinalres := 136;
  cardinalcnt := 13;
  cardinalres := cardinalres div cardinalcnt;
  Write('Value should be 10...');
  test(cardinalres, 10);

  { RIGHT : LOC_REFERENCE      }
  { LEFT : LOC_REFERENCE       }
  cardinalres := 10101010;
  cardinalcnt := 13;
  cardinalres := cardinalres mod cardinalcnt;
  Write('Value should be 10...');
  test(cardinalres, 10);

  WriteLn('(left) : LOC_REFERENCE; (right) : LOC_REGISTER');
  { RIGHT : LOC_REGISTER       }
  { LEFT : LOC_REFERENCE       }
  cardinalres := 11111111;
  cardinalres := cardinalres div getcardinalcnt;
  Write('Value should be 1111111...');
  test(cardinalres, 1111111);

  { RIGHT : LOC_REGISTER       }
  { LEFT : LOC_REFERENCE       }
  cardinalres := 1111111;
  cardinalres := cardinalres mod getcardinalcnt;
  Write('Value should be 1...');
  test(cardinalres, 1);

  { RIGHT : LOC_REFERENCE }
  { LEFT : LOC_REGISTER   }
  cardinalcnt := 2;
  cardinalres := getcardinalcnt div cardinalcnt;
  Write('Value should be 5...');
  test(cardinalres, 5);

  { RIGHT : LOC_REFERENCE }
  { LEFT : LOC_REGISTER   }
  cardinalcnt := 3;
  cardinalres := getcardinalcnt mod cardinalcnt;
  Write('Value should be 1...');
  test(cardinalres, 1);

  WriteLn('--------------------- INT64 ------------------------');
  { special tests for results }
  Writeln('special numeric values tests...');
  int64res := $7FFFFFFF shl 32;
  int64cnt := $80000000 shl 32;
  int64res := int64res div int64cnt;
  Write('Value should be 0...');
  test(int64res and $FFFFFFFF, 0);

  Writeln('special numeric values tests...');
  int64res := $7FFFFFFF shl 32;
  int64cnt := $80000000 shl 32;
  int64res := int64cnt div int64res;
  Write('Value should be 1...');
  test(int64res and $FFFFFFFF, 1);

  int64res := $7FFFFFFF;
  int64cnt := $80000000;
  int64res := int64res div int64cnt;
  Write('Value should be 0...');
  test(int64res and $FFFFFFFF, 0);

  Writeln('special numeric values tests...');
  int64res := $7FFFFFFF;
  int64cnt := $80000000;
  int64res := int64cnt div int64res;
  Write('Value should be 1...');
  test(int64res and $FFFFFFFF, 1);

  WriteLn('(left) : LOC_REFERENCE; (right) : ordinal constant');
  { RIGHT : power of 2 ordconstn   }
  { LEFT : LOC_REFERENCE           }
  int64res := 24;
  int64res := int64res div 4;
  Write('Value should be 6...');
  test(int64res and $FFFFFFFF, 6);

  { RIGHT : power of 2 ordconstn   }
  { LEFT : LOC_REFERENCE           }
  int64res := 24;
  int64res := int64res mod 4;
  Write('Value should be 0...');
  test(int64res and $FFFFFFFF, 0);


  WriteLn('(left) : LOC_REFERENCE; (right) : LOC_REFERENCE');
  { RIGHT : LOC_REFERENCE      }
  { LEFT : LOC_REFERENCE       }
  int64res := 136;
  int64cnt := -13;
  int64res := int64res div int64cnt;
  Write('Value should be -10...');
  test(int64res and $FFFFFFFF, -10);

  { RIGHT : LOC_REFERENCE      }
  { LEFT : LOC_REFERENCE       }
  int64res := 10101010;
  int64cnt := -13;
  int64res := int64res mod int64cnt;
  Write('Value should be 10...');
  test(int64res and $FFFFFFFF, 10);

  WriteLn('(left) : LOC_REFERENCE; (right) : LOC_REGISTER');
  { RIGHT : LOC_REGISTER       }
  { LEFT : LOC_REFERENCE       }
  int64res := -11111111;
  int64res := int64res div getint64cnt;
  Write('Value should be 1111111...');
  test(int64res and $FFFFFFFF, 1111111);

  { RIGHT : LOC_REGISTER       }
  { LEFT : LOC_REFERENCE       }
  int64res := -1111111;
  int64res := int64res mod getint64cnt;
  Write('Value should be -1...');
  test(int64res and $FFFFFFFF, -1);

  { RIGHT : LOC_REFERENCE }
  { LEFT : LOC_REGISTER   }
  int64cnt := 2;
  int64res := getint64cnt div int64cnt;
  Write('Value should be -5...');
  test(int64res and $FFFFFFFF, -5);

  { RIGHT : LOC_REFERENCE }
  { LEFT : LOC_REGISTER   }
  int64cnt := 3;
  int64res := getint64cnt mod int64cnt;
  Write('Value should be -1...');
  test(int64res and $FFFFFFFF, -1);

{$ENDIF}
end.