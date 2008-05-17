{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{****************************************************************}
{ NODE TESTED : secondnot()                                      }
{****************************************************************}
{ PRE-REQUISITES: secondload()                                   }
{                 secondassign()                                 }
{****************************************************************}
{ DEFINES:   VERBOSE = Write test information to screen          }
{            FPC     = Target is FreePascal compiler             }
{****************************************************************}
{ REMARKS:                                                       }
{                                                                }
{                                                                }
{                                                                }
{****************************************************************}
Program tnot;

{----------------------------------------------------}
{ Cases to test:                                     }
{   CURRENT NODE (result)                            }
{     - LOC_REGISTER                                 }
{     - LOC_FLAGS                                    }
{   LEFT NODE (value to complement)                  }
{     possible cases : int64,byte,word,longint       }
{                      boolean                       }
{     - LOC_CREGISTER                                }
{     - LOC_REFERENCE / LOC_MEM                      }
{     - LOC_REGISTER                                 }
{     - LOC_FLAGS                                    }
{     - LOC_JUMP                                     }
{----------------------------------------------------}


{$IFNDEF FPC}
type  smallint = integer;
{$ENDIF}

function getintres : smallint;
begin
 getintres := $7F7F;
end;

function getbyteboolval : boolean;
begin
  getbyteboolval := TRUE;
end;

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

const
  lb = longbool(false);

var
 longres :  longint;
 intres : smallint;
 byteboolval : bytebool;
 wordboolval : wordbool;
 longboolval : longbool;
 byteboolres : bytebool;
 wordboolres : wordbool;
 longboolres : longbool;
{$ifdef fpc}
 int64res : int64;
{$endif}
Begin
   WriteLn('------------------------------ LONGINT --------------------------------');
   { CURRENT NODE: REGISTER }
   { LEFT NODE : REFERENCE  }
   WriteLn('(current) : LOC_REGISTER; (left) : LOC_REFERENCE');
   longres := $7F7F7F7F;
   longres := not longres;
   Write('Value should be $80808080...');

   { the following test give range check errors }
   {$R-}
   test(longres,$80808080);

   { CURRENT NODE : REGISTER }
   { LEFT NODE : REGISTER    }
   WriteLn('(current) : LOC_REGISTER; (left) : LOC_REGISTER');
   longres := not getintres;
   Write('Value should be $8080...');
   test(longres, $FFFF8080);

   WriteLn('----------------------------- BOOLEAN -----------------------------------');

   { CURRENT NODE : LOC_REGISTER }
   { LEFT NODE :  LOC_REFERENCE  }
   WriteLn('(current) : LOC_REGISTER; (left) : LOC_REFERENCE');
   byteboolval := TRUE;
   byteboolres := not byteboolval;
   Write('Value should be FALSE...');
   test(ord(byteboolres),0);

   wordboolval := TRUE;
   wordboolres := not wordboolval;
   Write('Value should be FALSE...');
   test(longint(wordboolres),0);

   longboolval := TRUE;
   longboolres := not longboolval;
   Write('Value should be FALSE...');
   test(longint(longboolres),0);

   { CURRENT NODE : LOC_REGISTER }
   { LEFT NODE :  LOC_REGISTER  }
   WriteLn('(current) : LOC_REGISTER; (left) : LOC_REGISTER');
   longboolres := not getbyteboolval;
   Write('Value should be FALSE...');
   test(longint(longboolres),0);

   { CURRENT NODE : LOC_FLAGS }
   { LEFT NODE :  LOC_FLAGS  }
   WriteLn('(current) : LOC_FLAGS; (left) : LOC_FLAGS');
   intres := 1;
   byteboolres := TRUE;
   byteboolres:= not ((intres = 1));
   Write('Value should be FALSE...');
   test(ord(byteboolres),0);

  longboolres:=not(lb);
  Write('Value should be 1...');
  test(ord(longboolres),1);

  { !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! }
  { CURRENT_NODE : LOC_JUMP }
  { ???????????????????????}

  { !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! }
  { CURRENT_NODE : LOC_FLAGS          }
  { LEFT NODE : <> LOC_FLAGS          }
  { !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! }
{$IFDEF FPC}
   WriteLn('------------------------------  INT64  ----------------------------------');
   { CURRENT NODE: REGISTER }
   { LEFT NODE : REFERENCE  }
   WriteLn('(current) : LOC_REGISTER; (left) : LOC_REFERENCE');
   int64res := $7F7F7F7F;
   int64res := not int64res;
   Write('Value should be $80808080...');
   test(int64res and $FFFFFFFF,$80808080);

   { CURRENT NODE : REGISTER }
   { LEFT NODE : REGISTER    }
   WriteLn('(current) : LOC_REGISTER; (left) : LOC_REGISTER');
   int64res := not (word(getintres));
   Write('Value should be $8080...');
   test(int64res and $FFFFFFFF,$00008080);
{$ENDIF}
end.
