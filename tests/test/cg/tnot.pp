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
   if longres = $80808080 then
      WriteLn('Success.')
   else
      WriteLn('Failure.');

   { CURRENT NODE : REGISTER }
   { LEFT NODE : REGISTER    }
   WriteLn('(current) : LOC_REGISTER; (left) : LOC_REGISTER');
   longres := not getintres;
   Write('Value should be $8080...');
   if longres = $FFFF8080 then
      WriteLn('Success.')
   else
      WriteLn('Failure.');
   WriteLn('----------------------------- BOOLEAN -----------------------------------');

   { CURRENT NODE : LOC_REGISTER }
   { LEFT NODE :  LOC_REFERENCE  }
   WriteLn('(current) : LOC_REGISTER; (left) : LOC_REFERENCE');
   byteboolval := TRUE;
   byteboolres := not byteboolval;
   Write('Value should be FALSE...');
   if byteboolres = FALSE then
      WriteLn('Success.')
   else
      WriteLn('Failure.');

   wordboolval := TRUE;
   wordboolres := not wordboolval;
   Write('Value should be FALSE...');
   if wordboolres = FALSE then
      WriteLn('Success.')
   else
      WriteLn('Failure.');

   longboolval := TRUE;
   longboolres := not longboolval;
   Write('Value should be FALSE...');
   if longboolres = FALSE then
      WriteLn('Success.')
   else
      WriteLn('Failure.');

   { CURRENT NODE : LOC_REGISTER }
   { LEFT NODE :  LOC_REGISTER  }
   WriteLn('(current) : LOC_REGISTER; (left) : LOC_REGISTER');
   longboolres := not getbyteboolval;
   Write('Value should be FALSE...');
   if longboolres = FALSE then
      WriteLn('Success.')
   else
      WriteLn('Failure.');

   { CURRENT NODE : LOC_FLAGS }
   { LEFT NODE :  LOC_FLAGS  }
   WriteLn('(current) : LOC_FLAGS; (left) : LOC_FLAGS');
   intres := 1;
   byteboolres := TRUE;
   byteboolres:= not ((intres = 1));
   Write('Value should be FALSE...');
   if byteboolres = FALSE then
      WriteLn('Success.')
   else
      WriteLn('Failure.');

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
   if int64res = $80808080 then
      WriteLn('Success.')
   else
      WriteLn('Failure.');

   { CURRENT NODE : REGISTER }
   { LEFT NODE : REGISTER    }
   WriteLn('(current) : LOC_REGISTER; (left) : LOC_REGISTER');
   int64res := not (word(getintres));
   Write('Value should be $8080...');
   if int64res = $00008080 then
      WriteLn('Success.')
   else
      WriteLn('Failure.');
{$ENDIF}
end.

