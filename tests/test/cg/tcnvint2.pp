{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{****************************************************************}
{ NODE TESTED : secondtypeconvert() -> second_int_to_bool        }
{****************************************************************}
{ PRE-REQUISITES: secondload()                                   }
{                 secondassign()                                 }
{                 secondcalln()                                  }
{                 secondinline()                                 }
{****************************************************************}
{ DEFINES:                                                       }
{****************************************************************}
{ REMARKS: This code is specific to FPC, this testsuite FAILS    }
{ under Turbo Pascal / Borland Pascal.                           }
{****************************************************************}
program tcnvint2;


   function getbyte: byte;
     begin
       getbyte := $10;
     end;

   function getword: word;
     begin
       getword := $0F00;
     end;

   function getlongint: longint;
     begin
       getlongint := $10000000;
     end;

{$ifdef fpc}
   function getint64: int64;
     begin
       getint64 := $10000000;
     end;
{$endif}

var
 frombyte : byte;
 fromword : word;
 fromlong : longint;
{$ifdef fpc}
 fromint64 : int64;
{$endif}
 bb1 : bytebool;
 wb1 : wordbool;
 lb1 : longbool;
 bb2 : bytebool;
 wb2 : wordbool;
 lb2 : longbool;
begin

 { left : LOC_REGISTER  }
 { from : LOC_REFERENCE }
 WriteLn('Testing LOC_REFERENCE...');
 frombyte := $10;
 bb1 := bytebool(frombyte);
 WriteLn('byte-> bytebool : Value should be TRUE...',bb1);
 frombyte := $10;
 wb1 := wordbool(frombyte);
 WriteLn('byte -> wordbool : Value should be TRUE...',wb1);
 { ------------------------------------------------------------   }
 { WARNING : This test fails under Borland Pascal v7, but         }
 { works under Delphi 3.0 (normally it should give TRUE).         }
 { ------------------------------------------------------------   }
 fromword := $1000;
 wb1 := wordbool(fromword);
 WriteLn('word -> wordbool : Value should be TRUE...',wb1);
 frombyte := $10;
 lb1 := longbool(frombyte);
 WriteLn('byte -> longbool : Value should be TRUE...',lb1);
 { ------------------------------------------------------------   }
 { WARNING : This test fails under Borland Pascal v7, but         }
 { works under Delphi 3.0 (normally it should give TRUE).         }
 { ------------------------------------------------------------   }
 fromword := $1000;
 lb1 := longbool(fromword);
 WriteLn('word -> longbool : Value should be TRUE...',lb1);
 { ------------------------------------------------------------   }
 { WARNING : This test fails under Borland Pascal v7, but         }
 { works under Delphi 3.0 (normally it should give TRUE).         }
 { ------------------------------------------------------------   }
 fromlong := $00000100;
 lb1 := longbool(fromlong);
 WriteLn('longint -> longbool : Value should be TRUE...',lb1);
{$ifdef fpc}
 fromint64 := $10000000;
 lb1 := longbool(fromint64);
 WriteLn('int64 -> longbool : Value should be TRUE...',lb1);
{$endif}
 { left : LOC_REGISTER  }
 WriteLn('Testing LOC_REGISTER...');
 frombyte := $10;
 bb1 := bytebool(getbyte);
 WriteLn('byte-> bytebool : Value should be TRUE...',bb1);
 frombyte := $10;
 wb1 := wordbool(getbyte);
 WriteLn('byte -> wordbool : Value should be TRUE...',wb1);
 { ------------------------------------------------------------   }
 { WARNING : This test fails under Borland Pascal v7, but         }
 { works under Delphi 3.0 (normally it should give TRUE).         }
 { ------------------------------------------------------------   }
 fromword := $1000;
 wb1 := wordbool(getword);
 WriteLn('word -> wordbool : Value should be TRUE...',wb1);
 frombyte := $10;
 lb1 := longbool(getbyte);
 WriteLn('byte -> longbool : Value should be TRUE...',lb1);
 { ------------------------------------------------------------   }
 { WARNING : This test fails under Borland Pascal v7, but         }
 { works under Delphi 3.0 (normally it should give TRUE).         }
 { ------------------------------------------------------------   }
 fromword := $1000;
 lb1 := longbool(getword);
 WriteLn('word -> longbool : Value should be TRUE...',lb1);
 { ------------------------------------------------------------   }
 { WARNING : This test fails under Borland Pascal v7, but         }
 { works under Delphi 3.0 (normally it should give TRUE).         }
 { ------------------------------------------------------------   }
 fromlong := $00000100;
 lb1 := longbool(getlongint);
 WriteLn('longint -> longbool : Value should be TRUE...',lb1);
{$ifdef fpc}
 fromint64 := $10000000;
 lb1 := longbool(getint64);
 WriteLn('int64 -> longbool : Value should be TRUE...',lb1);
{$endif}
(* CURRENTLY NEVER GOES INTO THE LOC_FLAGS LOCATION!
 { left : LOC_FLAGS  }
 WriteLn('Testing LOC_FLAGS...');
 frombyte := 10;
 fromword := 2;
 bb1 := bytebool(frombyte > fromword);
 WriteLn('Value should be TRUE...',bb1);
 frombyte := $10;
 fromword := 2;
 wb1 := wordbool(frombyte > fromword);
 WriteLn('Value should be TRUE...',wb1);
 fromword := $1000;
 fromlong := $4000;
 wb1 := wordbool(fromlong > fromword);
 WriteLn('Value should be TRUE...',wb1);
 frombyte := $10;
 fromword := $20;
 lb1 := longbool(fromword > frombyte);
 WriteLn('Value should be TRUE...',lb1);
 fromword := $1000;
 fromlong := $0100;
 lb1 := longbool(fromlong > fromword);
 WriteLn('Value should be FALSE...',lb1);
{$ifdef fpc}
 fromint64 := $10000000;
 fromlong := $02;
 lb1 := longbool(fromint64 > fromlong);
 WriteLn('Value should be TRUE...',lb1);
{$endif}
*)
end.

{
   $Log$
   Revision 1.1  2001-08-31 23:56:45  carl
   + first revision (missing LOC_FLAGS location test)


}