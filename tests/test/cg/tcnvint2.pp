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

{$ifdef VER70}
  {$define tp}
{$endif}

var
  failed   : boolean;

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

{$ifndef tp}
   function getint64: int64;
     begin
       getint64 := $10000000;
     end;
{$endif}

   procedure Test(const s:string;b:boolean);
     begin
       Writeln(s,b);
       if not b then
        failed:=true;
     end;

var
 frombyte : byte;
 fromword : word;
 fromlong : longint;
{$ifndef tp}
 fromint64 : int64;
{$endif}
 b   : boolean;
 bb1 : bytebool;
 wb1 : wordbool;
 lb1 : longbool;
 bb2 : bytebool;
 wb2 : wordbool;
 lb2 : longbool;
begin
 failed:=false;

 { left : LOC_REGISTER  }
 { from : LOC_REFERENCE }
 Writeln('Testing LOC_REFERENCE...');
 frombyte := $10;
 bb1 := bytebool(frombyte);
 Test('byte-> bytebool : Value should be TRUE...',bb1);
 frombyte := $10;
 wb1 := wordbool(frombyte);
 Test('byte -> wordbool : Value should be TRUE...',wb1);
 { ------------------------------------------------------------   }
 { WARNING : This test fails under Borland Pascal v7, but         }
 { works under Delphi 3.0 (normally it should give TRUE).         }
 { ------------------------------------------------------------   }
 fromword := $1000;
 wb1 := wordbool(fromword);
 Test('word -> wordbool : Value should be TRUE...',wb1);
 frombyte := $10;
 lb1 := longbool(frombyte);
 Test('byte -> longbool : Value should be TRUE...',lb1);
 { ------------------------------------------------------------   }
 { WARNING : This test fails under Borland Pascal v7, but         }
 { works under Delphi 3.0 (normally it should give TRUE).         }
 { ------------------------------------------------------------   }
 fromword := $1000;
 lb1 := longbool(fromword);
 Test('word -> longbool : Value should be TRUE...',lb1);
 if not lb1 then
  failed:=true;
 { ------------------------------------------------------------   }
 { WARNING : This test fails under Borland Pascal v7, but         }
 { works under Delphi 3.0 (normally it should give TRUE).         }
 { ------------------------------------------------------------   }
 fromlong := $00000100;
 lb1 := longbool(fromlong);
 Test('longint -> longbool : Value should be TRUE...',lb1);
{$ifndef tp}
 fromint64 := $10000000;
 lb1 := longbool(fromint64);
 Test('int64 -> longbool : Value should be TRUE...',lb1);
{$endif}
 { left : LOC_REGISTER  }
 Writeln('Testing LOC_REGISTER...');
 frombyte := $10;
 bb1 := bytebool(getbyte);
 Test('byte-> bytebool : Value should be TRUE...',bb1);
 frombyte := $10;
 wb1 := wordbool(getbyte);
 Test('byte -> wordbool : Value should be TRUE...',wb1);
 { ------------------------------------------------------------   }
 { WARNING : This test fails under Borland Pascal v7, but         }
 { works under Delphi 3.0 (normally it should give TRUE).         }
 { ------------------------------------------------------------   }
 fromword := $1000;
 wb1 := wordbool(getword);
 Test('word -> wordbool : Value should be TRUE...',wb1);
 frombyte := $10;
 lb1 := longbool(getbyte);
 Test('byte -> longbool : Value should be TRUE...',lb1);
 { ------------------------------------------------------------   }
 { WARNING : This test fails under Borland Pascal v7, but         }
 { works under Delphi 3.0 (normally it should give TRUE).         }
 { ------------------------------------------------------------   }
 fromword := $1000;
 lb1 := longbool(getword);
 Test('word -> longbool : Value should be TRUE...',lb1);
 { ------------------------------------------------------------   }
 { WARNING : This test fails under Borland Pascal v7, but         }
 { works under Delphi 3.0 (normally it should give TRUE).         }
 { ------------------------------------------------------------   }
 fromlong := $00000100;
 lb1 := longbool(getlongint);
 Test('longint -> longbool : Value should be TRUE...',lb1);
{$ifndef tp}
 fromint64 := $10000000;
 lb1 := longbool(getint64);
 Test('int64 -> longbool : Value should be TRUE...',lb1);
{$endif}
(* CURRENTLY NEVER GOES INTO THE LOC_FLAGS LOCATION!
 { left : LOC_FLAGS  }
 Test('Testing LOC_FLAGS...');
 frombyte := 10;
 fromword := 2;
 bb1 := bytebool(frombyte > fromword);
 Test('Value should be TRUE...',bb1);
 frombyte := $10;
 fromword := 2;
 wb1 := wordbool(frombyte > fromword);
 Test('Value should be TRUE...',wb1);
 fromword := $1000;
 fromlong := $4000;
 wb1 := wordbool(fromlong > fromword);
 Test('Value should be TRUE...',wb1);
 frombyte := $10;
 fromword := $20;
 lb1 := longbool(fromword > frombyte);
 Test('Value should be TRUE...',lb1);
 fromword := $1000;
 fromlong := $0100;
 lb1 := longbool(fromlong > fromword);
 Test('Value should be FALSE...',lb1);
{$ifndef tp}
 fromint64 := $10000000;
 fromlong := $02;
 lb1 := longbool(fromint64 > fromlong);
 Test('Value should be TRUE...',lb1);
{$endif}
*)
  if failed then
   begin
     Writeln('Some tests failed!');
     halt(1);
   end;
end.

{
   $Log$
   Revision 1.4  2002-09-07 15:40:55  peter
     * old logs removed and tabs fixed

   Revision 1.3  2002/05/13 13:45:38  peter
     * updated to compile tests with kylix

   Revision 1.2  2002/03/29 12:36:03  peter
     * add check if tests are successfull or not

}
