{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{****************************************************************}
{ NODE TESTED : secondtypeconvert() -> second_string_string      }
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


{ source:                                                       }
{   LOC_REFERENCE, LOC_MEM                                      }
{ destination:                                                  }
type
  shortstr = string[127];
var
 s1: string[255];
 s2: shortstr;
 ansi1: ansistring;
Begin
  WriteLn('------------------- ANSI/SHORT ------------------------');
  WriteLn('(left) : LOC_REFERENCE; (right) : LOC_REFERENCE');
 { LOC_REFERENCE -> LOC_REFERENCE test }
 { SHORTSTRING -> ANSISTRING           }
 ansi1 := 'HELLO WORLD';
 s1 := 'HELLO WORLD';
 Write('Value should be ''HELLO WORLD''..');
 if s1 = ansi1 then
   WriteLn('Success.')
 else
   WriteLn('Failure.');
 ansi1 := '';
 s1 := '';
 Write('Value should be ''''...');
 if s1 = ansi1 then
   WriteLn('Success.')
 else
   WriteLn('Failure.');
 { LOC_REFERENCE -> LOC_REFERENCE test }
 { SHORTSTRING -> SHORTSTRING          }
  WriteLn('------------------- SHORT/SHORT ------------------------');
  WriteLn('(left) : LOC_REFERENCE; (right) : LOC_REFERENCE');
 s1 := 'HELLO THIS WORLD';
 s2 := 'HELLO THIS WORLD';
 Write('Value should be ''HELLO THIS WORLD''...');
 if shortstr(s1) = s2 then
   WriteLn('Success.')
 else
   WriteLn('Failure.');
 s1 := '';
 s2 := '';
 Write('Value should be ''''...');
 if shortstr(s1) = s2 then
   WriteLn('Success.')
 else
   WriteLn('Failure.');
End.

{
  $Log$
  Revision 1.2  2002-09-07 15:40:55  peter
    * old logs removed and tabs fixed

}
