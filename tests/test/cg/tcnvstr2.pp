{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{****************************************************************}
{ NODE TESTED : secondtypeconvert() -> second_cstring_to_pchar   }
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
{$H-}
const
 StrConst = 'HELLO WORLD';
 AnsiConst = 'COCORICCO!';
 AnsiConst2 = '';

procedure testcstring2pcharone;
 var
  p1 : pchar;
 begin
  WriteLn('(left) : LOC_MEM; (right) : LOC_MEM');
  p1 := pchar(strconst);
  WriteLn('Value should be ''HELLO WORLD''...',p1);
 end;

{ source:                                                       }
{   LOC_REFERENCE, LOC_MEM                                      }
{ destination:                                                  }
var
 p: pchar;
Begin
  WriteLn('------------------- STRING/PCHAR ------------------------');
  WriteLn('(left) : LOC_MEM; (right) : LOC_MEM');
 { LOC_MEM -> LOC_MEM test }
 { SHORTSTRING -> PCHAR           }
 p := pchar(strconst);
 WriteLn('Value should be ''HELLO WORLD''...',p);
 testcstring2pcharone;
 WriteLn('------------------- ANSI/PCHAR ------------------------');
 WriteLn('(left) : LOC_MEM; (right) : LOC_MEM');
 p:=pchar(ansistring(ansiconst));
 WriteLn('Value should be ''COCORICCO!''...',p);
 p:=pchar(ansistring(ansiconst2));
 WriteLn('Value should be ''''...',p);
End.
