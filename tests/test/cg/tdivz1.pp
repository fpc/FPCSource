{ %RESULT=200 }
{ %OPT=-Oonoconstprop }
{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{****************************************************************}
{ NODE TESTED : secondmoddiv() - division by zero test           }
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

{$ifdef VER70}
  {$define TP}
{$endif}

var
  cardinalres : cardinal;
  cardinalcnt : cardinal;
  int64res : int64;
  int64cnt : int64;
begin

  { RIGHT : LOC_REFERENCE }
  { LEFT : LOC_REGISTER   }
  int64res := 1;
  int64cnt := 0;
  int64res := int64res div int64cnt;
end.
