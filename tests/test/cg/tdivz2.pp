{ %RESULT=200 }
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
  longres : longint;
  longcnt : longint;
begin

  longres := 1;
  longcnt := 0;
  longres := longres div longcnt;
end.
{
  $Log$
  Revision 1.2  2004-05-02 12:11:44  peter
    * fixed linefeeds

  Revision 1.1  2002/09/21 13:28:06  carl
   + division by zero testing

}
