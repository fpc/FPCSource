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
  $Log: tdivz2.pp,v $
  Revision 1.3  2005/02/14 17:13:37  peter
    * truncate log

}
