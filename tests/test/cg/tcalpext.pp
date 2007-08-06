
{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{****************************************************************}
{ NODE TESTED : secondcalln()                                    }
{****************************************************************}
{ PRE-REQUISITES: secondload()                                   }
{                 secondassign()                                 }
{                 secondcalln()                                  }
{                 secondadd()                                    }
{                 secondtypeconv()                               }
{****************************************************************}
{ DEFINES:                                                       }
{****************************************************************}
{ This test check that the code created by Free Pascal for       }
{ functions declared with cdecl modifier are correct             }
{****************************************************************}

{$define USE_PASCAL_OBJECT}

{$MODE OBJFPC}
{$STATIC ON}
{$R+}
{ Compile ptest.o }
uses
  strings,
  ptest,
  ctypes
  ;


{$ifdef USE_PASCAL_OBJECT}
  {$ifdef win32}
    {$ifdef ver1_0}
      {$L ptest.ow}
    {$else}
      {$L ptest.o}
    {$endif}
  {$else}
  {$L ptest.o}
  {$endif not win32}
{$endif USE_PASCAL_OBJECT}

{$i tcalext.pp }

