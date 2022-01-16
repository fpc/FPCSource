
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
{$R+}
{ Compile ptest.o }
uses
  strings,
  ptest,
  ctypes
  ;

{$i tcalext.pp }

