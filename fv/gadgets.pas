{ $Id$  }
{********[ SOURCE FILE OF GRAPHICAL FREE VISION ]**********}
{                                                          }
{   System independent GRAPHICAL clone of GADGETS.PAS      }
{                                                          }
{   Interface Copyright (c) 1992 Borland International     }
{                                                          }
{   Copyright (c) 1999 by Leon de Boer                     }
{   ldeboer@attglobal.net  - primary e-mail address        }
{   ldeboer@starwon.com.au - backup e-mail address         }
{                                                          }
{****************[ THIS CODE IS FREEWARE ]*****************}
{                                                          }
{     This sourcecode is released for the purpose to       }
{   promote the pascal language on all platforms. You may  }
{   redistribute it and/or modify with the following       }
{   DISCLAIMER.                                            }
{                                                          }
{     This SOURCE CODE is distributed "AS IS" WITHOUT      }
{   WARRANTIES AS TO PERFORMANCE OF MERCHANTABILITY OR     }
{   ANY OTHER WARRANTIES WHETHER EXPRESSED OR IMPLIED.     }
{                                                          }
{*****************[ SUPPORTED PLATFORMS ]******************}
{     16 and 32 Bit compilers                              }
{        DOS      - Turbo Pascal 7.0 +      (16 Bit)       }
{        DPMI     - Turbo Pascal 7.0 +      (16 Bit)       }
{                 - FPC 0.9912+ (GO32V2)    (32 Bit)       }
{        WINDOWS  - Turbo Pascal 7.0 +      (16 Bit)       }
{                 - Delphi 1.0+             (16 Bit)       }
{        WIN95/NT - Delphi 2.0+             (32 Bit)       }
{                 - Virtual Pascal 2.0+     (32 Bit)       }
{                 - Speedsoft Sybil 2.0+    (32 Bit)       }
{                 - FPC 0.9912+             (32 Bit)       }
{        OS2      - Virtual Pascal 1.0+     (32 Bit)       }
{                                                          }
{*******************[ DOCUMENTATION ]**********************}
{                                                          }
{   This unit had to be for GFV due to some problems with  }
{  the original Borland International implementation.      }
{                                                          }
{   First it used the DOS unit for it's time calls in the  }
{  TClockView object. Since this unit can not be compiled  }
{  under WIN/NT/OS2 we use a new unit TIME.PAS which was   }
{  created and works under these O/S.                      }
{                                                          }
{   Second the HeapView object accessed MemAvail from in   }
{  the Draw call. As GFV uses heap memory during the Draw  }
{  call the OldMem value always met the test condition in  }
{  the update procedure. The consequence was the view      }
{  would continually redraw. By moving the memavail call   }
{  the update procedure this eliminates this problem.      }
{                                                          }
{   Finally the original object relied on the font char    }
{  blocks being square to erase it's entire view area as   }
{  it used a simple writeline call in the Draw method.     }
{  Under GFV font blocks are not necessarily square and    }
{  so both objects had their Draw routines rewritten. As   }
{  the Draw had to be redone it was done in the GFV split  }
{  drawing method to accelerate the graphical speed.       }
{                                                          }
{******************[ REVISION HISTORY ]********************}
{  Version  Date        Fix                                }
{  -------  ---------   ---------------------------------  }
{  1.00     12 Nov 99   First multi platform release       }
{**********************************************************}

UNIT Gadgets;

{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                                  INTERFACE
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}

{====Include file to sort compiler platform out =====================}
{$I Platform.inc}
{====================================================================}

{==== Compiler directives ===========================================}

{$IFNDEF PPC_FPC}{ FPC doesn't support these switches }
  {$F-} { Near calls are okay }
  {$A+} { Word Align Data }
  {$B-} { Allow short circuit boolean evaluations }
  {$O+} { This unit may be overlaid }
  {$G+} { 286 Code optimization - if you're on an 8088 get a real computer }
  {$P-} { Normal string variables }
  {$N-} { No 80x87 code generation }
  {$E+} { Emulation is on }
{$ENDIF}

{$X+} { Extended syntax is ok }
{$R-} { Disable range checking }
{$S-} { Disable Stack Checking }
{$I-} { Disable IO Checking }
{$Q-} { Disable Overflow Checking }
{$V-} { Turn off strict VAR strings }
{====================================================================}

USES FVConsts, Time, Objects, Drivers, Views, App;      { Standard GFV units }

{***************************************************************************}
{                        PUBLIC OBJECT DEFINITIONS                          }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                  THeapView OBJECT - ANCESTOR VIEW OBJECT                  }
{---------------------------------------------------------------------------}
TYPE
   THeapView = OBJECT (TView)
         OldMem: LongInt;                             { Last memory count }
      PROCEDURE Update;
      PROCEDURE DrawBackGround; Virtual;
   END;
   PHeapView = ^THeapView;                            { Heapview pointer }

{---------------------------------------------------------------------------}
{                 TClockView OBJECT - ANCESTOR VIEW OBJECT                  }
{---------------------------------------------------------------------------}
TYPE
   TClockView = OBJECT (TView)
         Refresh : Byte;                              { Refresh rate }
         LastTime: Longint;                           { Last time displayed }
         TimeStr : String[10];                        { Time string }
      CONSTRUCTOR Init (Var Bounds: TRect);
      FUNCTION FormatTimeStr (H, M, S: Word): String; Virtual;
      PROCEDURE Update; Virtual;
      PROCEDURE DrawBackGround; Virtual;
   END;
   PClockView = ^TClockView;                          { Clockview ptr }

{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                             IMPLEMENTATION
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}

{***************************************************************************}
{                              OBJECT METHODS                               }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                          THeapView OBJECT METHODS                         }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--THeapView----------------------------------------------------------------}
{  Update -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Nov99 LdB            }
{---------------------------------------------------------------------------}
PROCEDURE THeapView.Update;
BEGIN
   If (OldMem <> MemAvail) Then Begin                 { Memory differs }
     OldMem := MemAvail;                              { Hold memory avail }
     SetDrawMask(vdBackGnd OR vdInner);               { Set draw masks }
     DrawView;                                        { Now redraw }
   End;
END;

{--THeapView----------------------------------------------------------------}
{  DrawBackGround -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Nov99 LdB    }
{---------------------------------------------------------------------------}
PROCEDURE THeapView.DrawBackGround;
VAR HOfs: Integer; S: String;
BEGIN
   Str(OldMem, S);                                    { Convert to string }
   HOfs := ColourOfs;                                 { Hold any offset }
   ColourOfs := 2;                                    { Set colour offset }
   Inherited DrawBackGround;                          { Clear the backgound }
   ColourOfs := HOfs;                                 { Reset any offset }
   WriteStr(-(RawSize.X-TextWidth(S)+1), 0, S, 2);    { Write the string }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                        TClockView OBJECT METHODS                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TClockView---------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Nov99 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TClockView.Init (Var Bounds: TRect);
BEGIN
   Inherited Init(Bounds);                            { Call ancestor }
   FillChar(LastTime, SizeOf(LastTime), #$FF);        { Fill last time }
   TimeStr := '';                                     { Empty time string }
   Refresh := 1;                                      { Refresh per second }
END;

{--TClockView---------------------------------------------------------------}
{  FormatStr -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Nov99 LdB         }
{---------------------------------------------------------------------------}
FUNCTION TClockView.FormatTimeStr (H, M, S: Word): String;
VAR Hs, Ms, Ss: String;
BEGIN
   Str(H, Hs);                                        { Convert hour string }
   While (Length(Hs) < 2) Do Hs := '0' + Hs;          { Add lead zero's }
   Str(M, Ms);                                        { Convert min string }
   While (Length(Ms) < 2) Do Ms := '0' + Ms;          { Add lead zero's }
   Str(S, Ss);                                        { Convert sec string }
   While (Length(Ss) < 2) Do Ss := '0' + Ss;          { Add lead zero's }
   FormatTimeStr := Hs + ':'+ Ms + ':' + Ss;          { Return string }
END;

{--TClockView---------------------------------------------------------------}
{  Update -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Nov99 LdB            }
{---------------------------------------------------------------------------}
PROCEDURE TClockView.Update;
VAR Hour, Min, Sec, Sec100: Word;
BEGIN
   GetTime(Hour, Min, Sec, Sec100);                   { Get current time }
   If (Abs(Sec - LastTime) >= Refresh) Then Begin     { Refresh time elapsed }
     LastTime := Sec;                                 { Hold second }
     TimeStr := FormatTimeStr(Hour, Min, Sec);        { Create time string }
     SetDrawMask(vdBackGnd OR vdInner);               { Set draw masks }
     DrawView;                                        { Now redraw }
   End;
END;

{--TClockView---------------------------------------------------------------}
{  DrawBackGround -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Nov99 LdB    }
{---------------------------------------------------------------------------}
PROCEDURE TClockView.DrawBackGround;
VAR HOfs: Integer;
BEGIN
   HOfs := ColourOfs;                                 { Hold any offset }
   ColourOfs := 2;                                    { Set colour offset }
   Inherited DrawBackGround;                          { Clear the backgound }
   ColourOfs := HOfs;                                 { Reset any offset }
   WriteStr(0, 0, TimeStr, 2);                        { Write the string }
END;

END.
{
 $Log$
 Revision 1.3  2001-08-04 19:14:33  peter
   * Added Makefiles
   * added FV specific units and objects from old FV

 Revision 1.2  2000/08/24 12:00:21  marco
  * CVS log and ID tags


}
