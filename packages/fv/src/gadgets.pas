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
{$I platform.inc}
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
   THeapViewMode=(HVNormal,HVComma,HVKb,HVMb);

   THeapView = OBJECT (TView)
         Mode   : THeapViewMode;
         OldMem: LongInt;                             { Last memory count }
      constructor Init(var Bounds: TRect);
      constructor InitComma(var Bounds: TRect);
      constructor InitKb(var Bounds: TRect);
      constructor InitMb(var Bounds: TRect);
      PROCEDURE Update;
      PROCEDURE Draw; Virtual;
      Function  Comma ( N : LongInt ) : String;
   END;
   PHeapView = ^THeapView;                            { Heapview pointer }

{---------------------------------------------------------------------------}
{                 TClockView OBJECT - ANCESTOR VIEW OBJECT                  }
{---------------------------------------------------------------------------}
TYPE
   TClockView = OBJECT (TView)
         am : Char;
         Refresh : Byte;                              { Refresh rate }
         LastTime: Longint;                           { Last time displayed }
         TimeStr : String[10];                        { Time string }
      CONSTRUCTOR Init (Var Bounds: TRect);
      FUNCTION FormatTimeStr (H, M, S: Word): String; Virtual;
      PROCEDURE Update; Virtual;
      PROCEDURE Draw; Virtual;
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

constructor THeapView.Init(var Bounds: TRect);
begin
  inherited Init(Bounds);
  mode:=HVNormal;
  OldMem := 0;
end;

constructor THeapView.InitComma(var Bounds: TRect);
begin
  inherited Init(Bounds);
  mode:=HVComma;
  OldMem := 0;
end;

constructor THeapView.InitKb(var Bounds: TRect);
begin
  inherited Init(Bounds);
  mode:=HVKb;
  OldMem := 0;
end;

constructor THeapView.InitMb(var Bounds: TRect);
begin
  inherited Init(Bounds);
  mode:=HVMb;
  OldMem := 0;
end;

{--THeapView----------------------------------------------------------------}
{  Update -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Nov99 LdB            }
{---------------------------------------------------------------------------}
PROCEDURE THeapView.Update;
var
  status : TFPCHeapStatus;
BEGIN
   status:=GetFPCHeapStatus;
   If (OldMem <> status.CurrHeapUsed) Then Begin                 { Memory differs }
     OldMem := status.CurrHeapUsed;                              { Hold memory avail }
     DrawView;                                        { Now redraw }
   End;
END;

{--THeapView----------------------------------------------------------------}
{  Draw -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Nov99 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE THeapView.Draw;
VAR
  C : Byte;
  S : String;
  B : TDrawBuffer;
begin
  case mode of
    HVNormal :
      Str(OldMem:Size.X, S);
    HVComma :
      S:=Comma(OldMem);
    HVKb :
      begin
        Str(OldMem shr 10:Size.X-1, S);
        S:=S+'K';
      end;
    HVMb :
      begin
        Str(OldMem shr 20:Size.X-1, S);
        S:=S+'M';
      end;
  end;
  C:=GetColor(2);
  MoveChar(B,' ',C,Size.X);
  MoveStr(B,S,C);
  WriteLine(0,0,Size.X,1,B);
END;

Function THeapView.Comma ( n : LongInt) : String;
Var
  num, loc : Byte;
  s : String;
  t : String;
Begin
  Str (n,s);
  Str (n:Size.X,t);

  num := length(s) div 3;
  if (length(s) mod 3) = 0 then dec (num);

  delete (t,1,num);
  loc := length(t)-2;

  while num > 0 do
  Begin
    Insert (',',t,loc);
    dec (num);
    dec (loc,3);
  End;

  Comma := t;
End;

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
     DrawView;                                        { Now redraw }
   End;
END;

{--TClockView---------------------------------------------------------------}
{  DrawBackGround -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Nov99 LdB    }
{---------------------------------------------------------------------------}
PROCEDURE TClockView.Draw;
VAR
  C : Byte;
  B : TDrawBuffer;
BEGIN
  C:=GetColor(2);
  MoveChar(B,' ',C,Size.X);
  MoveStr(B,TimeStr,C);
  WriteLine(0,0,Size.X,1,B);
END;

END.
