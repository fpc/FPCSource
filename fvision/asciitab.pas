{ $Id$  }
{********[ SOURCE FILE OF GRAPHICAL FREE VISION ]**********}
{                                                          }
{   System independent GRAPHICAL clone of ASCIITAB.PAS     }
{                                                          }
{   Interface Copyright (c) 1992 Borland International     }
{                                                          }
{   Copyright (c) 2002 by Pierre Muller                    }
{   pierre@freepascal.org                                  }
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

UNIT AsciiTab;

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

USES FVConsts, Objects, Drivers, Views, App;      { Standard GFV units }

{***************************************************************************}
{                        PUBLIC OBJECT DEFINITIONS                          }
{***************************************************************************}


{---------------------------------------------------------------------------}
{                  TTABLE OBJECT - 32x32 matrix of all chars                }
{---------------------------------------------------------------------------}

type
  PTable = ^TTable;
  TTable = object(TView)
    procedure Draw; virtual;
    procedure HandleEvent(var Event:TEvent); virtual;
  end;

{---------------------------------------------------------------------------}
{                  TREPORT OBJECT - View with details of current char       }
{---------------------------------------------------------------------------}
  PReport = ^TReport;
  TReport = object(TView)
    ASCIIChar: LongInt;
    constructor Load(var S: TStream);
    procedure Draw; virtual;
    procedure HandleEvent(var Event:TEvent); virtual;
    procedure Store(var S: TStream);
  end;

{---------------------------------------------------------------------------}
{                  TASCIIChart OBJECT - the complete AsciiChar window       }
{---------------------------------------------------------------------------}

  PASCIIChart = ^TASCIIChart;
  TASCIIChart = object(TWindow)
    Report: PReport;
    Table: PTable;
    constructor Init;
    constructor Load(var S: TStream);
    procedure   Store(var S: TStream);
  end;

{---------------------------------------------------------------------------}
{ AsciiTableCommandBase                                                     }
{---------------------------------------------------------------------------}

const
  AsciiTableCommandBase: Word = 910;

{---------------------------------------------------------------------------}
{ Registrations records                                                     }
{---------------------------------------------------------------------------}

  RTable: TStreamRec = (
     ObjType: 10030;
     VmtLink: Ofs(TypeOf(TTable)^);
     Load:    @TTable.Load;
     Store:   @TTable.Store
  );
  RReport: TStreamRec = (
     ObjType: 10031;
     VmtLink: Ofs(TypeOf(TReport)^);
     Load:    @TReport.Load;
     Store:   @TReport.Store
  );
  RASCIIChart: TStreamRec = (
     ObjType: 10032;
     VmtLink: Ofs(TypeOf(TASCIIChart)^);
     Load:    @TASCIIChart.Load;
     Store:   @TASCIIChart.Store
  );

{---------------------------------------------------------------------------}
{ Registration procedure                                                    }
{---------------------------------------------------------------------------}
procedure RegisterASCIITab;



{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                             IMPLEMENTATION
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}

{***************************************************************************}
{                              OBJECT METHODS                               }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                          TTable OBJECT METHODS                            }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

procedure TTable.Draw;
var
  Color : byte;
  B : TDrawBuffer;
  x,y : sw_integer;
begin
  Color:=GetColor(1);
  For y:=0 to size.Y-1 do
    For x:=0 to size.X-1 do
      begin
        B[x]:=(Color shl 8) or ((y*Size.X+x) and $ff);
        WriteLine(0,Y,Size.X,1,B);
      end;
  DrawCursor;
end;

procedure TTable.HandleEvent(var Event:TEvent);
begin
  inherited HandleEvent(Event);
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                        TReport OBJECT METHODS                             }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

constructor TReport.Load(var S: TStream);
begin
  Inherited Load(S);
  S.Read(AsciiChar,Sizeof(AsciiChar));
end;

procedure TReport.Draw;
  var
    stHex,stDec : string[3];
    s : string;
begin
  Str(AsciiChar,StDec);
  stHex:=hexstr(AsciiChar,2);
  s:='Char "'+chr(AsciiChar)+'" Decimal: '+
     StDec+' Hex: $'+StHex+'   ';
  WriteStr(0,0,S,GetColor(1));
end;

procedure TReport.HandleEvent(var Event:TEvent);
begin
  inherited HandleEvent(Event);
end;

procedure TReport.Store(var S: TStream);
begin
  Inherited Store(S);
  S.Write(AsciiChar,Sizeof(AsciiChar));
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                        TAsciiChart OBJECT METHODS                         }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

constructor TASCIIChart.Init;
var
  R : Trect;
begin
  R.Assign(0,0,34,12);
  Inherited Init(R,'Ascii table',wnNoNumber);
  R.Assign(1,1,33,9);
  New(Report,Init(R));
  Insert(Report);
  R.Assign(11,1,33,12);
  New(Table,Init(R));
  Insert(Table);
end;

constructor TASCIIChart.Load(var S: TStream);
begin
  Inherited Load(S);
  Report:=PReport(S.Get);
  Table:=PTable(S.Get);
end;

procedure   TASCIIChart.Store(var S: TStream);
begin
  Inherited Store(S);
  S.Put(Report);
  S.Put(Table);
end;

{---------------------------------------------------------------------------}
{ Registration procedure                                                    }
{---------------------------------------------------------------------------}
procedure RegisterASCIITab;
begin
  RegisterType(RTable);
  RegisterType(RReport);
  RegisterType(RAsciiChart);
end;


END.
{
 $Log$
 Revision 1.1  2002-05-29 22:14:53  pierre
  Newfile


}
