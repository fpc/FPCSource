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
{     This source code is released for the purpose to      }
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
{        DPMI     - FPC 0.9912+ (GO32V2)    (32 Bit)       }
{        WIN95/NT - FPC 0.9912+             (32 Bit)       }
{                                                          }

{$IFNDEF FPC_DOTTEDUNITS}
{$ifdef FV_UNICODE}
UNIT UAsciiTab;
{$else FV_UNICODE}
UNIT AsciiTab;
{$endif FV_UNICODE}
{$ENDIF FPC_DOTTEDUNITS}

{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                                  INTERFACE
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}

{====Include file to sort compiler platform out =====================}
{$I platform.inc}
{====================================================================}

{==== Compiler directives ===========================================}
{$H-}
{$X+} { Extended syntax is ok }
{$R-} { Disable range checking }
{$S-} { Disable Stack Checking }
{$I-} { Disable IO Checking }
{$Q-} { Disable Overflow Checking }
{$V-} { Turn off strict VAR strings }

{====================================================================}

{$IFDEF FPC_DOTTEDUNITS}
USES System.Objects,
   {$ifdef FV_UNICODE}
   FreeVision.Ufvcommon,FreeVision.Udrivers,FreeVision.Uviews,FreeVision.Uapp,
   {$else}
   FreeVision.Fvcommon,FreeVision.Drivers,FreeVision.Views, FreeVision.App,
   {$endif}
   FreeVision.Fvconsts,System.Console.Video;
{$ELSE FPC_DOTTEDUNITS}
USES Objects,
    {$ifdef FV_UNICODE}
    UFvcommon, UDrivers, UViews, UApp,
    {$else}
    Fvcommon, Drivers, Views, App,          { Standard GFV units }
    {$endif}
    FVConsts,Video;
{$ENDIF FPC_DOTTEDUNITS}

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
  private
    procedure DrawCurPos(enable : boolean);
  end;

{---------------------------------------------------------------------------}
{                  TREPORT OBJECT - View with details of current AnsiChar       }
{---------------------------------------------------------------------------}
  PReport = ^TReport;
  TReport = object(TView)
    ASCIIChar: LongInt;
    constructor Init (Var Bounds: TRect);
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
    procedure HandleEvent(var Event:TEvent); virtual;
    procedure SizeLimits (Var Min, Max: TPoint); Virtual;
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
     ObjType: idTable;
     VmtLink: Ofs(TypeOf(TTable)^);
     Load:    @TTable.Load;
     Store:   @TTable.Store
  );
  RReport: TStreamRec = (
     ObjType: idReport;
     VmtLink: Ofs(TypeOf(TReport)^);
     Load:    @TReport.Load;
     Store:   @TReport.Store
  );
  RASCIIChart: TStreamRec = (
     ObjType: idASCIIChart;
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
{$ifdef FV_UNICODE}
const cAsciiTable : array[0..255] of WideChar = ( { cp437, except for NUL char }
  #$0020, #$263A, #$263B, #$2665, #$2666, #$2663, #$2660, #$2022, #$25D8, #$25CB, #$25D9, #$2642, #$2640, #$266A, #$266B, #$263C
, #$25BA, #$25C4, #$2195, #$203C, #$00B6, #$00A7, #$25AC, #$21A8, #$2191, #$2193, #$2192, #$2190, #$221F, #$2194, #$25B2, #$25BC
, #$0020, #$0021, #$0022, #$0023, #$0024, #$0025, #$0026, #$0027, #$0028, #$0029, #$002A, #$002B, #$002C, #$002D, #$002E, #$002F
, #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037, #$0038, #$0039, #$003A, #$003B, #$003C, #$003D, #$003E, #$003F
, #$0040, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047, #$0048, #$0049, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F
, #$0050, #$0051, #$0052, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058, #$0059, #$005A, #$005B, #$005C, #$005D, #$005E, #$005F
, #$0060, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067, #$0068, #$0069, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F
, #$0070, #$0071, #$0072, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078, #$0079, #$007A, #$007B, #$007C, #$007D, #$007E, #$2302 {#$007F is not displayable put #$2302}
, #$00C7, #$00FC, #$00E9, #$00E2, #$00E4, #$00E0, #$00E5, #$00E7, #$00EA, #$00EB, #$00E8, #$00EF, #$00EE, #$00EC, #$00C4, #$00C5
, #$00C9, #$00E6, #$00C6, #$00F4, #$00F6, #$00F2, #$00FB, #$00F9, #$00FF, #$00D6, #$00DC, #$00A2, #$00A3, #$00A5, #$20A7, #$0192
, #$00E1, #$00ED, #$00F3, #$00FA, #$00F1, #$00D1, #$00AA, #$00BA, #$00BF, #$2310, #$00AC, #$00BD, #$00BC, #$00A1, #$00AB, #$00BB
, #$2591, #$2592, #$2593, #$2502, #$2524, #$2561, #$2562, #$2556, #$2555, #$2563, #$2551, #$2557, #$255D, #$255C, #$255B, #$2510
, #$2514, #$2534, #$252C, #$251C, #$2500, #$253C, #$255E, #$255F, #$255A, #$2554, #$2569, #$2566, #$2560, #$2550, #$256C, #$2567
, #$2568, #$2564, #$2565, #$2559, #$2558, #$2552, #$2553, #$256B, #$256A, #$2518, #$250C, #$2588, #$2584, #$258C, #$2590, #$2580
, #$03B1, #$00DF, #$0393, #$03C0, #$03A3, #$03C3, #$00B5, #$03C4, #$03A6, #$0398, #$03A9, #$03B4, #$221E, #$03C6, #$03B5, #$2229
, #$2261, #$00B1, #$2265, #$2264, #$2320, #$2321, #$00F7, #$2248, #$00B0, #$2219, #$00B7, #$221A, #$207F, #$00B2, #$25A0, #$00A0
);
{$endif}

procedure TTable.Draw;
var
  NormColor : byte;
  B : TDrawBuffer;
  x,y : sw_integer;
begin
  NormColor:=GetColor(1);
  For y:=0 to size.Y-1 do begin
    For x:=0 to size.X-1 do
{$ifdef FV_UNICODE}
      begin
        B[x].Attribute:=NormColor;
        B[x].ExtendedGraphemeCluster:=cAsciiTable[(y*Size.X+x) and $ff];
      end;
{$else}
      B[x]:=(NormColor shl 8) or ((y*Size.X+x) and $ff);
{$endif}
    WriteLine(0,Y,Size.X,1,B);
  end;
  DrawCurPos(true);
end;

procedure TTable.DrawCurPos(enable : boolean);
var
  Color : byte;
{$ifdef FV_UNICODE}
  B : TEnhancedVideoCell;
{$else}
  B : word;
{$endif}
begin
  Color:=GetColor(1);
  { add blinking if enable }
  If Enable then
    Color:=((Color and $F) shl 4) or (Color shr 4);
{$ifdef FV_UNICODE}
  B.Attribute:=Color;
  B.ExtendedGraphemeCluster:=cAsciiTable[(Cursor.Y*Size.X+Cursor.X) and $ff];
{$else}
  B:=(Color shl 8) or ((Cursor.Y*Size.X+Cursor.X) and $ff);
{$endif}
  WriteLine(Cursor.X,Cursor.Y,1,1,B);
end;

procedure TTable.HandleEvent(var Event:TEvent);
var
  CurrentPos : TPoint;
  Handled : boolean;

  procedure SetTo(xpos, ypos : sw_integer;press:SmallInt);
  var
    newchar : ptrint;
  begin
{$ifdef FV_UNICODE}
    newchar:=ptrint(cAsciiTable[(ypos*size.X+xpos) and $ff]);
{$else}
    newchar:=(ypos*size.X+xpos) and $ff;
{$endif}
    DrawCurPos(false);
    SetCursor(xpos,ypos);
    Message(Owner,evCommand,AsciiTableCommandBase,
      pointer(newchar));
    if press>0 then
      begin
        Message(Owner,evCommand,AsciiTableCommandBase+press,pointer(newchar));
      end;
    DrawCurPos(true);
    ClearEvent(Event);
  end;

begin
  case Event.What of
    evMouseDown :
      begin
        If MouseInView(Event.Where) then
          begin
            MakeLocal(Event.Where, CurrentPos);
            SetTo(CurrentPos.X, CurrentPos.Y,1);
            exit;
          end;
      end;
    evKeyDown :
      begin
        Handled:=true;
        case Event.Keycode of
          kbUp   : if Cursor.Y>0 then
                   SetTo(Cursor.X,Cursor.Y-1,0);
          kbDown : if Cursor.Y<Size.Y-1 then
                   SetTo(Cursor.X,Cursor.Y+1,0);
          kbLeft : if Cursor.X>0 then
                   SetTo(Cursor.X-1,Cursor.Y,0);
          kbRight: if Cursor.X<Size.X-1 then
                   SetTo(Cursor.X+1,Cursor.Y,0);
          kbHome : SetTo(0,0,0);
          kbEnd  : SetTo(Size.X-1,Size.Y-1,0);
          kbEnter: SetTo(Cursor.X,Cursor.Y,1);
        else
          Handled:=false;
        end;
        if Handled then
          exit;
      end;
  end;
  inherited HandleEvent(Event);
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                        TReport OBJECT METHODS                             }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}


constructor TReport.Init (Var Bounds: TRect);
begin
  Inherited Init (Bounds);
{$ifdef FV_UNICODE}
  AsciiChar:=longint(cAsciiTable[0]);
{$else}
  AsciiChar:=0;
{$endif}
end;


constructor TReport.Load(var S: TStream);
begin
  Inherited Load(S);
  S.Read(AsciiChar,Sizeof(AsciiChar));
end;

procedure TReport.Draw;
  var
    stHex,stDec : string[5];
    s : sw_string;
begin
  Str(AsciiChar,StDec);
  while length(stDec)<3 do
    stDec:=' '+stDec;
{$ifdef FV_UNICODE}
  if length(stDec)<4 then
    stDec:=' '+stDec;
  if length(stDec)<5 then
    stDec:=stDec+' ';
  stHex:=hexstr(AsciiChar,4);
  s:='WChar "'+widechar(Word(AsciiChar))+'" Decimal:'+
     StDec+'Hex:$'+StHex+
     ''; // //{!ss:fill gap. FormatStr function using be better}
{$else}
  stHex:=hexstr(AsciiChar,2);
  s:='AnsiChar "'+chr(AsciiChar)+'" Decimal:'+
     StDec+' Hex:$'+StHex+
     '  '; // //{!ss:fill gap. FormatStr function using be better}
{$endif}
  WriteStr(0,0,S,1);
end;

procedure TReport.HandleEvent(var Event:TEvent);
begin
  if (Event.what=evCommand) and
     (Event.Command =  AsciiTableCommandBase) then
    begin
      AsciiChar:=PtrInt(Event.InfoPtr);
      Draw;
      ClearEvent(Event);
    end
  else inherited HandleEvent(Event);
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
  Inherited Init(R,'ASCII Table',wnNoNumber);
  Flags:=Flags and not (wfGrow or wfZoom);
  Palette:=wpGrayWindow;
  R.Assign(1,10,33,11);
  New(Report,Init(R));
  Report^.Options:=Report^.Options or ofFramed;
  Insert(Report);
  R.Assign(1,1,33,9);
  New(Table,Init(R));
  Table^.Options:=Table^.Options or (ofSelectable+ofTopSelect);
  Insert(Table);
  Table^.Select;
end;

constructor TASCIIChart.Load(var S: TStream);
begin
  Inherited Load(S);
  GetSubViewPtr(S,Table);
  GetSubViewPtr(S,Report);
end;

procedure TASCIIChart.Store(var S: TStream);
begin
  Inherited Store(S);
  PutSubViewPtr(S,Table);
  PutSubViewPtr(S,Report);
end;

procedure TASCIIChart.HandleEvent(var Event:TEvent);
begin
  {writeln(stderr,'ascii cmd',event.what, ' ', event.command);}
  if (Event.what=evCommand) and
     (Event.Command =  AsciiTableCommandBase) then
    begin
      Report^.HandleEvent(Event);
    end
  else inherited HandleEvent(Event);
end;

procedure TASCIIChart.SizeLimits (Var Min, Max: TPoint);
begin
  Min.X:=34;
  Min.Y:=12;
  Max:=Min;  { make sure no resize can take place }
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
