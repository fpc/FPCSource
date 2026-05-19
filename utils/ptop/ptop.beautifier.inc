{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt, member of
    the Free Pascal development team

    Pascal Pretty-Printer object implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  **********************************************************************}

Unit PtoP.Beautifier;
{$mode objfpc}
{$h+}

{
  This unit is based heavily on the code by

  Author:  Peter Grogono
    This program is based on a Pascal pretty-printer written by Ledgard,
    Hueras, and Singer.  See SIGPLAN Notices, Vol. 12, No. 7, July 1977,
    pages 101-105, and PP.DOC/HLP.
    This version of PP developed under Pascal/Z V4.0 or later.
    Very minor modifications for Turbo Pascal made by Willett Kempton
    March 1984 and Oct 84.  Runs under 8-bit Turbo or 16-bit Turbo.
    Toad Hall tweak, rewrite for TP 5, 28 Nov 89

  The following was changed :
  - Object oriented
  - Uses streams
  - Run-time customizable.

  In 2026, this unit has been renamed from PtopU to Ptop.Beautifier.
  Changes are made by Le Bao Nguyen, and they are:
  - Code split (to ptop.strutils, ptop.types and ptop.config)
  - Make more use of RTL and FCL types, functions and properties
  - Dotted unit compatibility.
}

Interface

uses
  {$ifdef FPC_DOTTEDUNITS}
  System.Classes,
  System.Sysutils,
  {$else}
  Classes,
  Sysutils,
  {$endif}
  ptop.types
  ;

type
  { TPrettyPrinter }
  TPrettyPrinter = Class(TObject)
  Private
    FTokenScope: TTokenScope;
{$ifdef DEBUG_PTOP}
    GobbleLevel : Integer;
{$endif DEBUG_PTOP}
    PreviousSymbol : keysymbol;
    RecordLevel : Integer;
    ClassSeen,ObjectSeen : Boolean;
    LastStruct : KeySymbol;
    CRPending : BOOLEAN;
    currchar,nextchar : charinfo;
    currsym,nextsym : symbolinfo;
    inlines,outlines : INTEGER;
    stack   : symbolstack;
    top,startpos,currlinepos,currmargin : Integer;
    option : OptionTable;
    FirstWordStackPos,
    FirstWordPos,
    FLineSize,
    FIndent : Integer;
    ins,outs,cfgs : TStream;
    Procedure GetChar;
    Procedure StoreNextChar(Var lngth: INTEGER;
                            var Value: Token);
    Procedure SkipBlanks(Out spacesbefore, crsbefore: INTEGER);
    Procedure GetComment(sym: symbolinfo);
    Procedure GetDoubleComment(sym: symbolinfo);
    Procedure GetDelphiComment(sym: symbolinfo);
    Procedure GetNumber(sym: symbolinfo);
    Procedure GetCharLiteral(sym: symbolinfo);
    Function  char_Type: keysymbol;
    Procedure GetSpecialChar(sym: symbolinfo);
    Procedure GetNextSymbol(sym: symbolinfo);
    Procedure GetIdentifier(sym: symbolinfo);
    Procedure GetSymbol;
    Procedure PopStack(Out indentsymbol: keysymbol;
                       Out prevmargin: INTEGER);
    Procedure PushStack(indentsymbol: keysymbol;
                        prevmargin: INTEGER );
    Procedure WriteCRs(numberofcrs: INTEGER);
    Procedure InsertCR;
    Procedure InsertBlankLine;
    Procedure LShiftOn(dindsym: keysymset);
    Procedure LShift;
    Procedure InsertSpace(VAR symbol: symbolinfo);
    Procedure MoveLinePos(newlinepos: INTEGER);
    Procedure PrintSymbol;
    Procedure PPSymbol;
    Procedure Gobble(terminators: keysymset);
    Procedure RShift(currmsym: keysymbol);
    Procedure RShiftIndent{$ifdef DEBUG_PTOP}(currmsym: keysymbol){$endif debug};
  Public
    Constructor Create;
    Function PrettyPrint : Boolean;
    Property LineSize : Integer Read FLineSize Write FLineSize;
    Property Indent : Integer Read FIndent Write FIndent;    { How many characters to indent ? }
    Property Source : TStream Read Ins Write Ins;
    Property Dest : TStream Read OutS Write Outs;
    Property Config : Tstream Read cfgS Write cfgs;
    Property CurrentScope : TTokenScope Read FTokenScope Write FTokenScope;
  end;
