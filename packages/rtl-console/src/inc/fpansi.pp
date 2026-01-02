{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Emit ANSI escape sequences
    (Based on an idea by Vianney Gagnière)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit FPAnsi;
{$ENDIF}

{$mode objfpc}
{$h+}
{$modeswitch advancedrecords}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.StrUtils
{$ELSE}
  SysUtils,StrUtils  
{$ENDIF}
  ;

type
  TRGBComponent = 0..5;
  TGrayScale = 0..23;
  TEraseDisplay = (edEndOfScreen,edBeginOfScreen,edScreen,edScreenAndBuffer);
  TEraseLine = (elEndOfLine,edBeginOfLine,edLine);
    
  TAnsi = record
  private
    FCmds : string;
    FSGR : string;
    FText: string;
    procedure SGR(const aEscape: string);
    procedure AddCommand(const aCommand : string);
  public
  const
    // https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit
    Black     = 0;
    Red       = 1;
    Green     = 2;
    Yellow    = 3;
    Blue      = 4;
    Magenta   = 5;
    Cyan      = 6;
    White     = 7;
   
    BrightBlack   = 8;  
    BrightRed     = 9;
    BrightGreen   = 10;
    BrightYellow  = 11;
    BrightBlue    = 12;
    BrightMagenta = 13;
    BrightCyan    = 14;
    BrightWhite   = 15;
   
    function RGB(R,G,B : TRGBComponent) : Byte;
    Function GrayScale(aScale : TGrayScale) : Byte;
    function FgRGB(R,G,B : Byte) : TAnsi;
    function BgRGB(R,G,B : Byte) : TAnsi;
    function Fg(const aColorCode: Byte): TAnsi;
    function Bg(const aColorCode: Byte): TAnsi;
    function Blinking: TAnsi;
    function Bold: TAnsi;
    function Faint: TAnsi;
    function Strikethrough: TAnsi;
    function ToString : String;
    // Return ANSI escape control string
    class function CursorAt(aCol,aRow : Word; asFormat : boolean = True) : string; static;
    class function CursorAtCol(aCol : Word) : string; static;
    class function CursorUp(aLines : Word = 1) : string; static;
    class function CursorDown(aLines : Word = 1) : string; static;
    class function CursorForward(aCols : Word = 1) : string; static;
    class function CursorBackward(aCols : Word = 1) : string; static;
    class function CursorNextLine(aLines : Word = 1) : string; static;
    class function CursorPreviousLine(aLines : Word = 1) : string; static;
    class function EraseDisplay(aWhich : TEraseDisplay) : string; static;
    class function EraseLine(aWhich : TEraseLine) : string; static;
    // Apply ANSI escape control string.
    function At(aCol,aRow : Word; asFormat : Boolean = True): TAnsi;
    function AtCol(aCol : Word): TAnsi;
    function Up(aLines : Word = 1): TAnsi;
    function Down(aLines : Word = 1): TAnsi;
    function Forward(aCols : Word = 1): TAnsi;
    function Backward(aCols : Word = 1): TAnsi;
    function NextLine(aLines : Word = 1): TAnsi;
    function PreviousLine(aLines : Word = 1): TAnsi;
    function Emit(aClear : Boolean = True) : TAnsi;
    function EmitLn(aClear : Boolean = True) : TAnsi;
    Function Clear(aClearText : Boolean = False) : TAnsi;
    class operator :=(const aText: AnsiString): TAnsi;
    class operator :=(const aAnsi: TAnsi): AnsiString;
    class operator :=(const aText: ShortString): TAnsi;
    class operator :=(const aAnsi: TAnsi): ShortString;
  end;

implementation

class operator TAnsi.:=(const aText: AnsiString): TAnsi;
begin
  Result:=Default(TAnsi);
  Result.FText := aText;
end;

class operator TAnsi.:=(const aText: ShortString): TAnsi;
begin
  Result:=Default(TAnsi);
  Result.FText := aText;
end;

class operator TAnsi.:=(const aAnsi: TAnsi): AnsiString;
begin
  Result:=aAnsi.ToString;
end;

class operator TAnsi.:=(const aAnsi: TAnsi): Shortstring;
begin
  Result:=aAnsi.ToString;
end;

procedure TAnsi.SGR(const aEscape: string);
begin
  if FSGR<>'' then
    FSGR:=FSGR+';';
  FSGR := FSGR+aEscape;
end;

function TAnsi.GrayScale(aScale : TGrayScale) : Byte;

begin
  Result:=232+aScale;
end; 

function TAnsi.RGB(R,G,B : TRGBComponent) : Byte;

begin
  Result:=16+(36*r)+(6*g)+b;
end;

function TAnsi.ToString : string;
begin
  Result:=FCmds+#27'['+FSGR+'m'+FText+#27'[0m';
end;

function TAnsi.Fg(const aColorCode: Byte): TAnsi;
begin
  SGR(Format('38;5;%d',[aColorCode]));
  Result := Self;
end;

function TAnsi.Bg(const aColorCode: Byte): TAnsi;
begin
  SGR(Format('48;5;%d',[aColorCode]));
  Result := Self;
end;

function Tansi.FgRGB(R,G,B : Byte) : TAnsi;
begin
  SGR(Format('38,2;%d;%d;%d',[R,G,B]));
end;

function TAnsi.BgRGB(R,G,B : Byte) : TAnsi;
begin
  SGR(Format('48,2;%d;%d;%d',[R,G,B]));
end;

class function TAnsi.CursorUp(aLines : Word = 1) : string;
begin
  Result:=Format(#27'[%dA',[aLines]);
end;

class function TAnsi.CursorDown(aLines : Word = 1) : string;
begin
  Result:=Format(#27'[%dB',[aLines]);
end;

class function TAnsi.CursorForward(aCols : Word = 1) : string;
begin
  Result:=Format(#27'[%dC',[aCols]);
end;

class function TAnsi.CursorBackward(aCols : Word = 1) : string;
begin
  Result:=Format(#27'[%dD',[aCols]);
end;

class function TAnsi.CursorNextLine(aLines : Word = 1) : string;
begin
  Result:=Format(#27'[%dE',[aLines]);
end;

class function TAnsi.CursorPreviousLine(aLines : Word = 1) : string;
begin
  Result:=Format(#27'[%dF',[aLines]);
end;

class function TAnsi.CursorAt(aCol,aRow : Word; asFormat : boolean = True) : string;

Const
  Chars : Array[Boolean] of String = ('H','f');
  
begin
  Result:=Format(#27'[%d;%d%s',[aCol,aRow,Chars[asFormat]]);
end;

class function TAnsi.CursorAtCol(aCol : Word) : string;

begin
  Result:=Format(#27'[%dG',[aCol]);
end;

class function TAnsi.EraseDisplay(aWhich : TEraseDisplay) : string; 

begin
  Result:=Format(#27'[%dJ',[Ord(aWhich)]);
end;

class function TAnsi.EraseLine(aWhich : TEraseLine) : string; 

begin
  Result:=Format(#27'[%dK',[Ord(aWhich)]);
end;

procedure TAnsi.AddCommand(const aCommand : string);
begin
  FCmds:=FCmds+aCommand;
end;

function TAnsi.At(aCol,aRow : Word; asFormat : boolean = True) : TAnsi;

begin
  AddCommand(CursorAt(aCol,aRow,asFormat));
  Result:=Self;
end;

function TAnsi.AtCol(aCol : Word) : TAnsi;

begin
  AddCommand(CursorAtCol(aCol));
  Result:=Self;
end;

function TAnsi.Up(aLines : Word = 1): TAnsi;
begin
  AddCommand(CursorUp(aLines));
  Result:=Self;
end;

function TAnsi.Down(aLines : Word = 1): TAnsi;
begin
  AddCommand(CursorDown(aLines));
  Result:=Self;
end;

function TAnsi.Forward(aCols : Word = 1): TAnsi;
begin
  AddCommand(CursorForward(aCols));
  Result:=Self;
end;

function TAnsi.Backward(aCols : Word = 1): TAnsi;
begin
  AddCommand(CursorBackWard(aCols));
  Result:=Self;
end;

function TAnsi.NextLine(aLines : Word = 1): TAnsi;
begin
  AddCommand(CursorNextLine(aLines));
  Result:=Self;
end;

function TAnsi.PreviousLine(aLines : Word = 1): TAnsi;
begin
  AddCommand(CursorPreviousLine(aLines));
  Result:=Self;
end;

function TAnsi.Blinking: TAnsi;
begin
  SGR('5');
  Result := Self;
end;

function TAnsi.Bold: TAnsi;
begin
  SGR('1');
  Result := Self;
end;

function TAnsi.Faint: TAnsi;
begin
  SGR('2');
  Result := Self;
end;

function TAnsi.StrikeThrough: TAnsi;
begin
  SGR('9');
  Result := Self;
end;

function TAnsi.Emit(aClear : Boolean = True) : TAnsi;
begin
  Write(ToString); 
  flush(output);
  if aClear then
    Clear(False);
  Result:=Self;  
end;

function TAnsi.EmitLn(aClear : Boolean = True) : TAnsi;
begin
  WriteLn(ToString); 
  flush(output);
  if aClear then
    Clear(False);
  Result:=Self;
end;

Function TAnsi.Clear(aClearText : Boolean = False) : TAnsi;

begin
  FCmds:='';
  FSGR:='';
  if aClearText then
    FText:='';
  result:=Self;  
end;

end.
