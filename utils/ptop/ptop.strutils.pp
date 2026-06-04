unit ptop.strutils;
{$modeswitch result}
{$h+}

interface

uses
  {$ifdef FPC_DOTTEDUNITS}
  system.classes,
  {$else}
  classes,
  {$endif}
  ptop.types
  ;

const Blank = ' ';
var BeVerbose: boolean = false;

Procedure Strip(var S: string);
Function trimMiddle(a:ansistring; lnght: integer; size: integer):string;
Procedure ClassID(Value: Token;
                  lngth: INTEGER;
                  VAR idtype: keysymbol;
                  VAR IsKeyWord: BOOLEAN);
Function ReadChar (S : TStream) : Char;
Function EoSLn (S : TStream) : Char;
Function ReadString (S: TStream): String;
Procedure WriteString (S : TStream; ST : String);
Procedure WriteAnsiString (S : TStream; ST : AnsiString);
Procedure WriteCR (S: TStream);
Procedure WriteLnString (S : TStream; ST : String);
Procedure Verbose(message: string);
Procedure LogWithLocation(lineno, pos: uint32; message: string; actualLine: string);

implementation

uses
  {$ifdef FPC_DOTTEDUNITS}
  system.sysutils
  {$else}
  sysutils
  {$endif}
  ;

Procedure Strip (Var S : String);

Const WhiteSpace = [#32, #9, #10, #13];

Var I,J : Longint;

begin
  If length(s)=0 then exit;
  I:=1;
  While (S[I] in whitespace) and (I<Length(S)) do inc(i);
  J:=length(S);
  While (S[J] in whitespace) and (J>1) do dec(j);
  If I<=J then
    S:=Copy(S,i,j-i+1)
  else
    S:='';
end;

Procedure ClassID(Value: Token;
                  lngth: INTEGER;
                  VAR idtype: keysymbol;
                  VAR IsKeyWord: BOOLEAN);
  { Classify an identifier.  We are only interested
    in it if it is a keyword, so we use the hash table. }
  VAR
    Keyvalue: String[MAXKEYLENGTH];
    Sym : keysymbol;

  BEGIN
    IF lngth > MAXKEYLENGTH THEN BEGIN
      idtype := othersym;
      IsKeyWord := FALSE
    END
    ELSE
      BEGIN
      IsKeyWord := FALSE;
      KeyValue:= UpperCase(Value);
      sym:=endsym;
      While (Not IsKeyword) and (sym<=lastformatsym) DO
         begin
         iskeyword:=(KeyValue=Keyword[sym]);
         if not iskeyword then
           Sym:=Succ(sym);
         end;
      if IsKeyWord then
        idtype:=sym
      ELSE
        idtype := othersym;
      END
  END; { of ClassID }

{ ---------------------------------------------------------------------
    Stream handling routines
  ---------------------------------------------------------------------}
{$push}{$warn 5036 off} // C seems to be not initialized

Function ReadChar (S : TStream) : Char;
Var C : Char;

begin
  repeat
    if S.Position=S.Size then
      C:=#0
    else
      S.Read(C,1);
  Until (C<>#13);
  ReadChar:=C;
end;

Function EoSLn (S : TStream) : Char;

Const WhiteSpace = [' ', #9, #13 ];

Var C : Char;

begin
  Repeat
    if S.Position = S.Size then
      C:=#0
    else
      S.Read(C,1);
  Until (Not (C in WhiteSpace)) or ((C=#10));
  EoSln:=C;
end;

{$pop} // warning code 5036

Function ReadString (S: TStream): String;

Var
  I : Byte;
  Count : Integer;

begin
  Result:='';
  I:=0;
  Repeat
    If ((I+1)>Length(Result)) then
      SetLength(Result,Length(Result)+255);
    Count:=S.Read(Result[I+1],1);
    If Count>0 then
      Inc(I);
  until (Result[I]=#10) or (Count=0);
  If Result[i]=#10 Then Dec(I);
  If Result[I]=#13 then Dec(I);
  SetLength(Result,I);
end;

Procedure WriteString (S : TStream; ST : String); inline;
begin
  S.Write(St[1],length(St));
end;

Procedure WriteAnsiString (S : TStream; ST : AnsiString); inline;
begin
  S.Write(St[1],length(St));
end;

Procedure WriteCR (S: TStream); inline;
begin
  WriteString(S,LineEnding);
end;

Procedure WriteLnString (S : TStream; ST : String); inline;
begin
  WriteString(S,ST);
  WriteCR(S);
end;

Function trimMiddle ( a:ansistring; lnght: integer; size: integer):string;
var
    half:Integer;
begin
    if lnght > size then
    begin
      half := (size - 3) div 2;
      trimMiddle := copy(a,1,half) + '...' + copy(a,lnght-half+1,half);
    end
    else
      trimMiddle := a;
end;

Procedure Verbose(message: string); inline;
begin
  if BeVerbose then writeln(message);
end;

procedure LogWithLocation(lineno, pos: uint32; message: string; actualLine: string);
const
  lineHeader = '    | ';
var
  ln: string;
begin
  ln := IntToStr(lineno) + ':' + IntToStr(pos) + lineHeader;

  writeln('In your configuration file:');
  writeln(ln + actualLine);

  write(StringOfChar(' ', Length(ln) + pos - 1) + '^ ');
  writeln(message);
end;

end.
