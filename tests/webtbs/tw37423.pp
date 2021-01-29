program inline_bug;

{$MODE objfpc}

{ bug is only triggered when inlining is on }
{$INLINE on}

{

[nickysn@dhcppc1 inline_bug]$ fpc inline_bug
Free Pascal Compiler version 3.2.0 [2020/06/21] for x86_64
Copyright (c) 1993-2020 by Florian Klaempfl and others
Target OS: Linux for x86-64
Compiling inline_bug.pas
Linking inline_bug
/usr/bin/ld: inline_bug.o: in function `P$INLINE_BUG$_$TMODULE_$__$$_DOSTUFF':
inline_bug.pas:(.text.n_p$inline_bug$_$tmodule_$__$$_dostuff+0x59): undefined reference to `.Lj62'
inline_bug.pas(69,1) Error: Error while linking
inline_bug.pas(69,1) Fatal: There were 1 errors compiling module, stopping
Fatal: Compilation aborted
Error: /usr/bin/ppcx64 returned an error exitcode

}

type
  TSample = class
  private
    function GetContainsData: Boolean; inline;
  public
    property ContainsData: Boolean read GetContainsData;
  end;
  TSampleList = class
  private
    FSampleList: array [1..10] of TSample;
    function GetSample(Index: Integer): TSample; inline;
  public
    constructor Create;
    property Sample[Index: Integer]: TSample read GetSample; default;
  end;
  TModule = class
  private
    FSamples: TSampleList;
  public
    constructor Create;
    procedure DoStuff;
  end;

function TSample.GetContainsData: Boolean; inline;
begin
  Result := False;
end;

constructor TSampleList.Create;
var
  I: Integer;
begin
  for I := 1 to 10 do
    FSampleList[I] := TSample.Create;
end;

function TSampleList.GetSample(Index: Integer): TSample; inline;
begin
  if (Index < Low(FSampleList)) or (Index > High(FSampleList)) then
    raise TObject.Create;
  Result := FSampleList[Index];
end;

constructor TModule.Create;
begin
  FSamples := TSampleList.Create;
end;

procedure TModule.DoStuff;
var
  I: Integer;
begin
  for I := 1 to 10 do
    if FSamples[I].ContainsData then
      begin
        Writeln('!!!');
        halt(1);
      end;
end;

var
  Module: TModule;
begin
  Module := TModule.Create;
  Module.DoStuff;
end.
