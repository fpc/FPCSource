{
    $Id: unitdiff.pp,v 1.6 2005/05/06 19:31:36 florian Exp $

    UnitDiff Copyright (C) 2004 by the Free Pascal team

    Show differences between unit interfaces.

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


program MakeSkel;

uses
  SysUtils, Classes, Gettext,
  dGlobals, PasTree, PParser,PScanner;

resourcestring
  SIdentifiersIn = 'Identifiers in file "%s"';
  SCmdLineInvalidOption = 'Ignoring unknown option "%s"';
  SErrNoInputFile = 'No input file specified';
  SWarnAssumingList = 'Only one input file specified. Assuming --list option.';
  SExtraIdentifier = 'Extra identifier in file "%s" : Name: %s';
  SExtraTypedIdentifier = 'Extra identifier in file "%s" : Type %s, Name: %s';
  SIdenticalUnits = 'Unit interfaces are identical.';

type
  TCmdLineAction = (actionHelp, actionDiff,ActionList);

  TSkelEngine = class(TFPDocEngine)
  public
    FList: TStringList;
    Constructor Create;
    Destructor Destroy;override;
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility :TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement; override;
  end;

Constructor TSkelEngine.Create;
begin
  Inherited Create;
  FList:=TStringList.Create;
end;

Destructor TSkelEngine.Destroy;

begin
  FreeAndNil(FList);
  Inherited;
end;

const
  CmdLineAction: TCmdLineAction = actionDiff;
  OSTarget: String = {$I %FPCTARGETOS%};
  CPUTarget: String = {$I %FPCTARGETCPU%};

var
  InputFile1,
  InputFile2 : String;
  DocLang: String;
  Engine1,
  Engine2: TSkelEngine;
  SparseList,
  DisableArguments,
  DisableProtected,
  DisablePrivate,
  DisableFunctionResults: Boolean;

  OutputName: String;
  f: Text;


function TSkelEngine.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility : TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;

  Function ExamineThisNode(APasElement : TPasElement)  : Boolean;

  begin
    Result:=Assigned(AParent) and (Length(AName) > 0) and
            (not DisableArguments or (APasElement.ClassType <> TPasArgument)) and
            (not DisableFunctionResults or (APasElement.ClassType <> TPasResultElement)) and
            (not DisablePrivate or (AVisibility<>visPrivate)) and
            (not DisableProtected or (AVisibility<>visProtected));
  end;

begin
  Result := AClass.Create(AName, AParent);
  if AClass.InheritsFrom(TPasModule) then
    CurModule := TPasModule(Result)
  else if ExamineThisNode(Result) then
    Flist.AddObject(Result.FullName,Result);
end;


Procedure Usage;

begin
  Writeln('Usage : ',ExtractFileName(Paramstr(0)),' [options] file1 file2');
  Writeln('Where [options] is one or more of :');
  Writeln(' --disable-arguments Do not check function arguments.');
  Writeln(' --disable-private   Do not check class private fields.');
  Writeln(' --disable-protected Do not check class protected fields.');
  Writeln(' --help              Emit help.');
  Writeln(' --input=cmdline     Input file to create skeleton for.');
  Writeln('                     Use options are as for compiler.');
  Writeln(' --lang=language     Use selected language.');
  Writeln(' --list              List identifiers instead of making a diff');
  Writeln(' --output=filename   Send output to file.');
  Writeln(' --sparse            Sparse list/diff (skip type identification)');
end;

procedure ParseOption(const s: String);

var
  i: Integer;
  Cmd, Arg: String;
begin
  if (s = '-h') or (s = '--help') then
    CmdLineAction := actionHelp
  else if s = '--disable-arguments' then
    DisableArguments := True
  else if s = '--disable-private' then
    DisablePrivate := True
  else if s = '--sparse' then
    SparseList := True
  else if s = '--disable-protected' then
    begin
    DisableProtected := True;
    DisablePrivate :=True;
    end
  else
    begin
    i := Pos('=', s);
    if i > 0 then
      begin
      Cmd := Copy(s, 1, i - 1);
      Arg := Copy(s, i + 1, Length(s));
      end
    else
      begin
      Cmd := s;
      SetLength(Arg, 0);
      end;
    if (Cmd = '-l') or (Cmd = '--lang') then
      DocLang := Arg
    else if (Cmd = '-o') or (Cmd = '--output') then
      OutputName := Arg
    else
      if (length(cmd)>0) and (cmd[1]='-') then
         WriteLn(StdErr, Format(SCmdLineInvalidOption, [s]))
      else if (InputFile1='') then
        InputFile1:=Cmd
      else if (InputFile2='') then
        InputFile2:=Cmd
      else
        WriteLn(StdErr, Format(SCmdLineInvalidOption, [s]));
  end;
end;

procedure ParseCommandLine;

Const
{$IFDEF Unix}
  MoFileTemplate = '/usr/local/share/locale/%s/LC_MESSAGES/makeskel.mo';
{$ELSE}
  MoFileTemplate ='intl/makeskel.%s.mo';
{$ENDIF}

var
  MOFilename: string;
  i: Integer;
begin
  CmdLineAction := actionDiff;
  DocLang:='';
  SparseList:=False;
  for i := 1 to ParamCount do
    ParseOption(ParamStr(i));
  If (DocLang<>'') then
    begin
    MOFilename:=Format(MOFileTemplate,[DocLang]);
    if FileExists(MOFilename) then
      gettext.TranslateResourceStrings(MoFileName)
    else
      writeln('NOTE: unable to find tranlation file ',MOFilename);
    // Translate internal documentation strings
    TranslateDocStrings(DocLang);
    end;
  if (cmdLineAction<>ActionHelp) and (InputFile1='') and (InputFile2='') then
  begin
    Writeln(StdErr,SErrNoInputFile);
    cmdLineAction := actionHelp;
  end else if (InputFile2='') and (CmdLineAction<>ActionList) then
    begin
    Writeln(StdErr,SWarnAssumingList);
    CmdLineAction:=ActionList;
    end;
end;

Function GetTypeDescription(El : TPasElement) : String;

begin
  If Assigned(El) then
    Result:=El.ElementTypeName
  else
    Result:='(unknown)';
end;

Procedure ListIdentifiers(Fn : String; List : TStrings);

Var
  I : Integer;

begin
  Writeln(f,Format(SIdentifiersIn,[FN]));
  For I:=0 to List.Count-1 do
    begin
    If Not SparseList then
      Write(GetTypeDescription(TPasElement(List.Objects[i])),' : ');
    Writeln(List[i]);
    end;
end;

Procedure WriteExtra(FN,Id : String; El: TPaselement);

begin
  If SparseList then
     Writeln(F,Format(SExtraIdentifier,[FN,ID]))
  else
     Writeln(F,Format(SExtraTypedIdentifier,[FN,GetTypeDescription(El),ID]));
end;

Procedure DoExtra(FN : String; L : TStrings);

Var
  I,Len : Integer;
  S : String;

begin
  I:=0;
  While (I<L.Count) do
    begin
    WriteExtra(FN,L[I],TPasElement(L.Objects[I]));
    // Delete possible subelements.
    S:=L[I]+'.';
    Len:=Length(S);
    While (I+1<L.Count) and (CompareText(Copy(L[I+1],1,Len),S)=0) do
      L.Delete(I+1);
    Inc(I);
    end;
end;

Procedure DiffIdentifiers(List1,List2 : TStrings);

Var
  L1,L2 : TStrings;
  I,J : Integer;

begin
  L1:=List1;
  L2:=List2;
  If List2.Count>List1.Count then
    begin
    L1:=List2;
    L2:=List1;
    end;
  // Remove all common elements.
  For I:=L1.Count-1 downto 0 do
    begin
    J:=L2.IndexOf(L1[i]);
    If (J<>-1) then
      begin
      L1.Delete(I);
      L2.Delete(J);
      end;
    end;
  If (List1.Count=0) and (List2.Count=0) then
    Writeln(F,SIdenticalUnits)
  else
    begin
    DoExtra(InputFile1,List1);
    DoExtra(InputFile2,List2);
    end;
end;


begin
  ParseCommandLine;
  if CmdLineAction = actionHelp then
    Usage
  else
    begin
    Assign(f, OutputName);
    Rewrite(f);
    Try
      Engine1:=TSkelEngine.Create;
      Try
        try
          Engine1.SetPackageName('diff'); // do not localize
          ParseSource(Engine1, InputFile1, OSTarget, CPUTarget);
          Engine1.FList.Sorted:=True;
          if (InputFile2<>'') then
            begin
              Engine2:=TSkelEngine.Create;
              Try
                Engine2.SetPackageName('diff'); // do not localize
                ParseSource(Engine2, InputFile2, OSTarget, CPUTarget);
                Engine2.FList.Sorted:=True;
                If cmdLineAction=ActionList then
                  begin
                  ListIdentifiers(InputFile1,Engine1.FList);
                  ListIdentifiers(InputFile2,Engine2.FList);
                  end
                else
                  DiffIdentifiers(Engine1.Flist,Engine2.Flist);
              finally
                Engine2.Free;
              end;
            end
          else
            ListIdentifiers(InputFile1,Engine1.FList);
        except
          on e: eparsererror do
            writeln(format('%s(%d,%d): Error: %s',[e.Filename,e.Row,e.Column,e.Message]));
        end;
      Finally
        Engine1.Free;
      end;
    Finally
      Close(f);
    end;
    end;
end.


{
  $Log: unitdiff.pp,v $
  Revision 1.6  2005/05/06 19:31:36  florian
    * better error reporting

  Revision 1.5  2005/02/14 17:13:39  peter
    * truncate log

  Revision 1.4  2005/01/01 19:56:29  armin
  * fixed access violation without file on command line

  Revision 1.3  2004/11/15 18:03:28  michael
  + Faster inserts by sorting after all elements were parsed (suggestion by Mattias Gaertner)

  Revision 1.2  2004/11/14 21:20:31  michael
  + Changed copyright

  Revision 1.1  2004/11/14 21:18:58  michael
  + Initial check-in

  Revision 1.13  2004/09/13 16:04:52  peter
    * fix nested for-loop with same index

  Revision 1.12  2004/08/29 15:32:41  michael
  + More intelligent handling of nodes. Do not write unused nodes.

  Revision 1.11  2004/08/28 18:18:59  michael
  + Do not write descr nodes for module when updating

  Revision 1.10  2004/08/28 18:15:14  michael
  + Check whether outputfile not in inputfilenames

  Revision 1.9  2004/08/28 18:04:06  michael
  + Added update mode

  Revision 1.8  2004/08/25 07:16:43  michael
  + Improved translation handling

  Revision 1.7  2004/08/24 14:48:25  michael
  + Translate now called correctly...

  Revision 1.6  2004/05/01 20:13:40  marco
   * got fed up with exceptions on file not found.  Fileresolver now raises a
        EFileNotFound error, and makeskel catches and exists gracefully

  Revision 1.5  2003/11/28 12:51:37  sg
  * Added support for source references

  Revision 1.4  2003/09/02 13:26:47  mattias
  MG: makeskel now ignores missing translation file

  Revision 1.3  2003/05/07 16:31:32  sg
  * Fixed a severe memory corruption problem on termination

  Revision 1.2  2003/03/28 13:01:36  michael
  + Patch from Charlie/iNQ to work with new scanner/parser

  Revision 1.1  2003/03/17 23:03:20  michael
  + Initial import in CVS
}
