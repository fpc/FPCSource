{
  unit generation tool
  (C) 2000 Alexander Stohr, alexs@freepage.de
  based upon the linux dynamic tool from Sebastian Guenther
  with latest version "1.1 1999/12/23 13:51:50 peter"
}

{$MODE objfpc}
{$H-}   { use normal strings }
(* do not enable! fpc bug with H+ *)

program c_gen;

uses
  SysUtils,
  Classes,
  buildgl;

// =====================================================================

type
  ptDefFile     = ^tDefFile;
  tDefFile      = record
                    Name        : String;
                    DefFile     : TDefReader;
                    pNext       : ptDefFile;
                  end;

  ptSectionKey  = ^tSectionKey;
  tSectionKey   = record
                    Keyword     : String;
                    Rule        : DWord;
                    pDefFile    : ptDefFile;
                    Option2     : String;
                    pNext       : ptSectionKey;
                  end;

// =====================================================================

const
  verbose       = 0;    // change this for debugging

const
  ST_NONE       = 0;
  ST_COMMON     = 1;
  ST_FILE       = 2;

  RULE_IG       = 0;
  RULE_TX       = 1;
  RULE_IF       = 2;
  RULE_PD       = 3;
  RULE_PL       = 4;
  RULE_PS       = 5;

// =====================================================================
// global vars

var
  ReturnVal     : Word;

  pSectionKey   : ptSectionKey;
  pAllDefFile   : ptDefFile;

  ToolName      : String;
  TargetText    : String;
  TargetDir     : String;

  SectionType   : DWord;
  SectionName   : String;

  TemplateName  : String;

// =====================================================================

procedure StripSpaces(var s : String);
var
  L : Byte;
begin
  // strip leading spaces
  while (Pos(' ',s)=1) or (Pos(#8,s)=1) do
    Delete(s,1,1);

  // strip trailing spaces
  L := Length(s);
  while L<>0 do
  begin
    if (s[L]=' ') or (s[L]=#8) then
    begin
      Delete(s,L,1);
      Dec(L);
    end
    else
      L := 0;
  end;
end;

function GetName(var s : String) : String;
var
  Name : String;
  P    : Byte;
begin
  Name := s;
  P := Pos(',',s);
  if p>0 then
  begin
    Delete(s,1,P);
    Delete(Name,P,255);
  end
  else
    s := '';

  StripSpaces(Name);

{  WriteLn('GetName, reminder = ',Name,',',s); }

  GetName := Name;
end;

function Name2Rule(Name : String) : DWord;
begin
  if Name='IG'
  then Name2Rule := RULE_IG
  else
  if Name='TX'
  then Name2Rule := RULE_TX
  else
  if Name='IF'
  then Name2Rule := RULE_IF
  else
  if Name='PD'
  then Name2Rule := RULE_PD
  else
  if Name='PL'
  then Name2Rule := RULE_PL
  else
  if Name='PS'
  then Name2Rule := RULE_PS
  else
  begin
    Name2Rule := RULE_IG;
    WriteLn('error - unknown rule: ',Name);
    ReturnVal := 1;
  end;
end;

function AddDefFile(Name : String) : ptDefFile;
var
  pDefFile : ptDefFile;
  pSearch  : ptDefFile;
begin
  pDefFile := NIL;

  // search if file is already loaded
  if pAllDefFile<>NIL then
  begin
    pSearch := pAllDefFile;
    while pSearch<>NIL do
    begin
      if pSearch^.Name = Name then
      begin
        pDefFile := pSearch;
        pSearch := NIL;
      end
      else
        pSearch := pSearch^.pNext;
    end;
  end;

  // create new file if its not loaded
  if pDefFile = NIL then
  begin
    New(pDefFile);

    pDefFile^.Name := Name;
    pDefFile^.DefFile := TDefReader.Create(Name);
    pDefFile^.pNext := pAllDefFile;     // chain in as first member

    pAllDefFile := pDefFile;
  end;

  AddDefFile := pDefFile;
end;

procedure AddSectionKey(s : string);
var
  pKey : ptSectionKey;
  t : string;
begin
  New(pKey);

  pKey^.Keyword  := GetName(s);
  pKey^.Rule     := Name2Rule(GetName(s));
  pKey^.pDefFile := AddDefFile(GetName(s));
  t := GetName(s);
  pKey^.Option2  := t;
  pKey^.pNext    := pSectionKey; // chain in as first member

  pSectionKey   := pKey;
end;

function GetSectionKey(s : string) : ptSectionKey;
var
  pSearch : ptSectionKey;
begin
  GetSectionKey := NIL;

  pSearch := pSectionKey;
  while pSearch<>NIL do
  begin
    if pSearch^.Keyword = s then
    begin
      GetSectionKey := pSearch;
      pSearch := NIL;
    end
    else pSearch := pSearch^.pNext;
  end;
end;

procedure FreeSectionKeys;
var
  pSearch, pNext : ptSectionKey;
begin
  pSearch := pSectionKey;
  while pSearch<>NIL do
  begin
    pNext := pSearch^.pNext;
    Dispose(pSearch);
    pSearch := pNext;
  end;
  pSectionKey := pSearch;
end;

// =====================================================================

procedure ResetCommonSecData;
begin
  ToolName   := 'BuildTool';
  TargetText := 'unknown';
  TargetDir  := '.\';
end;

procedure ResetFileSecData;
begin
  FreeSectionKeys;
  TemplateName := '';
end;

procedure InitGlobals;
begin
  ReturnVal := 0;

  SectionType := ST_NONE;
  pSectionKey := NIL;
  pAllDefFile := NIL;

  ResetCommonSecData;
  ResetFileSecData;
end;

// =====================================================================

procedure PrintInterface(var dest: Text; lines: TStringList);
var
  i: Integer;
begin
  for i := 0 to lines.Count - 1 do
    WriteLn(dest, lines.Strings[i]);
end;

procedure PrintProcDecls(var dest: Text; procs: TStringList; const Modifier : String);
var
  i, j: Integer;
  s: String;
begin
  for i := 0 to procs.Count - 1 do
  begin
    s := procs.Strings[i];
    j := Pos('//', s);
    if (Length(s) = 0)
    then
      WriteLn(dest)
    else
    if (Pos('{', s) = 1)
    then
      WriteLn(dest,procs.Strings[i])
    else
    if ((j > 0) and (Trim(s)[1] = '/')) then
      WriteLn(dest, s)
    else if j = 0 then
      WriteLn(dest, s, ' ',Modifier)
    else
      WriteLn(dest, TrimRight(Copy(s, 1, j-1)),
        ' ',Modifier,' ', Copy(s, j, Length(s)) );
  end;
end;

procedure PrintProcLoaders(var dest: Text; procs: TStringList; const libname: String);
var
  i, j: Integer;
  s: String;
begin
  for i := 0 to procs.Count - 1 do
  begin
    s := Trim(procs.Strings[i]);
    if (Pos('//', s) > 0)
    or (Pos('{', s) = 1)
    then
      WriteLn(dest,procs.Strings[i])
    else
    begin
      j := Pos(':', s);
      s := Trim(Copy(s, 1, j - 1));
      if (Length(s) = 0)
      then
        continue
      else
        WriteLn(dest, '  ', s, ' := GetProc(', libname, ', ''', s, ''');');
    end;
  end;
end;

procedure PrintProcStatic(var dest: Text; procs: TStringList; const Modifier: String);
var
  i, j, k: Integer;
  s: String;
  t: String;
begin
  for i := 0 to procs.Count - 1 do
  begin
    s := procs.Strings[i];
    j := Pos('//', s);
    if (Length(s) = 0) or ((j > 0) and (Trim(s)[1] = '/')) then
      WriteLn(dest, s)
    else
    begin
      // swap order of leading symbols and remove ':'
      t := Trim(procs.Strings[i]);
      j := Pos(':', t);
      t := Trim(Copy(t, 1, j - 1));

      j := Pos(':', s);
      Delete(s,1,j);
      s := Trim(s);

      j := Pos(';', s);
      k := Pos('(', s);
      if k>0 then if j>k then j := k;
      k := Pos(':', s);
      if k>0 then if j>k then j := k;

      Insert(t,s,j);
      Insert(' ',s,j);

      j := Pos('//', s);
      if j = 0 then
        WriteLn(dest, s, ' ',Modifier)
      else
        WriteLn(dest, TrimRight(Copy(s, 1, j-1)),
          ' ',Modifier,' ', Copy(s, j, Length(s)) );
    end;

  end;
end;

procedure PrintCVSLogSection(var dest: Text);
begin
  WriteLn(dest);
  WriteLn(dest);
  WriteLn(dest, '{');
  WriteLn(dest, '  $', 'Log:$');  // needed because _this_ file might be in CVS, too
  WriteLn(dest, '}');
end;

// =====================================================================

procedure ProcessFileSection;
var
  f             : Text;
  tpl           : Text;
  s             : String;
{  j             : Integer; }
  tmp           : String;
  pKey          : ptSectionKey;
begin
  WriteLn('Generating "',TargetDir+SectionName,'" ...');

  Assign(f, TargetDir+SectionName);
  Rewrite(f);

  Assign(tpl, TemplateName);
  Reset(tpl);

  while not EOF(tpl) do
  begin
    ReadLn(tpl, s);
    if Copy(s, 1, 1) = '%' then
    begin
      tmp := Copy(s,2,255);
      StripSpaces(tmp);

      pKey := GetSectionKey(tmp);

      if pKey=NIL then
      begin
        WriteLn(f, '// ### ',ToolName,': Don''t know what to insert here!: ', s);
        WriteLn('error - unknown keyword: ',tmp);
        ReturnVal := 1;
      end
      else
      begin
        case pKey^.Rule of
          RULE_IG : { ignore };
          RULE_TX : { todo };
          RULE_IF : PrintInterface(f, pKey^.pDefFile^.DefFile.InterfaceBlock);
          RULE_PD : PrintProcDecls(f, pKey^.pDefFile^.DefFile.Procs,
                      pKey^.Option2);
          RULE_PL : PrintProcLoaders(f, pKey^.pDefFile^.DefFile.Procs,
                      pKey^.Option2);
          RULE_PS : PrintProcStatic(f, pKey^.pDefFile^.DefFile.Procs,
                      pKey^.Option2);
        end;
      end;
    end
    else
    begin
      if Copy(s, 1, 1) <> '#'
      then WriteLn(f, s);
    end;
  end;
  PrintCVSLogSection(f);
  Close(f);

(*
    if Copy(s, 1, 1) <> '#' then
      begin
      j := Pos('#extdecl', s);
      if j = 0 then
        WriteLn(f, s)
      else
        WriteLn(f, Copy(s, 1, j - 1), 'cdecl', Copy(s, j + 8, Length(s)));
    end;
*)

end;

procedure ProcessCommonSection;
begin
  if verbose>0 then
  begin
    WriteLn('common section:');
    WriteLn('  ToolName   = ',ToolName);
    WriteLn('  TargetText = ',TargetText);
    WriteLn('  TargetDir  = ',TargetDir);
  end;
end;

// =====================================================================

procedure SectionComplete;
begin
  if ReturnVal=0 then { if we are error free }
  case SectionType of
    ST_NONE :
      begin
        // ignore
      end;
    ST_COMMON :
      begin
        ProcessCommonSection;
      end;
    ST_FILE :
      begin
        ProcessFileSection();
      end;
  end;
end;

var
  hFGen         : Text;
  Line          : String;
  KeyName       : String;
  KeyValue      : String;

begin
  InitGlobals;

  WriteLn('File Generator Tool for OpenGL related Units');

  if ParamCount<>1 then
  begin
    WriteLn('specify a generator file as parameter 1');
    Halt(1);
  end;

  // Open Generation File
  Assign(hFGen,ParamStr(1));
  Reset(hFGen);

  while Not(EOF(hFGen)) do
  begin
    ReadLn(hFGen,Line);
    if Length(Line)>0 then
    begin
      if Line[1]='[' then
      begin
        // its a new section
        SectionComplete;        // close previous section

        Delete(Line,Pos(']',Line),255);
        SectionName := Copy(Line,2,255);

        if verbose>0 then
          WriteLn('SectionName = ',SectionName);

        if SectionName='common' then
        begin
          SectionType := ST_COMMON;
          ResetCommonSecData;
        end
        else
        begin
          SectionType := ST_FILE;
          ResetFileSecData;
        end;
      end
      else
      if Pos(Line[1],'#*;''')<>0 then
      begin
        // just a comment - ignore
      end
      else
      begin
        // its a key in the section
        KeyName := Line;
        KeyValue := Line;

        Delete(KeyName,Pos('=',KeyName),255);
        Delete(KeyValue,1,Pos('=',KeyValue));

        StripSpaces(KeyName);
        StripSpaces(KeyValue);

//        WriteLn('KeyName  = ',KeyName);
//        WriteLn('KeyValue = ',KeyValue);

        case SectionType of
          ST_COMMON :
            begin
              if KeyName='TOOL_NAME'
              then ToolName := KeyValue
              else
              if KeyName='TARGET_TEXT'
              then TargetText := KeyValue
              else
              if KeyName='TARGET_DIR'
              then
              begin
                TargetDir := KeyValue;

              end
              else
              begin
                WriteLn('error in script file - inside common section');
                WriteLn('key line: ',Line);
                ReturnVal := 1;
              end;
            end;
          ST_FILE :
            begin
              if KeyName='TEMPLATE'
              then TemplateName := KeyValue
              else
              if KeyName='KEY'
              then AddSectionKey(KeyValue)
              else
              begin
                WriteLn('error in script file - inside file section');
                WriteLn('key line: ',Line);
                ReturnVal := 1;
              end;
            end;
          ELSE
            begin
              WriteLn('error in script file - not in a section');
              WriteLn('key line: ',Line);
              ReturnVal := 1;
            end;
        end;
      end
    end;
  end;

  SectionComplete;      // close last section

  Close(hFGen);

  WriteLn('Done...');

  Halt(ReturnVal);
end.
