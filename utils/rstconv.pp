{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Sebastian Guenther

    .rst resource string table file converter.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$MODE objfpc}
{$H+}

program rstconv;

uses sysutils, classes;

const
  help =
    'rstconv [-h|--help]    Displays this help'#10+
    'rstconv options        Convert rst file'#10#10+
    'Options are:'#10+
    '  -i file    Use specified file instead of stdin as input .rst (OPTIONAL)'#10+
    '  -o file    Write output to specified file (REQUIRED)'#10+
    '  -f format  Specifies the output format:'#10+
    '             po    GNU gettext .po (portable) format (DEFAULT)'#10;

  InvalidOption = 'Invalid option - ';
  OptionAlreadySpecified = 'Option has already been specified - ';
  NoOutFilename = 'No output filename specified';
  InvalidOutputFormat = 'Invalid output format -';


type

  TConstItem = class(TCollectionItem)
  public
    ModuleName, ConstName, Value: String;
  end;

var
  InFilename, OutFilename: String;
  ConstItems: TCollection;


procedure ReadRSTFile;
var
  f: Text;
  s: String;
  item: TConstItem;
  DotPos, EqPos, i, j: Integer;
begin
  Assign(f, InFilename);
  Reset(f);

  while not eof(f) do begin
    ReadLn(f, s);
    If (Length(S)=0) or (S[1]='#') then
      continue;
    item := TConstItem(ConstItems.Add);

    DotPos := Pos('.', s);
    EqPos := Pos('=', s);
    if DotPos > EqPos then // paranoia checking.
      DotPos := 0;
    item.ModuleName := Copy(s, 1, DotPos - 1);
    item.ConstName := Copy(s, DotPos + 1, EqPos - DotPos - 1);

    item.Value := '';
    i := EqPos + 1;
    while i <= Length(s) do begin
      if s[i] = '''' then begin
        Inc(i);
        j := i;
        while (i <= Length(s)) and (s[i] <> '''') do
          Inc(i);
        item.Value := item.Value + Copy(s, j, i - j);
        Inc(i);
      end else if s[i] = '#' then begin
        Inc(i);
        j := i;
        while (i <= Length(s)) and (s[i] in ['0'..'9']) do
          Inc(i);
        item.Value := item.Value + Chr(StrToInt(Copy(s, j, i - j)));
      end else if s[i] = '+' then begin
        ReadLn(f, s);
        i := 1;
      end else
        Inc(i);
    end;
  end;

  Close(f);
end;


procedure ConvertToGettextPO;
var
  i, j: Integer;
  f: Text;
  item: TConstItem;
  s: String;
  c: Char;

begin
  Assign(f, OutFilename);
  Rewrite(f);

  for i := 0 to ConstItems.Count - 1 do begin
    item := TConstItem(ConstItems.items[i]);

    // Convert string to C-style syntax
    s := '';
    for j := 1 to Length(item.Value) do begin
      c := item.Value[j];
      case c of
        #9:  s := s + '\t';
        #10: s := s + '\n';
        #1..#8, #11..#31, #128..#255:
          s := s + '\' +
            Chr(Ord(c) shr 6 + 48) +
            Chr((Ord(c) shr 3) and 7 + 48) +
            Chr(Ord(c) and 7 + 48);
        '\': s := s + '\\';
        '"': s := s + '\"';
      else s := s + c;
      end;
    end;

    // Write msg entry
    WriteLn(f, '#: ', item.ModuleName, ':', item.ConstName);
    WriteLn(f, 'msgid "', s, '"');
    WriteLn(f, 'msgstr ""');
    WriteLn(f);
  end;

  Close(f);
end;



type
  TConversionProc = procedure;
var
  i: Integer;
  ConversionProc: TConversionProc;

begin

  if (ParamStr(1) = '-h') or (ParamStr(1) = '--help') then begin
    WriteLn(help);
    exit;
  end;

  ConversionProc := @ConvertToGettextPO;

  i := 1;
  while i <= ParamCount do begin
    if ParamStr(i) = '-i' then begin
      if InFilename <> '' then begin
        WriteLn(StdErr, OptionAlreadySpecified, '-i');
        Halt(1);
      end;
      InFilename := ParamStr(i + 1);
      Inc(i, 2);
    end else if ParamStr(i) = '-o' then begin
      if OutFilename <> '' then begin
        WriteLn(StdErr, OptionAlreadySpecified, '-o');
        Halt(1);
      end;
      OutFilename := ParamStr(i + 1);
      Inc(i, 2);
    end else if ParamStr(i) = '-f' then begin
      if OutFilename <> '' then begin
        WriteLn(StdErr, OptionAlreadySpecified, '-f');
        Halt(1);
      end;
      if ParamStr(i + 1) = 'po' then
      else begin
        WriteLn(StdErr, InvalidOutputFormat, ParamStr(i + 1));
        Halt(1);
      end;
      Inc(i, 2);
    end else begin
      WriteLn(StdErr, InvalidOption, ParamStr(i));
      Halt(1);
    end;
  end;

  if OutFilename = '' then begin
    WriteLn(StdErr, NoOutFilename);
    Halt(1);
  end;

  ConstItems := TCollection.Create(TConstItem);

  ReadRSTFile;

  ConversionProc;
end.


{
  $Log$
  Revision 1.3  2002-09-07 15:40:31  peter
    * old logs removed and tabs fixed

}
