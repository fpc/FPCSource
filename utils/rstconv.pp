{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Sebastian Guenther
    Added OS/2 support in 2002 by Yuri Prokushev

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

resourcestring
  help =
    'rstconv [-h|--help]    Displays this help'+LineEnding+
    'rstconv options        Convert rst file'+LineEnding+LineEnding+
    'Options are:'+LineEnding+
    '  -i file        Use specified file instead of stdin as input .rst (OPTIONAL)'+LineEnding+
    '  -o file        Write output to specified file (REQUIRED)'+LineEnding+
    '  -f format      Specifies the output format:'+LineEnding+
    '                 po    GNU gettext .po (portable) format (DEFAULT)'+LineEnding+
    '                 msg   IBM OS/2 MSG file format'+LineEnding+LineEnding+
    'OS/2 MSG file only options are:'+LineEnding+
    '  -c identifier  Specifies the component identifier (REQUIRED).'+LineEnding+
    '                 Identifier is any three chars in upper case.'+LineEnding+
    '  -n number      Specifies the first message number [1-9999] (OPTIONAL).'+LineEnding;


  InvalidOption = 'Invalid option - ';
  RequiredOption = 'Required option is absent - ';
  OptionAlreadySpecified = 'Option has already been specified - ';
  NoOutFilename = 'No output filename specified';
  InvalidOutputFormat = 'Invalid output format -';
  MessageNumberTooBig = 'Message number too big';
  InvalidRange = 'Invalid range of the first message number';


type

  TConstItem = class(TCollectionItem)
  public
    ModuleName, ConstName, Value: String;
  end;

var
  InFilename, OutFilename: String;
  ConstItems: TCollection;
  Identifier: String[3];
  FirstMessage: Word;

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
{$IFNDEF UNIX}        
        #13: ;
        #1..#8, #11..#12, #14..#31, #128..#255:
{$ELSE}
        #1..#8, #11..#31, #128..#255:
{$ENDIF}
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

procedure ConvertToOS2MSG;
var
  i, j: Integer;
  f: Text;
  item: TConstItem;
  s: String;
begin
  If (ConstItems.Count+FirstMessage-1)>9999 then
  begin
    WriteLn(MessageNumberTooBig);
    Halt(1);
  end;
  
  Assign(f, OutFilename);
  Rewrite(f);
  
  WriteLn(f, Identifier);
  
  // Fake entry, because MKMSGF limitation
  WriteLn(f, Format('%s%.4d?: ',[Identifier, FirstMessage-1]));
  
  for i := 0 to ConstItems.Count - 1 do begin
    item := TConstItem(ConstItems.items[i]);
    
    // Prepare comment string
    // Convert string to C-style syntax
    s := '';
    j:=1;
    while j<=Length(item.Value) do
    begin
      if copy(item.Value, j, 2)=#13#10 then
      begin
        s:=s+#13#10';';
        Inc(j, 2);
      end else begin 
        s := s + item.Value[j];
        Inc(j);
      end;
    end;

    // Write msg entry
    WriteLn(f, ';', item.ModuleName, '.', item.ConstName);
    WriteLn(f, Format(';%s%.4d: %s %%0',[Identifier, i+FirstMessage, s]));
    WriteLn(f, Format('%s%.4dP: %s %%0',[Identifier, i+FirstMessage, Item.Value]));
  end;

  Close(f);
end;


type
  TConversionProc = procedure;
var
  i: Integer;
  ConversionProc: TConversionProc;
  OutputFormat: String;
begin

  if (ParamStr(1) = '-h') or (ParamStr(1) = '--help') then begin
    WriteLn(help);
    exit;
  end;

  ConversionProc := @ConvertToGettextPO;
  OutputFormat:='';
  Identifier:='';
  FirstMessage:=0;

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
      if OutputFormat <> '' then begin
        WriteLn(StdErr, OptionAlreadySpecified, '-f');
        Halt(1);
      end;
      if ParamStr(i + 1) = 'po' then
        OutputFormat:='po'
      else if ParamStr(i + 1) = 'msg' then begin
        OutputFormat:='msg';
        ConversionProc := @ConvertToOS2MSG;
      end else begin
        WriteLn(StdErr, InvalidOutputFormat, ParamStr(i + 1));
        Halt(1);
      end;
      Inc(i, 2);
    end else if ParamStr(i) = '-c' then begin
      if Identifier <> '' then begin
        WriteLn(StdErr, OptionAlreadySpecified, '-c');
        Halt(1);
      end;
      Identifier:=Copy(UpperCase(ParamStr(i+1)), 1, 3);
      Inc(i, 2);
    end else if ParamStr(i) = '-n' then begin
      if FirstMessage <> 0 then begin
        WriteLn(StdErr, OptionAlreadySpecified, '-n');
        Halt(1);
      end;
      try
        FirstMessage := StrToInt(ParamStr(i + 1));
        If (FirstMessage<1) or (FirstMessage>9999) then raise EConvertError.Create(InvalidRange+' '+ParamStr(i+1));
      except
        on EConvertError do
        begin
          WriteLn(StdErr, InvalidOption, ParamStr(i));
          Halt(1);
        end;
      end;
      Inc(i, 2);
    end else begin
      WriteLn(StdErr, InvalidOption, ParamStr(i));
      Halt(1);
    end;
  end;
  
  If (OutputFormat<>'msg') and ((Identifier<>'') or (FirstMessage<>0)) then begin
    WriteLn(StdErr, InvalidOption, '');
    Halt(1);
  end;

  If (OutputFormat='msg') and (Identifier='') then begin
    WriteLn(StdErr, RequiredOption, '-c');
    Halt(1);
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
  Revision 1.4  2002-09-22 10:58:25  hajny
    + support of IBM MSG files added by Yuri Prokushev

  Revision 1.3  2002/09/07 15:40:31  peter
    * old logs removed and tabs fixed

}
