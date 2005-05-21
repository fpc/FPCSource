program bin2obj;
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt, member of the
    Free Pascal development team

    Binary file to include file converter.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}

uses classes,getopts, iostream,zstream,idea,sysutils,dos;

var
  ConstName,
  OutFileName,
  UnitName : String;
  WriteAsciiData,
  CompressData,
  EnCodeData,
  CompileUnit : Boolean;
  Cryptkey : IDEAcryptKey;
  InStream,
  MemStream,
  CryptStream,
  CompStream : TStream;

Procedure Usage;

begin
  Writeln ('Usage: bin2obj [options] -c constname [infile] ');
  Writeln ('Where options is a combination of : ');
  Writeln ('   -a        write asciii data instead of bytes');
  Writeln ('   -z        compress data.');
  Writeln ('   -e key    encrypt data with key (must have 8 characters)');
  Writeln ('   -o        output filename');
  Writeln ('   -u [name] make a unit instead of an include file (unit name is outfile)');
  Writeln ('   -U [name] same as -u, and compile the unit. (requires outfile)');
  Halt(1);
end;

Procedure ProcessCommandLine;

Var C : Char;
    I : longint;
    NeedUnitName : Boolean;

begin
  OptErr:=False;
  ConstName:='';
  CompressData:=False;
  EncodeData:=False;
  CompileUnit:=False;
  UnitName:='';
  NeedUnitName:=False;
  WriteAsciiData:=False;
  Repeat
    c:=GetOpt('ac:e:o:zhu::U::');
    Case C of
      'a' : WriteAsciiData:=True;
      'c' : ConstName:=OptArg;
      'h','?' : usage;
      'z' : CompressData := True;
      'e' : begin
            EncodeData:=True;
            If Length(OptArg)<8 then
              Usage;
            For I:=0 to 7 do
              CryptKey[i]:=Ord(OptArg[I+1]);
            end;
      'o' : OutFileName:=optArg;
      'u','U':
            begin
            UnitName:=OptArg;
            If Length(UnitName)=0 then
              NeedUnitName:=True;
            If C='U' then
              CompileUnit:=True;
            end;
    end;
  until C=EndOfOptions;
  if ConstName='' then
    usage;
  If NeedUnitName then
    If Length (OutFileName)=0 then
      begin
      Writeln ('Error : cannot determine unitname from filename');
      Usage;
      end
    else
      UnitName:=ExtractFileName(OutFileName);
  if CompileUnit and (Length(OutFileName)=0) then
    usage;
end;

Function SetupInput : TStream;

begin
  if OptInd=ParamCount then
    InStream:=TFileStream.Create(Paramstr(Optind),fmOpenRead)
  else
    InStream:=TIOStream(iosInput);
  Result:=InStream;
end;

Function SetupOutput : TStream;

Var Key : ideaKey;

begin
  MemStream:=TMemoryStream.Create;
  Result:=MemStream;
  If ComPressData then
    begin
    CompStream:=TCompressionStream.Create(cldefault,Result);
    Result:=CompStream;
    end;
  if EncodeData Then
    begin
    EnKeyIdea(CryptKey,Key);
    CryptStream:=TIDEAEncryptStream.Create(Key,Result);
    Result:=CryptStream;
    end;
end;

Procedure CopyStreams (Ins,Outs : TStream);

Const BufSize = 1024;

Var Buffer : Array[1..BufSize] of byte;
    Count : longint;

begin
  repeat
     Count:=Ins.Read(Buffer,SizeOf(Buffer));
      If Count>0 then
       Outs.Write(Buffer,Count);
  until Count<SizeOf(Buffer);
  {
    freeing these streams will flush their buffers.
    Order is important !!!
  }
  CryptStream.Free;
  CompStream.Free;
  // Now Out stream has all data.
end;

Procedure WriteMemStream;

Var OutStream : TStream;

  Procedure WriteStr(Const St : String);

  begin
    OutStream.Write(St[1],Length(St));
  end;

  Procedure WriteStrLn(Const St : String);

  Const
  {$ifdef unix}
     Eoln : String = #10;
  {$else}
     Eoln : String = #13#10;
  {$endif}

  begin
    OutStream.Write(St[1],Length(St));
    OutStream.Write(Eoln[1],Length(Eoln));
  end;

Const Prefix = '     ';
      MaxLineLength = 72;

Var I,Count  : longint;
    b    : byte;
    Line,ToAdd : String;

begin
  If Length(OutFileName)=0 Then
    OutStream:=TIOStream.Create(iosOutput)
  else
    OutStream:=TFileStream.Create(OutFileName,fmCreate);
  If UnitName<>'' then
    begin
    WriteStrLn(Format('Unit %s;',[UnitName]));
    WriteStrLn('');
    WriteStrLn('Interface');
    WriteStrLn('');
    end;
  WriteStrLn('');
  WriteStrLn('Const');
  MemStream.Seek(0,soFromBeginning);
  Count:=MemStream.Size;
  If WriteAsciidata then
    WriteStrLn(Format('  %s : Array[0..%d] of char = (',[ConstName,Count-1]))
  else
    WriteStrLn(Format('  %s : Array[0..%d] of byte = (',[ConstName,Count-1]));
  Line:=Prefix;
  For I:=1 to Count do
    begin
    MemStream.Read(B,1);
    If Not WriteAsciiData then
       ToAdd:=Format('%3d',[b])
    else
      If (B in [32..127]) and not (B in [10,13,39]) then
        ToAdd:=''''+Chr(b)+''''
      else
//        ToAdd:=Format('''%s''',[Chr(b)]);
        ToAdd:=Format('#%d',[B]);
    If I<Count then
      ToAdd:=ToAdd+',';
    Line:=Line+ToAdd;
    If Length(Line)>=MaxLineLength Then
      begin
      WriteStrLn(Line);
      Line:=PreFix;
      end;
    end;
  WriteStrln(Line+');');
  If Length(UnitName)<>0 then
    begin
    WriteStrLn('');
    WriteStrLn('Implementation');
    WriteStrln('');
    WriteStrLn('end.')
    end;
  MemStream.Free;
end;

Procedure CompileTheUNit;

begin
  Exec('ppc386',' -Un '+UnitName);
end;

begin
  ProcessCommandline;
  CopyStreams(SetupInput,SetupOutPut);
  WriteMemStream;
  If CompileUNit then
    CompileTheUnit;
end.
