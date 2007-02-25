{=================================================================
 TestReg.pp versión 1.2 12/02/2002
 Copyright (C) 2000-2002 by Luis Digital (luis@luis-digital.8m.com)

 TestReg pone en prueba y sirve de ejemplo para TRegistry.

 Este es software libre bajo la licencia GNU/GPL.
 Ver COPYING.FPC y COPYING incluidos con este programa.

 Este programa es distribuido esperando que sea útil,
 pero SIN NINGUNA GARANTIA.
 =================================================================}

Program TestReg;

{$mode objfpc} {$H+}

Uses registry, SysUtils;

Const
  SInteger    = 'Integer';
  SBinaryData = 'Binary';
  SBoolean    = 'Boolean';
  SCurrency   = 'Currency';
  SExString   = 'ExpandString';
  SDate       = 'Date';
  STime       = 'Time';
  SDateTime   = 'DateTime';
  SString     = 'String';
  SFloat      = 'Float';

  IntVal      = 321;
  FloatVal    = 123.456;
  CurrencyVal = 456;
  BoolVal     = False;
  StringVal   = 'This is a normal string';
  SExpandVal  = 'This is an expand string: "%SystemDir%"';

Var
  I: Integer;
  SubKey: String;
  B : Boolean;
  F: Double;
  C : Currency;
  S : String;
  BinData: Array [0..15] of Byte;
  D : TDateTime;
  DateVal,TimeVal,DateTimeVal : TDateTime;

Begin
  With TRegistry.Create do
    Try
      RootKey := HKEY_CURRENT_USER;
      SubKey := '\Software\FPC\testreg';
      CreateKey(SubKey);
      If Not OpenKey(SubKey,False) then
        Writeln('Could not open key: ',SubKey)
      else
        begin
        Writeln('Writing data');
        WriteInteger(SInteger, IntVal);
        For I:= 0 To 15 Do
          BinData[I] := I;
        WriteBinaryData(SBinaryData,BinData,SizeOf(BinData));
        WriteBool(SBoolean, BoolVal);
        WriteCurrency(SCurrency, CurrencyVal);
        WriteFloat(SFloat, FloatVal);
        WriteExpandString(SExString,SExpandVal);
        WriteString(SString,StringVal);
        DateVal:=Date;
        WriteDate(SDate, DateVal);
        TimeVal:=Time;
        WriteTime(STime, TimeVal);
        DateTimeVal:=Now;
        WriteDateTime(SDateTime, DateTimeVal);
        Writeln('Reading data');
        I:=ReadInteger(Sinteger);
        If (I<>IntVal) then
          Writeln('Read Integer differs: ',I);
        FillChar(BinData,SizeOf(Bindata),0);
        I:=GetDataSize(SBinaryData);
        If I<>16 then
          Writeln('Size Binary Data differs: ',I)
        else
          begin
          ReadBinaryData(SBinaryData, BinData,I);
          For I:=0 to 15 do
            If BinData[i]<>I then
              Write('Binary Data byte ',i,' differs : ',BinData[i]);
          end;
        B:=ReadBool(SBoolean);
        If (B<>BoolVal) then
          Writeln('Boolean value differs : ',B);
        C:=ReadCurrency(SCurrency);
        If (C<>CurrencyVal) then
           Writeln('Currency value differs: ', C);
        S:=ReadString(SString);
        If (S<>StringVal) then
          Writeln('Read String differs: "',S,'"(',Length(s),')<>"',StringVal,'"(',length(StringVal),')');
        D:=ReadDateTime(SDateTime);
        If (D<>DateTimeVal) then
          Writeln('Read DateTime differs : ',D);
        D:=ReadDate(SDate);
        If (D<>DateVal) then
          Writeln('Read Date differs : ',D);
        D:=ReadDateTime(STime);
        If (D<>TimeVal) then
          Writeln('Read Time differs : ',D);
        F:=ReadFloat(SFloat);
        If ((F-FloatVal)>1e-4) then
          Writeln('Read Float differs: ',F);
       If Not DeleteValue(SFloat) Then
         Writeln('Error: could not delete float value');
       CloseKey;
       SubKey:='\Software\fpc\testreg2';
       Createkey(SubKey);
       If Not DeleteKey(SubKey) Then
         Writeln('Error: could not delete key',subkey);
       end;
    Finally
      CloseKey;
      free;
    end;
End.
