{ %VERSION=1.1 }
Program widetest;

uses
  erroru;

{ -------------------------------------------------------------------
    General stuff
  ------------------------------------------------------------------- }


Procedure DoRef (P : Pointer);

Type PLongint = ^Longint;

begin
  If P=Nil then
    Writeln ('(Ref : Empty string)')
  else
    begin
{$ifdef fpc}
  {$if defined(ver1_0) or defined(ver1_9_4)}
      Writeln (' (Ref: ',Plongint(Longint(P)-4)^,',Len: ',PLongint(Longint(P)-8)^,')');
  {$else}
      Writeln (' (Ref: ',PSizeInt(PtrInt(P)-sizeof(SizeInt)*2)^,',Len: ',PSizeInt(PtrInt(P)-sizeof(SizeInt))^,')');
  {$endif}
{$else}
      Writeln (' (Ref: ',Plongint(Longint(P)-8)^,',Len: ',PLongint(Longint(P)-4)^ div sizeof(WideChar),')');
{$endif}
    end;
end;

{ -------------------------------------------------------------------
    Initialize/Finalize test
  ------------------------------------------------------------------- }


Procedure TestInitFinal;

Type ARec = record
       FirstName, LastName : WideString;
       end;
     AnArray = Array [1..10] of WideString;


Var
    S   : WideString;
    AR  : Arec;
    AAR : AnArray;
    I   : longint;

Begin
  S:='This is an WideString!';
  If Pointer(AR.FirstNAme)<>Nil then
    Writeln ('AR.FirstName not OK');
  If Pointer(AR.LastName)<>Nil then
    Writeln ('AR.LastName not OK');
  for I:=1 to 10 do
    If Pointer(AAR[I])<>Nil then
      Writeln ('Array (',I,') NOT ok');
  AR.FirstName:='Napoleon';
  AR.LastName:='Bonaparte';
  For I:=1 to 10 do
    AAR[I]:='Yet another WideString';
  Writeln ('S : ',S);
  Writeln (AR.FirstName, ' ', AR.LastName);
  For I:=1 to 10 do
    Writeln (I:2,' : ',AAR[i]);
end;

{ -------------------------------------------------------------------
    Parameter passing test
  ------------------------------------------------------------------- }


Procedure TestVarParam (Var Sv : WideString);

Var LS : WideString;

begin
  Write ('TestVarParam : Got S="',Sv,'"');
  DoRef(Pointer(Sv));
  Sv:='This is a var parameter WideString';
  Write ('S Changed to : ',Sv);
  DoRef (Pointer(Sv));
  Ls:=Sv;
  Write ('Assigned to local var: "',ls,'"');
  DoRef (Pointer(Sv));
end;

Procedure TestValParam (S : WideString);

Var LS : WideString;

begin
  Write ('TestValParam : Got S="',S,'"');
  S:='This is a value parameter WideString';
  Write ('S Changed to : ',S);
  DoRef(Pointer(S));
  Ls:=S;
  Write ('Assigned to local var: "',ls,'"');
  DoRef(Pointer(S));
end;

Procedure TestConstParam (Const Sc : WideString);

Var LS : WideString;

begin
  Write ('TestConstParam : Got S="',Sc,'"');
  DoRef(Pointer(Sc));
  Ls:=Sc;
  Write ('Assigned to local var: "',ls,'"');
  DoRef(Pointer(Sc));
end;

Procedure TestParams;

Var S : WideString;
    Mem : PtrInt;

begin
  Mem:=0;
  DoMem(Mem);
  S :='This is another WideString';
  Writeln ('Calling testvalparam with "',s,'"');
  testvalparam (s);
  DoMem(Mem);
  Writeln ('Calling testConstparam with "',s,'"');
  testconstparam (s);
  DoMem(Mem);
  Writeln ('Calling testvarparam with "',s,'"');
  testvarparam (s);
  Writeln ('TestVarParam returned with "',S,'"');
  DoMem(Mem);
end;

{ -------------------------------------------------------------------
    Comparision operators test
  ------------------------------------------------------------------- }

Procedure TestCompare;

Const S1 : WideString = 'Teststring 1';
      S2 : WideString = 'Teststring 1';
      S3 : WideString = 'Teststring 2';
      S4 : WideString = '';
      PC : Pchar = 'Teststring 1';

Var S,T : WideString;
    ss : Shortstring;

begin
  If S1=S2 then writeln ('S1 and S2 are the same');
  If S4='' then Writeln ('S4 is empty. OK');
  If Not(S4='Non-empty') then writeln ('S4 is not  non-empty');
  if S3='Teststring 2' then writeln('S3 equals "Teststring 2". OK.');
  Write ('S3<>S2 : ');
  If S2<>S3 Then writeln ('OK') else writeln ('NOT OK');
  Write ('S3>S2  : ');
  If (S3>S2) Then Writeln ('OK') else writeln ('NOT OK');
  Write ('S1<S3  : ');
  if (S1<S3) Then writeln ('OK') else writeln ('NOT OK');
  S:=S2;
  T:=S;
  Write ('Assigned S to T. ');Doref(Pointer(T));
  If S=T then Writeln ('S=T, OK');
  SS:='Teststring 1';
  If SS=S then
    Writeln ('Shortstring and WideString are the same. OK')
 else
    Writeln ('Shortstring and WideString NOT equal. PROBLEM !');
  If S=PC then
    Writeln ('Pchar and WideString are the same. OK')
 else
    Writeln ('Pchar and WideString NOT equal. PROBLEM !');
end;

{ -------------------------------------------------------------------
    Type conversion test
  ------------------------------------------------------------------- }

Procedure DoPchar (P : Pchar);

begin
  Writeln ('DoPchar : Got : "',P,'"');
end;


Procedure TestConversion;

Var Pstr : Pchar;
    Sstr : String[40];
    Astr : WideString;

Const PC : Pchar = 'A PCHAR constant string';

begin
  Writeln ('Astr empty : "',Astr,'"');
  Pstr:=PChar(Astr);
  Writeln ('WideString Assigned to Pchar : "',Pstr,'"');
  DoPchar(Pchar(Astr));
  Astr:='An WideString';
  Writeln ('Astr: "',Astr,'"');
  Pstr:=PChar(Astr);
  Writeln ('WideString Assigned to Pchar : "',Pstr,'"');
  DoPchar(Pchar(Astr));
  SStr:='A ShortString';
  Writeln ('Shortstring : "',Sstr,'"');
  Astr:=Sstr;
  Write ('ShortString assigned to WideString : "',Astr,'"');
  DoRef(Pointer(Astr));
  Astr:=PC;
  Write ('PChar assigned to WideString : "',Astr,'"');
  DoRef(Pointer(Astr));
end;

{ -------------------------------------------------------------------
    Adding of strings test.
  ------------------------------------------------------------------- }

Procedure TestAdd;

Const S1 : WideString = 'This is WideString 1 ';
      S2 : WideString = 'This is WideString 2 ';
      S3 : WideString = 'This is WideString 3';
Var S : WideString;
    S4 : String;

begin
   S:=S1+S2;
   //!! Reference count is 2, should be 1...
   Write ('Adding S1+S2 : ',S,' '); DoRef(Pointer(S));
   S:=S1+S2+S3;
   Write ('Adding S1+S2+S3 : ',S,' '); DoRef(Pointer(S));
   S:=S+'...Added tail';
   Write ('Added tail to S ! : ',S);DoRef(Pointer(S));
   S4:=' This is a shortstring';
   //!! This crashes the program...
   S:=S1+S4;
   Write ('Adding S1+S4 : ',S,' '); DoRef(Pointer(S));
   S:=S1+'@';
   Write ('Adding S1+''@'' : ',S,' '); DoRef(Pointer(S));
end;

{ -------------------------------------------------------------------
    SetLength test.
  ------------------------------------------------------------------- }

Procedure TestSetlength;

Const S1 : WideString = 'This is WideString 1';
      S2 : WideString = 'This is WideString 2 and it is longer';

Var S : WideString;

begin
   Setlength(S,length(S1));
   Write ('Set length of s to ',length(s1));Doref(pointer(s));
   Move (Pointer(S1)^,Pointer(S)^,(Length(S1)+1)*sizeof(WideChar));
   Write ('S = "',S,'" '); DoRef(Pointer(S));
   Setlength(S,length(S2));
   Write ('Set length of s to ',length(s2));Doref(pointer(s));
   Move (Pointer(S2)^,Pointer(S)^,(Length(S2)+1)*sizeof(WideChar));
   Write ('S = "',S,'" '); DoRef(Pointer(S));
   SetLength(S,10);
   Write ('Set length of s to 10 ');Doref(pointer(s));
   Write ('S = "',S,'" '); DoRef(Pointer(S));
   SetLength(S,0);
   Write ('Set length of S to 0 ');Doref(Pointer(S));
   Write ('S = "',S,'" ');Doref(Pointer(s));
end;

{ -------------------------------------------------------------------
    Index test.
  ------------------------------------------------------------------- }

Procedure testIndex;

Var S,T : WideString;
    I,Len : longint;

begin
  S:='ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  Write ('S = "',S,'" ');doref(pointer(S));
  Write ('S = "');
  Len:=Length(S);
  For I:=1 to Len do
   Write(S[i]);
  write ('" ');Doref(pointer(S));
  Write ('Inverting S, ');
  For I:=1 to Len do
    S[i]:='A';
//    Asc(Ord('Z')+1-i);
  Write ('S = "',S,'" ');doref(pointer(S));
  T:=S;
  Write ('Assigned S to T '); Doref(Pointer(S));
  Write ('Again inverting S. ');
  For I:=1 to Len do
    S[i]:='B';
  Write ('S = "',S,'" ');doref(pointer(S));
  Write ('T = "',T,'" ');doref(pointer(T));
end;

{ -------------------------------------------------------------------
    Adding in expressions test.
  ------------------------------------------------------------------- }

Procedure TestAddExpr;

Const S1 : WideString = 'ABC';
      S2 : WideString = 'DEF';
      OK = 'OK';
      NOK = 'NOK';

Var I : Integer;
    S3 : WideString;
    mem : PtrInt;

begin
 mem:=0;
 DoMem(Mem);
 S3 := 'ABCDEF';
 Write ('S1+S2=S3 :');
 If S1+S2=S3 then writeln (ok) else writeln (nok);
 Write ('S1+S2=ABCDEF');
 If S1+S2='ABCDEF' then writeln (ok) else writeln (nok);
 Write ('Testing repeat');
 I:=0;
 S3:='';
 Repeat
   Inc(i);
   If I=10 then s3:='ABCDEF';
 until S1+S2=S3;
 Writeln (' Done.');
 I:=2;
 S3:='';
 Write ('Testing While');
 While S1+S2<>S3 do
   begin
   INc(i);
   If I=10 then s3:='ABCDEF';
   end;
 Writeln (' Done');
end;

Procedure TestStdFunc;


Var S,T : WideString;
    SS : ShortString;
    C : Char;
    Ca : Cardinal;
    L : longint;
    I : Integer;
    W : Word;
    B : Byte;
    R : Real;
    D : Double;
    E : Extended;
    Si : Single;
    Co : Comp;
    TempMem:PtrInt;
begin
  TempMem:=0;
  DoMem(TempMem);
  S:='ABCDEF';
  Write ('S = "',S,'"');Doref(Pointer(S));
  T:=Copy(S,1,3);
  Write ('T : "',T,'"');DoRef(Pointer(T));
  T:=Copy(S,3,3);
  Write ('T : "',T,'"');DoRef(Pointer(T));
  T:=Copy(S,3,6);
  Write ('T : "',T,'"');DoRef(Pointer(T));
  Writeln ('Inserting "123" in S at pos 4');
  Insert ('123',S,4);
  Write ('S = "',S,'"');DoRef(Pointer(S));
  Writeln ('Deleting 3 characters From S starting Pos 4');
  Delete (S,4,3);
  Write ('S = "',S,'"');Doref(Pointer(S));
  Writeln ('Pos ''DE'' in S is : ',Pos('DE',S));
  Write ('S = "',S,'"');Doref(Pointer(S));
  Writeln ('Setting T to ''DE''.');
  T:='DE';
  //!! Here something weird is happening ? S is lost ???
  Writeln('***');
  Writeln ('Pos T in S is : ',Pos(T,S));
  Write ('S = "',S,'"');Doref(Pointer(S));
  Writeln ('Setting T to ''D''.');
  T:='D';
  Writeln ('Pos T in S is : ',Pos(T,S));
  Write ('S = "',S,'"');Doref(Pointer(S));
  Writeln ('Setting T to ''DA''.');
  T:='DA';
  Writeln ('Pos T in S is : ',Pos(T,S));
  Write ('S = "',S,'"');Doref(Pointer(S));
  Writeln ('SS:=''DE''');
  Writeln('***');
  SS:='DE';
  Writeln ('Pos SS in S is : ',Pos(SS,S));
  Write ('S = "',S,'"');Doref(Pointer(S));
  Writeln ('C:=''D''');
  C:='D';
  Writeln ('Pos C in S is : ',Pos(C,S));
  Write ('S = "',S,'"');Doref(Pointer(S));
  Writeln ('Pos ''D'' in S is : ',Pos('D',S));
  Write ('S = "',S,'"');Doref(Pointer(S));
  Write ('str(Ca,S)= ');
  ca:=1;
  str(Ca,S);
  Writeln (S);
  Write ('str(L,S)= ');
  L:=2;
  str(L,S);
  Writeln (S);
  Write ('str(I,S)= ');
  I:=3;
  str(I,S);
  Writeln (S);
  Write ('str(W,S)= ');
  W:=4;
  str(W,S);
  Writeln (S);
  Write ('str(R,S)= ');
  R:=1.0;
  str(R,S);
  Writeln (S);
  Write ('str(D,S)= ');
  D:=2.0;
  str(D,S);
  Writeln (S);
  Write ('str(E,S)= ');
  E:=3.0;
  str(E,S);
  Writeln (S);
  Write ('str(Co,S)= ');
{$ifdef cpui386}
  Co:=4.0;
{$else cpui386}
  Co := 4;
{$endif cpui386}
  str(Co,S);
  Writeln (S);
  Write ('str(Si,S)= ');
  Si:=5.0;
  str(Si,S);
  Writeln (S);
end;

Var GlobalStartMem,StartMem : PtrInt;

begin
  GlobalStartMem:=0;
  StartMem:=0;
  DoMem(GlobalStartMem);
  DoMem(StartMem);
  Writeln ('Testing Initialize/Finalize.');
  TestInitFinal;
  Write ('End of Initialize/finalize test : ');DoMem(StartMem);

  Writeln;Writeln ('Testing parameter passing.');
  TestParams;
  Write ('End of Parameter passing test : ');DoMem(StartMem);

  Writeln;Writeln ('Testing comparision operators');
  TestCompare;
  Write ('End of compare test : ');DoMem(StartMem);

  Writeln;Writeln ('Testing setlength of WideStrings');
  TestSetLength;
  Write ('End of setlength test : ');DoMem(StartMem);

  Writeln;Writeln ('Testing Adding of WideStrings');
  TestAdd;
  Write ('End of adding test : ');DoMem(StartMem);

  Writeln;Writeln ('Testing Adding of WideStrings in expressions');
  TestAddExpr;
  Write ('End of adding in expressions test : ');DoMem(StartMem);

  Writeln;Writeln ('Testing type conversion.');
//  TestConversion;
  Write ('End of typeconversion test : ');DoMem(StartMem);

  Writeln;Writeln ('Testing indexed access.');
  TestIndex;
  Write ('End of index access test : ');DoMem(StartMem);

  Writeln;Writeln ('Testing standard functions.');
  TestStdfunc;
  Write ('End of standard functions: ');DoMem(StartMem);
  Write ('For the whole program ');DoMem(GlobalStartMem);
end.
