{ Basic test suite for the strings unit }
Program TStrings1;

uses
   strings;

procedure failed;
  begin
     writeln('Failed.');
     halt(1);
  end;

procedure teststrlen;
  Const
     P1 : PChar = '';
     P2 : PChar = 'This is a constant pchar string';
  begin
     if strlen(P1)<>0 then
       failed;
     if strlen(P2)<>31 then
       failed;
  end;

procedure teststrcomp;
  Const
     P1 : PChar = 'This is the first string.';
     P2 : PCHar = 'This is the second string.';
     P3 : PChar = 'This is the first string.';
  begin
     If StrComp (P1,P2)=0 then
       failed;
     If StrComp (P1,P3)<>0 then
       failed;
     If StrComp (P1,P2)>0 then
       failed;
     If StrComp (P2,P1)<0 then
       failed;
  end;

procedure teststrpas;
  Const
     P1 : PChar = 'This is a PCHAR string';
     P2 : PChar = '';
  var
     S : string;
  begin
     S:=StrPas(P1);
     if S<>'This is a PCHAR string' then
       failed;
     S:=StrPas(P2);
     if S<>'' then
       failed;
end;


procedure teststrlcomp;
  Const
     P1 : PChar = 'This is the first string.';
     P2 : PCHar = 'This is the second string.';
     P3 : PChar = 'This is the first string.';
  Var
     L : Longint;
  begin
     L:=1;
     While StrLComp(P1,P2,L)=0 do
       inc (L);
     if L<>13 then failed;
     If StrLComp (P1,P2,255)=0 then
       failed;
     If StrLComp (P1,P3,100)<>0 then
       failed;
     If StrLComp (P1,P2,65535)>0 then
       failed;
     If StrLComp (P2,P1,12341234)<0 then
       failed;
  end;


procedure teststrpcopy;
  Const
     S1 = 'This is a normal string.';
     S2 = '';
  Var
     P : array[0..255] of char;
  begin
     if StrPCopy(P,S1)<>P then
       failed;
     if StrComp(P,S1)<>0 then
       failed;
     if StrPCopy(P,S2)<>P then
       failed;
     if StrComp(P,S2)<>0 then
       failed;
  end;


procedure teststrend;
  Const
     P : PChar = 'This is a PCHAR string.';
  begin
     If StrEnd(P)-P<>23 then
       failed;
  end;


procedure teststrcopy;
  Const
     P1 : PChar = 'This a test string 012345678901234567890123456789012345678901234567890123456789';
     P2 : PChar = '';
  var
     Buf : array[0..255] of char;
  begin
     if StrCopy(Buf,P1)<>Buf then
       failed;
     if StrComp(Buf,P1)<>0 then
       failed;
     if StrCopy(Buf,P2)<>Buf then
       failed;
     if StrComp(Buf,P2)<>0 then
       failed;
  end;


procedure teststrscanstrrscan;
  Const
    P : PChar = 'This is a PCHAR string.';
    S : Char = 's' ;
begin
  if StrComp(StrScan(P,s),'s is a PCHAR string.')<>0 then
    failed;
  if StrComp(StrRScan(P,s),'string.')<>0 then
    failed;
end;


begin
   write('Testing strlen ... ');
   teststrlen;
   writeln('Success.');
   write('Testing strcomp ... ');
   teststrcomp;
   writeln('Success.');
   write('Testing strlcomp ... ');
   teststrlcomp;
   writeln('Success.');
   write('Testing strpas ... ');
   teststrpas;
   writeln('Success.');
   write('Testing strcopy ... ');
   teststrcopy;
   writeln('Success.');
   write('Testing strpcopy ... ');
   teststrpcopy;
   writeln('Success.');
   write('Testing strend ... ');
   teststrend;
   writeln('Success.');
   write('Testing strscan/strrscan ... ');
   teststrscanstrrscan;
   writeln('Success.');
end.
