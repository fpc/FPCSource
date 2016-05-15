{$mode objfpc}
{$codepage utf8}

var
  name: utf8string;
  
procedure check(index, lookahead: longint; combiningdiacritics: boolean; expectedresult: longint; checknr: longint);
begin
  if Utf8CodePointLen(pchar(@name[index]),lookahead,combiningdiacritics)<>expectedresult then
    begin
      writeln('check ',checknr,': Utf8CodePointLen(',copy(name,index,length(name)),',',lookahead,',',combiningdiacritics,') = ',Utf8CodePointLen(pchar(@name[index]),lookahead,false),' <> expected ',expectedresult);
      halt(1);
    end;
end;

begin
  name:='a';
  check(1,0,false,0,1);
  check(1,1,false,1,2);
  check(1,1,true,1,3);
  check(1,6,false,1,4);
  check(1,6,true,1,5);
  name:='ab';
  check(1,6,false,1,6);
  check(1,6,true,1,7);
  check(1,1,false,1,8);
  check(1,1,true,1,9);
  check(2,6,false,1,10);
  check(2,6,true,1,11);
  name:='é';
  check(1,1,false,0,12);
  check(1,1,true,0,13);
  check(1,2,false,2,14);
  check(1,2,true,2,15);
  check(2,1,false,-1,16);
  check(2,1,true,-1,17);
  check(2,3,false,-1,18);
  check(2,3,true,-1,19);
  name:='éa';
  check(1,1,false,0,20);
  check(1,1,true,0,21);
  check(1,2,false,2,22);
  check(1,2,true,2,23);
  check(2,1,false,-1,24);
  check(2,1,true,-1,25);
  check(2,3,false,-1,26);
  check(2,3,true,-1,27);
  check(3,1,false,1,28);
  check(3,1,true,1,29);
  check(3,4,false,1,30);
  check(3,4,true,1,31);
  name[3]:=name[2];
  check(1,1,false,0,32);
  check(1,1,true,0,33);
  check(1,2,false,2,34);
  check(1,2,true,2,35);
  check(2,1,false,-1,36);
  check(2,1,true,-1,37);
  check(2,3,false,-1,38);
  check(2,3,true,-1,39);
  check(3,1,false,-1,40);
  check(3,1,true,-1,41);
  check(3,4,false,-1,42);
  check(3,4,true,-1,43);
  { e + combining ` }
  name:='e'#$0300'b';
  { check just the e without accent }
  check(1,1,false,1,44);
  check(1,1,true,1,45);
  check(1,2,false,1,46);
  { partial diacritical mark }
  check(1,2,true,0,47);
  check(1,3,false,1,48);
  { complete diacritical mark }
  check(1,3,true,3,49);
  check(1,4,false,1,50);
  { complete diacritical mark (ignore extra character) }
  check(1,4,true,3,51);
  { start of combining diacritical mark -- treated as independent utf-8 codepoint }
  check(2,1,false,0,52);
  check(2,1,true,0,53);
  check(2,3,false,2,54);
  check(2,3,true,2,55);
  { middle of the combining diacritical mark }
  check(3,1,false,-1,56);
  check(3,1,true,-1,57);
  check(3,4,false,-1,58);
  check(3,4,true,-1,59);
  { corrupt diacritical mark = no diacritical mark }
  name[3]:=name[4];
  { partial diacritical mark (the corrupted byte is not included in the
    lookahead) }
  check(1,2,true,0,60);
  check(1,3,false,1,61);
  { ignore corrupt diacritical mark }
  check(1,3,true,1,62);
  check(1,4,false,1,63);
  check(1,4,true,1,64);
  { e + combining circle + combining superscript 'n' }
  name:='e'#$20DD#$1DE0'b';
  { partial diacritical mark }
  check(1,2,true,0,65);
  check(1,3,false,1,66);
  check(1,3,true,0,67);
  check(1,4,false,1,68);
  { complete diacritical mark }
  check(1,4,true,4,69);
  check(1,4,false,1,70);
  { partial second diacritical mark }
  check(1,5,true,0,71);
  check(1,5,false,1,72);
  check(1,6,true,0,73);
  check(1,6,false,1,74);
  { complete both diacritical marks }
  check(1,7,true,7,75);
  check(1,7,false,1,76);
  check(1,10,true,7,77);
  check(1,10,false,1,78);
  { complete both diacritical marks without first character }
  check(2,6,true,6,79);
  check(2,20,true,6,80);
  { only the first one, treated as independent codepoint }
  check(2,7,false,3,81);
  { corrupt second diacritical mark }
  name[7]:=name[8];
  { partial second diacritical mark }
  check(1,5,true,0,82);
  check(1,5,false,1,83);
  check(1,6,true,0,84);
  check(1,6,false,1,85);
  { including bad byte -> ignore second diacritical mark completely
    (can't know it's part of a diacritical mark or something else) }
  check(1,7,true,4,86);
  check(1,7,false,1,87);
  { complete both diacritical marks without first character,
    but with corrupted byte }
  check(2,7,true,3,88);
  check(2,20,true,3,89);
  { corrupted diacritical mark by itself }
  { 1) incomplete }
  check(5,1,false,0,90);
  check(5,1,true,0,91);
  check(5,2,false,0,92);
  check(5,2,true,0,93);
  { 2) invalid }
  check(5,3,false,-2,94);
  check(5,3,true,-2,95);
end.
