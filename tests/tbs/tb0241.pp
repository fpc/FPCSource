{ %OPT=-al }
{ %SKIPTARGET=macos,win64 }
{ On macos, PPCAsm chokes on this and crashes}

{ this forces use of GNU as }
{ Old file: tbs0282.pp }
{ long mangledname problem with -Aas                   OK 0.99.13 (PFV) }


type very____long_____string___identifier= string[200];

procedure test(very__long_variable01: very____long_____string___identifier;
               very__long_variable02: very____long_____string___identifier;
               very__long_variable03: very____long_____string___identifier;
               very__long_variable04: very____long_____string___identifier;
               very__long_variable05: very____long_____string___identifier;
               very__long_variable06: very____long_____string___identifier;
               very__long_variable07: very____long_____string___identifier;
               very__long_variable08: very____long_____string___identifier;
               very__long_variable09: very____long_____string___identifier;
               very__long_variable10: very____long_____string___identifier;
               very__long_variable11: very____long_____string___identifier;
               very__long_variable12: very____long_____string___identifier;
               very__long_variable13: very____long_____string___identifier;
               very__long_variable14: very____long_____string___identifier;
               very__long_variable15: very____long_____string___identifier;
               very__long_variable16: very____long_____string___identifier;
               very__long_variable17: very____long_____string___identifier;
               very__long_variable18: very____long_____string___identifier);
begin
  writeln('hi!');
end;

begin
  writeln('vreemd!');
  test('','','','','','','','','','',
       '','','','','','','','');
end.
