Program Hhex;
{
   Small example/test of the html help OCX.
   Marco van de Voort (C) 2009

   Copy ref.chm from the CHM distribution to this dir.
}
{$mode objfpc}{$H+}
uses htmlhelp;


var 
   HelpfileName : AnsiString;
   htmltopic    : AnsiString;
   res 		: Integer;
 
Begin
  Helpfilename:='ref.chm';
  htmltopic   := 'ref/refli3.html';

  Writeln('Html example 1');
  Writeln('note:    Copy ref.chm from the CHM distribution to this dir');
  
  // HH_DISPLAY_INDEX  or HH_DISPLAY_SEARCH work too. 
  Writeln('calling TOC');
  Res:=HtmlHelpA(0,pchar(helpfilename) ,HH_DISPLAY_TOC,0);	
  Writeln('program now blocked on readln, press enter (in console window) to continue');
  
  readln;
  writeln('Showing a topic that is probably about dialog.');
  // probably because due to automatic generation filenames and exact contact can drift.
  Res:=HtmlHelpA(0,pchar(helpfilename) ,HH_DISPLAY_TOPIC,ptruint(pchar(htmltopic)));	 
  Writeln('program now blocked on readln, press enter (in console window) to continue');
  readln;
  Writeln('ready. Note that the windows die automatically on exit of the program');

{
   Not demoed yet : HH_HELPCONTEXT. Load on ID, because we have no files that do that yet
}


end.
  