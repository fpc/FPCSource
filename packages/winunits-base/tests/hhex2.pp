Program hhex2;
{
   Small example/test of the html help OCX.
   Marco van de Voort (C) 2009

   Copy rtl.chm from the CHM distribution to this dir. Test keyword/alink search.
}

Uses HTMLHelp;

var
   keyword      : ansistring; 
   HelpfileName : AnsiString;
   htmltopic    : AnsiString;
   res 		: Integer;
   ah           : PHH_AKLINK ;
   
Begin
  Helpfilename:='rtl.chm';
  keyword:='Sysutils' ;
 
  New(ah);
  fillchar(ah^,sizeof(ah^),#0); 
  ah.cbstruct:=sizeof(tagHH_AKLINK);
  ah.fReserved   := FALSE ;
  ah.pszKeywords :=pansichar(keyword);  
  ah.pszUrl      := NIL ;
  ah.pszMsgText  :='Text succes' ;
  ah.pszMsgTitle :='Text fail';
  ah.pszWindow   := NIL ;
  ah.fIndexOnFail:= false;

 
  Res:=HtmlHelpA(0,pchar(helpfilename) ,HH_DISPLAY_INDEX,PTRUINT(PAnsiChar(Keyword)));	

  // keyword search seems to have same effect.
  Res:=HtmlHelpA(0,pchar(helpfilename) ,HH_ALINK_LOOKUP,PTRUINT(AH));	
  writeln(ah.pszkeywords);
  writeln(ah.pszurl);
  writeln(ah.pszmsgtext);
  writeln(ah.pszmsgtitle);
  writeln(ah.pszwindow);
  writeln(res);

 readln;
end.
