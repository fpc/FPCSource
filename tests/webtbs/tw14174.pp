program Test;

type
  TToken = (
    tkNil,tkEOF,tkNumber,tkOpenBrace,tkCloseBrace,
    tkPlus,tkMinus,tkTimes,tkSlash,tkCaret,tkSemiColon
  );

function TokenToStr(const Token: TToken): String;

  function Quote(const S: String): String;
  inline; // comment out to avoid the internal error
  begin
    Quote:='"'+S+'"';
  end;

begin
  case Token of
    tkNil : TokenToStr:=Quote('Unknown');
    tkEOF : TokenToStr:=Quote('EOF');
    tkNumber : TokenToStr:=Quote('Number');
    tkOpenBrace : TokenToStr:=Quote('(');
    tkCloseBrace: TokenToStr:=Quote(')');
    tkPlus : TokenToStr:=Quote('+');
    tkMinus : TokenToStr:=Quote('-');
    tkTimes : TokenToStr:=Quote('*');
    tkSlash : TokenToStr:=Quote('/');
    tkCaret : TokenToStr:=Quote('^');
    tkSemiColon : TokenToStr:=Quote(';');
  end;
end;

begin
  if (TokenToStr(tkNil)<>'"Unknown"') then
    halt(1);
end.
