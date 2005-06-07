  type
     FontCharInfoType = record
          offset : SByte;
          width : SByte;
       end;

     FontType = record
          fontType : SWord;
          firstChar : SWord;
          lastChar : SWord;
          maxWidth : SWord;
          kernMax : SWord;
          nDescent : SWord;
          fRectWidth : SWord;
          fRectHeight : SWord;
          owTLoc : SWord;
          ascent : SWord;
          descent : SWord;
          leading : SWord;
          rowWords : SWord;
       end;

     FontPtr = ^FontType;

     FontTablePtr = ^FontPtr;

     fontID = (stdFont := $00,boldFont,largeFont,symbolFont,
       symbol11Font,symbol7Font,ledFont,largeBoldFont,
       fntAppFontCustomBase := $80);

  const
     checkboxFont = symbol11Font;

  function FntIsAppDefined(fnt : FontID) : boolean;

  function FntGetFont:FontID;systrap sysTrapFntGetFont;
  function FntSetFont(font:FontID):FontID;systrap sysTrapFntSetFont;
  function FntGetFontPtr:FontPtr;systrap sysTrapFntGetFontPtr;
  function FntBaseLine:SWord;systrap sysTrapFntBaseLine;
  function FntCharHeight:SWord;systrap sysTrapFntCharHeight;
  function FntLineHeight:SWord;systrap sysTrapFntLineHeight;
  function FntAverageCharWidth:SWord;systrap sysTrapFntAverageCharWidth;
  function FntCharWidth(ch:Char):SWord;systrap sysTrapFntCharWidth;
  function FntCharsWidth(chars:pChar; len:Word):SWord;systrap sysTrapFntCharsWidth;
  procedure FntCharsInWidth(string:pChar; stringWidthP:pSWord; stringLengthP:pSWord; fitWithinWidth:pBoolean);systrap sysTrapFntCharsInWidth;
  function FntDescenderHeight:SWord;systrap sysTrapFntDescenderHeight;
  function FntLineWidth(pChars:pChar; length:Word):SWord;systrap sysTrapFntLineWidth;
  function FntWordWrap(chars:pChar; maxWidth:Word):Word;systrap sysTrapFntWordWrap;
  procedure FntWordWrapReverseNLines(chars:pChar; maxWidth:Word; linesToScrollP:WordPtr; scrollPosP:WordPtr);systrap sysTrapFntWordWrapReverseNLines;
  procedure FntGetScrollValues(chars:pChar; width:Word; scrollPos:Word; linesP:WordPtr; topLine:WordPtr);systrap sysTrapFntGetScrollValues;
  function FntDefineFont(font:FontID; fontP:FontPtr):Err;systrap sysTrapFntDefineFont;
