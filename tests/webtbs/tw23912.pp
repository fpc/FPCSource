program crash_2_7_1;

{$mode objfpc}{$H+}

//uses

type
  TSynCommentType = (sctAnsi, sctBor, sctSlash);
  TSynCommentIndentFlag = (
    // * For Matching lines (FCommentMode)
      // By default indent is the same as for none comment lines (none overrides sciAlignOpen)
      sciNone,      // Does not Indent comment lines (Prefix may contain a fixed indent)
      sciAlignOpen, // Indent to real opening pos on first line, if comment does not start at BOL "Foo(); (*"
      sciAddTokenLen,        // add 1 or 2 spaces to indent (for the length of the token)
      sciAddPastTokenIndent, // Adds any indent found past the opening token  "(*", "{" or "//".
      sciMatchOnlyTokenLen,        // Apply the Above only if first line matches. (Only if sciAddTokenLen is specified)
      sciMatchOnlyPastTokenIndent,
      sciAlignOnlyTokenLen,        // Apply the Above only if sciAlignOpen was used (include via max)
      sciAlignOnlyPastTokenIndent,
      sciApplyIndentForNoMatch  // Apply above rules For NONE Matching lines (FCommentMode),
                                // includes FIndentFirstLineExtra
    );
  TSynCommentIndentFlags = set of TSynCommentIndentFlag;
  TSynCommentContineMode = (
      sccNoPrefix,      // May still do indent, if matched
      sccPrefixAlways,  // If the pattern did not match all will be done, except the indent AFTER the prefix (can not be detected)
      sccPrefixMatch
    );
  TSynCommentMatchMode = (
      scmMatchAfterOpening, // will not include (*,{,//. The ^ will match the first char after
      scmMatchOpening,      // will include (*,{,//. The ^ will match the ({/
      scmMatchWholeLine,    // Match the entire line
      scmMatchAtAsterisk    // AnsiComment only, will match the * of (*, but not the (
    );
  TSynCommentMatchLine = (
      sclMatchFirst, // Match the first line of the comment to get substitutes for Prefix ($1)
      sclMatchPrev   // Match the previous line of the comment to get substitutes for Prefix ($1)
    );
  TSynBeautifierIndentType = (sbitSpace, sbitCopySpaceTab, sbitPositionCaret);
  TSynCommentExtendMode = (
      sceNever,                // Never Extend
      sceAlways,               // Always
      sceSplitLine,            // If the line was split (caret was not at EOL, when enter was pressed
      sceMatching,             // If the line matched (even if sccPrefixAlways or sccNoPrefix
      sceMatchingSplitLine
    );


function dbgs(AIndentFlag: TSynCommentIndentFlag): String;
begin
  Result := ''; WriteStr(Result, AIndentFlag);
end;

function dbgs(AIndentFlags: TSynCommentIndentFlags): String;
var
  i: TSynCommentIndentFlag;
begin
  Result := '';
  for i := low(TSynCommentIndentFlag) to high(TSynCommentIndentFlag) do
    if i in AIndentFlags then
      if Result = ''
      then Result := dbgs(i)
      else Result := Result + ',' + dbgs(i);
  if Result <> '' then
    Result := '[' + Result + ']';
end;


procedure Foo(Atype: TSynCommentType;
    AIndentMode: TSynCommentIndentFlags;
    AIndentFirstLineMax:   Integer; AIndentFirstLineExtra: String;
    ACommentMode: TSynCommentContineMode; AMatchMode: TSynCommentMatchMode;
    AMatchLine: TSynCommentMatchLine; ACommentIndent: TSynBeautifierIndentType;
    AMatch: String;  APrefix: String;
    AExtenbSlash: TSynCommentExtendMode = sceNever);
var
  s: String;
begin
    writestr(s, AType,':',
             ' IMode=', dbgs(AIndentMode), ' IMax=', AIndentFirstLineMax, ' IExtra=', AIndentFirstLineExtra,
             ' CMode=', ACommentMode, ' CMatch=', AMatchMode, ' CLine=', AMatchLine,
             ' M=''', AMatch, ''' R=''', APrefix, ''' CIndent=', ACommentIndent
            );
    if s<>'sctAnsi: IMode=[sciAddTokenLen] IMax=5 IExtra=   CMode=sccPrefixMatch CMatch=scmMatchOpening CLine=sclMatchPrev M=''.'' R=''+'' CIndent=sbitCopySpaceTab' then
      halt(1);
end;

begin
  Foo(sctAnsi, [sciAddTokenLen], 5, '  ', sccPrefixMatch, scmMatchOpening,
      sclMatchPrev, sbitCopySpaceTab, '.', '+');

end.

