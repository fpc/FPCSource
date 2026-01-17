unit System.RegularExpressionsConsts;

interface

resourcestring
  SRegExExpressionError    = 'Error in regular expression at offset %d: %s';
  SRegExIndexOutOfBounds   = 'Index out of bounds (%d)';
  SRegExInvalidGroupName   = 'Invalid group name (%s)';
  SRegExInvalidIndexType   = 'Invalid index type';
  SRegExMatchRequired      = 'Successful match required';
  SRegExMatchError         = 'Error matching the regex: %s';
  SRegExMissingExpression  = 'A regular expression specified in RegEx is required';
  SRegExStringsRequired    = 'Strings parameter cannot be nil';
  SRegExStudyError         = 'Error studying the regex: %s';
  SErrRegexOvectorTooSmall = 'output vector was not big enough for all the captured substrings';
  SRegExMatcStartAfterEnd  = '\K was used in an assertion to set the match start after its end.'+sLineBreak+
                             'From end to start the match was: %s';
  SErrInvalidNameIndex     = 'Invalid group name index: %d, valid range = [0..%d[';

implementation

end.

