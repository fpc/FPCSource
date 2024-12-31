{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    YAML string constants

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpyaml.strings;

{$mode ObjFPC}{$H+}

interface

resourcestring
  // Data
  SErrIsNotA = '%s is not a valid %s';
  SErrIndexOutOfBounds = 'Index out of bounds: %d';
  // Scanner
  SErrInvalidAnchorAliasName = 'An empty %s name is not allowed.';
  SErrInvalidDocumentIndicator = 'Unexpected document indicator.';
  SErrInvalidCharacterInAnchor = 'Invalid character in anchor or alias: "%s".';
  SErrInvalidCharacterInVersion = 'Invalid character in version: "%s".';
  SErrInvalidEscapeChar = 'Invalid escape character "%s".';
  SErrInvalidHexChar = 'Invalid hexadecimal character: "%s".';
  SErrInvalidIndentationChar = 'Indentation specifier cannot be 0.';
  SErrInvalidIndentChar = 'Invalid tab character, expected indentation space.';
  SErrInvalidTagEnd = 'Tag did not end on ">".';
  SErrInvalidUnicodePoint = 'Invalid Unicode character escape code.';
  SErrInvalidURIEscapeChar = 'Invalid URI escaped octet in "%s".';
  SErrInvalidUTF8Char = 'Invalid UTF-8 character in "%s".';
  SErrInvalidVersionNumber = 'Invalid version number: "%s".';
  SErrInvalidWhitespace = 'Invalid character: Expected whitespace or line break, got: "%s".';
  SErrMappingKeysAreNotAllowedInBlockContext = 'Mapping keys are not allowed in block context.';
  SErrMappingValuesNotAllowedInThisContext = 'Mapping values are not allowed in this context.';
  SErrMissingColonInKey = 'Missing ":" while scanning for a simple key.';
  SErrMissingDirectiveName = 'Missing directive name.';
  SErrMissingTagURI = 'Missing tag URI.';
  SErrNoSimpleKeyAvailable = 'No simple key available.';
  SErrNoWhiteSpaceAtDirectiveEnd = 'Tag directive must end with whitespace or line break.';
  SErrUnexpectedBlockEntry = 'block entries are not allowed in flow context.';
  SErrUnexpectedCharacter = 'Unexpected character found: "%s".';
  SErrUnexpectedCharacterInTag = 'Unexpected character in "%s". Expected "!", got "%s" instead.';
  SErrUnexpectedCharInDirectiveValue = 'Unexpected character in directive value.';
  SErrUnexpectedColon = 'Unexpected ":".';
  SErrUnexpectedEndOfLine = 'Did not find expected line break or comment.';
  SErrUnexpectedEOS = 'Unexpected end of stream.';
  SErrUnexpectedTab = 'Tab character encountered that violates indentation.';
  SErrUnexpectedTrailingData = 'Unexpected trailing data after directive.';
  SErrUnknownCharacter = 'Unknown character encountered.';
  SErrUnknownDirective = 'Unknown directive name: "%s".';
  SErrVersionNumberTooLong = 'Version number too long.';

  // Parser
  SErrDoubleAnchor    = 'Double anchor: encountered new anchor "%s", current is "%s".';
  SErrDoubleVersion   = 'Double version directive: encountered new version "%s", current is "%s".';
  SErrAliasNotAllowed = 'Alias not allowed at stream level.';
  SErrUnexpectedToken = 'Unexpected token %s with value: "%s".';
  SErrUnknownAlias    = 'Unknown alias: "%s".';

  // Convert to JSON
  SErrOnlyScalarKeys = 'Only scalar keys can be converted to JSON keys.';
  SErrUnknownYAMLtype = 'Unknown YAML data type : "%s".';
  SErrOnlySingleDocument = 'Can only convert YAML stream with 1 document.';
  SErrOnlySingleValue = 'Can only convert YAML document with 1 value.';

implementation

end.

