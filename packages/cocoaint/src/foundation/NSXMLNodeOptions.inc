{ Parsed from Foundation.framework NSXMLNodeOptions.h }

{$ifdef TYPES}
const
  NSXMLNodeOptionsNone = 0;
  NSXMLNodeIsCDATA = 1 shl 0;
  NSXMLNodeExpandEmptyElement = 1 shl 1;
  NSXMLNodeCompactEmptyElement = 1 shl 2;
  NSXMLNodeUseSingleQuotes = 1 shl 3;
  NSXMLNodeUseDoubleQuotes = 1 shl 4;
  NSXMLNodeNeverEscapeContents = 1 shl 5;
  NSXMLDocumentTidyHTML = 1 shl 9;
  NSXMLDocumentTidyXML = 1 shl 10;
  NSXMLDocumentValidate = 1 shl 13;
  NSXMLNodeLoadExternalEntitiesAlways = 1 shl 14;
  NSXMLNodeLoadExternalEntitiesSameOriginOnly = 1 shl 15;
  NSXMLNodeLoadExternalEntitiesNever = 1 shl 19;
  NSXMLDocumentXInclude = 1 shl 16;
  NSXMLNodePrettyPrint = 1 shl 17;
  NSXMLDocumentIncludeContentTypeDeclaration = 1 shl 18;
  NSXMLNodePreserveNamespaceOrder = 1 shl 20;
  NSXMLNodePreserveAttributeOrder = 1 shl 21;
  NSXMLNodePreserveEntities = 1 shl 22;
  NSXMLNodePreservePrefixes = 1 shl 23;
  NSXMLNodePreserveCDATA = 1 shl 24;
  NSXMLNodePreserveWhitespace = 1 shl 25;
  NSXMLNodePreserveDTD = 1 shl 26;
  NSXMLNodePreserveCharacterReferences = 1 shl 27;
  NSXMLNodePromoteSignificantWhitespace = 1 shl 28;
  NSXMLNodePreserveEmptyElements = NSXMLNodeExpandEmptyElement + NSXMLNodeCompactEmptyElement;
  NSXMLNodePreserveQuotes = NSXMLNodeUseSingleQuotes + NSXMLNodeUseDoubleQuotes;
  NSXMLNodePreserveAll = NSXMLNodePreserveNamespaceOrder + NSXMLNodePreserveAttributeOrder + NSXMLNodePreserveEntities + NSXMLNodePreservePrefixes + NSXMLNodePreserveCDATA + NSXMLNodePreserveEmptyElements + NSXMLNodePreserveQuotes + NSXMLNodePreserveWhitespace + NSXMLNodePreserveDTD + NSXMLNodePreserveCharacterReferences + $FFF00000;
{$endif}

