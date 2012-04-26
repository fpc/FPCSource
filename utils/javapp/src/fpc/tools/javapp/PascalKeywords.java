package fpc.tools.javapp;

import java.util.HashSet;

public class PascalKeywords {
	static private HashSet<String> pascalKeywords;
	
	static public boolean isPascalKeyword(String str) {
		return pascalKeywords.contains(str.toUpperCase());
	}
	
	static public String escapeIfPascalKeyword(String str) {
		if (isPascalKeyword(str.toUpperCase()))
			return "&"+str;
		return str;
	}

	static {
		pascalKeywords = new HashSet<String>();
		pascalKeywords.add("AS");
		pascalKeywords.add("DO");
		pascalKeywords.add("IF");
		pascalKeywords.add("IN");
		pascalKeywords.add("IS");
		pascalKeywords.add("OF");
		pascalKeywords.add("ON");
		pascalKeywords.add("OR");
		pascalKeywords.add("TO");
		pascalKeywords.add("AND");
		pascalKeywords.add("ASM");
		pascalKeywords.add("DIV");
		pascalKeywords.add("END");
		pascalKeywords.add("FOR");
		pascalKeywords.add("MOD");
		pascalKeywords.add("NIL");
		pascalKeywords.add("NOT");
		pascalKeywords.add("SET");
		pascalKeywords.add("SHL");
		pascalKeywords.add("SHR");
		pascalKeywords.add("TRY");
		pascalKeywords.add("VAR");
		pascalKeywords.add("XOR");
		pascalKeywords.add("CASE");
		pascalKeywords.add("ELSE");
		pascalKeywords.add("FILE");
		pascalKeywords.add("GOTO");
		pascalKeywords.add("THEN");
		pascalKeywords.add("TRUE");
		pascalKeywords.add("TYPE");
		pascalKeywords.add("UNIT");
		pascalKeywords.add("USES");
		pascalKeywords.add("WITH");
		pascalKeywords.add("ARRAY");
		pascalKeywords.add("BEGIN");
		pascalKeywords.add("CLASS");
		pascalKeywords.add("CONST");
		pascalKeywords.add("FALSE");
		pascalKeywords.add("FINAL");
		pascalKeywords.add("LABEL");
		pascalKeywords.add("UNTIL");
		pascalKeywords.add("RAISE");
		pascalKeywords.add("WHILE");
		pascalKeywords.add("EXCEPT");
		pascalKeywords.add("DOWNTO");
		pascalKeywords.add("OBJECT");
		pascalKeywords.add("PACKED");
		pascalKeywords.add("PUBLIC");
		pascalKeywords.add("RECORD");
		pascalKeywords.add("REPEAT");
		pascalKeywords.add("STRING");
		pascalKeywords.add("STRICT");
		pascalKeywords.add("EXPORTS");
		pascalKeywords.add("FINALLY");
		pascalKeywords.add("LIBRARY");
		pascalKeywords.add("PROGRAM");
		pascalKeywords.add("PRIVATE");
		pascalKeywords.add("ABSTRACT");
		pascalKeywords.add("CPPCLASS");
		pascalKeywords.add("FUNCTION");
		pascalKeywords.add("OPERATOR");
		pascalKeywords.add("PROPERTY");
		pascalKeywords.add("BITPACKED");
		pascalKeywords.add("INHERITED");
		pascalKeywords.add("INTERFACE");
		pascalKeywords.add("OTHERWISE");
		pascalKeywords.add("PROCEDURE");
		pascalKeywords.add("PROTECTED");
		pascalKeywords.add("THREADVAR");
		pascalKeywords.add("DESTRUCTOR");
		pascalKeywords.add("CONSTRUCTOR");
		pascalKeywords.add("DISPINTERFACE");
		pascalKeywords.add("IMPLEMENTATION");
		pascalKeywords.add("RESOURCESTRING");		
	}
}
