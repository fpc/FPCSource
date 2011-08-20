package fpc.tools.javapp;

import java.util.HashMap;

public class ClassIdentifierInfo {
	
	protected static HashMap<String,ClassIdentifierInfo> identifierStore = new HashMap<String,ClassIdentifierInfo>();
	
	String[] superClasses;
	HashMap<String,String> identifiers;

	protected ClassIdentifierInfo(String[] superClasses) {
		identifiers = new HashMap<String,String>();
		this.superClasses = superClasses;
	}
	
	public static void registerClassInfo(String className, String superClass, String[] superIntf) {
		if (identifierStore.get(className) == null) {
			String[] combinedSuperInfo;
			if (superIntf.length==0) {
				combinedSuperInfo = new String[superClass!=null?1:0];
			} else {
				combinedSuperInfo = new String[superIntf.length+1];
				for (int i = 0; i < superIntf.length; i++)
					combinedSuperInfo[i] = superIntf[i];
			}
			// also sets java.lang.Object for interfaces, but doesn't matter since those
			// identifiers cannot be used by any class implementing this interface anyway
			// (since they will inherit from java.lang.Interface)
			if (superClass != null)
				combinedSuperInfo[combinedSuperInfo.length-1] = superClass;
			
			ClassIdentifierInfo classIdInfo = new ClassIdentifierInfo(combinedSuperInfo);
			identifierStore.put(className,classIdInfo);
		}
	}

	private String checkSafeIdentifierName(String id) {
		for (int i = 0; i < superClasses.length; i++) {
			id = getSafeIdentifierNameForClass(superClasses[i],id);
		}
		id = PascalKeywords.escapeIfPascalKeyword(id);
		String testName = id.toUpperCase();
		String orgName;
		if (id.indexOf('$') != -1) {
			System.out.println("  Warning, cannot represent identifier '"+id+"', hiding");
			id = id.replace("$","__");
		}
		while (((orgName = identifiers.get(testName)) != null) &&
				!orgName.equals(id)) {
			id = id + "_";
			testName = testName + "_";
		}
		return id;
	}

	private String addIdentifier(String id) {
		String testName = id.toUpperCase();
		for (int i = 0; i < superClasses.length; i++) {
			id = getSafeIdentifierNameForClass(superClasses[i],id);
		}
		id = checkSafeIdentifierName(id);
		identifiers.put(testName, id);
		return id;
	}
	
	
	public static String AddIdentifierNameForClass(String className, String identifier) {
		ClassIdentifierInfo classIdInfo = identifierStore.get(className);
		if (classIdInfo == null) {
			throw new IllegalStateException("Class info for "+className+" not registered");
		}
		return classIdInfo.addIdentifier(identifier);
	}
	
	public static String getSafeIdentifierNameForClass(String className, String identifier) {
		ClassIdentifierInfo classIdInfo = identifierStore.get(className);
		if (classIdInfo == null) {
			throw new IllegalStateException("Class info for "+className+" not registered");
		}
		return classIdInfo.checkSafeIdentifierName(identifier);
	}
	
}
