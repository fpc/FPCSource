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

	private static String getMapIdentifer(String id) {
		String testName;
		if (id.equals("<init>"))
			testName = "CREATE";
		else if (id.equals("<clinit"))
			testName = "CLASSCONSTRUCTOR";
		else
			testName = id.toUpperCase();
		return testName;
	}
	
	private String checkSafeIdentifierName(String id, String testName, boolean checkSupers) {
		if (checkSupers) {
			for (int i = 0; i < superClasses.length; i++) {
				id = getSafeIdentifierNameForClassInternal(superClasses[i],id,testName,checkSupers);
			}
		}
		id = PascalKeywords.escapeIfPascalKeyword(id);
		String orgName;
		if (id.contains("$")) {
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
		String testName = getMapIdentifer(id);
		id = checkSafeIdentifierName(id,testName,true);
		identifiers.put(testName, id);
		return id;
	}
		
	private String addMethodIdentifier(String id) {
		String testName = getMapIdentifer(id);
		id = checkSafeIdentifierName(id,testName,false);
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
	
	public static String AddMethodIdentifierNameForClass(String className, String identifier) {
		/** all constructors are called <init> and will be renamed to create
		 *  -> any method called create will have to be renamed. This happens automatically
		 *  for classes (since the constructor will be added first), but not for interfaces
		 *  (they don't have a constructor) -> force it here 
		 */
		if (identifier.equals("create"))
			identifier = "create_";
		ClassIdentifierInfo classIdInfo = identifierStore.get(className);
		if (classIdInfo == null) {
			throw new IllegalStateException("Class info for "+className+" not registered");
		}
		return classIdInfo.addMethodIdentifier(identifier);
	}

	
	private static String getSafeIdentifierNameForClassInternal(String className, String identifier, String testName, boolean checkSupers) {
		ClassIdentifierInfo classIdInfo = identifierStore.get(className);
		if (classIdInfo == null) {
			throw new IllegalStateException("Class info for "+className+" not registered");
		}
		return classIdInfo.checkSafeIdentifierName(identifier,testName,checkSupers);
	}
	
	public static String getSafeIdentifierNameForClass(String className, String identifier) {
		return getSafeIdentifierNameForClassInternal(className,identifier,getMapIdentifer(identifier),false);
	}
	
	public static String getSafeMethodIdentifierNameForClass(String className, String identifier) {
		/** all constructors are called <init> and will be renamed to create
		 *  -> any method called create will have to be renamed. This happens automatically
		 *  for classes (since the constructor will be added first), but not for interfaces
		 *  (they don't have a constructor) -> force it here 
		 */
		if (identifier.equals("create"))
			identifier = "create_";
	return getSafeIdentifierNameForClassInternal(className,identifier,getMapIdentifer(identifier),true);
	}
}
