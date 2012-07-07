package fpc.tools.javapp;

import java.io.PrintWriter;
import java.util.*;

public class PascalUnit {
	
	private static class SkelItem implements Comparable<SkelItem> {
		String className;
		String kind;
		
		public SkelItem(String className, String kind) {
			this.className = className;
			this.kind = kind;
		}

		public int compareTo(SkelItem o) {
			return className.compareTo(o.className);
		}
		
		public String toString() {
			return className;
		}
	}
	
	private PrintWriter unitFile;
	private String[] pkgPrefixes;
	private String[] excludePrefixes;
	private String[] skelPrefixes;
	private String includeName;
	private HashSet<String> registeredExternalClasses, registeredInternalClasses;
	private SortedSet<SkelItem> registeredSkelObjs;
	private HashSet<String> registeredExternalInterfaces, registeredInternalInterfaces;
	// maps full java class names (pkg.classname) to unique "short" Pascal names in the current unit
	// maps from short to full name
	private HashMap<String,String> classShortToLong, classLongToShort;
	private JavapEnvironment env;

	public PascalUnit(PrintWriter unitFile, JavapEnvironment env, ArrayList<String> pkgPrefixes, String includeName) {
		this.unitFile = unitFile;
		this.env = env;
		this.pkgPrefixes = new String[pkgPrefixes.size()];
		for (int i = 0; i < pkgPrefixes.size(); i++) {
			this.pkgPrefixes[i] = pkgPrefixes.get(i).replace('.', '/');
		}
		this.excludePrefixes = new String[env.excludePrefixes.size()];
		for (int i = 0; i < env.excludePrefixes.size(); i++) {
			this.excludePrefixes[i] = env.excludePrefixes.get(i).replace('.', '/');
		}
		this.skelPrefixes = new String[env.skelPrefixes.size()];
		for (int i = 0; i < env.skelPrefixes.size(); i++) {
			this.skelPrefixes[i] = env.skelPrefixes.get(i).replace('.', '/');
		}
		this.includeName = includeName;
		registeredExternalClasses = new HashSet<String>();
		registeredInternalClasses = new HashSet<String>();
		registeredExternalInterfaces = new HashSet<String>();
		registeredInternalInterfaces = new HashSet<String>();
		registeredSkelObjs = new TreeSet<SkelItem>();
		classShortToLong = new HashMap<String,String>();
		classLongToShort = new HashMap<String,String>();
	}
	

	public void registerClassName(String fullName) {
		String newClassShortName = getDefaultShortPascalName(fullName);
	    String prevVal = classShortToLong.put(newClassShortName,fullName);
	    while (prevVal != null) {
	    	// remove the old shortname item
	    	classShortToLong.remove(newClassShortName);
	    	// make the two shortnames unique
	    	newClassShortName = "";
	    	String oldClassShortName = "";
	    	StringTokenizer name1tok = new StringTokenizer(fullName.replace("$", "__").replace('-', '_'), "/.");
	    	StringTokenizer name2tok = new StringTokenizer(prevVal.replace("$", "__").replace('-', '_'), "/.");
    		String name1elem = "";
    		String name2elem = "";
	    	// create new short names that contain the differing characters in
	    	// addition to just the first characters
	    	while (name1tok.hasMoreTokens()) {
	    		name1elem = name1tok.nextToken();
	    		name2elem = name2tok.nextToken();
	    		// ignore class name itself }
	    		if (!name1tok.hasMoreTokens() ||
	    				!name2tok.hasMoreTokens())
	    			break;
    			newClassShortName = newClassShortName + Character.toUpperCase(name1elem.charAt(0));
    			oldClassShortName = oldClassShortName + Character.toUpperCase(name2elem.charAt(0));
    			if (!name1elem.equals(name2elem)) {
    				int minlen = Math.min(name1elem.length(),name2elem.length());
    				for (int i = 1; i < minlen; i++) {
    					if (name1elem.charAt(i) != name2elem.charAt(i)) {
    						newClassShortName = newClassShortName + name1elem.charAt(i);
    						oldClassShortName = oldClassShortName + name2elem.charAt(i);
    					}
    				}
    				for (int i = minlen; i < name1elem.length(); i++)
						newClassShortName = newClassShortName + name1elem.charAt(i);
    				for (int i = minlen; i < name2elem.length(); i++)
    					oldClassShortName = oldClassShortName + name2elem.charAt(i);    					
    			}
	    	}
	    	newClassShortName = newClassShortName + name1elem.replace('.','_');
	    	oldClassShortName = oldClassShortName + name2elem.replace('.', '_');
	    	assert (!newClassShortName.equals(oldClassShortName));
	    	// return the previous shortname, ignore
	    	classLongToShort.put(prevVal,oldClassShortName);
	    	prevVal = classShortToLong.put(oldClassShortName,prevVal);
	    	// we don't support a collision for the new shortname of the old class
	    	assert(prevVal==null);
	    	prevVal = classShortToLong.put(newClassShortName,fullName);
	    }
	    classLongToShort.put(fullName,newClassShortName);
	}
	
	private String getDefaultShortPascalName(String classname) {
		String shortname = PascalClassData.getShortClassName(env,classname);
		// inner class -> done (no naming problems)
		if (classname.indexOf('$') != -1)
			return env.prefix_innerclass+shortname;

		// no package?
		if (shortname.equals(classname))
			return shortname;
		// add package component prefixes
		String pkgname = PascalClassData.getClassPackageName(classname);
		String prefix = Character.toString(Character.toUpperCase(classname.charAt(0)));
		for (int i = 1; i < pkgname.length(); i++)
			if ((pkgname.charAt(i) == '.'))
				prefix = prefix + Character.toUpperCase(pkgname.charAt(i+1));
        return prefix+shortname;
	}
	
	public boolean isExternalInnerClass(String className) {
		return registeredExternalClasses.contains(className) || registeredExternalInterfaces.contains(className);
	}
	
	public String getShortPascalName(String className) {
		if (PascalClassData.isInnerClass(className)) {
			int nestedIndex = className.indexOf('$');
			// get the abbreviated Pascal name of the top-level class, followed by
			// env.prefix_innerclass + the nested class names (to avoid identifier conflicts)
			String res = getShortPascalName(className.substring(0,nestedIndex));
			// create valid identifier for inner classes that only exist in external version
			if (isExternalInnerClass(className)) {
				res = res + className.substring(nestedIndex);
				res = res.replace('.', '_').replace("$","__");
			} else {
				StringTokenizer innerTypes = new StringTokenizer(className.substring(nestedIndex+1), "$");
				while (innerTypes.hasMoreTokens()) {
					res = res + "." + env.prefix_innerclass+innerTypes.nextToken();
				}
			}
			return res;
		}
		else className = className.replace("$", "__");
		String res = classLongToShort.get(className.replace('.','/'));
		// happens with one class in classes.jar: com.sun.org.apache.xalan.internal.xsltc.compiler.CUP$XPathParser$actions
		// unlike what the name suggests, it's not an inner class, and com.sun.org.apache.xalan.internal.xsltc.compiler.CUP
		// does not exist
		if (res == null)
			res = getDefaultShortPascalName(className);
		return res;
	}
	
	public void registerInnerClassAsExternalClass(String className) {
		if (!registeredExternalClasses.contains(className)) {
			registerClassName(className);
			registeredExternalClasses.add(className);
		}
	}

	/**
	 * 
	 * @param currentName name to check
	 * @param currentPrefix if prefix ends in '.' or '/', assumed to be package, otherwise class
	 * @return whether currentName is a class inside currentPrefix
	 */
	public static boolean classOrPackageInPrefix(String currentName, String currentPrefix) {
		boolean res = currentName.startsWith(currentPrefix);
		char lastPrefixChar = currentPrefix.charAt(currentPrefix.length()-1);
		if ((lastPrefixChar != '.') &&
				(lastPrefixChar != '/')) {
			res &=
				(currentName.length() == currentPrefix.length()) ||
				((currentName.length() > currentPrefix.length()) &&
						(currentName.charAt(currentPrefix.length()) == '$'));
		}
		return res;
	}
	
		
	public void registerUsedClass(String className) {
		className = className.replace('.','/');
		PascalClassData classData;
		boolean isLocal;
		boolean isSkel;
		
		isLocal = false;
		isSkel = false;
		// first check for skeleton classes/packages
		for (int i = 0; i < skelPrefixes.length; i++) {
			if (classOrPackageInPrefix(className,skelPrefixes[i])) {
				isSkel = true;
				break;
			}
		}
		if (!isSkel) {
			// we cannot create forward definitions in the global scope for nested classes
			if (PascalClassData.isInnerClass(className))
				return;
			
			// check whether we should fully print it; if not,
			// declare as anonymous external
			for (int i = 0; i < pkgPrefixes.length; i++) {
				if (classOrPackageInPrefix(className,pkgPrefixes[i])) {
					boolean excluded = false;
					// then excluded
					for (int j = 0; j < excludePrefixes.length; j++) {
						if (classOrPackageInPrefix(className,excludePrefixes[j])) {
							excluded = true;
							break;
						}
					}
					if (!excluded) {
						isLocal = true;
						break;
					}
				}
			}
		}
		StringTokenizer classComponents = new StringTokenizer(className,"$");
		String completeName = "";
		
		if (isSkel) {
			do {
				SkelItem item;
				boolean isClass;
				completeName += classComponents.nextToken();
				item = new SkelItem(completeName, "");
				classData = new PascalClassData(env.getFileInputStream(completeName),null,env,false);
				if (registeredSkelObjs.contains(item)) {
					completeName += "$";
					continue;
				}
				isClass = classData.isClass();
				item.kind = isClass?"class":"interface";
				registeredSkelObjs.add(item);
				completeName += "$";
			} while (classComponents.hasMoreTokens());
		} else {
			Set<String> pickedSet = null;
			do {
				completeName += classComponents.nextToken();
				if (isLocal) {
					if (registeredInternalClasses.contains(completeName) ||
							registeredInternalInterfaces.contains(completeName))
						continue;
					if (pickedSet == null) {
						classData = new PascalClassData(env.getFileInputStream(completeName),null,env,false);
						if (classData.isClass()) 
							pickedSet = registeredInternalClasses;
						else
							pickedSet = registeredInternalInterfaces;
					}
				} else {
					if (registeredInternalClasses.contains(completeName) ||
							registeredInternalInterfaces.contains(completeName))
						continue;
					if (pickedSet == null) {
						classData = new PascalClassData(env.getFileInputStream(completeName),null,env,false);
						if (classData.isClass())
							pickedSet= registeredExternalClasses;
						else
							pickedSet = registeredExternalInterfaces;
					}
				}
				pickedSet.add(completeName);
				completeName += "$";
			} while (classComponents.hasMoreTokens());
		}
	}
	
	public boolean parentIsKnownInterface(String className) {
		className = className.substring(0,className.lastIndexOf('$'));
		className = className.replace('.','/');
		return
		  registeredInternalInterfaces.contains(className) ||
		  registeredExternalInterfaces.contains(className);
	}
	
	public static void printArrayTypes(PrintWriter out, String prefix, String shortName, String shortSafeName) {
		out.println(prefix+"Arr1"+shortName+" = array of "+shortSafeName+";");
		out.println(prefix+"Arr2"+shortName+" = array of Arr1"+shortName+";");
		out.println(prefix+"Arr3"+shortName+" = array of Arr2"+shortName+";");
	}
	
	private void printArrayTypes(String prefix, String shortname) {
		printArrayTypes(unitFile,prefix,shortname, shortname);
	}
	
	private String RealPkgName(String name) {
		if (name.equals("./"))
			return "<nameless package>";
		else
			return name.replace('/', '.');
	}
	
	private void printInternalObjs(Enumeration<String> iterator, String kind) {
		while (iterator.hasMoreElements()) {
			String curClass = iterator.nextElement();
			unitFile.println("  "+getShortPascalName(curClass)+" = "+kind+";");
	    	// create formal array types for array parameters
			printArrayTypes("  ",getShortPascalName(curClass));
	        unitFile.println();
		}
	}
	
	/**
	 * @param iterator
	 */
	private void printExternalObjs(Enumeration<String> iterator, String kind) {
		while (iterator.hasMoreElements()) {
			String curClass = iterator.nextElement();
			String shortPascalName = getShortPascalName(curClass);
			String shortExternalName = PascalClassData.getExternalClassName(curClass);
			String pkgExternalName = PascalClassData.getClassPackageName(curClass).replace('/', '.');
			unitFile.println("  "+shortPascalName+" = "+kind+" external '"+pkgExternalName+"' name '"+shortExternalName+"';");
	    	// create formal array types for array parameters
			printArrayTypes("  ",shortPascalName);
	        unitFile.println();
		}
	}
	
	private void printSkelObjs(Enumeration<SkelItem> iterator) {
		String prefix="  ";
		ArrayList<String> nestedClasses = new ArrayList<String>();
		String curClass = " ";
		if (!iterator.hasMoreElements())
			return;
		while (iterator.hasMoreElements()) {
			SkelItem curSkelItem = iterator.nextElement();
			curClass = curSkelItem.className;
			String shortPascalName;
			String shortExternalName = PascalClassData.getExternalClassName(curClass);
			String pkgExternalName = PascalClassData.getClassPackageName(curClass).replace('/', '.');

			// finish earlier nested types if needed
			if (nestedClasses.size()>0) {
				while ((nestedClasses.size()>0) &&
						!curClass.startsWith(nestedClasses.get(nestedClasses.size()-1))) {
					String finishingName = nestedClasses.get(nestedClasses.size()-1);
					// remove added '$' again
					finishingName = finishingName.substring(0,finishingName.length()-1);
					if (nestedClasses.size()>1) {
						finishingName = PascalClassData.getShortClassName(env,finishingName);
					} else {
						finishingName = PascalClassData.getShortPascalClassName(finishingName);
					}
					unitFile.println(prefix+"end;");
					printArrayTypes(prefix,finishingName);
					nestedClasses.remove(nestedClasses.size()-1);
					if (prefix.length()>4)
						prefix = prefix.substring(4);
				}
			}
			if (nestedClasses.size()>0) {
				unitFile.println(prefix+"  type");
				prefix = prefix + "    ";
				shortPascalName = PascalClassData.getShortClassName(env,curClass);
			} else {
				shortPascalName = PascalClassData.getShortPascalClassName(curClass);
		        unitFile.println();
			}
			unitFile.println(prefix+shortPascalName+" = "+curSkelItem.kind+" external '"+pkgExternalName+"' name '"+shortExternalName+"'");
			// make sure we only match inner classes, not classes that start with the word in the current package
			nestedClasses.add(curClass+"$");
		}
		while (nestedClasses.size()>0) {
			String finishingName = nestedClasses.get(nestedClasses.size()-1);
			// remove added '$' again
			finishingName = finishingName.substring(0,finishingName.length()-1);
			if (nestedClasses.size()>1) {
				finishingName = PascalClassData.getShortClassName(env,finishingName);
			} else {
				finishingName = PascalClassData.getShortPascalClassName(finishingName);
			}
			unitFile.println(prefix+"end;");
			printArrayTypes(prefix,finishingName);
			nestedClasses.remove(nestedClasses.size()-1);
			if (prefix.length()>4)
				prefix = prefix.substring(4);
		}
	}

	public void printUnit() {
		Enumeration<String> strIterator;
		Enumeration<SkelItem> skelIterator;
		
		unitFile.print("{ Imports for Java packages/classes: "+RealPkgName(pkgPrefixes[0]));
		for (int i = 1; i < pkgPrefixes.length; i++) {
			unitFile.print(", "+RealPkgName(pkgPrefixes[i]));
		}
		unitFile.println(" }");
		if (!env.generateInclude) {
			unitFile.println("unit "+env.outputName+";");
			unitFile.println("{$mode delphi}");
			unitFile.println();
			unitFile.println("interface");
			unitFile.println();
		}
		unitFile.println("type");
		// forward declaration for all classes/interfaces in this package
		strIterator = Collections.enumeration(registeredInternalClasses);
		printInternalObjs(strIterator,"class");
		strIterator = Collections.enumeration(registeredInternalInterfaces);
		printInternalObjs(strIterator,"interface");
		// anonymous external declaration for all classes/interfaces from other packages 
		strIterator = Collections.enumeration(registeredExternalClasses);
		printExternalObjs(strIterator,"class");
		strIterator = Collections.enumeration(registeredExternalInterfaces);
		printExternalObjs(strIterator,"interface");
		skelIterator = Collections.enumeration(registeredSkelObjs);
		printSkelObjs(skelIterator);
		unitFile.println();
		if (!env.generateInclude) {
			unitFile.println("{$include "+includeName+"}");
			unitFile.println();
			unitFile.println("implementation");
			unitFile.println();
			unitFile.println("end.");
		}
	}


}
