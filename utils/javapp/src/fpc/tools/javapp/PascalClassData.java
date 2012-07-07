package fpc.tools.javapp;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.StringTokenizer;
import java.util.Vector;

import fpc.tools.javapp.JavapEnvironment;

public class PascalClassData extends ClassData {

	public PascalClassData outerClass;
	private HashSet<String> nestedDependencies;
	boolean setOuterDependencies; 
	
	static public PascalUnit currentUnit; 
	
	static final String[] nonNestedDollarClasses;
	

	
	public PascalClassData(InputStream infile, PascalClassData outerClass, JavapEnvironment env, boolean doCollectDependencies) {
		super (env,infile);
		this.outerClass = outerClass;
		this.nestedDependencies = new HashSet<String>();
		ClassIdentifierInfo.registerClassInfo(getClassName(),getSuperClassName(),getSuperInterfaces());
		if (doCollectDependencies) {
			collectDependencies();
		}
	}

	/**
	 * 
	 */
	private void collectDependencies() {
		if (this.outerClass != null) {
			HashSet<String> myDeps = getDependencies();
			String mySuperClass = getSuperClassName();
			boolean foundMatch = false;
			String [] interfaces = getSuperInterfaces();
			boolean intfMatches[] = new boolean[interfaces.length];
			PascalClassData outerMostClass = this;
			while (outerMostClass.outerClass != null) {
				/**
				 *  adjust dependencies for inner types, e.g.
				 *  CSNHAuthenticator = class abstract external 'com.sun.net.httpserver' name 'Authenticator' (JLObject)
				 *  type
				 *    Retry = class external 'com.sun.net.httpserver' name 'Authenticator$Retry' (Result)
				 *    ..
				 *    end;
				 *    
				 *    Result = class abstract external 'com.sun.net.httpserver' name 'Authenticator$Result' (JLObject)
				 *    ..
				 *    end;
				 *    
				 *  end;
				 *    -> Retry must depend on Result
				 */
				String outerClassName = outerMostClass.outerClass.getClassName();
				if (!foundMatch &&
						mySuperClass.startsWith(outerClassName) &&
						!mySuperClass.equals(outerClassName)) {
					foundMatch = true;
					outerMostClass.addNestedDepdency(mySuperClass);
				}
				for (int i = 0; i < interfaces.length; i++) {
					if (!intfMatches[i]) {
						String intf = interfaces[i];
						if (intf.startsWith(outerClassName) &&
								!intf.equals(outerClassName)) {
							intfMatches[i] = true;
							outerMostClass.addNestedDepdency(intf);
						}
					}
				}
				outerMostClass = outerMostClass.outerClass;
			}
			myDeps.remove(outerMostClass.getMasterClassName());
			outerMostClass.addNestedDepdencies(myDeps);
		}
	}
	
	public void addNestedDepdencies(HashSet<String> nestedDeps) {
		this.nestedDependencies.addAll(nestedDeps);
	}
	
	public void addNestedDepdency(String nestedDep) {
		this.nestedDependencies.add(nestedDep);
	}

/*	
	public String getSafeMethodIdentifer(String id) {
		if (id.charAt(0) == '<')
			return id;
		id = PascalKeywords.escapeIfPascalKeyword(id);
		String testName = id.toUpperCase();
		while (usedIdentifiers.contains(testName)) {
			id = id + "_";
			testName = testName + "_";
		}
		if (id.indexOf('$') != -1) {
			System.out.println("  Warning, cannot represent identifier '"+id+"', hiding");
			id = id.replace("$","__");
		}
		return id;
	}
*/
	public boolean isStatic() {
		return (access & ACC_STATIC) != 0;
	}

	public static boolean isInnerClass(String className) {
		if (className.indexOf('$') == -1)
			return false;
		className = className.replace('.', '/');
		for (String name : nonNestedDollarClasses) {
			if (className.equals(name))
				return false;
		}
		return true;
	}
	
	public boolean isInnerClass() {
		return outerClass != null;
	}
	
	public static String getShortClassName(JavapEnvironment env, String className) {
		int index;
		className = className.replace('-', '_');
		if (isInnerClass(className)) {
			index=className.lastIndexOf("$")+1;
			return env.prefix_innerclass+className.substring(index);
		}
		else
			className = className.replace("$","__");
        index=className.lastIndexOf("/")+1;
        if (index==0) {
        	index=className.lastIndexOf(".")+1;
        }
        if (index!=0) {
            return className.substring(index);
        }
        return className;
	}

	/**
	 * For a non-nested class, returns the name of the class itself
	 * For a nested class, returns the name of the outermost class
	 * 
	 */
	public static String getMasterClassName(String className) {
		int index;
//		className = className.replace('-', '_');
		if (!isInnerClass(className.replace('.','/')))
			return className;
        index=className.indexOf('$');
        if (index!=-1) {
            return className.substring(0,index);
        }
        return className;
	}

	public static String getShortPascalClassName(String className) {
		currentUnit.registerUsedClass(className);
		return currentUnit.getShortPascalName(className);

		/*
		
		String shortname = getShortClassName(classname);
		// inner class -> done (no naming problems)
		if (classname.indexOf('$') != -1)
			return shortname;

		// no package?
		if (shortname.equals(classname))
			return shortname;
		// add package component prefixes
		String pkgname = getClassPackageName(classname);
		String prefix = Character.toString(classname.charAt(0)).toUpperCase();
		for (int i = 1; i < pkgname.length(); i++)
			if ((pkgname.charAt(i) == '.'))
				prefix = prefix + Character.toString(pkgname.charAt(i+1)).toUpperCase();
        return prefix+shortname;
*/
	}
	
	
	public static String getClassPackageName(String className) {
		int index;
      	index=className.lastIndexOf("/");
        if (index==-1) {
        	index=className.lastIndexOf(".");
        }
        if (index!=-1) {
            return className.substring(0,index).replace('/', '.');
        }
        return null;
	}
	
	public static String getFullPascalClassName(String className) {
		return getShortPascalClassName(className);
/*
		if (isInnerClass(className)) {
			int nestedIndex = className.indexOf('$');
			String res = getShortPascalClassName(className.substring(0,nestedIndex));
			StringTokenizer innerTypes = new StringTokenizer(className.substring(nestedIndex+1), "$");
			while (innerTypes.hasMoreTokens()) {
				res = res + "." + PascalKeywords.escapeIfPascalKeyword(innerTypes.nextToken());
			}
			return res;
		} else
			return getShortPascalClassName(className);
*/			
	}
	
	public static String getExternalClassName(String className) {
		int index = className.lastIndexOf('/');
		if (index != -1) {
			return className.substring(index+1);
		}
		else
			return className;
	}

	/* returns the name of the class according to the JVM
	 * (in case of nested class: just the name of the nested
	 * class, ignoring parent classes)
	 */
	public static String getExternalShortClassName(String className) {
		int index;
		
		if (isInnerClass(className)) {
			index=className.lastIndexOf("$")+1;
			return className.substring(index);
		}
		return getExternalClassName(className);
	}

	public String getShortClassName() {
		return getShortClassName(env,getClassName());
    }
	
	public String getShortPascalClassName() {
		return getShortPascalClassName(getClassName());
	}
	
	public String getClassPackageName() {
		return getClassPackageName(getClassName());
    }
	
	public String getFullPascalClassName() {
		return getFullPascalClassName(getClassName());
	}

	public String getMasterClassName() {
		return getMasterClassName(getClassName());
	}
	
	public String getExternalClassName() {
		return getExternalClassName(getClassName());
	}

	public String getExternalShortClassName() {
		return getExternalShortClassName(getClassName());
	}	
	
	public String[] getPascalSuperInterfaces(){
		String[] res = super.getSuperInterfaces();
		for (int i=0; i<res.length; i++)
			res[i] = getFullPascalClassName(res[i]);
		return res;
	}	
    /**
     * Returns the visibility of this class or interface.
     */
    public String getVisibilitySectionName(){
    	if (isInnerClass()) {
    		if (isPublic()) {
        		return "public";
        	}
    		else {
    			return "strict private";
    		}
    	}
    	else {
    		return null;
    	}
    }

	
    public String[] getModifiers(){
        Vector<String> v = new Vector<String>();
        if ((access & ACC_FINAL)    !=0) v.addElement("sealed");
        if ((access & ACC_ABSTRACT) !=0) v.addElement("abstract");
        String[] accflags = new String[v.size()];
        v.copyInto(accflags);
        return accflags;
    }

    
    protected void readFields(DataInputStream in) throws IOException {
        int fields_count = in.readUnsignedShort();
        fields=new FieldData[fields_count];
        for (int k = 0; k < fields_count; k++) {
            FieldData field=new PascalFieldData(env,this);
            field.read(in);
            fields[k]=field;
        }
    }

    
    /**
     * Reads and stores Method info.
     */
    protected void readMethods(DataInputStream in) throws IOException {
        int methods_count = in.readUnsignedShort();
        methods=new PascalMethodData[methods_count];
        for (int k = 0; k < methods_count ; k++) {
            MethodData method=new PascalMethodData(env,this);
            method.read(in);
            methods[k]=method;
        }
    }

    /**
     * Returns string at that index.
     */
    public String StringValue(int cpx) {
        if (cpx==0) throw new ClassFormatError("Invalid CP index: 0");
        int tag;
        Object x;
        try {
            tag=tags[cpx];
            x=cpool[cpx];
        } catch (IndexOutOfBoundsException e) {
        	throw new ClassFormatError("<Incorrect CP index:"+cpx+">");
        }

        if (x==null) return "nil";
        switch (tag) {
        case CONSTANT_UTF8: {
            StringBuffer sb=new StringBuffer();
            String s=(String)x;
            sb.append('\'');
            for (int k=0; k<s.length(); k++) {
                char c=s.charAt(k);
                if (c == '\'')
                	sb.append("''");
                else if ((c >= ' ') &&
                		(c <= '~'))
                	sb.append(c);
                else
                	sb.append("'#$"+String.format("%04x",(int)c)+"'");
            }
            sb.append('\'');
            return sb.toString();
        }
        case CONSTANT_DOUBLE: {
            Double d=(Double)x;
            String sd=d.toString();
            sd = sd.replace("Infinity", "1.0/0.0").replace("NaN", "0.0/0.0");
            return "jdouble("+sd+")";
        }
        case CONSTANT_FLOAT: {
            Float f=(Float)x;
            String sf=(f).toString();
            sf = sf.replace("Infinity", "1.0/0.0").replace("NaN", "0.0/0.0");
            return "jfloat("+sf+")";
        }
        case CONSTANT_LONG: {
            Long ln = (Long)x;
            return "jlong("+ln.toString()+')';
        }
        case CONSTANT_INTEGER: {
            Integer in = (Integer)x;
            return in.toString();
        }
        case CONSTANT_CLASS:
            return javaName(getClassName(cpx));
        case CONSTANT_STRING:
            return StringValue(((CPX)x).cpx);
        case CONSTANT_FIELD:
        case CONSTANT_METHOD:
        case CONSTANT_INTERFACEMETHOD:
            //return getShortClassName(((CPX2)x).cpx1)+"."+StringValue(((CPX2)x).cpx2);
             return javaName(getClassName(((CPX2)x).cpx1))+"."+StringValue(((CPX2)x).cpx2);

        case CONSTANT_NAMEANDTYPE:
        	throw new ClassFormatError("Don't try to print out field/method/interfacemethod constant values");
        default:
        	throw new ClassFormatError("Unknown constant tag: "+tag);
        }
    }
    

    protected InnerClassData NewInnerClassData() {
    	return new PascalInnerClassData(this);
    }
    
    public HashSet<String> getDependencies() {
    	HashSet<String> res = new HashSet<String>();
    	// inheritance dependencies (superclass and implemented interfaces)
    	String superClass = getSuperClassName();
    	if (superClass != null)
    		res.add(getMasterClassName(superClass));
    	String []interfacelist = getSuperInterfaces();
    	for (String intf : interfacelist)
    		res.add(getMasterClassName(intf));
    	/** most dependencies in parameters/result types/field types can be
    	 *  handled via forward declarations, but there is one exception:
    	 *  dependencies on nested classes declared in another class
    	 */
    	// field types
    	FieldData[] fields = getFields();
    	for (FieldData field : fields) {
    		if (TypeSignature.checkAccess(field.getAccess(),env)) {
    			String fieldType = ((PascalFieldData)field).getRawBaseType();
    			if (isInnerClass(fieldType))
    				res.add(getMasterClassName(fieldType.replace('.', '/')));
    		}
    	}
    	
    	// method parameter/return types
    	MethodData[] methods = getMethods();
    	for (MethodData method : methods) {
    		if (TypeSignature.checkAccess(method.getAccess(),env)) {
    			String retType = ((PascalMethodData)method).getRawBaseReturnType();
    			if (isInnerClass(retType))
    				res.add(getMasterClassName(retType.replace('.', '/')));
    			TypeSignature methodSig = new TypeSignature(method.getInternalSig());
    			Vector<String> paras = methodSig.getParametersList(methodSig.parameterdes);
    			for (int i = 0; i < paras.size(); i++) {
    				String paraType = paras.get(i);
    		        int arrPos = paraType.indexOf('[');
    		        if (arrPos != -1)
    		        	paraType = paraType.substring(0,arrPos);
    				if (isInnerClass(paraType))
    					res.add(getMasterClassName(paraType.replace('.', '/')));
    			}
    		}
    	}
    	// dependencies added by nested classes (in case we are a top-level class/interface)
    	res.addAll(nestedDependencies);
    	// remove ourselves
    	res.remove(getClassName());
    	return res;
    }
    
    
    static {
    	nonNestedDollarClasses = new String[1];
    	nonNestedDollarClasses[0] = "com/sun/org/apache/xalan/internal/xsltc/compiler/CUP$XPathParser$actions";    	
    }
}
