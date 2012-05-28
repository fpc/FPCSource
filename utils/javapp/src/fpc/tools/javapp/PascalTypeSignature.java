package fpc.tools.javapp;

import java.util.Vector;

public class PascalTypeSignature extends TypeSignature {
	
    private java.util.Set<ParaFlags> paraFlags;
	private JavapEnvironment env;

	public enum ParaFlags {
	    // use open arrays rather than dynamic arrays for array parameters
	    OpenArrays,
	    // when creating open array parameters, declare them as "const" rather than var
	    // (done for constructors, under the assumption that these won't change the
	    //  incoming data)
	    OpenConstArrays,
	    // translate array parameters into var parameters of single elements
	    SingleVar
	}
	
	public PascalTypeSignature(JavapEnvironment env, String JVMSignature, ClassData cls, java.util.Set<ParaFlags> paraFlags) {
	    this.env = env;
	    this.paraFlags=paraFlags;
		init(JVMSignature);
	}

    /**
     * Returns Pascal type signature for a base type.
     */
    public String getBaseType(String baseType){
        if(baseType != null){
            if(baseType.equals("B")) return "jbyte";
            else if(baseType.equals("C")) return "jchar";
            else if(baseType.equals("D")) return "jdouble";
            else if(baseType.equals("F")) return "jfloat";
            else if(baseType.equals("I")) return "jint";
            else if(baseType.equals("J")) return "jlong";
            else if(baseType.equals("S")) return "jshort";
            else if(baseType.equals("Z")) return "jboolean";
            else if(baseType.equals("V")) return "";
        }
        return null;
    }


    /**
     * Returns Pascal type signature for a object type.
     */
    public String getObjectType(String JVMobjectType) {
        String objectType = super.getObjectType(JVMobjectType).replace('.','/');
        if (objectType != null) {
        	if (!PascalClassData.currentUnit.isExternalInnerClass(objectType)) {
        		objectType = PascalClassData.getFullPascalClassName(objectType);
        	} else
        		objectType = PascalClassData.getShortPascalClassName(objectType);
        }
        if (PascalKeywords.isPascalKeyword(objectType))
        	objectType = "&" + objectType;
        return objectType;
    }

    /**
     * Returns Pascal type signature for array type.
     */
    public String getArrayType(String arrayType) {
        if(arrayType != null){
        	int dimCount = 0;

            for (int i = 0; i < arrayType.length(); i++) {
            	if (arrayType.charAt(i) != '[') {
            		arrayType = arrayType.substring(i);
            		break;
            	}
            	dimCount++;
            }

            String componentType = "";
    		String outerClass = "";
            if(arrayType.startsWith("L")){
            	String baseType = arrayType.substring(1, arrayType.length()-1);
            	if (PascalClassData.isInnerClass(baseType) &&
            			!PascalClassData.currentUnit.isExternalInnerClass(baseType)) {
            		int index = baseType.lastIndexOf('$');
            		outerClass = PascalClassData.getShortPascalClassName(baseType.substring(0,index))+".";
            		componentType = env.prefix_innerclass+baseType.substring(index+1).replace('$', '.'); 
            	} else {
            		outerClass = "";
                	componentType = PascalClassData.getShortPascalClassName(baseType);
            	}
            }else {
                componentType = getBaseType(arrayType);
            }
            if ((!paraFlags.contains(ParaFlags.OpenArrays) &&
                    !paraFlags.contains(ParaFlags.SingleVar)) ||
            		(dimCount>1))
            	return outerClass+"Arr"+dimCount+componentType;
            else
            	return "array of "+outerClass+componentType;
        }
        return null;
    }

    
    protected String parameterSignatureFromParameters(Vector<String> parameters){
        /* number of arguments of a method.*/
        argumentlength =  parameters.size();
        /* Pascal type signature.*/
        StringBuilder parametersignature = new StringBuilder("(");
        int i;
        
        for(i = 0; i < argumentlength; i++){
        	String paraType = (String)parameters.elementAt(i);
        	// contents of open arrays could be changed -> var parameters
        	if (paraType.contains("array of")) {
                if (paraFlags.contains(ParaFlags.SingleVar))
                {
                    parametersignature.append("var ");
                    paraType = paraType.substring(paraType.indexOf("array of ")+"array of ".length());
                }
                else if (!paraFlags.contains(ParaFlags.OpenConstArrays))
        		  parametersignature.append("var ");
        		else
          		  parametersignature.append("const ");
        	}
        	parametersignature.append("para");
        	parametersignature.append(i+1);
        	parametersignature.append(": ");
        	parametersignature.append(paraType);
            if(i != parameters.size()-1){
                parametersignature.append("; ");
            }
        }
        parametersignature.append(")");
        return parametersignature.toString();
    }
    
}
