package fpc.tools.javapp;

import java.io.DataInputStream;
import java.io.IOException;
import java.util.Vector;

public class PascalFieldData extends FieldData {
	
	private String cachedName;

	public PascalFieldData(JavapEnvironment env, ClassData cls){
       super(env, cls);
    }
	
    public String getVisibilitySectionName(){
        if ((access & ACC_PUBLIC)   !=0) return "public";
        if ((access & ACC_PRIVATE)   !=0) return "strict private";
        if ((access & ACC_PROTECTED)   !=0) return "strict protected";
        /* package visibility = visible in this unit */
        return "private";
    }
    
    public boolean isFormalConst() {
    	for (int i=0; i < attrs.size(); i++)
            if(((AttrData)attrs.elementAt(i)).getAttrName().equals("ConstantValue"))
            	return true;
    	return false;
    }
    
    public String getModifiers() {
        Vector<String> v = new Vector<String>();
        if (isFormalConst()) return "const";
        else {
        	if ((access & ACC_FINAL)    !=0) v.addElement("final");
        	if ((access & ACC_STATIC)   !=0) v.addElement("class var");
        	else
        		v.addElement("var");
        }
        String res = v.elementAt(0);
        for (int i=1; i < v.size(); i++)
        	res=res+" "+v.elementAt(i);
        return res;
    }

    /**
     * Returns Pascal type signature of a field.
     */
    public String getType(){
        return new PascalTypeSignature(env,getInternalSig(),cls,java.util.EnumSet.noneOf(PascalTypeSignature.ParaFlags.class)).getFieldType();
    }

    /**
     * Returns Java type signature of a field.
     */
    public String getRawBaseType(){
        String res = new TypeSignature(getInternalSig()).getFieldType();
        int arrIndex = res.indexOf('[');
        if (arrIndex != -1)
        	res = res.substring(0,arrIndex);
        return res;
    }

    public String getName() {
    	if (cachedName == null) {
    		String realName = super.getName();
    		// we prepend an "f" for fields to prevent name clashes
    		if (!isFormalConst())
    			realName = env.prefix_field + realName;
    		else
    			realName = env.prefix_constant + realName;
    		cachedName = ClassIdentifierInfo.AddIdentifierNameForClass(cls.getClassName(),realName);    				
    	}
    	return cachedName;
    }

}
