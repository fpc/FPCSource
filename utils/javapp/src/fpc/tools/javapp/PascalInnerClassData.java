package fpc.tools.javapp;

public class PascalInnerClassData extends InnerClassData {

    public PascalInnerClassData(ClassData cls) {
        super(cls);
    }
    
	public boolean isStatic() {
		return (access & ACC_STATIC) != 0;
	}
}
