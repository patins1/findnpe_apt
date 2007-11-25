package joe.restricted;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.IBinding;

public class RSTScope {

	public Map<IBinding, SolidityInfo> varDeclToLastValue = new HashMap<IBinding, SolidityInfo>();
	public Map<IBinding, SolidityInfo> varDeclToLastValueNeg = new HashMap<IBinding, SolidityInfo>();

	public void assign(RSTScope scope) {
		this.varDeclToLastValue.clear();
		varDeclToLastValue.putAll(scope.varDeclToLastValue);
		this.varDeclToLastValueNeg.clear();
		varDeclToLastValueNeg.putAll(scope.varDeclToLastValueNeg);
	}
	
	public void assignNegated(RSTScope scope) {
		this.varDeclToLastValue.clear();
		varDeclToLastValue.putAll(scope.varDeclToLastValueNeg);
		this.varDeclToLastValueNeg.clear();
		varDeclToLastValueNeg.putAll(scope.varDeclToLastValue);
		
	}

	public void clear() {
		varDeclToLastValue.clear();
		varDeclToLastValueNeg.clear();
	}

	public SolidityInfo getValueFor(IBinding typeBinding, boolean positive) {
		if (positive) {
			return varDeclToLastValue.get(typeBinding);
		}
		return varDeclToLastValueNeg.get(typeBinding);
	}

}
