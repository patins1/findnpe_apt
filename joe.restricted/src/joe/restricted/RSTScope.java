package joe.restricted;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.IBinding;

public class RSTScope {

	public Map<IBinding, SolidityInfo> varDeclToLastValue = new HashMap<IBinding, SolidityInfo>();
	public Map<IBinding, SolidityInfo> varDeclToLastValueNeg = new HashMap<IBinding, SolidityInfo>();

	public void assign(RSTScope scope) {
		varDeclToLastValue = scope.varDeclToLastValue;
		varDeclToLastValueNeg = scope.varDeclToLastValueNeg;
		duplicate();
	}

	public void assignNegated(RSTScope scope) {
		varDeclToLastValue = scope.varDeclToLastValueNeg;
		varDeclToLastValueNeg = scope.varDeclToLastValue;
		duplicate();
	}

	private void duplicate() {
		if (varDeclToLastValue != null) {
			varDeclToLastValue = new HashMap<IBinding, SolidityInfo>(varDeclToLastValue);
		}
		if (varDeclToLastValueNeg != null) {
			varDeclToLastValueNeg = new HashMap<IBinding, SolidityInfo>(varDeclToLastValueNeg);
		}
	}

	public void clear() {
		varDeclToLastValue.clear();
		varDeclToLastValueNeg.clear();
	}

	public SolidityInfo getValueFor(IBinding typeBinding, boolean positive) {
		if (positive) {
			return varDeclToLastValue == null ? null : varDeclToLastValue.get(typeBinding);
		}
		return varDeclToLastValueNeg == null ? null : varDeclToLastValueNeg.get(typeBinding);
	}

}
