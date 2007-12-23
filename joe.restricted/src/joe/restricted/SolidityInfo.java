package joe.restricted;

import org.eclipse.jdt.core.dom.ASTNode;

public class SolidityInfo {

	private ASTNode node;

	private ASTNode reason;

	private SolidityType type;

	public enum SolidityType {
		MayBeNull, IsNull, IsNotNull, IsNotNullRec
	};

	public SolidityInfo(ASTNode node, SolidityType type, ASTNode reason) {
		this.node = node;
		this.type = type;
		this.reason = reason;
	}

	public SolidityInfo(ASTNode node, SolidityType type) {
		this.node = node;
		this.type = type;
	}
	

	public SolidityType getType() {
		return type;
	}

	public ASTNode getNode() {
		return node;
	}
	
	public ASTNode getReason() {
		return reason;
	}
}
