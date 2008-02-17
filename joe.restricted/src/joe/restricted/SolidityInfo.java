package joe.restricted;

import org.eclipse.jdt.core.dom.ASTNode;

public class SolidityInfo {

	@Override
	public String toString() {
		return "" + type;
	}

	private ASTNode node;

	private ASTNode reason;

	private SolidityType type;

	public enum SolidityType {
		MayBeNull, IsNull, IsNotNull// , IsNotNullRec
		;

		public boolean canNotBeNullRec() {
			return this == IsNotNull;
		}

		public boolean canNotBeNull() {
			return this == IsNotNull || this == getIsNotNullRec();
		}

		static SolidityType getIsNotNullRec() {
			return IsNotNull;
		}
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
