package joe.restricted;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import joe.restricted.SolidityInfo.SolidityType;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.astview.GenericVisitor;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.AnnotationTypeDeclaration;
import org.eclipse.jdt.core.dom.AnnotationTypeMemberDeclaration;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.ArrayAccess;
import org.eclipse.jdt.core.dom.ArrayCreation;
import org.eclipse.jdt.core.dom.ArrayInitializer;
import org.eclipse.jdt.core.dom.ArrayType;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.ContinueStatement;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.EnumConstantDeclaration;
import org.eclipse.jdt.core.dom.EnumDeclaration;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IAnnotationBinding;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IMemberValuePairBinding;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InstanceofExpression;
import org.eclipse.jdt.core.dom.MarkerAnnotation;
import org.eclipse.jdt.core.dom.MemberValuePair;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.NormalAnnotation;
import org.eclipse.jdt.core.dom.NullLiteral;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.PackageDeclaration;
import org.eclipse.jdt.core.dom.ParameterizedType;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SimpleType;
import org.eclipse.jdt.core.dom.SingleMemberAnnotation;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.jdt.core.dom.SuperConstructorInvocation;
import org.eclipse.jdt.core.dom.SuperMethodInvocation;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.jdt.core.dom.ThrowStatement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.TypeLiteral;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.jdt.core.dom.WhileStatement;
import org.eclipse.jdt.core.util.IModifierConstants;
import org.eclipse.jdt.internal.core.hierarchy.TypeHierarchy;

@Solid
public class FindNPESourcesVisitor extends GenericVisitor {

	enum ValidationLevel {
		LocalVariables, QueryBased
	};

	/**
	 * If a node X is mapped to a non-null value Y, it means, that X can
	 * evaluate to <code>null</code> because Y can evaluate to
	 * <code>null</code>, but however X is required to evaluate to a non-null
	 * value.
	 */
	private List<SolidityInfo> mustBeNotNull = new ArrayList<SolidityInfo>();

	private Map<ASTNode, Object> constantValue = new HashMap<ASTNode, Object>();

	/**
	 * If a node X is mapped on null, it means, that X will never evaluate to
	 * <code>null</code>.
	 * 
	 * If a node X is mapped to a non-null value Y, it means, that X can
	 * evaluate to <code>null</code> because Y can evaluate to
	 * <code>null</code>.
	 * 
	 * If a node X is not mapped at all, it has been semantically not yet
	 * initialized.
	 */
	private Map<ASTNode, SolidityInfo> expressionsReturningNullableBecauseOf = new HashMap<ASTNode, SolidityInfo>();

	private Map<ASTNode, SolidityInfo> expressionsReturningNullableBecauseOfNegativated = new HashMap<ASTNode, SolidityInfo>();

	// private List<RSTScope> scopes = new ArrayList<RSTScope>();

	private Map<ASTNode, RSTScope> preserver = new HashMap<ASTNode, RSTScope>();

	private TypeHierarchy typeHierarchy;

	private IJavaProject aptProject;

	private List<ASTNode> performanceWarnings = new ArrayList<ASTNode>();
	private List<ASTNode> signatureProblems = new ArrayList<ASTNode>();

	protected boolean visitNode(ASTNode node) {
		preserver.put(node, new RSTScope());
		return true;
	}

	protected boolean processIfThenElse(ASTNode condition, ASTNode thenExpression, ASTNode elseExpression, ASTNode current) {
		return processIfThenElse(condition, thenExpression, elseExpression, current, true, true);
	}

	/**
	 * Calculates bindings for if-then-else logic.
	 * 
	 * @param condition
	 * @param thenExpression
	 *            if <code>null</code>, <code>true</code> is assumed
	 * @param elseExpression
	 *            if <code>null</code>, <code>true</code> is assumed
	 * @param current
	 * @return
	 */
	protected boolean processIfThenElse(ASTNode condition, ASTNode thenExpression, ASTNode elseExpression, ASTNode current, boolean posThen, boolean posElse) {
		ASTNode parent = condition.getParent();
		if (condition == current) {
			if (thenExpression != null) {
				// THEN-node will be visited next
				preserver.get(parent).assign(preserver.get(condition));
			} else {
				// ELSE-node will be visited next
				preserver.get(parent).assignNegated(preserver.get(condition));
			}
			return false;
		} else if (thenExpression == current) {
			if (elseExpression != null) {
				// ELSE-node will be visited next
				preserver.get(parent).assignNegated(preserver.get(condition));
				return false;
			} else {
				// see below
			}
		}

		// NO MORE CHILDREN WILL BE VISITED
		{

			Map<IBinding, SolidityInfo> thenScopePos = mergeSequence(getScope(condition, true), getScope(thenExpression, posThen));
			Map<IBinding, SolidityInfo> thenScopeNeg = mergeSequence(getScope(condition, true), getScope(thenExpression, !posThen));
			Map<IBinding, SolidityInfo> elseScopePos = mergeSequence(getScope(condition, false), getScope(elseExpression, posElse));
			Map<IBinding, SolidityInfo> elseScopeNeg = mergeSequence(getScope(condition, false), getScope(elseExpression, !posElse));

			Map<IBinding, SolidityInfo> ifPos = mergeAlternatives(thenScopePos, elseScopePos, parent.getParent());
			Map<IBinding, SolidityInfo> ifNeg = mergeAlternatives(thenScopeNeg, elseScopeNeg, parent.getParent());
			preserver.get(parent).varDeclToLastValue = ifPos;
			preserver.get(parent).varDeclToLastValueNeg = ifNeg;
			return false;
		}

	}

	private Map<IBinding, SolidityInfo> getScope(ASTNode thenExpression, boolean positive) {
		RSTScope result = preserver.get(thenExpression);
		if (result != null) {
			return positive ? result.varDeclToLastValue : result.varDeclToLastValueNeg;
		}
		return positive ? new HashMap<IBinding, SolidityInfo>() : null;
	}

	// Or-Executed: AssertStatement CatchClause
	// DoStatement ForStatement SwitchCase SwitchStatement EnhancedForStatement
	protected void endVisitNode(ASTNode current) {
		ASTNode parent = current.getParent();
		if (parent != null && preserver.get(current) != null && preserver.get(parent) != null) {
			if (parent instanceof IfStatement) {
				// IF a THEN b ELSE c = a ? b : c
				// IF a THEN b = a ? b : true
				IfStatement ifStatement = (IfStatement) parent;
				processIfThenElse(ifStatement.getExpression(), ifStatement.getThenStatement(), ifStatement.getElseStatement(), current);
			} else if (parent instanceof ConditionalExpression) {
				// a ? b : c = a ? b : c
				ConditionalExpression conditionalExpression = (ConditionalExpression) parent;
				processIfThenElse(conditionalExpression.getExpression(), conditionalExpression.getThenExpression(), conditionalExpression.getElseExpression(), current);
			} else if (parent instanceof WhileStatement) {
				// while expression do body = expression ? body : true
				WhileStatement whileStatement = (WhileStatement) parent;
				processIfThenElse(whileStatement.getExpression(), whileStatement.getBody(), null, current);
			} else if (parent instanceof InfixExpression) {
				InfixExpression infixExpression = (InfixExpression) parent;
				if (infixExpression.getOperator().toString().equals("&&")) {
					// a && b = a ? b : false
					processIfThenElse(infixExpression.getLeftOperand(), infixExpression.getRightOperand(), null, current, true, false);
				} else if (infixExpression.getOperator().toString().equals("||")) {
					// a || b = a ? true : b
					processIfThenElse(infixExpression.getLeftOperand(), null, infixExpression.getRightOperand(), current);
				} else if (infixExpression.getLeftOperand().resolveTypeBinding() != null && "boolean".equals(infixExpression.getLeftOperand().resolveTypeBinding().getName())) {
					if (infixExpression.getOperator().toString().equals("==")) {
						// a == b = a ? b : not b
						processIfThenElse(infixExpression.getLeftOperand(), infixExpression.getRightOperand(), infixExpression.getRightOperand(), current, true, false);
					} else if (infixExpression.getOperator().toString().equals("!=")) {
						// a != b = a ? not b : b
						processIfThenElse(infixExpression.getLeftOperand(), infixExpression.getRightOperand(), infixExpression.getRightOperand(), current, false, true);
					}
				}
			} else if (parent instanceof PrefixExpression) {
				PrefixExpression prefixExpression = (PrefixExpression) parent;
				if (prefixExpression.getOperator().toString().equals("!")) {
					preserver.get(prefixExpression).varDeclToLastValue = preserver.get(prefixExpression.getOperand()).varDeclToLastValueNeg;
					preserver.get(prefixExpression).varDeclToLastValueNeg = preserver.get(prefixExpression.getOperand()).varDeclToLastValue;
				}
			} else if (parent instanceof DoStatement) {
				DoStatement doStatement = (DoStatement) parent;
				if (doStatement.getBody() == current) {
					// EXPRESSION will be executed next
					preserver.get(doStatement).assign(preserver.get(doStatement.getBody()));
				} else {
					mergeSequence(preserver.get(doStatement), preserver.get(doStatement.getExpression()).varDeclToLastValueNeg);
				}
			} else if (parent instanceof ForStatement) {
				ForStatement forStatement = (ForStatement) parent;
				// TODO!

				// if (forStatement.getBody() == current) {
				// // all children processed
				// preserver.get(forStatement).clear();
				// mergeSequence(preserver.get(forStatement), forStatement);
				// mergeOptional(preserver.get(forStatement),
				// varDeclToLastValue);
				// mergeSequence(preserver.get(forStatement),
				// preserver.get(doStatement.getExpression()).varDeclToLastValueNeg);
				// } else {
				// }
			} else if (parent instanceof EnhancedForStatement) {
				EnhancedForStatement enhancedForStatement = (EnhancedForStatement) parent;

				if (enhancedForStatement.getExpression() == current) {
					// BODY will be visited next
					SimpleName simpleName = enhancedForStatement.getParameter().getName();
					IBinding typeBinding = simpleName.resolveBinding();
					if (typeBinding != null) {
						addBinding(enhancedForStatement, typeBinding, enhancedForStatement.getExpression());
					}
				} else if (enhancedForStatement.getBody() == current) {
					// all children visited
					preserver.get(enhancedForStatement).clear();
					mergeOptional(enhancedForStatement, preserver.get(enhancedForStatement.getBody()).varDeclToLastValue);
				}
			} else if (parent instanceof TryStatement) {
				TryStatement tryStatement = (TryStatement) parent;
				if (tryStatement.getFinally() == current) {
					// executed at any case
					mergeSequence(preserver.get(tryStatement), preserver.get(current).varDeclToLastValue);
				} else if (tryStatement.getBody() == current) {
					if (!tryStatement.catchClauses().isEmpty()) {
						mergeOptional(tryStatement, preserver.get(current).varDeclToLastValue);
					} else {
						mergeSequence(preserver.get(tryStatement), preserver.get(current).varDeclToLastValue);
					}
				} else {
					// catch
					mergeOptional(tryStatement, preserver.get(current).varDeclToLastValue);
				}
			} else if (parent instanceof InstanceofExpression || parent instanceof ParenthesizedExpression || parent instanceof VariableDeclarationStatement || parent instanceof VariableDeclarationExpression || parent instanceof Assignment
					|| parent instanceof ExpressionStatement || parent instanceof Block) {
				mergeSequence(preserver.get(parent), preserver.get(current).varDeclToLastValue);
			} else if (parent instanceof ThrowStatement || parent instanceof PostfixExpression || parent instanceof TypeLiteral || parent instanceof CastExpression || parent instanceof ReturnStatement || parent instanceof ArrayAccess || parent instanceof FieldAccess
					|| parent instanceof MethodInvocation || parent instanceof SuperMethodInvocation || parent instanceof SuperConstructorInvocation || parent instanceof ExpressionStatement || parent instanceof ArrayCreation || parent instanceof ArrayInitializer
					|| parent instanceof ClassInstanceCreation || parent instanceof VariableDeclarationFragment || parent instanceof SimpleType || parent instanceof ArrayType || parent instanceof ParameterizedType || parent instanceof QualifiedName
					|| parent instanceof MarkerAnnotation || parent instanceof SingleMemberAnnotation || parent instanceof AnnotationTypeMemberDeclaration || parent instanceof SingleVariableDeclaration || parent instanceof MethodDeclaration || parent instanceof FieldDeclaration
					|| parent instanceof TypeDeclaration || parent instanceof AnonymousClassDeclaration || parent instanceof AnnotationTypeDeclaration || parent instanceof CompilationUnit || parent instanceof PackageDeclaration || parent instanceof ImportDeclaration
					|| parent instanceof EnumConstantDeclaration || parent instanceof EnumDeclaration) {
				// nothing to pass to parent
			} else {
				System.err.println("Node type not handled: " + parent.getClass().getName() + "(line " + getSource(parent) + ") for child " + current.getClass().getName());
			}
		}
		if (!isDefined(current)) {
			if (current instanceof Expression && ((Expression) current).resolveTypeBinding() != null) {
				ITypeBinding tb = ((Expression) current).resolveTypeBinding();
				if (isPrimitiveType(tb)) {
					setCanBeNotNullRec(current);
					return;
				}
			}
			setCanBeNull(current, null);
		}
	}

	private boolean isPrimitiveType(ITypeBinding tb) {
		String n = tb.getName();
		return n.equals("byte") || n.equals("short") || n.equals("int") || n.equals("long") || n.equals("float") || n.equals("double") || n.equals("char") || n.equals("boolean");
	}

	static private String getSource(ASTNode parent) {
		if (parent.getRoot() instanceof CompilationUnit) {
			CompilationUnit cu = ((CompilationUnit) parent.getRoot());
			return "" + cu.getLineNumber(parent.getStartPosition()) + " in " + cu.getJavaElement().getPath();
		}
		return "";
	}

	static public int getLine(ASTNode parent) {
		if (parent.getRoot() instanceof CompilationUnit) {
			CompilationUnit cu = ((CompilationUnit) parent.getRoot());
			return cu.getLineNumber(parent.getStartPosition());
		}
		return -1;
	}

	/**
	 * Merges two alternative bindings.
	 * 
	 * Note: it is a little faster to pass an empty binding set as last
	 * parameter.
	 * 
	 * Note: Bindings in the first passed binding set have a higher probability
	 * to be included in the result binding set.
	 * 
	 * @param alternativeOne
	 * @param alternativeTwo
	 * @param parent
	 * @return
	 */
	private Map<IBinding, SolidityInfo> mergeAlternatives(Map<IBinding, SolidityInfo> alternativeOne, Map<IBinding, SolidityInfo> alternativeTwo, ASTNode parent) {
		if (alternativeOne == null) {
			return alternativeTwo;
		}
		if (alternativeTwo == null) {
			return alternativeOne;
		}
		Map<IBinding, SolidityInfo> result = new HashMap<IBinding, SolidityInfo>();
		examineOneAlternative(alternativeTwo, alternativeOne, parent, result);
		examineOneAlternative(alternativeOne, alternativeTwo, parent, result);
		return result;
	}

	private void examineOneAlternative(Map<IBinding, SolidityInfo> alternativeOne, Map<IBinding, SolidityInfo> alternativeTwo, ASTNode parent, Map<IBinding, SolidityInfo> result) {
		for (Map.Entry<IBinding, SolidityInfo> binding : alternativeOne.entrySet()) {
			SolidityInfo thenValue = binding.getValue();
			IBinding ibinding = binding.getKey();
			SolidityInfo otherBinding = alternativeTwo.get(ibinding);
			if (otherBinding == null) {
				otherBinding = findCurrentBindingOf(parent, ibinding);
			}
			if (otherBinding == null) {
				continue;
			}
			if (otherBinding.getType() == thenValue.getType()) {
				result.put(ibinding, thenValue);
			} else {
				// write SolidityType.MayBeNull
				if (thenValue.getType() == SolidityType.MayBeNull) {
					result.put(ibinding, thenValue);
				} else if (otherBinding.getType() == SolidityType.MayBeNull) {
					result.put(ibinding, otherBinding);
				} else if (thenValue.getType() == SolidityType.IsNull) {
					result.put(ibinding, new SolidityInfo(thenValue.getNode(), SolidityType.MayBeNull));
				} else {
					if (otherBinding.getType() != SolidityType.IsNull) {
						throw new RuntimeException("Programming error at " + otherBinding);
					}
					result.put(ibinding, new SolidityInfo(otherBinding.getNode(), SolidityType.MayBeNull));
				}
			}
		}
	}

	private Map<IBinding, SolidityInfo> mergeSequence(Map<IBinding, SolidityInfo> varDeclToLastValue1, Map<IBinding, SolidityInfo> varDeclToLastValue2) {
		if (varDeclToLastValue1 == null || varDeclToLastValue2 == null) {
			return null;
		}
		Map<IBinding, SolidityInfo> result = new HashMap<IBinding, SolidityInfo>(varDeclToLastValue1);
		result.putAll(varDeclToLastValue2);
		return result;
	}

	// private Map<IBinding, SolidityInfo> filterNullable(Map<IBinding,
	// SolidityInfo> varDeclToLastValue) {
	// Map<IBinding, SolidityInfo> result = new HashMap<IBinding,
	// SolidityInfo>();
	// for (Map.Entry<IBinding, SolidityInfo> entry :
	// varDeclToLastValue.entrySet()) {
	// if (canBeNull(entry.getValue())) {
	// result.put(entry.getKey(), entry.getValue());
	// }
	// }
	// return result;
	// }

	private void mergeOptional(ASTNode node, Map<IBinding, SolidityInfo> varDeclToLastValue) {
		mergeSequence(preserver.get(node), mergeAlternatives(varDeclToLastValue, new HashMap<IBinding, SolidityInfo>(), node.getParent()));
	}

	// private void setOptional(RSTScope scope) {
	// scope.varDeclToLastValue = mergeAlternatives(scope.varDeclToLastValue,
	// new HashMap<IBinding, SolidityInfo>()); //
	// filterNullable(scope.varDeclToLastValue);
	// }

	private void mergeSequence(RSTScope scope, Map<IBinding, SolidityInfo> varDeclToLastValue) {
		scope.varDeclToLastValue = mergeSequence(scope.varDeclToLastValue, varDeclToLastValue);
	}

	private boolean isDefined(ASTNode expression) {
		return expressionsReturningNullableBecauseOf.containsKey(expression);
	}

	private boolean canBeNull(SolidityInfo expression) {
		return expression.getType() == SolidityType.IsNull || expression.getType() == SolidityType.MayBeNull;
	}

	private boolean canNotBeNull(SolidityInfo expression) {
		return canNotBeNull(expression.getType());
	}

	private boolean canNotBeNullRec(SolidityInfo expression) {
		return canNotBeNullRec(expression.getType());
	}

	private boolean canNotBeNullRec(SolidityType solidityType) {
		return solidityType != null && solidityType.canNotBeNullRec();
	}

	private boolean canNotBeNull(SolidityType solidityType) {
		return solidityType != null && solidityType.canNotBeNull();
	}

	private boolean isNull(SolidityInfo expression) {
		return expression.getType() == SolidityType.IsNull;
	}

	private void setCanBeNotNull(ASTNode node, SolidityType solidityType) {
		if (canNotBeNull(solidityType)) {
			expressionsReturningNullableBecauseOf.put(node, new SolidityInfo(node, solidityType, node));
			expressionsReturningNullableBecauseOfNegativated.put(node, new SolidityInfo(node, SolidityType.IsNull, node));
		}
	}

	private void setCanBeNotNull(ASTNode node) {
		expressionsReturningNullableBecauseOf.put(node, new SolidityInfo(node, SolidityType.IsNotNull, node));
		expressionsReturningNullableBecauseOfNegativated.put(node, new SolidityInfo(node, SolidityType.IsNull, node));
	}

	private void setCanBeNotNullRec(ASTNode node) {
		expressionsReturningNullableBecauseOf.put(node, new SolidityInfo(node, SolidityType.getIsNotNullRec(), node));
		expressionsReturningNullableBecauseOfNegativated.put(node, new SolidityInfo(node, SolidityType.IsNull, node));
	}

	private void setCanBeNull(ASTNode node, ASTNode reason) {
		expressionsReturningNullableBecauseOf.put(node, new SolidityInfo(node, SolidityType.MayBeNull, reason));
		expressionsReturningNullableBecauseOfNegativated.put(node, new SolidityInfo(node, SolidityType.MayBeNull, reason));
	}

	// private void setCanBeNull(ASTNode node) {
	// expressionsReturningNullableBecauseOf.put(node, new SolidityInfo(node,
	// SolidityType.MayBeNull));
	// expressionsReturningNullableBecauseOfNegativated.put(node, new
	// SolidityInfo(node, SolidityType.MayBeNull));
	// }

	private void setIsNull(ASTNode node, ASTNode reason) {
		expressionsReturningNullableBecauseOf.put(node, new SolidityInfo(node, SolidityType.IsNull, reason));
		expressionsReturningNullableBecauseOfNegativated.put(node, new SolidityInfo(node, SolidityType.IsNotNull, reason));
	}

	private void setCanBeNullDependsOn(ASTNode node, ASTNode name) {
		expressionsReturningNullableBecauseOf.put(node, expressionsReturningNullableBecauseOf.get(name));
		expressionsReturningNullableBecauseOfNegativated.put(node, expressionsReturningNullableBecauseOfNegativated.get(name));
	}

	// private ASTNode getNullReason(ASTNode expression) {
	// return expressionsReturningNullableBecauseOf.get(expression);
	// }

	private void addBinding(ASTNode node, IBinding typeBinding, ASTNode node2) {
		SolidityInfo pos = expressionsReturningNullableBecauseOf.get(node2);
		if (pos != null) {
			preserver.get(node).varDeclToLastValue.put(typeBinding, pos);
		} else {
			System.err.println("No solidity info for expression " + node2 + " at " + getSource(node2));
		}
		SolidityInfo neg = expressionsReturningNullableBecauseOfNegativated.get(node2);
		if (neg != null) {
			preserver.get(node).varDeclToLastValueNeg.put(typeBinding, neg);
		}
	}

	private void addBindingForChildren(ASTNode node, IBinding typeBinding, ASTNode node2, boolean singleValue) {
		SolidityInfo pos = expressionsReturningNullableBecauseOf.get(node2);
		SolidityInfo currentSolidityInfo = findCurrentBindingOf(node, typeBinding);
		if (pos != null && currentSolidityInfo != null) {
			if (singleValue) {
				if (canNotBeNull(pos) && !canNotBeNullRec(pos) && canNotBeNullRec(currentSolidityInfo)) {
					pos = new SolidityInfo(pos.getNode(), SolidityType.IsNotNull, pos.getReason());
				} else {
					pos = new SolidityInfo(pos.getNode(), currentSolidityInfo.getType(), pos.getReason());
				}
				preserver.get(node).varDeclToLastValue.put(typeBinding, pos);
			} else {
				if (canNotBeNull(pos) && !canNotBeNullRec(pos) && canNotBeNullRec(currentSolidityInfo)) {
					pos = new SolidityInfo(pos.getNode(), SolidityType.IsNotNull, pos.getReason());
				} else {
					pos = new SolidityInfo(pos.getNode(), currentSolidityInfo.getType(), pos.getReason());
				}
				preserver.get(node).varDeclToLastValue.put(typeBinding, pos);
			}
		} else {
			System.err.println("No solidity info for expression " + node2 + " at " + getSource(node2));
		}
	}

	/**
	 * expression.methodName(arguments)
	 * 
	 * @param methodName
	 * @param arguments
	 * @param methodInvocation
	 *            the whole expression
	 * @param expression
	 *            may be <code>null</code>
	 */
	public void lookAtMethod(IMethodBinding methodBinding, String methodName, List arguments, ASTNode methodInvocation, Expression expression) {
		if (/* methodName.resolveBinding() instanceof IMethodBinding */methodBinding != null) {
			IMethodBinding methodDeclaration = methodBinding;// (IMethodBinding)methodName.resolveBinding();
			ITypeBinding decl = methodDeclaration.getDeclaringClass();
			if (decl != null) {
				SolidityType isSolid = isSolidByAnnotation(methodBinding); // isSolidByAnnotation(methodName);

				if (canNotBeNull(isSolid)) {
					setCanBeNotNull(methodInvocation, isSolid);
				} else if (decl.getBinaryName().startsWith("org.eclipse.emf.ecore.")) {
					if (!(decl.getName().equals("EClass") && methodName.equals("getEIDAttribute") || decl.getName().equals("EClass") && methodName.equals("getEStructuralFeature") || decl.getName().equals("EClassifier") && methodName.equals("getDefaultValue"))) {
						setCanBeNotNullRec(methodInvocation);
					}
				}
				if (hasInterface(decl, "java.util.List") && methodName.toString().equals("get") || decl.getBinaryName().equals("java.util.Iterator") && methodName.toString().equals("next")) {
					if (isRec(expression)) {
						setCanBeNotNullRec(methodInvocation);
					}
				} else if (hasInterface(decl, "java.util.List") && methodName.toString().equals("toArray")) {
					setCanBeNullDependsOn(methodInvocation, expression);
				} else if (hasInterface(decl, "java.util.Map") && (methodName.toString().equals("keySet") || methodName.toString().equals("entrySet") || methodName.toString().equals("values"))) {
					if (isRec(expression)) {
						setCanBeNotNullRec(methodInvocation);
					}
				} else if (hasInterface(decl, "java.util.Map$Entry") && (methodName.toString().equals("getKey") || methodName.toString().equals("getValue"))) {
					if (isRec(expression)) {
						setCanBeNotNullRec(methodInvocation);
					}
				}

				if (hasInterface(decl, "java.util.Collection") && !hasInterface(decl, "java.util.Set")
						&& (methodName.toString().equals("contains") || methodName.toString().equals("containsAll") || methodName.toString().equals("remove") || methodName.toString().equals("removeAll") || methodName.toString().equals("indexOf"))) {
					performanceWarnings.add(methodInvocation);
				}

				boolean manipulateChildrenValidity = false;

				SolidityType solidByClass = isSolidByAnnotation(decl);
				if (!canNotBeNull(solidByClass)) {
					if (decl.getBinaryName().equals("java.util.List") && (methodName.toString().equals("add") || methodName.toString().equals("addAll"))) {
						manipulateChildrenValidity = true;
						if (isRec(expression)) {
							if (methodName.toString().equals("addAll")) {
								solidByClass = SolidityType.getIsNotNullRec();
							} else {
								solidByClass = SolidityType.getIsNotNullRec();
							}
						}
					}
					if (decl.getBinaryName().equals("java.util.Map") && (methodName.toString().equals("put") || methodName.toString().equals("putAll"))) {
						manipulateChildrenValidity = true;
						if (isRec(expression)) {
							if (methodName.toString().equals("putAll")) {
								solidByClass = SolidityType.getIsNotNullRec();
							} else {
								solidByClass = SolidityType.getIsNotNullRec();
							}
						}
					}
				}

				Iterator iter = arguments.iterator();
				int i = 0;
				while (iter.hasNext()) {
					Object a = iter.next();
					IAnnotationBinding[] annos = methodDeclaration.getParameterAnnotations(i);
					if (annos != null && a instanceof ASTNode) {
						ASTNode argument = (ASTNode) a;
						SolidityType x = hasSolidAnnotation(annos);
						if (x == null) {
							x = solidByClass;
						}
						if (manipulateChildrenValidity) {
							// addBindingForChildren();
							requireSolid(argument, x);
						} else {
							if (canNotBeNull(x)) {
								requireSolid(argument, x);
							} else {
								requireSolid(argument, x);
							}
						}
					}
					i++;
				}
			}

		}

	}

	private boolean hasInterface(ITypeBinding decl, String string) {
		if (decl.getBinaryName().equals(string)) {
			return true;
		}
		for (ITypeBinding binding : decl.getInterfaces()) {
			if (hasInterface(binding, string)) {
				return true;
			}
		}
		return false;
	}

	private boolean isRec(Expression expression) {
		return true;// expressionsReturningNullableBecauseOf.get(expression) !=
		// null &&
		// canNotBeNullRec(expressionsReturningNullableBecauseOf.get(expression));
	}

	public void endVisit(ClassInstanceCreation node) {
		setCanBeNotNull(node, SolidityType.getIsNotNullRec());
		endVisitNode(node);
	}

	public void endVisit(ArrayCreation node) {
		setCanBeNotNull(node);
		endVisitNode(node);
	}

	public void endVisit(NumberLiteral node) {
		setCanBeNotNullRec(node);
		endVisitNode(node);
	}

	public void endVisit(BooleanLiteral node) {
		setCanBeNotNullRec(node);
		endVisitNode(node);
	}

	public void endVisit(StringLiteral node) {
		setCanBeNotNullRec(node);
		endVisitNode(node);
	}

	public void endVisit(TypeLiteral node) {
		setCanBeNotNullRec(node);
		endVisitNode(node);
	}

	public void endVisit(NullLiteral node) {
		setIsNull(node, node);
		endVisitNode(node);
	}

	public void endVisit(ThisExpression node) {
		setCanBeNotNullRec(node);
		endVisitNode(node);
	}

	public void endVisit(ContinueStatement node) {
		impossibleControlFlow(node);
		endVisitNode(node);
	}

	public void endVisit(ThrowStatement node) {
		impossibleControlFlow(node);
		endVisitNode(node);
	}

	public void endVisit(InfixExpression node) {
		String op = node.getOperator().toString();
		if ("!=".equals(op) || "==".equals(op)) {
			if (node.getLeftOperand() instanceof SimpleName && node.getRightOperand() instanceof NullLiteral) {
				SimpleName simpleName = (SimpleName) node.getLeftOperand();
				SolidityInfo info = expressionsReturningNullableBecauseOf.get(simpleName);
				if (info != null && !(isNull(info) || canNotBeNull(info))) {
					IBinding typeBinding = simpleName.resolveBinding();
					if (typeBinding != null) {
						if ("!=".equals(op)) {
							setCanBeNotNull(simpleName);
						} else {
							setIsNull(simpleName, node.getRightOperand());
						}
						addBinding(node, typeBinding, simpleName);
					}
				}
				if (info != null && (isNull(info) || canNotBeNull(info))) {
					constantValue.put(node, canNotBeNull(info) == "!=".equals(op));
				}
			}
		}
		setCanBeNotNullRec(node);
		endVisitNode(node);
	}

	public void endVisit(SimpleName node) {
		if (canNotBeNull(isSolidByAnnotation(node))) {
			setCanBeNotNull(node, isSolidByAnnotation(node));
		} else if (isSolidFinal(node)) {
			setCanBeNotNullRec(node);
		} else if (isBoundToSolidValue(node) != null) {
			expressionsReturningNullableBecauseOf.put(node, new SolidityInfo(node, isBoundToSolidValue(node).getType(), isBoundToSolidValue(node).getReason()));
		}
		endVisitNode(node);
	}

	public void endVisit(QualifiedName node) {
		setCanBeNullDependsOn(node, node.getName());
		endVisitNode(node);
	}

	public void endVisit(ExpressionStatement node) {
		setCanBeNullDependsOn(node, node.getExpression());
		endVisitNode(node);
	}

	public void endVisit(NormalAnnotation node) {
		endVisitNode(node);
	}

	public void endVisit(MemberValuePair node) {
		endVisitNode(node);
	}

	public void endVisit(ParenthesizedExpression node) {
		setCanBeNullDependsOn(node, node.getExpression());
		endVisitNode(node);
	}

	public void endVisit(CastExpression node) {
		setCanBeNullDependsOn(node, node.getExpression());
		endVisitNode(node);
	}

	public void endVisit(ConditionalExpression node) {
		Object constant = node.getExpression().resolveConstantExpressionValue();
		ASTNode nullReason = null;
		SolidityInfo thenInfo = expressionsReturningNullableBecauseOf.get(node.getThenExpression());
		SolidityInfo elseInfo = expressionsReturningNullableBecauseOf.get(node.getElseExpression());
		if (Boolean.TRUE.equals(constant)) {
			preserver.get(node.getElseExpression()).assign(preserver.get(node.getThenExpression()));
			nullReason = node.getThenExpression();
		} else if (Boolean.FALSE.equals(constant)) {
			preserver.get(node.getThenExpression()).assign(preserver.get(node.getElseExpression()));
			nullReason = node.getElseExpression();
		} else {
			if (thenInfo != null && elseInfo != null && isGreater(thenInfo.getType(), elseInfo.getType())) {
				nullReason = node.getElseExpression();
			} else {
				nullReason = node.getThenExpression();
			}
		}
		setCanBeNullDependsOn(node, nullReason);
		endVisitNode(node);
	}

	public void endVisit(IfStatement node) {
		Object constant = node.getExpression().resolveConstantExpressionValue();
		if (node.getElseStatement() != null) {
			if (Boolean.TRUE.equals(constant)) {
				preserver.get(node.getElseStatement()).assign(preserver.get(node.getThenStatement()));
			}
			if (Boolean.FALSE.equals(constant)) {
				preserver.get(node.getThenStatement()).assign(preserver.get(node.getElseStatement()));
			}
		}
		endVisitNode(node);
	}

	public void endVisit(ReturnStatement node) {
		MethodDeclaration methodDeclaration = getMethodDeclaration(node);
		if (methodDeclaration != null) {
			requireSolid(node.getExpression(), isSolidByAnnotation(methodDeclaration.getName()));
		}
		impossibleControlFlow(node);
		endVisitNode(node);
	}

	public void endVisit(MethodInvocation node) {
		if (node.getExpression() != null) {
			requireSolid(node.getExpression());
		}
		lookAtMethod(node.resolveMethodBinding(), node.getName().toString(), node.arguments(), node, node.getExpression());
		endVisitNode(node);
	}

	public void endVisit(SuperMethodInvocation node) {
		lookAtMethod(node.resolveMethodBinding(), node.getName().toString(), node.arguments(), node, null);
		endVisitNode(node);
	}

	public void endVisit(SuperConstructorInvocation node) {
		if (node.getExpression() != null) {
			requireSolid(node.getExpression());
		}
		lookAtMethod(node.resolveConstructorBinding(), "", node.arguments(), node, node.getExpression());
		endVisitNode(node);
	}

	public void endVisit(FieldAccess node) {
		if (node.getExpression() != null) {
			requireSolid(node.getExpression());
		}
		setCanBeNotNull(node, isSolidByAnnotation(node.getName()));
		endVisitNode(node);
	}

	public void endVisit(ArrayAccess node) {
		if (node.getIndex() != null) {
			requireSolid(node.getIndex());
		}
		endVisitNode(node);
	}

	class MyTypeHierarchy extends TypeHierarchy {

		public MyTypeHierarchy(IType type, ICompilationUnit[] workingCopies, IJavaProject project, boolean computeSubtypes) {
			super(type, workingCopies, project, computeSubtypes);
			try {
				compute();
			} catch (JavaModelException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (CoreException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			// this.initialize(1);
		}

	}

	public boolean visit(TypeDeclaration node) {
		typeHierarchy = new MyTypeHierarchy((IType) node.resolveBinding().getJavaElement(), null, aptProject, true);
		return visitNode(node);
	}

	public void endVisit(MethodDeclaration node) {
		// MethodOverrideTester fMethodOverrideTester = new
		// MethodOverrideTester((IType)
		// node.resolveBinding().getDeclaringClass().getJavaElement(),
		// typeHierarchy);
		// try {
		// IMethod x = fMethodOverrideTester.findOverriddenMethod((IMethod)
		// node.resolveBinding().getJavaElement(), false);
		// if (x != null) {
		// if (x.toString().contains("findByPath")) {
		// if (node.resolveBinding() != null && node.getRoot() instanceof
		// CompilationUnit) {
		// return ((CompilationUnit) node.getRoot()).findDeclaringNode(x);
		// }
		//
		// x = fMethodOverrideTester.findOverriddenMethod((IMethod)
		// node.resolveBinding().getJavaElement(), false);
		// }
		// System.out.println(x.toString());
		// }
		// x = x;
		// } catch (JavaModelException e) {
		// e.printStackTrace();
		// }

		IMethodBinding methodBinding = node.resolveBinding();
		if (hasOverrideAnnotation(methodBinding)) {
			ITypeBinding decl = methodBinding.getDeclaringClass();
			IMethodBinding superMethod = findSuperMethod(decl, methodBinding);
			if (superMethod != null) {
				if (isSolidByAnnotation(methodBinding) != isSolidByAnnotation(superMethod)) {
					signatureProblems.add(node.getReturnType2());
				} else {
					for (int i = 0; i < methodBinding.getParameterTypes().length; i++) {
						IAnnotationBinding[] annos = methodBinding.getParameterAnnotations(i);
						IAnnotationBinding[] annosSuper = superMethod.getParameterAnnotations(i);
						if (annos != null && annosSuper != null) {
							SolidityType typeOverriding = hasSolidAnnotation(annos);
							SolidityType typeOverridden = hasSolidAnnotation(annosSuper);
							if (typeOverriding == null) {
								typeOverriding = isSolidByAnnotation(decl);
							}
							if (typeOverridden == null) {
								typeOverridden = isSolidByAnnotation(superMethod.getDeclaringClass());
							}
							if (typeOverridden != typeOverriding) {
								signatureProblems.add((ASTNode) node.parameters().get(i));
							}
						}
					}

				}
			} else {
				System.err.println(decl.getBinaryName() + "::" + methodBinding.toString() + " has no supermethod found!");
			}
		}
		endVisitNode(node);
	}

	private IMethodBinding getSuperMethod(ITypeBinding binding, IMethodBinding methodBinding) {
		for (IMethodBinding superMethod : binding.getDeclaredMethods()) {
			if (!hasOverrideAnnotation(superMethod) && methodBinding.overrides(superMethod)) {
				return superMethod;
			}
		}
		return null;
	}

	private IMethodBinding findSuperMethod(ITypeBinding decl, IMethodBinding methodBinding) {
		ITypeBinding superClass = decl.getSuperclass();
		if (superClass != null) {
			IMethodBinding superMethod = getSuperMethod(superClass, methodBinding);
			if (superMethod != null) {
				return superMethod;
			}
			superMethod = findSuperMethod(superClass, methodBinding);
			if (superMethod != null) {
				return superMethod;
			}
		}
		for (ITypeBinding binding : decl.getInterfaces()) {
			IMethodBinding superMethod = getSuperMethod(binding, methodBinding);
			if (superMethod != null) {
				return superMethod;
			}
			superMethod = findSuperMethod(binding, methodBinding);
			if (superMethod != null) {
				return superMethod;
			}
		}
		return null;
	}

	public void endVisit(EnhancedForStatement node) {
		if (node.getExpression() != null) {
			requireSolid(node.getExpression(), SolidityType.getIsNotNullRec());
		}
		endVisitNode(node);
	}

	public void endVisit(Assignment node) {
		SimpleName simpleName = null;
		if (node.getLeftHandSide() instanceof FieldAccess) {
			simpleName = ((FieldAccess) node.getLeftHandSide()).getName();
		} else if (node.getLeftHandSide() instanceof SimpleName) {
			simpleName = (SimpleName) node.getLeftHandSide();
		}
		if (simpleName != null) {
			IBinding typeBinding = simpleName.resolveBinding();
			if (typeBinding != null) {
				addBinding(node, typeBinding, node.getRightHandSide());
			}
			requireSolid(node.getRightHandSide(), isSolidByAnnotation(simpleName));
		}
		endVisitNode(node);
	}

	public void endVisit(VariableDeclarationFragment node) {
		if (node.getInitializer() != null) {
			SimpleName simpleName = node.getName();
			IBinding typeBinding = simpleName.resolveBinding();
			if (typeBinding != null) {
				addBinding(node, typeBinding, node.getInitializer());
			}
			requireSolid(node.getInitializer(), isSolidByAnnotation(simpleName));
		}
		endVisitNode(node);
	}

	private void impossibleControlFlow(ASTNode node) {
		preserver.get(node).varDeclToLastValue = null;
	}

	private RSTScope getCurrentScope(ASTNode node) {
		return this.preserver.get(node);
		// return scopes.get(scopes.size() - 1);
	}

	private MethodDeclaration getMethodDeclaration(ASTNode node) {
		while (node != null) {
			if (node instanceof MethodDeclaration) {
				return (MethodDeclaration) node;
			} else {
				node = node.getParent();
			}
		}
		return null;
	}

	private void requireSolid(ASTNode expression) {
		SolidityInfo info = expressionsReturningNullableBecauseOf.get(expression);
		if (info != null && canBeNull(info)) {
			mustBeNotNull.add(info);
		}
	}

	private void requireSolid(ASTNode expression, SolidityType requiredSolidityType) {
		if (canNotBeNull(requiredSolidityType)) {
			SolidityInfo info = expressionsReturningNullableBecauseOf.get(expression);
			if (info != null && isGreater(requiredSolidityType, info.getType())) {
				mustBeNotNull.add(info);
			}
		}
	}

	boolean isGreater(SolidityType first, SolidityType second) {
		return canNotBeNull(first) && !canNotBeNull(second) || canNotBeNullRec(first) && !canNotBeNullRec(second);
	}

	private SolidityType isSolidByAnnotation(SimpleName simpleName) {
		IBinding typeBinding = simpleName.resolveBinding();
		if (typeBinding != null) {
			if (hasSolidAnnotation(typeBinding) != null) {
				return hasSolidAnnotation(typeBinding);
			}
			ITypeBinding decl = findDeclaringClass(simpleName);
			if (decl != null && hasSolidAnnotation(decl) != null) {
				return hasSolidAnnotation(decl);
			}
			if (decl != null && decl.getBinaryName().equals("java.lang.String")) {
				return SolidityType.getIsNotNullRec();
			}
		}
		return SolidityType.MayBeNull;
	}

	private SolidityType isSolidByAnnotation(IMethodBinding typeBinding) {
		if (hasSolidAnnotation(typeBinding) != null) {
			return hasSolidAnnotation(typeBinding);
		}
		ITypeBinding decl = typeBinding.getDeclaringClass();
		if (decl != null && hasSolidAnnotation(decl) != null) {
			return hasSolidAnnotation(decl);
		}
		if (decl != null && decl.getBinaryName().equals("java.lang.String")) {
			return SolidityType.getIsNotNullRec();
		}
		if (typeBinding.getName().equals("toString")) {
			return SolidityType.getIsNotNullRec();
		}
		if (isPrimitiveType(typeBinding.getReturnType())) {
			return SolidityType.getIsNotNullRec();
		}
		return SolidityType.MayBeNull;
	}

	private SolidityType isSolidByAnnotation(ITypeBinding typeBinding) {
		if (typeBinding != null && hasSolidAnnotation(typeBinding) != null) {
			return hasSolidAnnotation(typeBinding);
		}
		return SolidityType.MayBeNull;
	}

	private ITypeBinding findDeclaringClass(SimpleName name) {
		ASTNode decl = findDeclaringNode(name);
		if (decl != null && decl.getParent() instanceof MethodDeclaration) {
			name = ((MethodDeclaration) decl.getParent()).getName();
		}
		if (name.resolveBinding() instanceof IMethodBinding) {
			IMethodBinding binding = (IMethodBinding) name.resolveBinding();
			return binding.getDeclaringClass();
		}
		if (name.resolveBinding() instanceof IVariableBinding) {
			IVariableBinding binding = (IVariableBinding) name.resolveBinding();
			return binding.getDeclaringClass();
		}
		return null;
	}

	private ASTNode findDeclaringNode(SimpleName name) {
		if (name.resolveBinding() != null && name.getRoot() instanceof CompilationUnit) {
			return ((CompilationUnit) name.getRoot()).findDeclaringNode(name.resolveBinding());
		}
		return null;
	}

	private SolidityType hasSolidAnnotation(IAnnotationBinding[] annos) {
		for (IAnnotationBinding annotation : annos) {
			if (Solid.class.getSimpleName().equals(annotation.getName())) {
				SolidityType result = SolidityType.MayBeNull;
				result = SolidityType.getIsNotNullRec();
				for (IMemberValuePairBinding pair : annotation.getDeclaredMemberValuePairs()) {
					if (pair.getName().equals("value")) {
						if (!(Boolean) pair.getValue()) {
							result = SolidityType.MayBeNull;
						}
					}
					if (pair.getName().equals("children")) {
						if (!(Boolean) pair.getValue()) {
							result = SolidityType.IsNotNull;
						}
					}
				}
				return result;
			}
			if ("Nullable".equals(annotation.getName())) {
				return SolidityType.MayBeNull;
			}
			if ("NotNull".equals(annotation.getName())) {
				return SolidityType.IsNotNull;
			}
		}
		return null;
	}

	private boolean hasOverrideAnnotation(IMethodBinding binding) {
		for (IAnnotationBinding annotation : binding.getAnnotations()) {
			if ("Override".equals(annotation.getName())) {
				return true;
			}
		}
		return false;
	}

	private SolidityType hasSolidAnnotation(ITypeBinding typeBinding) {
		if (typeBinding.getPackage() != null && typeBinding.getPackage().getName().startsWith("de.ikv.medini.")) {
			return SolidityType.getIsNotNullRec();
		}
		return hasSolidAnnotation((IBinding) typeBinding);
	}

	private SolidityType hasSolidAnnotation(IBinding typeBinding) {
		return hasSolidAnnotation(typeBinding.getAnnotations());
	}

	private boolean isSolidFinal(SimpleName simpleName) {
		IBinding typeBinding = simpleName.resolveBinding();
		if (typeBinding instanceof IVariableBinding) {
			IVariableBinding var = (IVariableBinding) typeBinding;
			if (var.isField() && (var.getModifiers() & IModifierConstants.ACC_FINAL) != 0) {
				return true; // assume that a solid field is assigned
			}
		} else if (typeBinding instanceof ITypeBinding) {
			return true;
		}
		return false;
	}

	private SolidityInfo isBoundToSolidValue(SimpleName simpleName) {
		IBinding typeBinding = simpleName.resolveBinding();
		return findCurrentBindingOf(simpleName, typeBinding);
	}

	private SolidityInfo findCurrentBindingOf(ASTNode scoping, IBinding typeBinding) {
		ASTNode child = scoping;
		while (scoping != null) {
			RSTScope scope = preserver.get(scoping);
			SolidityInfo value = scope.getValueFor(typeBinding, true);
			if (value != null) {
				return value;
			}
			child = scoping;
			scoping = scoping.getParent();
		}
		return null;
	}

	public List<SolidityInfo> getMustBeNotNull() {
		return mustBeNotNull;
	}

	public Map<ASTNode, Object> getConstantValue() {
		return constantValue;
	}

	public void setJavaProject(IJavaProject aptProject) {
		this.aptProject = aptProject;
	}

	public List<ASTNode> getPerformanceWarnings() {
		return performanceWarnings;
	}

	public List<ASTNode> getSignatureProblems() {
		return signatureProblems;
	}

}
