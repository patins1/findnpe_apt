package joe.restricted;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import joe.restricted.SolidityInfo.SolidityType;

import org.eclipse.jdt.astview.GenericVisitor;
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
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
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

	private Map<SolidityInfo, Object> constantValue = new HashMap<SolidityInfo, Object>();

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
	 *            if <code>null</code>, <code>false</code> is assumed
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

			Map<IBinding, SolidityInfo> ifPos = mergeAlternatives(thenScopePos, elseScopePos);
			Map<IBinding, SolidityInfo> ifNeg = mergeAlternatives(thenScopeNeg, elseScopeNeg);
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
		return new HashMap<IBinding, SolidityInfo>();
	}

	// Or-Executed: AssertStatement CatchClause
	// DoStatement ForStatement SwitchCase SwitchStatement EnhancedForStatement
	protected void endVisitNode(ASTNode current) {
		ASTNode parent = current.getParent();
		if (parent != null && preserver.get(current) != null && preserver.get(parent) != null) {
			if (parent instanceof IfStatement) {
				IfStatement ifStatement = (IfStatement) parent;
				processIfThenElse(ifStatement.getExpression(), ifStatement.getThenStatement(), ifStatement.getElseStatement(), current);
			} else if (parent instanceof ConditionalExpression) {
				ConditionalExpression conditionalExpression = (ConditionalExpression) parent;
				processIfThenElse(conditionalExpression.getExpression(), conditionalExpression.getThenExpression(), conditionalExpression.getElseExpression(), current);
			} else if (parent instanceof WhileStatement) {
				WhileStatement whileStatement = (WhileStatement) parent;
				processIfThenElse(whileStatement.getExpression(), whileStatement.getBody(), null, current);
			} else if (parent instanceof InfixExpression) {
				InfixExpression infixExpression = (InfixExpression) parent;
				if (infixExpression.getOperator().toString().equals("&&")) {
					processIfThenElse(infixExpression.getLeftOperand(), infixExpression.getRightOperand(), null, current);
				} else if (infixExpression.getOperator().toString().equals("||")) {
					processIfThenElse(infixExpression.getLeftOperand(), null, infixExpression.getRightOperand(), current);
				} else if (infixExpression.getLeftOperand().resolveTypeBinding() != null && "boolean".equals(infixExpression.getLeftOperand().resolveTypeBinding().getName())) {
					if (infixExpression.getOperator().toString().equals("==")) {
						processIfThenElse(infixExpression.getLeftOperand(), infixExpression.getRightOperand(), infixExpression.getRightOperand(), current, true, false);
					} else if (infixExpression.getOperator().toString().equals("!=")) {
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
					mergeOptional(preserver.get(enhancedForStatement), preserver.get(enhancedForStatement.getBody()).varDeclToLastValue);
				}
			} else if (parent instanceof TryStatement) {
				TryStatement tryStatement = (TryStatement) parent;
				if (tryStatement.getFinally() == current) {
					// executed at any case
					mergeSequence(preserver.get(parent), preserver.get(current).varDeclToLastValue);
				} else {
					// BODY or CATCH
					mergeOptional(preserver.get(tryStatement), preserver.get(tryStatement.getBody()).varDeclToLastValue);
				}
			} else if (parent instanceof InstanceofExpression || parent instanceof ParenthesizedExpression || parent instanceof VariableDeclarationStatement || parent instanceof VariableDeclarationExpression || parent instanceof Assignment
					|| parent instanceof ExpressionStatement || parent instanceof Block) {
				mergeSequence(preserver.get(parent), preserver.get(current).varDeclToLastValue);
			} else if (parent instanceof ThrowStatement || parent instanceof PostfixExpression || parent instanceof TypeLiteral || parent instanceof CastExpression || parent instanceof ReturnStatement || parent instanceof ArrayAccess || parent instanceof FieldAccess
					|| parent instanceof MethodInvocation || parent instanceof SuperMethodInvocation || parent instanceof ExpressionStatement || parent instanceof ArrayCreation || parent instanceof ArrayInitializer || parent instanceof ClassInstanceCreation
					|| parent instanceof VariableDeclarationFragment || parent instanceof SimpleType || parent instanceof ArrayType || parent instanceof ParameterizedType || parent instanceof QualifiedName || parent instanceof MarkerAnnotation
					|| parent instanceof SingleMemberAnnotation || parent instanceof AnnotationTypeMemberDeclaration || parent instanceof SingleVariableDeclaration || parent instanceof MethodDeclaration || parent instanceof FieldDeclaration || parent instanceof TypeDeclaration
					|| parent instanceof AnonymousClassDeclaration || parent instanceof AnnotationTypeDeclaration || parent instanceof CompilationUnit || parent instanceof PackageDeclaration || parent instanceof ImportDeclaration || parent instanceof EnumConstantDeclaration
					|| parent instanceof EnumDeclaration) {
				// nothing to pass to parent
			} else {
				System.err.println("Node type not handled: " + parent.getClass().getName() + "(line " + getSource(parent) + ") for child " + current.getClass().getName());
			}
		}
		if (!isDefined(current)) {
			if (current instanceof Expression && ((Expression) current).resolveTypeBinding() != null) {
				ITypeBinding tb = ((Expression) current).resolveTypeBinding();
				String n = tb.getName();
				if (n.equals("byte") || n.equals("short") || n.equals("int") || n.equals("long") || n.equals("float") || n.equals("double") || n.equals("char") || n.equals("boolean")) {
					setCanBeNotNull(current);
					return;
				}
			}
			setCanBeNull(current, null);
		}
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
	 * @return
	 */
	private Map<IBinding, SolidityInfo> mergeAlternatives(Map<IBinding, SolidityInfo> alternativeOne, Map<IBinding, SolidityInfo> alternativeTwo) {
		Map<IBinding, SolidityInfo> result = new HashMap<IBinding, SolidityInfo>();
		for (Map.Entry<IBinding, SolidityInfo> binding : alternativeOne.entrySet()) {
			SolidityInfo thenValue = binding.getValue();
			if (canBeNull(thenValue)) {
				result.put(binding.getKey(), thenValue);
			}
		}
		for (Map.Entry<IBinding, SolidityInfo> binding : alternativeTwo.entrySet()) {
			SolidityInfo thenValue = binding.getValue();
			if (canBeNull(thenValue) && !isNull(thenValue)) {
				result.put(binding.getKey(), thenValue);
				/**
				 * now, only if
				 * <code>isNull(thenValue) AND elseValue!=null && isNull(elseValue)<code> is <code>true</code>, the node is included as "isNull" in the result
				 */
			} else {
				SolidityInfo elseValue = alternativeOne.get(binding.getKey());
				if (canNotBeNull(thenValue) && elseValue != null && canNotBeNull(elseValue)) {
					result.put(binding.getKey(), thenValue);
				}
			}
		}
		return result;
	}

	private Map<IBinding, SolidityInfo> mergeSequence(Map<IBinding, SolidityInfo> varDeclToLastValue1, Map<IBinding, SolidityInfo> varDeclToLastValue2) {
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

	private void mergeOptional(RSTScope scope, Map<IBinding, SolidityInfo> varDeclToLastValue) {
		mergeSequence(scope, mergeAlternatives(varDeclToLastValue, new HashMap<IBinding, SolidityInfo>()));
	}

	// private void setOptional(RSTScope scope) {
	// scope.varDeclToLastValue = filterNullable(scope.varDeclToLastValue);
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
		return expression.getType() == SolidityType.IsNotNull;
	}

	private boolean isNull(SolidityInfo expression) {
		return expression.getType() == SolidityType.IsNull;
	}

	private void setCanBeNotNull(ASTNode node) {
		expressionsReturningNullableBecauseOf.put(node, new SolidityInfo(node, SolidityType.IsNotNull, node));
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

	public void lookAtMethod(SimpleName methodName, List arguments, ASTNode methodInvocation) {
		if (methodName.resolveBinding() instanceof IMethodBinding) {
			IMethodBinding methodDeclaration = (IMethodBinding) methodName.resolveBinding();
			ITypeBinding decl = methodDeclaration.getDeclaringClass();
			if (decl != null) {
				boolean isSolid = isSolidByAnnotation(methodName);
				// if (!isSolid) {
				// if (decl.getBinaryName().equals("java.util.List")) {
				// if (methodName.toString().equals("get")) {
				// isSolid = true;
				// }
				//					}
				//				}
				if (isSolid) {
					setCanBeNotNull(methodInvocation);
				}
				boolean solidByClass = isSolidByAnnotation(decl);
				Iterator iter = arguments.iterator();
				int i = 0;
				while (iter.hasNext()) {
					Object a = iter.next();
					IAnnotationBinding[] annos = methodDeclaration.getParameterAnnotations(i);
					if (annos != null && a instanceof ASTNode) {
						ASTNode argument = (ASTNode) a;
						if (hasSolidAnnotation(annos) != null ? hasSolidAnnotation(annos) : solidByClass) {
							requireSolid(argument);
						}
					}
					i++;
				}
			}

		}

	}

	public void endVisit(ClassInstanceCreation node) {
		setCanBeNotNull(node);
		endVisitNode(node);
	}

	public void endVisit(ArrayCreation node) {
		setCanBeNotNull(node);
		endVisitNode(node);
	}

	public void endVisit(NumberLiteral node) {
		setCanBeNotNull(node);
		endVisitNode(node);
	}

	public void endVisit(BooleanLiteral node) {
		setCanBeNotNull(node);
		endVisitNode(node);
	}

	public void endVisit(StringLiteral node) {
		setCanBeNotNull(node);
		endVisitNode(node);
	}

	public void endVisit(TypeLiteral node) {
		setCanBeNotNull(node);
		endVisitNode(node);
	}

	public void endVisit(NullLiteral node) {
		setIsNull(node, node);
		endVisitNode(node);
	}

	public void endVisit(ThisExpression node) {
		setCanBeNotNull(node);
		endVisitNode(node);
	}

	public void endVisit(InfixExpression node) {
		String op = node.getOperator().toString();
		if ("!=".equals(op) || "==".equals(op)) {
			if (node.getLeftOperand() instanceof SimpleName && node.getRightOperand() instanceof NullLiteral) {
				SimpleName simpleName = (SimpleName) node.getLeftOperand();
				SolidityInfo info = expressionsReturningNullableBecauseOf.get(simpleName);
				if (info != null && canBeNull(info) && !isNull(info)) {
					IBinding typeBinding = simpleName.resolveBinding();
					if (typeBinding != null) {
						if ("!=".equals(op)) {
							setCanBeNotNull(simpleName);
						} else {
							setIsNull(simpleName, node.getRightOperand());
						}
						addBinding(node, typeBinding, simpleName);
					}
				} else if (info != null && (isNull(info) || canNotBeNull(info))) {
					constantValue.put(info, canNotBeNull(info) == "!=".equals(op));
				}
			}
		}
		setCanBeNotNull(node);
		endVisitNode(node);
	}

	public void endVisit(SimpleName node) {
		if (isSolidFinal(node) || isSolidByAnnotation(node)) {
			setCanBeNotNull(node);
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
			if (thenInfo != null && canBeNull(thenInfo)) {
				nullReason = node.getThenExpression();
			}
		} else if (Boolean.FALSE.equals(constant)) {
			preserver.get(node.getThenExpression()).assign(preserver.get(node.getElseExpression()));
			if (elseInfo != null && canBeNull(elseInfo)) {
				nullReason = node.getElseExpression();
			}
		} else {
			if (thenInfo != null && canBeNull(thenInfo)) {
				nullReason = node.getThenExpression();
			}
			if (elseInfo != null && canBeNull(elseInfo)) {
				nullReason = node.getElseExpression();
			}
		}
		if (nullReason != null) {
			setCanBeNull(node, nullReason);
		} else {
			setCanBeNotNull(node);
		}
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
			if (isSolidByAnnotation(methodDeclaration.getName())) {
				requireSolid(node.getExpression());
			}
		}
		endVisitNode(node);
	}

	public void endVisit(MethodInvocation node) {
		if (node.getExpression() != null) {
			requireSolid(node.getExpression());
		}
		lookAtMethod(node.getName(), node.arguments(), node);
		endVisitNode(node);
	}

	public void endVisit(SuperMethodInvocation node) {
		lookAtMethod(node.getName(), node.arguments(), node);
		endVisitNode(node);
	}

	public void endVisit(FieldAccess node) {
		if (node.getExpression() != null) {
			requireSolid(node.getExpression());
		}
		if (isSolidByAnnotation(node.getName())) {
			setCanBeNotNull(node);
		}
		endVisitNode(node);
	}

	public void endVisit(ArrayAccess node) {
		if (node.getIndex() != null) {
			requireSolid(node.getIndex());
		}
		endVisitNode(node);
	}

	public void endVisit(EnhancedForStatement node) {
		if (node.getExpression() != null) {
			requireSolid(node.getExpression());
		}
		endVisitNode(node);
	}

	public void endVisit(Assignment node) {
		if (node.getLeftHandSide() instanceof SimpleName) {
			SimpleName simpleName = (SimpleName) node.getLeftHandSide();
			IBinding typeBinding = simpleName.resolveBinding();
			if (typeBinding != null) {
				addBinding(node, typeBinding, node.getRightHandSide());
			}
			if (isSolidByAnnotation(simpleName)) {
				requireSolid(node.getRightHandSide());
			}
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
			if (isSolidByAnnotation(simpleName)) {
				requireSolid(node.getInitializer());
			}
		}
		endVisitNode(node);
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

	private boolean isSolidByAnnotation(SimpleName expression) {
		if (expression instanceof SimpleName) {
			SimpleName simpleName = (SimpleName) expression;
			IBinding typeBinding = simpleName.resolveBinding();
			if (typeBinding != null) {
				if (hasSolidAnnotation(typeBinding) != null) {
					return hasSolidAnnotation(typeBinding);
				}
				ITypeBinding decl = findDeclaringClass(expression);
				if (decl != null && hasSolidAnnotation(decl) != null) {
					return hasSolidAnnotation(decl);
				}
				if (decl != null && decl.getBinaryName().equals("java.lang.String")) {
					return true;
				}
			}
		}
		return false;
	}

	private boolean isSolidByAnnotation(ITypeBinding typeBinding) {
		if (typeBinding != null && hasSolidAnnotation(typeBinding) != null) {
			return hasSolidAnnotation(typeBinding);
		}
		return false;
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

	private Boolean hasSolidAnnotation(IBinding typeBinding) {
		for (IAnnotationBinding annotation : typeBinding.getAnnotations()) {
			if (Solid.class.getSimpleName().equals(annotation.getName())) {
				for (IMemberValuePairBinding pair : annotation.getDeclaredMemberValuePairs()) {
					if (pair.getName().equals("value")) {
						return (Boolean) pair.getValue();
					}
				}
				return true;
			}
		}
		return null;
	}

	private Boolean hasSolidAnnotation(IAnnotationBinding[] annos) {
		for (IAnnotationBinding annotation : annos) {
			if (Solid.class.getSimpleName().equals(annotation.getName())) {
				for (IMemberValuePairBinding pair : annotation.getDeclaredMemberValuePairs()) {
					if (pair.getName().equals("value")) {
						return (Boolean) pair.getValue();
					}
				}
				return true;
			}
		}
		return null;
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

	public Map<SolidityInfo, Object> getConstantValue() {
		return constantValue;
	}

}
