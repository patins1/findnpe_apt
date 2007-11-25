package joe.restricted;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import joe.restricted.SolidityInfo.SolidityType;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.apt.core.internal.env.AbstractCompilationEnv;
import org.eclipse.jdt.apt.core.internal.env.BuildEnv;
import org.eclipse.jdt.apt.core.internal.env.MessagerImpl;
import org.eclipse.jdt.apt.core.internal.env.AbstractCompilationEnv.EnvCallback;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.compiler.BuildContext;
import org.eclipse.jdt.core.compiler.CategorizedProblem;
import org.eclipse.jdt.core.compiler.CompilationParticipant;
import org.eclipse.jdt.core.compiler.ReconcileContext;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.CompilationUnit;

import com.sun.mirror.declaration.AnnotationTypeDeclaration;

public class RSTCompilationParticipant extends CompilationParticipant {

	private BuildContext[] _allfiles;

	public RSTCompilationParticipant() {

	}

	public boolean isAnnotationProcessor() {
		return true;
	}

	public void processAnnotations(BuildContext[] allfiles) {

		// This should not happen. There should always be file that that needs
		// building when
		final int total = allfiles == null ? 0 : allfiles.length;
		if (total == 0)
			return;

		final IProject project = allfiles[0].getFile().getProject();
		final IJavaProject javaProject = JavaCore.create(project);

		EnvCallback buildCallback = new EnvCallback() {
			public void run(AbstractCompilationEnv env) {
				build((BuildEnv) env);
			}
		};

		_allfiles = allfiles;

		// Construct build environment, this invokes the build inside a callback
		// in order to keep open the DOM AST pipeline
		BuildEnv.newBuildEnv(allfiles, new BuildContext[] {}, javaProject, buildCallback);
	}

	protected void build(BuildEnv processorEnv) {

//		final BuildContext[] cpResults = processorEnv.getFilesWithAnnotation();
//		final Map<BuildContext, Set<AnnotationTypeDeclaration>> file2AnnotationDecls = new HashMap<BuildContext, Set<AnnotationTypeDeclaration>>(cpResults.length * 4 / 3 + 1);
//		final Map<String, AnnotationTypeDeclaration> annotationDecls = processorEnv.getAllAnnotationTypes(file2AnnotationDecls);
//		for (BuildContext bc : _allfiles) {
//			List<CategorizedProblem> problems = new ArrayList<CategorizedProblem>();
//			@NotNull
//			CompilationUnit ast = processorEnv.getASTFrom(bc.getFile());
//			ast = processorEnv.getASTFrom(bc.getFile());
//			if (ast != null) {
//				FindNPESourcesVisitor npeSources = new FindNPESourcesVisitor();
//				ast.accept(npeSources);
//				System.err.println(ast.getJavaElement().getPath());
//				for (SolidityInfo nodeInfo : npeSources.getMustBeNotNull()) {
//					ASTNode node = nodeInfo.getNode();
//					String reason = "";
//					if (nodeInfo.getReason() != null) {
//						if (nodeInfo.getType() == SolidityType.IsNull) {
//							reason = "(" + "\"" + nodeInfo.getReason() + "\" evaluates to null)";
//						} else {
//							reason = "(" + "\"" + nodeInfo.getReason() + "\" may evaluate to null)";
//						}
//
//					}
//					RSTProblem problem = new RSTProblem("This expression is required to be solid" + reason, MessagerImpl.Severity.ERROR, bc.getFile(), node.getStartPosition(), node.getLength() + node.getStartPosition() - 1, FindNPESourcesVisitor.getLine(node), null);
//					problems.add(problem);
//				}
//				for (Map.Entry<SolidityInfo, Object> nodeInfo : npeSources.getConstantValue().entrySet()) {
//					ASTNode node = nodeInfo.getKey().getNode();
//					RSTProblem problem = new RSTProblem("This expression evaluates always to " + nodeInfo.getValue(), MessagerImpl.Severity.WARNING, bc.getFile(), node.getStartPosition(), node.getLength() + node.getStartPosition() - 1, FindNPESourcesVisitor.getLine(node), null);
//					problems.add(problem);
//				}
//
//			}
//			bc.recordNewProblems(problems.toArray(new CategorizedProblem[] {}));
//		}
	}

	public boolean isActive(IJavaProject project) {
		return true;
	}

	public void reconcile(ReconcileContext context) {
		final ICompilationUnit workingCopy = context.getWorkingCopy();
		IJavaProject javaProject = workingCopy.getJavaProject();
		final IFile file = (IFile) workingCopy.getResource();
	}

}
