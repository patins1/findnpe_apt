package joe.restricted;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import javax.annotation.processing.Completion;
import javax.annotation.processing.Messager;
import javax.annotation.processing.ProcessingEnvironment;
import javax.annotation.processing.Processor;
import javax.annotation.processing.RoundEnvironment;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.tools.Diagnostic;

import org.eclipse.jdt.internal.apt.pluggable.core.dispatch.IdeBuildProcessingEnvImpl;

import com.sun.mirror.declaration.Declaration;

public class Java6Processor implements Processor {

	private ProcessingEnvironment processingEnv;

	@Override
	public Iterable<? extends Completion> getCompletions(Element element, AnnotationMirror annotation, ExecutableElement member, String userText) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Set<String> getSupportedAnnotationTypes() {
		Set<String> result = new HashSet<String>();
		result.add("Solid");
		result.add("joe.restricted.Solid");
		// result.add("*");
		return result;
	}

	@Override
	public Set<String> getSupportedOptions() {
		Set<String> result = new HashSet<String>();
		return result;
	}

	@Override
	public SourceVersion getSupportedSourceVersion() {
		return SourceVersion.RELEASE_6;
	}

	@Override
	public void init(ProcessingEnvironment processingEnv) {
		this.processingEnv = processingEnv;

	}

	@Override
	public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {

		if (processingEnv instanceof IdeBuildProcessingEnvImpl) {
			IdeBuildProcessingEnvImpl env = (IdeBuildProcessingEnvImpl) processingEnv;
			env = env;
		}
		Messager messager = processingEnv.getMessager();

		Set<? extends Element> annotatedTypes = roundEnv.getElementsAnnotatedWith(Solid.class);

		for (Element annotatedType : annotatedTypes) {
			messager.printMessage(Diagnostic.Kind.ERROR, "msg", annotatedType);
		}

		return true;
	}

}
