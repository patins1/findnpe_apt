package joe.restricted;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Indicates that the annotated method is a test method. This annotation should
 * be used only on parameterless static methods.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target( { ElementType.LOCAL_VARIABLE, ElementType.FIELD, ElementType.METHOD,
		ElementType.PARAMETER, ElementType.TYPE })
public @interface Solid {
	boolean value() default true;

	boolean map() default true;

	boolean collection() default true;
}