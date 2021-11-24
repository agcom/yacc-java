package io.github.agcom.yaccjava;

import io.github.agcom.yaccjava.internal;
import static io.github.agcom.yaccjava.internal.*;

@Deprecated("Just because")
public final class Main<A, B, C> extends This implements That, AndThat permits AnotherOne, AndAnotherOne {
	
	public static void main(final String[] args) {
		System.out.println("Hello world!");
	}
	
}

public interface SomeInterface {}
public @interface SomeAnnInterface {}
public enum SomeEnum {}
public record SomeRecord() {}
