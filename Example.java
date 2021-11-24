package io.github.agcom.yaccjava;

import io.github.agcom.yaccjava.internal;
import static io.github.agcom.yaccjava.internal.*;

@Deprecated("Just because")
public final class Main<A extends SomeInterface, B, C> extends This implements That, AndThat permits AnotherOne, AndAnotherOne {
	
	static {
		// Static initializer
	}
	
	final String name = "Alireza Ghasemi";
	
	public static void main(final String[] args) {
		System.out.printf("Hello %s!", name);
	}
	
	public Main() {
		// Constructor
	}
	
}

public interface SomeInterface {}
public @interface SomeAnnInterface {}
public enum SomeEnum {}
public record SomeRecord() {}
