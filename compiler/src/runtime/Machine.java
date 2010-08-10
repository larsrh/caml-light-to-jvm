/*
 * This file is Java, because it is not an actual part of the compiler
 * but rather the runtime. It contains an implementation of the MaMa
 * machine instructions which will get compiled, disassembled and inserted
 * into the bytecode generator
 *
 * Disassemble using
 *
 * $ java -cp asm.jar:asm-util.jar org.objectweb.asm.util.ASMifierClassVisitor
 *   Machine.class
 */

package runtime;

import java.util.Stack;

public class Machine {
	private final int fp;
	private final int sp;
	abstract class MachineData {};
	private class Base extends MachineData {
		private int v;
	}

	private class Vector extends MachineData {
		private int n;
		private int[] v;
	}

	// TODO: determine what cp exactly is
	private class Function extends MachineData {
		private int cp;
		private Vector ap;
		private Vector gp;
	}

	private class Closure extends MachineData {
		private int cp;
		private Function gp;
	}

	private final Stack<MachineData> stack;

	public Machine() {
		this.stack = new Stack<MachineData>();
		this.fp = 0;
		this.sp = 0;
	}
}

/*
 * For how to implement jumps, see
 * http://snippets.dzone.com/posts/show/3812
 */
