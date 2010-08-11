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
 *
 * An alternate approach is compiling the Bytecode and then using the ASM
 * library, walk into it and create a main() method.
 *
 * Actually, this machine does not correspond completely to MaMa, because
 * some instructions take additional params and some return the next label
 * to jump to, instead of changing the PC. Therefore, a better name for the
 * machine would be deineMama.
 */

package runtime;

import java.util.Stack;

public class Machine {
	private int fp;
	private int sp;
	private Vector gp;

	abstract class MachineData {};
	private class Base extends MachineData {
		private int v;

		private Base(int constant) {
			v = constant;
		}
	}

	/* when stuff is not basic - we still need a wrapper type for that */
	// future optimization: remove all raw types from the machine
	private class Raw extends MachineData {
		private int v;

		private Raw(int constant) {
			v = constant;
		}

		public String toString() {
			// nice example of weak typing in Java
			return "" + v;
		}
	}

	private class Vector extends MachineData {
		private final int n;
		private final MachineData[] v;

		private Vector(MachineData[] v) {
			this.v = v;
			this.n = v.length;
		}

		public String toString() {
			return "V(" + n + ", TODO)";
		}
	}

	private class Function extends MachineData {
		// cp is the code pointer, that is, label
		private int cp;
		private Vector ap;
		private Vector gp;

		private Function(int label, Vector ap, Vector gp) {
			this.cp = label;
			this.ap = ap;
			this.gp = gp;
		}

		public String toString() {
			return "F(" + cp + ", TODO, TODO)";
		}
	}

	private class Closure extends MachineData {
		private int cp;
		private Vector gp;

		private Closure(int cp, Vector gp) {
			this.cp = cp;
			this.gp = gp;
		}

		public String toString() {
			return "C(" + cp + ", TODO)";
		}
	}

	private final Stack<MachineData> stack;

	public Machine() {
		this.stack = new Stack<MachineData>();
		// yeah, this is ugly, admittedly
		this.fp = -1;
		this.sp = -1;
	}

	public void loadc(int constant) {
		this.stack.push(new Raw(constant));
		this.sp++;
	}

	// thows a ClassCastException when the code is wrong, which is more
	// or less what we want. Me wants pattern match in this Java!
	public void getbasic() {
		Base b = (Base)this.stack.pop();
		this.stack.push(new Raw(b.v));
	}

	public void pushloc(int depth) {
		stack.push(stack.get(sp - depth));
		sp++;
	}

	public void pushglob(int j) {
		stack.push(gp.v[j]);
		sp++;
	}

	public void add() {
		Raw r1 = (Raw)stack.pop();
		Raw r2 = (Raw)stack.pop();
		stack.push(new Raw(r1.v + r2.v));
		sp--;
	}

	public void mul() {
		Raw r1 = (Raw)stack.pop();
		Raw r2 = (Raw)stack.pop();
		stack.push(new Raw(r1.v * r2.v));
		sp--;
	}

	/* delete k elements under the topmost element */
	public void slide(int k) {
		MachineData save = stack.pop();
		for (int i = 0; i < k; i++) {
			stack.pop();
			sp--;
		}
		stack.push(save);
	}

	public void mkvec(int g) {
		MachineData[] v = new MachineData[g];
		for (int i = 0; i < g; i++) {
			v[i] = stack.pop();
			sp--;
		}
		Vector V = new Vector(v);
		stack.push(V);
		sp++;
	}

	public void mkfunval(int label) {
		MachineData[] av = new MachineData[0];
		Vector ap = new Vector(av);
		Vector gp = (Vector)stack.pop();

		Function fun = new Function(label, ap, gp);
		stack.push(fun);
	}

	public void mark(int label) {
		stack.push(gp);
		stack.push(new Raw(fp));
		stack.push(new Raw(label));
		sp += 3;
		fp = sp;
	}

	/* apply returns the label to which to jump */
	public int apply() {
		Function h = (Function)stack.pop();
		Vector a = h.ap;
		int n = a.n;
		for (int i = 0; i < n; i++) {
			stack.push(a.v[i]);
		}
		// args pushed, function popped
		sp = sp + n - 1;
		gp = h.gp;
		return h.cp;
	}

	/* contrary to what MaMa did, targ takes a label to jump to, in case
	 * that the function had to few arguments.
	 */
	public int targ(int k, int label) {
		if (sp - fp < k) {
			mkvec0();
			wrap(label);
			return popenv();
		}
		else {
			// TODO: find a better 'invalid' value
			return -1;
		}
	}

	/* pops elements until FP and creates a Vector */
	public void mkvec0() {
		int n = sp - fp;
		MachineData[] a = new MachineData[n];
		sp = fp + 1;
		// put them into the array in *reverse* order
		for (int i = n - 1; i >= 0; i--) {
			a[i] = stack.pop();
		}
		stack.push(new Vector(a));
	}

	public void wrap(int label) {
		stack.push(new Function(label, (Vector)stack.pop(), gp));
	}

	/* returns the label to which to jump */
	public int popenv() {
		gp = (Vector)stack.get(fp - 2);
		int label = ((Base)stack.get(fp)).v;
		stack.set(fp - 2, stack.pop());
		stack.pop();
		sp -= 2;
		fp--;
		return label;
	}

	/* yeah, return is taken and Java does not support `return` syntax */
	public int return_(int k) {
		if (sp - fp - 1 <= k) {
			return popenv();
		} else {
			slide(k);
			return apply();
		}
	}

	public void alloc(int n) {
		for (int i = 0; i < n; i++) {
			stack.push(new Closure(-1, null));
			sp++;
		}
	}

	public void rewrite(int j) {
		MachineData value = stack.pop();
		stack.set(sp - j, value);
		sp--;
	}

	/* eval needs to supply a label that is right *after* the call of eval */
	public int eval(int label) {
		try {
			Closure c = (Closure)stack.peek();
			mark0(label);
			pushloc(3);
			return apply0();
		}
		catch (ClassCastException e) {
			return -1;
		}
	}

	public void mark0(int label) {
		stack.push(gp);
		stack.push(new Base(fp));
		stack.push(new Base(label));
		sp += 3;
		fp = sp;
	}

	public int apply0() {
		Closure top = (Closure)stack.pop();
		gp = top.gp;
		sp--;
		return top.cp;
	}

	public void mkclos(int label) {
		Vector gp = (Vector)stack.pop();
		stack.push(new Closure(label, gp));
	}

	public int update() {
		int ret = popenv();
		rewrite(1);
		return ret;
	}

	public void copyglob() {
		stack.push(gp);
		sp++;
	}

	public void getvec(int k) {
		Vector h = (Vector)stack.pop();
		sp--;
		assert (h.n == k);

		for (int i = 0; i < k; i++) {
			stack.push(h.v[i]);
			sp++;
		}
	}

	public void _pstack() {
		for (MachineData item: stack) {
			System.out.println(item);
		}
	}

	public static void main(String[] args) {
		Machine m = new Machine();
		m.loadc(42);
		m.loadc(23);
		m.add();
		m._pstack();
	}
}

/*
 * For how to implement jumps, see
 * http://snippets.dzone.com/posts/show/3812
 */
