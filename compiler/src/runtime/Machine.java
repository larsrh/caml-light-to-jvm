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
 *
 * Changes from MaMa:
 *  - targ takes an additional argument, label, which should point at a label
 *    directly proceeding the targ call
 *  - eval takes an additional argument, label, which should point at a label
 *    directly after the eval call
 *  - return is called return_ because that is a Java keyword. Would be possible
 *    in Scala and probably the JVM would support it too.
 *    TODO: determine whether we can just rename the method using bytecode
 *    transformations, low priority though
 *  - apply and targ return integers which represent labels that we need to jump
 *    to
 *  - jump* is not implemented, because jumps are done by setting the _goto
 *    marker and calling continue in the outer code
 *  - targ and eval return -1 if they are meant to do nothing
 */

package runtime;

import java.util.Stack;

public class Machine {
	private int fp;
	private int sp;
	private Vector gp;

	abstract class MachineData {}

    private class Base extends MachineData {
		private int v;

		private Base(int constant) {
			v = constant;
		}

		public String toString() {
			return "B(" + v + ")";
		}
	}

	/* when stuff is not basic - we still need a wrapper type for that */
	// future optimization: remove all raw types from the machine
	private class Raw extends MachineData {
		private int v;

		private Raw(int constant) {
			v = constant;
		}
		
		private Raw(boolean constant) {
			v = constant ? 1 : 0;
		}
		
		/* maybe raise some exception if v != 0 and v != 1*/
		private boolean asBool() {
			 return v != 0;
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
			String pre = "V(" + n + ", [";
			for (int i = 0; i < v.length; i++) {
				pre += ", " + v[i];
			}
			pre += "])";
			return pre;
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
			return "F(" + cp + ", " + ap + ", " + gp + ")";
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
			return "C(" + cp + ", " + gp + ")";
		}
	}
	
	private class List extends MachineData {
		private boolean empty;
		private MachineData head;
		private MachineData tail;
		
		private List() {
				this.empty = true;
		}
		
		private List(MachineData head, MachineData tail) {
				this.empty = false;
				this.head = head;
				this.tail = tail;
		}
			
		public String toString() {
			return "L(" + (empty ? "Nil" : "Cons") + "," + head + "," + tail+ ")";
		}
	}

	private final Stack<MachineData> stack;

	public Machine() {
		this.stack = new Stack<MachineData>();
		// yeah, this is ugly, admittedly
		this.fp = -1;
		this.sp = -1;
	}
	
	public void add() {
		Raw r1 = (Raw)stack.pop();
		Raw r2 = (Raw)stack.pop();
		stack.push(new Raw(r1.v + r2.v));
		sp--;
	}
	
	public void alloc(int n) {
		for (int i = 0; i < n; i++) {
			stack.push(new Closure(-1, null));
			sp++;
		}
	}
	
	public void and() {
		Raw r1 = (Raw)stack.pop();
		Raw r2 = (Raw)stack.pop();
		stack.push(new Raw(r1.asBool() && r2.asBool()));
		sp--;
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
	
	public int apply0() {
		Closure top = (Closure)stack.pop();
		gp = top.gp;
		sp--;
		return top.cp;
	}
	
	public void cons() {
			MachineData tail = stack.pop();
			MachineData head = stack.pop();
			stack.push(new List(head,tail));
			sp--;
	}		
	
	public void copyglob() {
		stack.push(gp);
		sp++;
	}
	
	public void div() {
		Raw r1 = (Raw)stack.pop();
		Raw r2 = (Raw)stack.pop();
		stack.push(new Raw(r1.v / r2.v));
		sp--;
	}
	
	public void eq() {
		Raw r1 = (Raw)stack.pop();
		Raw r2 = (Raw)stack.pop();
		if (r1.v == r2.v) {
			// push true
			stack.push(new Raw(1));
		} else {
			stack.push(new Raw(0));
		}
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
	
	public void geq() {
		Raw r1 = (Raw)stack.pop();
		Raw r2 = (Raw)stack.pop();
		if (r1.v >= r2.v) {
			// push true
			stack.push(new Raw(1));
		} else {
			stack.push(new Raw(0));
		}
		sp--;
	}
	
	public void get(int j) {
		Vector h = (Vector)stack.pop();
		stack.push(h.v[j]);
	}
	
	// thows a ClassCastException when the code is wrong, which is more
	// or less what we want. Me wants pattern match in this Java!
	public void getbasic() {
		Base b = (Base)stack.pop();
		stack.push(new Raw(b.v));
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
	
	public void gr() {
		Raw r1 = (Raw)stack.pop();
		Raw r2 = (Raw)stack.pop();
		if (r1.v > r2.v) {
			// push true
			stack.push(new Raw(1));
		} else {
			stack.push(new Raw(0));
		}
		sp--;
	}

	public void halt() {
			throw new RuntimeException("Pattern match failure... unlucky you!");
	}
	
	public void le() {
		Raw r1 = (Raw)stack.pop();
		Raw r2 = (Raw)stack.pop();
		if (r1.v < r2.v) {
			// push true
			stack.push(new Raw(1));
		} else {
			stack.push(new Raw(0));
		}
		sp--;
	}	
	
	public void leq() {
		Raw r1 = (Raw)stack.pop();
		Raw r2 = (Raw)stack.pop();
		if (r1.v <= r2.v) {
			// push true
			stack.push(new Raw(1));
		} else {
			stack.push(new Raw(0));
		}
		sp--;
	}	

	public void loadc(int constant) {
		this.stack.push(new Raw(constant));
		this.sp++;
	}
	
	public void mark(int label) {
		stack.push(gp);
		stack.push(new Raw(fp));
		stack.push(new Raw(label));
		sp += 3;
		fp = sp;
	}
	
	public void mark0(int label) {
		stack.push(gp);
		stack.push(new Raw(fp));
		stack.push(new Raw(label));
		sp += 3;
		fp = sp;
	}

	public void mkbasic() {
		Raw r = (Raw)stack.pop();
		stack.push(new Base(r.v));
	}
	
	public void mkclos(int label) {
		Vector gp = (Vector)stack.pop();
		stack.push(new Closure(label, gp));
	}

	public void mkfunval(int label) {
		MachineData[] av = new MachineData[0];
		Vector ap = new Vector(av);
		Vector gp = (Vector)stack.pop();

		Function fun = new Function(label, ap, gp);
		stack.push(fun);
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

	public void mul() {
		Raw r1 = (Raw)stack.pop();
		Raw r2 = (Raw)stack.pop();
		stack.push(new Raw(r1.v * r2.v));
		sp--;
	}
	
	public void neg() {
		Raw r = (Raw)stack.pop();
		stack.push(new Raw(-r.v));
		sp--;
	}
	
	public void neq() {
		Raw r1 = (Raw)stack.pop();
		Raw r2 = (Raw)stack.pop();
		if (r1.v != r2.v) {
			// push true
			stack.push(new Raw(1));
		} else {
			stack.push(new Raw(0));
		}
		sp--;
	}	
	
	public void nil() {
			stack.push(new List());
			sp++;
	}
	
	public void not() {
		Raw r = (Raw)stack.pop();
		stack.push(new Raw(!r.asBool()));
		sp--;
	}
	
	public void or() {
		Raw r1 = (Raw)stack.pop();
		Raw r2 = (Raw)stack.pop();
		stack.push(new Raw(r1.asBool() || r2.asBool()));
		sp--;
	}
	
	public void pop() {
		stack.pop();
		sp--;
	}
	
	/* returns the label to which to jump */
	public int popenv() {
		gp = (Vector)stack.get(fp - 2);
		int label = ((Raw)stack.get(fp)).v;
		stack.set(fp - 2, stack.peek());

		int prevfp = ((Raw)stack.get(fp - 1)).v;

		for (int i = sp; i > fp - 2; i--) {
			stack.pop();
		}

		sp = fp - 2;
		fp = prevfp;
		return label;
	}
	
	/*
	 * additional deineMama instruction, needed to get a number from the machine
	 * stack into the JVM stack
	 */
	public int popraw() {
		Raw r = (Raw)stack.pop();
		sp--;
		return r.v;
	}


	public void pushglob(int j) {
		stack.push(gp.v[j]);
		sp++;
	}
	
	public void pushloc(int depth) {
		stack.push(stack.get(sp - depth));
		sp++;
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

	public void rewrite(int j) {
		MachineData value = stack.pop();
		stack.set(sp - j, value);
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

	/* contrary to what MaMa did, targ takes a label to jump to, in case
	 * that the function had to few arguments.
	 */
	public int targ(int k, int label) {
		if (sp - fp < k) {
			mkvec0();
			wrap(label);
			return popenv();/* returns the label to which to jump */
		}
		else {
			return -1;
		}
	}
	
	public int tlist(int label) {
			List l = (List)stack.pop();
			if(l.empty) {
					sp--;
					return -1;
			} else {
					stack.push(l.head);
					stack.push(l.tail);
					sp++;
					return label;
			}
	}

	public int update() {
		int ret = popenv();
		rewrite(1);
		return ret;
	}

	public void wrap(int label) {
		stack.push(new Function(label, (Vector)stack.pop(), gp));
	}
	
	public void _pstack() {
		System.out.println("---- TOP OF STACK ----");
		for (int i = stack.size(); i > 0; i--) {
			System.out.println(stack.get(i-1));
		}
		System.out.println("---- BTM OF STACK ----");
	}

	/* this method is only for demonstration purposes, the proper code
	 * will be generated.
	 */
	public static void main_(String[] args) {
		Machine m = new Machine();
		m.loadc(19);
		m.mkbasic();
		m.pushloc(0);
		m.getbasic();
		m.pushloc(1);
		m.getbasic();
		m.mul();
		m.mkbasic();
		m.pushloc(1);
		m.getbasic();
		m.pushloc(1);
		m.getbasic();
		m.add();
		m.mkbasic();
		m.slide(1);
		m.slide(1);
		m.getbasic();

		m._pstack();
	}

	/* version of a main that uses switch to simulate goto */
	public static void main_switch(String[] args) {
		Machine m = new Machine();

		int _goto = 0;
		boolean terminate = false;

		while(!terminate) {
			switch(_goto)
			{
				case 0:
					System.out.println("Init");
				case 1:
					m.loadc(97);
					m.loadc(97);
					m.eq();
					if (m.popraw() == 0) {
						_goto = 2;
						continue;
					}
					m.loadc(97);
					_goto = 3;
					continue;
				case 2:
					m.loadc(0);
				case 3:
					terminate = true;
			}
		}

		m._pstack();
	}

	public static int minus1() {
		return -1;
	}

	public static void main_comp(String[] args) {
		int candidate = minus1();
		if (candidate != -1) {
			System.out.println("Jumping to candidate");
		}
		System.out.println("Ending");
	}
}

/*
 * For how to implement jumps, see
 * http://snippets.dzone.com/posts/show/3812
 */
