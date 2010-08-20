
package entry

object Util {

	object PrettyPrinter {
		def print[T <: PrettyPrintable](sep: String, items: T*): String =
			items.map(_.prettyPrint).mkString("(", ")"+sep+"(", ")")
	}

	trait PrettyPrintable {
		def prettyPrint: String
		final def pp = prettyPrint
	}

	final class PrettyFormatString(str: String) {
		def pformat(args: PrettyPrintable*) = str.format(args.map(_.prettyPrint): _*)
	}

	implicit def prettyFormatString(str: String) = new PrettyFormatString(str.replace("%", "%s"))

	implicit def any2PrettyPrintable(obj: Any) = obj match {
		case p: PrettyPrintable => p
		case _ => new PrettyPrintable { override def prettyPrint = obj.toString }
	}

	trait SList[Base, L <: Base] { self: L => }

	trait SCons[Base, L <: Base] extends SList[Base, L] { self: L =>
		val head: Base
		val tail: Base
	}

	trait SNil[Base, L <: Base] extends SList[Base, L] { self: L => }

	implicit def unCons[Base, L <: Base](b: Base): List[Base] = b match {
		case cons: SCons[Base, L] => cons.head :: unCons(cons.tail)
		case nil: SNil[_, _] => List()
		case rest => List(rest)
	}

	def fromSeq[S, T](nil: => T, cons: (S, T) => T) = { (s: Seq[S]) =>
		s.foldRight(nil)(cons)
	}

}
