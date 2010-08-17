
package tests

import scala.collection.mutable.ListBuffer

object Tests {

	def main(args: Array[String]) {
		ParserTests.run
	}

}

class Tests {

	final case class Ignored(message: String)

	sealed trait Result[+Val, +Err] {
		def toEither = this match {
			case Success(value) => Left(value)
			case Failure(err) => Right(err)
		}
	}

	final case class Success[+Val](value: Val) extends Result[Val, Nothing]
	final case class Failure[+Err](err: Err) extends Result[Nothing, Err]

	object FailOnExceptionTest {
		trait Error[+Err]
		final case class ExceptionThrown[+Err](ex: Exception) extends Error[Err]
		final case class OtherError[+Err](err: Err) extends Error[Err] {
			override def toString = err.toString
		}
	}

	object FailWithTest {
		trait Error[+Val, +Err]
		final case class NoError[+Val](value: Val) extends Error[Val, Nothing]
		final case class WrongError[+Err](err: Err) extends Error[Nothing, Err]
	}

	object ShouldSufficeTest {
		trait Error[+Val, +Err]
		final case class WrongValue[+Val](wrong: Val) extends Error[Val, Nothing]
		final case class OtherError[+Err](err: Err) extends Error[Nothing, Err] {
			override def toString = err.toString
		}
	}

	object AllTest {
		trait Error[+SelfErr, +Val, +Err]
		case class SelfError[+SelfErr](err: SelfErr) extends Error[SelfErr, Nothing, Nothing]
		case class TesterError[+Val, +Err](results: List[Result[Val, Err]]) extends Error[Nothing, Val, Err]
	}

	abstract class Test[+Val, +Err] { self =>
		def run: Result[Val, Err]

		final def failOnException = new Test[Val, FailOnExceptionTest.Error[Err]] {
			import FailOnExceptionTest._
			override def run = {
				try {
					self.run match {
						case s @ Success(value) => s
						case Failure(err) => Failure(OtherError(err))
					}
				}
				catch {
					case ex: Exception => Failure(ExceptionThrown(ex))
				}
			}
		}

		final def shouldFail = new Test[Err, Val] {
			def run = self.run match {
				case Success(value) => Failure(value)
				case Failure(err) => Success(err)
			}
		}

		final def shouldFailWith(f: Err => Boolean) = new Test[Err, FailWithTest.Error[Val, Err]] {
			import FailWithTest._
			def run = self.run match {
				case Success(value) =>
					Failure(NoError(value))
				case Failure(err) =>
					if (f(err))
						Success(err)
					else
						Failure(WrongError(err))
			}
		}

		final def shouldThrow[X <: Exception](implicit ev: ClassManifest[X]) = {
			val catching = failOnException
			catching.shouldFailWith {
				case FailOnExceptionTest.ExceptionThrown(ex) if ev.erasure.isInstance(ex) => true
				case _ => false
			}
		}

		final def shouldSuffice(f: Val => Boolean) = new Test[Val, ShouldSufficeTest.Error[Val, Err]] {
			import ShouldSufficeTest._
			override def run = self.run match {
				case s @ Success(result) =>
					if (f(result)) s
					else           Failure(WrongValue(result))
				case Failure(err) => Failure(OtherError(err))
			}
		}

		final def shouldBe[V >: Val](value: V) = shouldSuffice(value.equals) // looks way better than 'value =='

		final def couldBe[V](value: V)(implicit ev: <:<[Val, Option[V]]) = shouldSuffice({ (v: Val) =>
			ev(v) match {
				case None | Some(`value`) => true
				case _ => false
			}
		})

		final def mapValue[V](vf: Val => V) = new Test[V, Err] {
			override def run = self.run match {
				case Success(result) => Success(vf(result))
				case f @ Failure(err) => f
			}
		}

		final def mapError[E](ef: Err => E) = new Test[Val, E] {
			override def run = self.run match {
				case s @ Success(result) => s
				case Failure(err) => Failure(ef(err))
			}
		}

		final def map[V, E](value: Val => V = { (v: Val) => v }, error: Err => E = { (e: Err) => e }) =
			self mapValue value mapError error

		final def all[V, E](tester: Val => Test[V, E]*) = new Test[List[V], AllTest.Error[Err, V, E]] {
			import AllTest._
			override def run = self.run match {
				case Success(result) =>
					val results = tester map { t => t(result).run } toList;
					if (results.forall(_.isInstanceOf[Success[_]]))
						Success(results map { case res => res.toEither.left.get })
					else
						Failure(TesterError(results))
				case Failure(err) => Failure(SelfError(err))
			}
		}

		final def neverFail = new Test[Result[Val, Err], Nothing] {
			override def run = Success(self.run)
		}

		final def ignore(message: String = self.toString) = alwaysIgnore(message)

	}

	def alwaysIgnore(message: String) = new Test[Ignored, Nothing] {
		override def run = Success(Ignored(message))
	}

	def alwaysFail[E](err: => E) = new Test[Nothing, E] {
		def run = Failure(err)
	}

	def alwaysThrow(exc: => Exception) = new Test {
		def run = throw exc
	}

	def alwaysSucceed[V](value: => V) = new Test[V, Nothing] {
		def run = Success(value)
	}

	def calculate[V](expr: => V) = alwaysSucceed(expr)

	private val tests = ListBuffer[Test[_, _]]()

	protected final def test(t: Test[_, _]) { tests += t }

	final def run {
		for (t <- tests) {
			try {
				t.run match {
					case Success(Ignored(message)) => println("IGNORED " + message)
					case Success(value) => println("SUCCESS " + value)
					case Failure(err) => println("FAILURE " + err)
				}
			}
			catch {
				case ex => println("FAILURE uncaught exception " + ex)
			}
		}
	}

}
