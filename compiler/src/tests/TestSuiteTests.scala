
package tests

import java.lang.reflect.InvocationTargetException

class TestSuiteTests extends TestSuite {

	import TestSuite._

	test(alwaysThrow(new IllegalArgumentException).shouldThrow[IllegalArgumentException])
	test(alwaysThrow(new IllegalArgumentException).shouldThrow[InvocationTargetException].shouldFail)

	test(alwaysFail(5).shouldFail.shouldBe(5))
	test(alwaysFail(5).shouldFail.shouldBe(4).shouldFail)
	test(alwaysFail(5).shouldFail.shouldBe(4).ignore().shouldSuffice(_.isInstanceOf[Ignored]).mapValue(_ => ()))

	test(alwaysSucceed[Option[String]](None).couldBe("foo"))
	test(alwaysSucceed[Option[String]](Some("foo")).couldBe("foo"))
	test(alwaysSucceed[Option[String]](Some("foo")).couldBe(new Object()).shouldFail)

	test(alwaysSucceed(1).all(
		{ (n: Int) => alwaysSucceed(n+1) },
		{ (n: Int) => alwaysFail(n+1) }
	).shouldFail.shouldBe(AllTest.TesterError(List(Success(2), Failure(2)))))

	test(alwaysSucceed(1).all(
		{ (n: Int) => alwaysSucceed(n+1) },
		{ (n: Int) => alwaysSucceed(n+2) }
	).shouldBe(List(2, 3)))

	test(assertEquals("expected", throw new Exception).failOnException.shouldFail)
	test(assertEquals("expected", "expected"))

}
