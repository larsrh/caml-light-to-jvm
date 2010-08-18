package entry

import org.github.scopt._

object Entrypoint {
	def main(args: Array[String]) {

		val parser = new OptionParser("scopt") {
			opt("o", "output", "the output file", {v: String => })
			arg("file", "file", {v: String => })
		}

		if (parser.parse(args)) {
			// do stuff
		} else {
			// arguments are bad, usage message will have been displayed
		}
	}
}
