package runtime
import scala.io.Source
import java.io.FileOutputStream
import java.util.zip.{ZipOutputStream, ZipEntry}

object JarPacker {
	def createJar(fileName: String) = new ZipOutputStream(
		new FileOutputStream(fileName))

	def addManifest(zip: ZipOutputStream) = {
		zip.putNextEntry(new ZipEntry("META-INF/MANIFEST.MF"))
		// i could use "foo" splitlines map strip mkstring, but Java string ops suck
		zip.write("""Manifest-Verion: 1.0
Main-Class: runtime.Machine""" getBytes)
	}

	/* copies all Machine$* stuff that is not to be changed into the JAR */
	def copyClasses(zip: ZipOutputStream) = {
		val classes = List("1", // this is an anonymous class that we need to add
								 "Base", "Closure", "Function", "MachineData", "Raw", "Vector")

		for (entry <- classes) {
			val `class` = Class.forName("runtime.Machine$" + entry)
			val className = `class`.getName.replaceAll("\\.", "/") + ".class"
			val bytecodeStream = `class`.getClassLoader.getResourceAsStream(className)

			val buf = new Array[Byte](1024);
			zip.putNextEntry(new ZipEntry("runtime/Machine$" + entry + ".class"))

			// holy cow, how much I hate these IOStreams!
			// Scala makes it even harder to work with
			var done = false
			while (!done) {
				val len = bytecodeStream.read(buf);
				if (len == -1) {
					done = true;
				} else {
					zip.write(buf, 0, len)
				}
			}
		}
	}

	/* writes Machine.class */
	def injectCode(zip: ZipOutputStream) = {
	}
  
  def main(args: Array[String]): Unit = {
    val filename = args(0)
		val jar = createJar(filename)
		addManifest(jar)
		copyClasses(jar)
		injectCode(jar)
		jar close
  }

}
