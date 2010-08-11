package runtime
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

	//def copyClasses

	//def injectCode
  
  def main(args: Array[String]): Unit = {
    val filename = "program.jar"
		val jar = createJar(filename)
		addManifest(jar)
		jar close
  }

}
