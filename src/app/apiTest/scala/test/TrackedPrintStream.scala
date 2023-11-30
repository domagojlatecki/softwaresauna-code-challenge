package test

import java.io.{ ByteArrayOutputStream, PrintStream }
import java.nio.charset.StandardCharsets

class TrackedPrintStream private (private val output: ByteArrayOutputStream) extends PrintStream(output) {

  def this() = this(new ByteArrayOutputStream())

  def capturedOutput: String = new String(output.toByteArray, StandardCharsets.UTF_8)
}
