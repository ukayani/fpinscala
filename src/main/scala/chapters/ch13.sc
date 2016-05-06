import chapters.TailRecMonad._

val reader: ConsoleReader[String] = ConsoleReader(s => Return(s))

val test = reader.flatMap(r => ConsoleReader(s => Return(r + s)))

test.flatMap(r => ConsoleReader(s => Return(r + s))).run("Hello")

