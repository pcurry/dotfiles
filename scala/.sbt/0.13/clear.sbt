// Clear SBT console with `clear`
// See http://underscore.io/blog/posts/2015/11/09/sbt-commands.html
def clearConsoleCommand = Command.command("clear") { state =>
  val cr = new jline.console.ConsoleReader()
  cr.clearScreen
  state
}

commands += clearConsoleCommand
