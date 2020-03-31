import scala.collection.mutable

object ControlFunction {
  /**
   * Master's task
   *
   * @param bot Bot
   */
  def forMaster(bot: Bot): Unit = {
    val targetPriority = Array('B', 'P', '?')
    val direction = directionToBestTarget(bot, targetPriority)
    bot.move(direction._1)

    if (bot.energy >= 200 && bot.slaves < 4) {
      tryLaunchMissile(bot)
    }
  }

  /**
   * Finds best direction from given target priority
   *
   * @param bot            Bot
   * @param targetPriority Target priority
   * @return Best direction to target
   */
  def directionToBestTarget(bot: Bot, targetPriority: Array[Char]): (XY, Char) = {
    for (target <- targetPriority) {
      bot.view.offsetToNearest(target) match {
        case Some(offset) =>
          bot.log("[Mini-Bot] Nearest offset found: " + offset)
          bot.view.getPathBFS(offset, bot) match {
            case Some(path) =>
              var pathString = "Found path:\n"
              for (shit <- path) {
                pathString += shit.toString + "\n"
              }

              bot.log(pathString)
              return (path(0), target)
            case None =>
              bot.log("[Mini-Bot] Nearest path not found: ")
          }
        case None =>
          bot.log("[Mini-Bot] Nearest offset not found: ")
      }
    }

    (XY.Zero, '_')
  }

  /**
   * Tries to launch homing missile if enemy master is near by
   *
   * @param bot Bot
   */
  def tryLaunchMissile(bot: Bot): Unit = {
    // Bus prideta po gynimo
  }

  /**
   * Slave's task
   *
   * @param bot Bot
   */
  def forSlave(bot: MiniBot): Unit = {
    val targetPriority = Array('m', 'b')
    val direction = directionToBestTarget(bot, targetPriority)
    bot.move(direction._1)

    // Bus prideta po gynimo
  }
}

case class View(cells: String) {
  val size: Int = math.sqrt(cells.length).toInt
  val center: XY = XY(size / 2, size / 2)

  /**
   * Finds the nearest relative position from the center to the given type cell's position
   *
   * @param c Cell's type
   * @return Offset
   */
  def offsetToNearest(c: Char): Option[XY] = {
    val matchingXY = cells.view.zipWithIndex.filter(_._1 == c)
    if (matchingXY.isEmpty)
      None
    else {
      val nearest = matchingXY.map(p => relPosFromIndex(p._2)).minBy(_.length)
      Some(nearest)
    }
  }

  def apply(relPos: XY): Char = cellAtRelPos(relPos)

  def indexFromAbsPos(absPos: XY): Int = absPos.x + absPos.y * size

  def absPosFromIndex(index: Int): XY = XY(index % size, index / size)

  def absPosFromRelPos(relPos: XY): XY = relPos + center

  def cellAtAbsPos(absPos: XY): Char = cells.apply(indexFromAbsPos(absPos))

  def setCellAtRelPos(relPos: XY, value: Char): View = {
    val index = indexFromRelPos(relPos)
    val newString = cells.substring(0, index) + value + cells.substring(index + 1)
    new View(newString)
  }


  def indexFromRelPos(relPos: XY): Int = indexFromAbsPos(absPosFromRelPos(relPos))

  def relPosFromAbsPos(absPos: XY): XY = absPos - center

  def relPosFromIndex(index: Int): XY = relPosFromAbsPos(absPosFromIndex(index))

  def cellAtRelPos(relPos: XY): Char = cells(indexFromRelPos(relPos))

  /**
   * Finds shortest path to the given relative position (Used BFS algorithm)
   *
   * @param position Relative position
   * @return
   */
  def getPathBFS(position: XY, bot: Bot): Option[Array[XY]] = {
    // Bus prideta po gynimo

    None
  }

  /**
   * Extracts shortest path from the visited cells information
   *
   * @param startPos     Path's start position
   * @param endPos       Path's end position
   * @param visitedCells Visited cells
   * @return Shortest path between start and end positions
   */
  def extractPath(startPos: XY, endPos: XY, visitedCells: Set[(XY, XY)]): Array[XY] = {
    var path = mutable.ArrayBuffer[XY]()
    path += endPos

    var currentPos = endPos
    while (currentPos != startPos) {
      val nextPos = visitedCells.filter(c => c._1.equals(currentPos)).toArray
      currentPos = nextPos(0)._2
      path += currentPos
    }

    path.toArray.dropRight(1).reverse.map(c => relPosFromAbsPos(c))
  }

  /**
   * Founds all available position's neighbours
   *
   * @param position Position
   * @return Position's neighbours
   */
  def getNeighbours(position: XY, bot: Bot): Set[XY] = {
    // Bus prideta po gynimo

    Set.empty
  }
}

case class XY(x: Int, y: Int) {
  override def toString: String = x + ":" + y

  def isNonZero: Boolean = x != 0 || y != 0

  def isZero: Boolean = x == 0 && y == 0

  def isNonNegative: Boolean = x >= 0 && y >= 0

  def updateX(newX: Int): XY = XY(newX, y)

  def updateY(newY: Int): XY = XY(x, newY)

  def addToX(dx: Int): XY = XY(x + dx, y)

  def addToY(dy: Int): XY = XY(x, y + dy)

  def +(pos: XY): XY = XY(x + pos.x, y + pos.y)

  def -(pos: XY): XY = XY(x - pos.x, y - pos.y)

  def *(factor: Double): XY = XY((x * factor).intValue, (y * factor).intValue)

  def distanceTo(pos: XY): Double = (this - pos).length

  def length: Double = math.sqrt(x * x + y * y)

  def signum: XY = XY(x.signum, y.signum)

  def negate: XY = XY(-x, -y)

  def negateX: XY = XY(-x, y)

  def negateY: XY = XY(x, -y)

}

object XY {
  def apply(s: String): XY = {
    val xy = s.split(':').map(_.toInt) // e.g. "-1:1" => Array(-1,1)
    XY(xy(0), xy(1))
  }

  val Zero: XY = XY(0, 0)
  val One: XY = XY(1, 1)

  val Right: XY = XY(1, 0)
  val RightUp: XY = XY(1, -1)
  val Up: XY = XY(0, -1)
  val UpLeft: XY = XY(-1, -1)
  val Left: XY = XY(-1, 0)
  val LeftDown: XY = XY(-1, 1)
  val Down: XY = XY(0, 1)
  val DownRight: XY = XY(1, 1)
}

/**
 * Parses command string to opcode and arguments map tuple
 */
object CommandParser {
  def apply(command: String): (String, Map[String, String]) = {
    def splitParam(param: String) = {
      val segments = param.split('=')
      if (segments.length != 2)
        throw new IllegalStateException("invalid key/value pair: " + param)
      (segments(0), segments(1))
    }

    val segments = command.split('(')
    if (segments.length != 2)
      throw new IllegalStateException("invalid command: " + command)

    val params = segments(1).dropRight(1).split(',')
    val keyValuePairs = params.map(splitParam).toMap
    (segments(0), keyValuePairs)
  }
}

class ControlFunctionFactory {
  def create: String => String = (input: String) => {
    val (opcode, params) = CommandParser(input)
    opcode match {
      case "React" =>
        val bot = BotImpl(params)
        if (bot.generation == 0) {
          ControlFunction.forMaster(bot)
        } else {
          ControlFunction.forSlave(bot)
        }
        bot.toString
      case _ => "" // OK
    }
  }
}

trait Bot {
  // inputs
  def inputOrElse(key: String, fallback: String): String

  def inputAsIntOrElse(key: String, fallback: Int): Int

  def inputAsXYOrElse(keyPrefix: String, fallback: XY): XY

  var view: View

  def energy: Int

  def generation: Int

  def spawnedBots: Int

  def slaveDirection: XY

  def slaves: Int

  def lastPosition: XY

  def isSlave: Boolean

  // outputs
  def move(delta: XY): Bot

  def say(text: String): Bot

  def status(text: String): Bot

  def spawn(offset: XY, params: (String, Any)*): Bot

  def set(params: (String, Any)*): Bot

  def log(text: String): Bot
}

trait MiniBot extends Bot {
  // inputs
  def collision: Option[XY]

  def offsetToMaster: XY

  // outputs
  def explode(blastRadius: Int): Bot
}

case class BotImpl(inputParams: Map[String, String]) extends MiniBot {
  // input
  def inputOrElse(key: String, fallback: String): String = inputParams.getOrElse(key, fallback)

  def inputAsIntOrElse(key: String, fallback: Int): Int = inputParams.get(key).map(_.toInt).getOrElse(fallback)

  def inputAsXYOrElse(key: String, fallback: XY): XY = inputParams.get(key).map(s => XY(s)).getOrElse(fallback)

  var view: View = View(inputParams("view"))
  val energy: Int = inputParams("energy").toInt
  val generation: Int = inputParams("generation").toInt
  var spawnedBots: Int = inputParams.get("spawnedBots") match {
    case Some(number) => number.toInt
    case None => 0
  }

  val isSlave: Boolean = generation > 0

  val slaves: Int = inputParams.get("slaves") match {
    case Some(number) => number.toInt
    case None => 0
  }

  val slaveDirection: XY = inputParams.get("slaveDirection") match {
    case Some(number) => XY(number)
    case None => XY.Zero
  }

  val collision: Option[XY] = inputParams.get("collision") match {
    case Some(col) => Some(XY(col))
    case None => None
  }

  var lastPosition: XY = inputParams.get("lastPosition") match {
    case Some(position) =>
      val lastPos = XY(position)
      val changedView = this.view.setCellAtRelPos(lastPos, 'W')
      this.view = changedView
      lastPos
    case None => XY(-10, -10)
  }

  def offsetToMaster: XY = inputAsXYOrElse("master", XY.Zero)


  // output

  private var stateParams = Map.empty[String, Any] // holds "Set()" commands
  private var commands = "" // holds all other commands
  private var debugOutput = "" // holds all "Log()" output

  /** Appends a new command to the command string; returns 'this' for fluent API. */
  private def append(s: String): Bot = {
    commands += (if (commands.isEmpty) s else "|" + s);
    this
  }

  /** Renders commands and stateParams into a control function return string. */
  override def toString: String = {
    set(("spawnedBots", spawnedBots))
    set(("slaveDirection", slaveDirection))
    set(("lastPosition", lastPosition))

    var result = commands
    if (stateParams.nonEmpty) {
      if (!result.isEmpty) result += "|"
      result += stateParams.map(e => e._1 + "=" + e._2).mkString("Set(", ",", ")")
    }
    if (!debugOutput.isEmpty) {
      if (!result.isEmpty) result += "|"
      result += "Log(text=" + debugOutput + ")"
    }
    result
  }

  def log(text: String): BotImpl = {
    debugOutput += text + "\n";
    this
  }

  def move(direction: XY): Bot = {
    append("Move(direction=" + direction + ")")
    log("Moving Direction: " + direction)
    this.lastPosition = direction * (-1)
    this
  }

  def say(text: String): Bot = append("Say(text=" + text + ")")

  def status(text: String): Bot = append("Status(text=" + text + ")")

  def explode(blastRadius: Int): Bot = append("Explode(size=" + blastRadius + ")")

  def spawn(offset: XY, params: (String, Any)*): Bot = {
    spawnedBots = spawnedBots + 1
    append("Spawn(direction=" + offset +
      (if (params.isEmpty) "" else "," + params.map(e => e._1 + "=" + e._2).mkString(",")) +
      ")")
  }

  def set(params: (String, Any)*): BotImpl = {
    stateParams ++= params;
    this
  }

  def set(keyPrefix: String, xy: XY): BotImpl = {
    stateParams ++= List(keyPrefix + "x" -> xy.x, keyPrefix + "y" -> xy.y);
    this
  }
}



