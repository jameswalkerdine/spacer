package com.jameswalkerdine.spacer

import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode

import scala.scalajs.js
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.HTMLImageElement

case class Position(x: Int, y: Int)

case class Monster(pos: Position)

sealed trait Direction

case object Right extends Direction

case object Left extends Direction

case object Up extends Direction

case object Down extends Direction

sealed trait InvaderShape

case object Closed extends InvaderShape {
  def reverse = Open
}

case object Open extends InvaderShape {
  def reverse = Closed
}

abstract class PeriodicallyMoveable() {
  def pos: Position

  def direction: Direction

  def speed: Int
}

//case class Missile(pos: Position, direction: Direction, speed: Int, fired: Long = System.currentTimeMillis()) extends PeriodicallyMoveable {
//}

trait Moves {
  def pos: Position

  def direction: Direction

  def speed: Int // movement in pixels per second
  def image: Image
}

case class Movable(
                    pos: Position,
                    direction: Direction,

                    fired: Long = System.currentTimeMillis(),
                    image: Image,
                    speed: Int
                  ) extends Moves {

  def move(): Movable = {
    direction match {
      case Up => this.copy(pos = Position(pos.x, pos.y + speed))
      case Down => this.copy(pos = Position(pos.x, pos.y - speed))
      case Left => this.copy(pos = Position(pos.x + speed, pos.y))
      case Right => this.copy(pos = Position(pos.x - speed, pos.y))
    }
  }
}

object Invader {
  def apply(pos: Position,
            direction: Direction,
            shape: InvaderShape,
            fired: Long = System.currentTimeMillis(),
            image: Image,
            speed: Int): Invader = {
    new Invader(pos,
      direction,
      shape,
      fired,
      image: Image,
      speed: Int)
  }
}

class Invader(
               override val pos: Position,
               override val direction: Direction,
               val shape: InvaderShape,
               override val fired: Long = System.currentTimeMillis(),
               override val image: Image,
               override val speed: Int
             ) extends Movable(pos, direction, fired, image, speed) {


  def copy2(pos: Position = pos,
            direction: Direction = direction,
            shape: InvaderShape = shape,
            fired: Long = fired,
            image: Image = image,
            speed: Int = speed): Invader = {

    Invader(pos, direction, shape, fired, image, speed)
  }
}


class Image(src: String) {
  private var ready: Boolean = false
  val element = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
  element.onload = (e: dom.Event) => ready = true
  element.src = src

  def isReady: Boolean = ready
}

object SimpleCanvasGame {

  def main(args: Array[String]): Unit = {
    initGame
  }

  //  def isValidPosition(pos: Position, canvas: Canvas): Boolean = {
  //    0 <= pos.x && (pos.x + Movable.size) <= canvas.width && 0 <= pos.y && (pos.y + Movable.size) <= canvas.height
  //  }

  def areTouching(m1: Movable, m2: Movable): Boolean = {
    m1.pos.x <= (m2.pos.x + m2.image.element.width) && m2.pos.x <= (m1.pos.x + m1.image.element.width) && m1.pos.y <= (m2.pos.y + m2.image.element.height) && m2.pos.y <= (m1.pos.y + m1.image.element.height)
  }

  def initGame(): Unit = {
    // Create the canvas
    val canvas = dom.document.createElement("canvas").asInstanceOf[Canvas]
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    canvas.width = (0.80 * dom.window.innerWidth).toInt
    canvas.height = (0.80 * dom.window.innerHeight).toInt
    dom.document.body.appendChild(canvas)

    val bgImage = new Image("spacer/images/nasa.jpg")
    val closedInvader = new Image("spacer/images/closed_si.png")
    val openInvader = new Image("spacer/images/open_si.png")
    val gunTurretImage = new Image("spacer/images/gun_turret.png")
    val missileImage = new Image("spacer/images/missile.png")

    //missileImage.element.width


    var missiles = Vector[Movable]()
    var invaderMissiles = Seq[Movable]()

    var spaceInvaders = {
      for {colunms <- 1 to 10
           rows <- 1 to 5
           xPosition = colunms * 65
           yPosition = rows * 50
      } yield Invader(speed = 1, pos = Position(xPosition, yPosition), shape = Closed, direction = Left, image = closedInvader)
    }
    var gunTurret = Movable(pos = Position((canvas.width * 0.1).toInt, (canvas.height * .90).toInt), direction = Left, image = gunTurretImage, speed = 1)


    var mainInvasionPartyDirection: Direction = Right

    var score = 0
    var lives = 3

    // Handle keyboard controls
    import scala.collection.mutable.HashMap
    val keysDown = HashMap[Int, Boolean]()

    dom.window.addEventListener("keydown", (e: dom.KeyboardEvent) => {
      keysDown += e.keyCode -> true
    }, false)

    dom.window.addEventListener("keyup", (e: dom.KeyboardEvent) => {
      keysDown -= e.keyCode
    }, false)

    // Reset the game when the player catches a monster
    def reset() = {
      //spaceInvader = spaceInvader.copy(pos = Position(canvas.width / 2, canvas.height / 2))

    }

    def gameOver(): Unit = {
      // Score
      ctx.fillStyle = "rgb(250, 250, 250)"
      ctx.font = "52px Helvetica"
      ctx.textAlign = "centre"
      ctx.textBaseline = "centre"
      ctx.fillText("Game Over, play again (y)", 400, 300)
    }

    def isGameOver: Boolean = if(lives <= 0) true else false

    // Update game objects
    def update(modifier: Double) {
      val newSpaceInvaders = spaceInvaders.map(spaceInvader => {
        val modif = (spaceInvader.speed * modifier).toInt
        var Position(x, y) = spaceInvader.pos
        if (keysDown.contains(KeyCode.Left)) x -= modif
        if (keysDown.contains(KeyCode.Right)) x += modif
        if (keysDown.contains(KeyCode.Up)) y -= modif
        if (keysDown.contains(KeyCode.Down)) y += modif
        val newPos = Position(x, y)
        spaceInvader.copy2(pos = newPos)
      })
      spaceInvaders = newSpaceInvaders


    }

    def maybeChangeDirection: Int = {
      mainInvasionPartyDirection match {
        case Right if spaceInvaders.maxBy(spaceInvader => spaceInvader.pos.x).pos.x > canvas.width - 100 => {
          mainInvasionPartyDirection = Left
          30
        }
        case Left if spaceInvaders.minBy(spaceInvader => spaceInvader.pos.x).pos.x < 0 => {
          mainInvasionPartyDirection = Right
          30
        }
        case _ => 0
      }
    }


    def adjustInvadersSpeeds() = {


      // 1 for 50 to 10 for 1
      var newSpeed = 51 - (spaceInvaders.size /2)

      val ni = for {
        spaceInvader <- spaceInvaders


      } yield (spaceInvader.copy2(speed = newSpeed))
      spaceInvaders = ni
    }


    def periodicInvasionForceMove(downMove: Int) = {
      val newSpaceInvaders = spaceInvaders.map(spaceInvader => {
        var Position(x, y) = spaceInvader.pos

        mainInvasionPartyDirection match {
          case Right => x += spaceInvader.speed
          case Left => x -= spaceInvader.speed
        }
        y += downMove
        //        if (mainInvasionPartyDirection == Right.contains(KeyCode.Left)) x -= modif


        //        if (keysDown.contains(KeyCode.Up)) y -= modif
        //        if (keysDown.contains(KeyCode.Down)) y += modif

        val newPos = Position(x, y)
        //if (isValidPosition(newPos, canvas)) {
        //  spaceInvader = spaceInvader.copy(pos = newPos)
        //}
        spaceInvader.copy2(pos = newPos, shape = if (spaceInvader.shape == Closed) Open else Closed)
      }
      )
      spaceInvaders = newSpaceInvaders
    }


    def moveGunTurret() = {
      val gunTurretNewPos = if (keysDown.contains(KeyCode.Right) && gunTurret.pos.x < (canvas.width - gunTurret.image.element.width)) gunTurret.copy(pos = Position(gunTurret.pos.x + 1, gunTurret.pos.y)) else if (keysDown.contains(KeyCode.Left) && gunTurret.pos.x > 0) gunTurret.copy(pos = Position(gunTurret.pos.x - 1, gunTurret.pos.y)) else
        gunTurret
      gunTurret = gunTurretNewPos
    }


    def checkMissiles() = {
      val toRemove = for {
        spaceInvader <- spaceInvaders
        missile <- missiles
        if (areTouching(missile, spaceInvader))
      } yield (spaceInvader, missile)


      val toRemoveE = for {
        missile <- missiles
        if (missile.pos.y < 10)
      } yield missile


      val ir = toRemove.map(_._1).toSet
      val mr = toRemove.map(_._2).toSet


      score = score + (ir.size * 100)
      //spaceInvaders = spaceInvaders.filter(ir.contains(_))
      //missiles = missiles.filter(mr.contains(_))

      val itokeep = for {
        si <- spaceInvaders
        if (!ir.contains(si))
      } yield si


      spaceInvaders = itokeep
      adjustInvadersSpeeds


      val mtokeep = for {
        m <- missiles
        if (!mr.contains(m) && (!toRemoveE.contains(m)))
      } yield m

      missiles = mtokeep

    }


    def checkInvaderMissiles() = {
      val toRemove = for {
        missile <- invaderMissiles
        if (areTouching(missile, gunTurret))
      } yield gunTurret

      if (toRemove.size > 0) {
        // oops you die
        lives = lives - 1
        if (lives <= 0) {
          gameOver()
        }
        gunTurret = Movable(speed = 1, pos = Position((canvas.width * 0.1).toInt, (canvas.height * .90).toInt), direction = Left, image = gunTurretImage)

      }


    }


    def continueGame(): Unit = {
      missiles = Vector[Movable]()
      invaderMissiles = Seq[Movable]()

      spaceInvaders = {
        for {colunms <- 1 to 10
             rows <- 1 to 5
             xPosition = colunms * 65
             yPosition = rows * 50
        } yield Invader(pos = Position(xPosition, yPosition), shape = Closed, direction = Left, speed = 2, image = closedInvader)
      }
    }

    def fireMissle() = {


      if (keysDown.contains(KeyCode.Space)) {
        //let gun cool down
        val now = System.currentTimeMillis()
        //val tm = missiles.foldLeft(1001L)((r, c) => if (now - c.fired < r) now - c.fired else r)
        // if (tm > 200) {
        if (missiles.size < 4) {
          val missile = Movable(Position(gunTurret.pos.x + 37, gunTurret.pos.y - 25), image = missileImage, direction = Up, speed = -3)
          missiles = missiles :+ missile
        }
      }
    }


    def fireInvaderMissiles(existingMissiles: Seq[Movable], invaders: Seq[Movable]): Seq[Movable] = {
      val rnd = new scala.util.Random
      // find lowest invaders
      val lowest = invaders.foldLeft(0)((low: Int, invader: Movable) => if (invader.pos.y > low) invader.pos.y else low)

      val newMissiles = for {
        invader <- invaders
       // if (invader.pos.y == lowest && rnd.nextInt(100) > ( 60 - (40 - invaders.size)))
        if (invader.pos.y == lowest && rnd.nextInt(100) > ( 94 - (40 - invaders.size)))
      } yield Movable(Position(invader.pos.x + 37, invader.pos.y + 25), image = missileImage, direction = Down, speed = -2)

      existingMissiles ++ newMissiles
    }

    // Draw everything
    def render() {
      if (bgImage.isReady) {
        ctx.drawImage(bgImage.element, 0, 0, canvas.width, canvas.height)
      }
      if (closedInvader.isReady && openInvader.isReady) {
        spaceInvaders.foreach(spaceInvader =>
          ctx.drawImage(spaceInvader.shape match { case Closed => closedInvader.element case Open => openInvader.element }, spaceInvader.pos.x, spaceInvader.pos.y))
      }
      if (gunTurretImage.isReady) {
        ctx.drawImage(gunTurretImage.element, gunTurret.pos.x, gunTurret.pos.y)
      }

      if (missileImage.isReady) missiles.foreach(missile => ctx.drawImage(missileImage.element, missile.pos.x, missile.pos.y))

      if (missileImage.isReady) invaderMissiles.foreach(missile => ctx.drawImage(missileImage.element, missile.pos.x, missile.pos.y))


      // Score
      ctx.fillStyle = "rgb(250, 250, 250)"
      ctx.font = "24px Helvetica"
      ctx.textAlign = "left"
      ctx.textBaseline = "top"
      ctx.fillText(s"Score: $score lives: $lives", 32, 32)

      if( isGameOver) {
        gameOver()
      }
    }

    var prev = js.Date.now()
    var lastMainForceMove = js.Date.now()
    var lastMissileMove = js.Date.now()


    // The main game loop
    val gameLoop = () => {
      val now = js.Date.now()
      if (!isGameOver) {
        if (now - lastMainForceMove > 200) {
          val downMove = maybeChangeDirection
          periodicInvasionForceMove(downMove)
          lastMainForceMove = now
          invaderMissiles = fireInvaderMissiles(invaderMissiles, invaders = spaceInvaders)
        }


        checkMissiles()
        checkInvaderMissiles()
        if (spaceInvaders.size == 0) {
          continueGame()
          render()
        }


        if (now - lastMissileMove > 5) {
          missiles = missiles.map(m => m.move())
          invaderMissiles = invaderMissiles.map(_.move())
          lastMissileMove = now

        }

        //update(delta / 1000)
        //spaceInvaders.map(spaceInvader => spaceInvader.copy(shape = spaceInvader.shape.reverse))
        moveGunTurret()
        fireMissle()
      } else {
        if (keysDown.contains(KeyCode.Y)) {
          continueGame()
          lives = 3
          render()
        }
      }



      render()

      prev = now

    }

    reset()

    dom.window.setInterval(gameLoop, 1) // Execute as fast as possible


  }


}