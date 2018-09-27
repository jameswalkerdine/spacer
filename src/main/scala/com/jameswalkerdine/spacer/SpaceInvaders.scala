package com.jameswalkerdine.spacer

import com.jameswalkerdine.spacer
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode

import scala.scalajs.js
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.HTMLImageElement

import scala.concurrent.Future

case class Position(x: Int, y: Int)

case class Monster(pos: Position)

sealed trait Direction
case object Right extends Direction
case object Left extends Direction
case object Up extends Direction
case object Down extends Direction

sealed trait InvaderShape
case object Closed extends InvaderShape { def reverse = Open}
case object Open extends InvaderShape { def reverse = Closed}

abstract class PeriodicallyMoveable()
{
  def pos:Position
  def direction: Direction
  def speed: Int
}

//case class Missile(pos: Position, direction: Direction, speed: Int, fired: Long = System.currentTimeMillis()) extends PeriodicallyMoveable {
//}



case class Movable(
                 pos: Position,
                 direction: Direction,
                 shape: InvaderShape,
                 speed: Int = 256, // movement in pixels per second
                 fired: Long = System.currentTimeMillis(),
                 image: Image
               ) extends PeriodicallyMoveable {

  def move() : Movable = {
    direction match {
      case Up => this.copy(pos = Position(pos.x, pos.y + speed))
      case Down => this.copy(pos = Position(pos.x, pos.y - speed))
      case Left => this.copy(pos = Position(pos.x + speed, pos.y))
      case Right => this.copy(pos = Position(pos.x - speed, pos.y))
    }
  }
}






case class GunTurret(
                    pos : Position
                    )

object Movable {
  val size = 20
}

class Image(src: String) {
  private var ready: Boolean = false
  val element = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
  element.onload = (e: dom.Event) => ready = true
  element.src = src
  def isReady: Boolean = ready
}

object SimpleCanvasGame{

  def main(args: Array[String]): Unit = {
    initGame
  }

  def isValidPosition(pos: Position, canvas: Canvas): Boolean = {
    0 <= pos.x && (pos.x + Movable.size) <= canvas.width && 0 <= pos.y && (pos.y + Movable.size) <= canvas.height
  }

  def areTouching(posA: Position, posB: Position): Boolean = {
    posA.x <= (posB.x + Movable.size) && posB.x <= (posA.x + Movable.size) && posA.y <= (posB.y + Movable.size) && posB.y <= (posA.y + Movable.size)
  }

  def initGame(): Unit = {
    // Create the canvas
    val canvas = dom.document.createElement("canvas").asInstanceOf[Canvas]
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    canvas.width = (0.80 * dom.window.innerWidth).toInt
    canvas.height = (0.80 * dom.window.innerHeight).toInt
    dom.document.body.appendChild(canvas)

    val bgImage = new Image("spacer/images/background.png")
    val closedInvader = new Image("spacer/images/closed_si.png")
    val openInvader = new Image("spacer/images/open_si.png")
    val gunTurretImage = new Image("spacer/images/gun_turret.png")
    val missileImage = new Image("spacer/images/missile.png")


    var missiles = Vector[Movable]()

    var spaceInvaders = {
      for{ colunms <- 1 to 10
           rows <- 1 to 5
          xPosition = colunms * 65
          yPosition = rows * 50
      } yield Movable(pos = Position(xPosition, yPosition),shape = Closed, direction = Left, speed = 1, image = closedInvader)
    }
    var gunTurret = GunTurret(Position((canvas.width * 0.1).toInt, (canvas.height *.90).toInt))
    var mainInvasionPartyDirection : Direction = Right

    var score = 0

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

    // Update game objects
    def update(modifier: Double) {
      val newSpaceInvaders = spaceInvaders.map( spaceInvader => {
        val modif = (spaceInvader.speed * modifier).toInt
        var Position(x, y) = spaceInvader.pos
        if (keysDown.contains(KeyCode.Left)) x -= modif
        if (keysDown.contains(KeyCode.Right)) x += modif
        if (keysDown.contains(KeyCode.Up)) y -= modif
        if (keysDown.contains(KeyCode.Down)) y += modif
        val newPos = Position(x, y)
        spaceInvader.copy(pos = newPos)
      })
      spaceInvaders = newSpaceInvaders



    }

    def maybeChangeDirection: Int = {
      mainInvasionPartyDirection match {
        case Right if spaceInvaders.maxBy(spaceInvader => spaceInvader.pos.x).pos.x > canvas.width => {
          mainInvasionPartyDirection = Left
          10
        }
        case Left if spaceInvaders.minBy(spaceInvader => spaceInvader.pos.x).pos.x < 0 => {
          mainInvasionPartyDirection = Right
          10
        }
        case _ => 0
      }
    }


    def periodicInvasionForceMove(downMove : Int) = {
      val newSpaceInvaders = spaceInvaders.map( spaceInvader => {
        val modif = (10).toInt
        var Position(x, y) = spaceInvader.pos

        mainInvasionPartyDirection match {
          case Right => x += modif
          case Left => x -= modif
        }
        y += downMove
        //        if (mainInvasionPartyDirection == Right.contains(KeyCode.Left)) x -= modif




        //        if (keysDown.contains(KeyCode.Up)) y -= modif
        //        if (keysDown.contains(KeyCode.Down)) y += modif

        val newPos = Position(x, y)
        //if (isValidPosition(newPos, canvas)) {
        //  spaceInvader = spaceInvader.copy(pos = newPos)
        //}
        spaceInvader.copy(pos = newPos, shape = if (spaceInvader.shape == Closed) Open else Closed)
      }
        )
      spaceInvaders = newSpaceInvaders
    }


    def moveGunTurret()  = {
      val gunTurretNewPos = if (keysDown.contains(KeyCode.Right)) gunTurret.copy(pos = Position(gunTurret.pos.x + 1, gunTurret.pos.y)) else
      if (keysDown.contains(KeyCode.Left)) gunTurret.copy(pos = Position(gunTurret.pos.x -1 , gunTurret.pos.y)) else
        gunTurret
      gunTurret = gunTurretNewPos
    }



    def checkMissiles() = {
      val toRemove = for {
        spaceInvader <- spaceInvaders
        missile <- missiles
        if(areTouching(missile.pos, spaceInvader.pos))
      }  yield (spaceInvader, missile)

      val ir = toRemove.map(_._1).toSet
      val mr = toRemove.map(_._2).toSet

      //spaceInvaders = spaceInvaders.filter(ir.contains(_))
      //missiles = missiles.filter(mr.contains(_))

      val itokeep = for {
        si <- spaceInvaders
        if( !ir.contains(si))
      } yield si

      spaceInvaders = itokeep


      val mtokeep = for {
        m <- missiles
        if( !mr.contains(m))
      } yield m

      missiles = mtokeep

    }


    def fireMissle() = {


      if(keysDown.contains(KeyCode.Space)) {
        //let gun cool down
        val now = System.currentTimeMillis()
        val tm = missiles.foldLeft(1001L)((r,c) => if(now - c.fired < r) now - c.fired else r)
        if( tm > 200 ) {
          val missile = Movable(Position(gunTurret.pos.x+37, gunTurret.pos.y - 25),shape = Closed, image = missileImage, direction = Up, speed = -1)
          missiles = missiles :+ missile
        }
      }
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

        // Score
      ctx.fillStyle = "rgb(250, 250, 250)"
      ctx.font = "24px Helvetica"
      ctx.textAlign = "left"
      ctx.textBaseline = "top"
      ctx.fillText("Score: " + score, 32, 32)
    }

    var prev = js.Date.now()
    var lastMainForceMove = js.Date.now()
    var lastMissileMove = js.Date.now()


    // The main game loop
    val gameLoop = () => {
      val now = js.Date.now()

      if(now - lastMainForceMove > 200) {
        val downMove = maybeChangeDirection
        periodicInvasionForceMove(downMove)
        lastMainForceMove = now
      }

      if(now - lastMissileMove > 10) {
        missiles = missiles.map(m => m.move())
        lastMissileMove = now
        checkMissiles()
      }

      //update(delta / 1000)
      //spaceInvaders.map(spaceInvader => spaceInvader.copy(shape = spaceInvader.shape.reverse))
      moveGunTurret()
      fireMissle()
      render()

      prev = now

    }

    reset()

    dom.window.setInterval(gameLoop, 1) // Execute as fast as possible
  }



}