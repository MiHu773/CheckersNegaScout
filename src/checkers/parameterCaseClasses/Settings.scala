package checkers.parameterCaseClasses

import checkers.Type.Type
import checkers.{Element, Type}
import checkers.findingNextMove.MoveSet

case class Settings (moveSet: MoveSet,
                     element : Element,

                     other : Type,
                     otherQ : Type,
                     direction : Int,
                     self : Type = Type.empty,
                     selfQ : Type = Type.empty)
{
  val nextRow = element.posX + direction
  val previousRow = element.posX - direction
  val nextColumnL : Int = element.posY - 1
  val nextColumnR : Int = element.posY + 1

  val NR = (direction, 1)
  val NL = (direction, -1)
  val PR = (-direction, 1)
  val PL = (-direction, -1)

  val pozMNL = (nextRow, nextColumnL)
  val pozMNR = (nextRow, nextColumnR)
  val pozMPL = (previousRow, nextColumnL)
  val pozMPR = (previousRow, nextColumnR)

  val pozJmpNL = (nextRow+direction, nextColumnL-1)
  val pozJmpNR = (nextRow+direction, nextColumnR+1)
  val pozJmpPL = (previousRow-direction, nextColumnL-1)
  val pozJmpPR = (previousRow-direction, nextColumnR+1)

  val checkedMNL =  moveSet.check(pozMNL)
  val checkedMNR =  moveSet.check(pozMNR)
  val checkedMPL =  moveSet.check(pozMPL)
  val checkedMPR =  moveSet.check(pozMPR)

  val checkedJmpNL =  moveSet.check(pozJmpNL)
  val checkedJmpNR =  moveSet.check(pozJmpNR)
  val checkedJmpPL =  moveSet.check(pozJmpPL)
  val checkedJmpPR =  moveSet.check(pozJmpPR)
}
 /* val nextRow = element.posX + direction
  val previousRow = element.posX - direction
  val nextColumnL : Int = element.posY - 1
  val nextColumnR : Int = element.posY + 1

  val NR = (direction, 1)
  val NL = (direction, -1)
  val PR = (-direction, 1)
  val PL = (-direction, -1)

  val pozMNL = (nextRow, nextColumnL)
  val pozMNR = (nextRow, nextColumnR)
  val pozMPL = (previousRow, nextColumnL)
  val pozMPR = (previousRow, nextColumnR)

  val pozJmpNL = (nextRow+direction, nextColumnL-1)
  val pozJmpNR = (nextRow+direction, nextColumnR+1)
  val pozJmpPL = (previousRow-direction, nextColumnL-1)
  val pozJmpPR = (previousRow-direction, nextColumnR+1)

  val checkedMNL =  moveSet.check(pozMNL)
  val checkedMNR =  moveSet.check(pozMNR)
  val checkedMPL =  moveSet.check(pozMPL)
  val checkedMPR =  moveSet.check(pozMPR)

  val checkedJmpNL =  moveSet.check(pozJmpNL)
  val checkedJmpNR =  moveSet.check(pozJmpNR)
  val checkedJmpPL =  moveSet.check(pozJmpPL)
  val checkedJmpPR =  moveSet.check(pozJmpPR)*/


