package web.css

import cats.data.NonEmptySeq
import enumeratum._
// import org.scalajs.dom.{console, window}
// import web.base.react.hooks._
// import web.theme._

import scala.annotation.unused

import scalajs.js

/**
  * A partition of screen width that is type-safe and amenable to exhaustiveness checks.
  */
trait MediaPartition[P] {
  val partitionName: String

  val values: IndexedSeq[P]

  /** Convert a function from part, to an ordered map from part. */
  implicit def profileMap[A](f: P => A)(implicit mp: P => MediaProfile): NonEmptySeq[(MediaProfile, A)] = {
    val xs   = values.map(p => mp(p) -> f(p))
    val vals = xs.map(_._2)

    if (vals.distinct.length == 1)
      NonEmptySeq.one(MediaProfile.all -> vals(0))
    else
      NonEmptySeq.fromSeq(xs).getOrElse {
        throw new js.JavaScriptException("Inconceivable! MediaPartitions must have at least one profile.")
      }
  }

  implicit def profileMapSeq[A](f: P => A)(implicit mp: P => MediaProfile): Seq[(MediaProfile, A)] =
    profileMap(f)(mp).toSeq

  // /** Get the current part of partition P as a react state. */
  // def usePart()(implicit mq: P => MediaProfile): P = {
  //   def currentPart() = {
  //     val w     = window.innerWidth
  //     val parts = values.map(v => mq(v) -> v)
  //     val part  = parts
  //       .collectFirst {
  //         case (p, v) if p.encompassesWidth(w.px) => v
  //       }
  //       .getOrElse {
  //         console.error("Inconceivable! MediaPartitions should cover all window widths.")
  //         console.error("Failing partition:", partitionName)
  //         console.error("window.innerWidth:", w)
  //         console.error("Using first part as fallback:", parts.head._2)
  //         parts.head._2
  //       }
  //     part
  //   }
  //   val (part, setPart) = useState(currentPart())
  //   val handler         = useCallbackStable(
  //     (_: Any) => {
  //       setPart(currentPart())
  //     },
  //     List(setPart)
  //   )
  //   useCancellableLayoutEffectStable(
  //     () => {
  //       window.addEventListener("resize", handler)
  //       () => window.removeEventListener("resize", handler)
  //     },
  //     List(handler)
  //   )
  //   part
  // }
}

object MediaPartition {

  /** A screen width partition equivalent to the media query 'all'. */
  sealed trait ByThePowerOfGrayskull

  case object ByThePowerOfGrayskull extends ByThePowerOfGrayskull with MediaPartition[ByThePowerOfGrayskull] {
    val partitionName = "ByThePowerOfGrayskull"

    val values = IndexedSeq(ByThePowerOfGrayskull)

    implicit def mediaProfile(@unused p: ByThePowerOfGrayskull): MediaProfile = MediaProfile.all
  }

  /** A screen width partition with three parts. */
  sealed trait ByDevice extends EnumEntry {

    def fold[A](phone: A, tablet: A, desktop: A): A = this match {
      case ByDevice.Phone   => phone
      case ByDevice.Tablet  => tablet
      case ByDevice.Desktop => desktop
    }
  }

  object ByDevice extends Enum[ByDevice] with MediaPartition[ByDevice] {
    val partitionName = "ByDevice"
    val values        = findValues
    case object Phone   extends ByDevice
    case object Tablet  extends ByDevice
    case object Desktop extends ByDevice

    def fold[A](phone: A, tablet: A, desktop: A): ByDevice => A = _.fold(phone, tablet, desktop)

    def foldSeq[A](phone: A, tablet: A, desktop: A): Seq[(ByDevice, A)]        =
      values.map(fold(Phone -> phone, Tablet -> tablet, Desktop -> desktop))

    // implicit def mediaProfile(p: ByDevice)(implicit t: HasMedia): MediaProfile =
    //   p match {
    //     case Phone   => t.media.phone
    //     case Tablet  => t.media.tablet
    //     case Desktop => t.media.desktop
    //   }
  }

  sealed trait ByColumn extends EnumEntry {

    def fold[A](one: A, two: A): A = this match {
      case ByColumn.One => one
      case ByColumn.Two => two
    }
  }

  object ByColumn extends Enum[ByColumn] with MediaPartition[ByColumn] {
    val partitionName = "ByColumn"
    val values        = findValues
    case object One extends ByColumn
    case object Two extends ByColumn

    def fold[A](one: A, two: A): ByColumn => A = _.fold(one, two)

    def foldSeq[A](one: A, two: A): Seq[(ByColumn, A)] = values.map(fold(One -> one, Two -> two))

    // implicit def mediaProfile(p: ByColumn)(implicit t: HasMedia): MediaProfile =
    //   p match {
    //     case One => t.media.oneColumn
    //     case Two => t.media.twoColumns
    //   }
  }
}
