package web.css

import cats.data.NonEmptySeq

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

}
