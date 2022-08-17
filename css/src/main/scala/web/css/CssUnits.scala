package web.css

sealed trait Num
sealed trait Px
sealed trait Percent
sealed trait Em
sealed trait Ex
sealed trait Ch
sealed trait Rem
sealed trait Lh
sealed trait Vw
sealed trait Vh
sealed trait Vmin
sealed trait Vmax
sealed trait Fr
sealed trait Sec
sealed trait Ms
sealed trait Deg

sealed trait UnitSuffix[Q] {
  def unitSuffix: String

  override def toString: String = unitSuffix
}

object UnitSuffix {
  @inline def apply[Q](implicit ev: UnitSuffix[Q]): UnitSuffix[Q] = ev

  def instance[Q](suffix: String): UnitSuffix[Q] = new UnitSuffix[Q] {
    def unitSuffix: String = suffix
  }

  implicit val numSuffix: UnitSuffix[Num]         = instance[Num]("")
  implicit val pxSuffix: UnitSuffix[Px]           = instance[Px]("px")
  implicit val percentSuffix: UnitSuffix[Percent] = instance[Percent]("%")
  implicit val emSuffix: UnitSuffix[Em]           = instance[Em]("em")
  implicit val exSuffix: UnitSuffix[Ex]           = instance[Ex]("ex")
  implicit val chSuffix: UnitSuffix[Ch]           = instance[Ch]("ch")
  implicit val remSuffix: UnitSuffix[Rem]         = instance[Rem]("rem")
  implicit val lhSuffix: UnitSuffix[Lh]           = instance[Lh]("lh")
  implicit val vwSuffix: UnitSuffix[Vw]           = instance[Vw]("vw")
  implicit val vhSuffix: UnitSuffix[Vh]           = instance[Vh]("vh")
  implicit val vminSuffix: UnitSuffix[Vmin]       = instance[Vmin]("vmin")
  implicit val vmaxSuffix: UnitSuffix[Vmax]       = instance[Vmax]("vmax")
  implicit val frSuffix: UnitSuffix[Fr]           = instance[Fr]("fr")
  implicit val secSuffix: UnitSuffix[Sec]         = instance[Sec]("s")
  implicit val msSuffix: UnitSuffix[Ms]           = instance[Ms]("ms")
  implicit val degSuffix: UnitSuffix[Deg]         = instance[Deg]("deg")
}

trait RelativeUnit[Q]

object RelativeUnit {

  private[this] val instance: RelativeUnit[Any] = new RelativeUnit[Any] {}

  implicit val relativePercentage: RelativeUnit[Percent] = instance.asInstanceOf[RelativeUnit[Percent]]
}
