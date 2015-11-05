import SpaceAge._

class SpaceAge(val seconds: Seconds) {

  lazy val onEarth: Age = age(1)

  private def age(orbitalPeriod: OrbitalPeriod): Age = {
    def roundOnSecondDecimal(d: Double): Double = math.round(d * 100d) / 100d

    roundOnSecondDecimal(seconds / orbitalPeriod / EarthYear)
  }

  lazy val onMercury: Age = age(0.2408467)

  lazy val onVenus: Age = age(0.61519726)

  lazy val onMars: Age = age(1.8808158)

  lazy val onJupiter: Age = age(11.862615)

  lazy val onUranus: Age = age(84.016846)

  lazy val onSaturn: Age = age(29.447498)

  lazy val onNeptune: Age = age(164.79132)
}

object SpaceAge {
  type Seconds = Long
  type Age = Double
  type OrbitalPeriod = Double

  def apply(age: Seconds) = new SpaceAge(age)

  val EarthYear: Seconds = 31557600
}