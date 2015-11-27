public class SpaceAge {

    private final long seconds;

    public SpaceAge(int seconds) {
        this((long)seconds);
    }

    public SpaceAge(long seconds) {
        this.seconds = seconds;
    }

    private static final long EarthYear = 31557600L;

    private double age(double orbitalPeriod) {
      return roundOnSecondDecimal(seconds / orbitalPeriod / EarthYear);
    }

    private double roundOnSecondDecimal(double d) {
        return Math.round(d * 100d) / 100d;
    }

    public long getSeconds() { return seconds; }

    public double onEarth() { return age(1); }

    public double onMercury() { return age(0.2408467); }

    public double onVenus() { return age(0.61519726); }

    public double onMars() { return age(1.8808158); }

    public double onJupiter() { return age(11.862615); }

    public double onSaturn() { return age(29.447498); }

    public double onUranus() { return age(84.016846); }

    public double onNeptune() { return age(164.79132); }
}
