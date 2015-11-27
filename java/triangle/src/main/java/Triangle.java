import java.util.stream.Stream;

public class Triangle {

    private final double side1;
    private final double side2;
    private final double side3;

    public Triangle(double side1, double side2, double side3) throws TriangleException {
        if (!isLogical(side1, side2, side3)) throw new TriangleException();

        this.side1 = side1;
        this.side2 = side2;
        this.side3 = side3;
    }

    public TriangleKind getKind() throws TriangleException {
        switch (equalSides()) {
            case 3:
                return TriangleKind.EQUILATERAL;
            case 2:
                return TriangleKind.ISOSCELES;
            default:
                return TriangleKind.SCALENE;
        }
    }

    private int equalSides() {
        return 4 - (int)Stream.of(side1, side2, side3).distinct().count();
    }

    private static boolean isLogical(double side1, double side2, double side3) {
        boolean sidesMustBeGreaterThanZero = side1 > 0 && side2 > 0 && side3 > 0;
        boolean sumOfTwoSidesMustBeGreaterThatThirdSide =
                side1 + side2 > side3 && side2 + side3 > side1 && side1 + side3 > side2;

        return sidesMustBeGreaterThanZero && sumOfTwoSidesMustBeGreaterThatThirdSide;
    }
}
