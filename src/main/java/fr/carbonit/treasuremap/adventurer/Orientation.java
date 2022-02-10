package fr.carbonit.treasuremap.adventurer;

import java.awt.*;

public enum Orientation {
    N {
        @Override
        Orientation getNewOrientation(String rotation) {
            if (rotation.equals("G")) {
                return Orientation.O;
            }
            return Orientation.E;
        }

        @Override
        Point getNewCoordinates(Point point, Point maxPoint) {
            Point newPoint = new Point(point.x, point.y - 1);
            if (newPoint.y >= 0) {
                return newPoint;
            }
            return point;
        }
    },
    S {
        @Override
        Orientation getNewOrientation(String rotation) {
            if (rotation.equals("G")) {
                return Orientation.E;
            }
            return Orientation.O;
        }

        @Override
        Point getNewCoordinates(Point point, Point maxPoint) {
            Point newPoint = new Point(point.x, point.y + 1);
            if (newPoint.y < maxPoint.y) {
                return newPoint;
            }
            return point;
        }
    },
    O {
        @Override
        Orientation getNewOrientation(String rotation) {
            if (rotation.equals("G")) {
                return Orientation.S;
            }
            return Orientation.N;
        }

        @Override
        Point getNewCoordinates(Point point, Point maxPoint) {
            Point newPoint = new Point(point.x - 1, point.y);
            if (newPoint.x >= 0) {
                return newPoint;
            }
            return point;
        }
    },
    E {
        @Override
        Orientation getNewOrientation(String rotation) {
            if (rotation.equals("G")) {
                return Orientation.N;
            }
            return Orientation.S;
        }

        @Override
        Point getNewCoordinates(Point point, Point maxPoint) {
            Point newPoint = new Point(point.x + 1, point.y);
            if (newPoint.x < maxPoint.x) {
                return newPoint;
            }
            return point;
        }
    };

    abstract Orientation getNewOrientation(String rotation);

    abstract Point getNewCoordinates(Point point, Point maxPoint);
}
