package fr.carbonit.treasuremap.adventurer;

import java.awt.*;

public enum AdventurerOrientation {
    N {
        @Override
        AdventurerOrientation getNewOrientation(String rotation) {
            if (rotation.equals("G")) {
                return AdventurerOrientation.O;
            }
            return AdventurerOrientation.E;
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
        AdventurerOrientation getNewOrientation(String rotation) {
            if (rotation.equals("G")) {
                return AdventurerOrientation.E;
            }
            return AdventurerOrientation.O;
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
        AdventurerOrientation getNewOrientation(String rotation) {
            if (rotation.equals("G")) {
                return AdventurerOrientation.S;
            }
            return AdventurerOrientation.N;
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
        AdventurerOrientation getNewOrientation(String rotation) {
            if (rotation.equals("G")) {
                return AdventurerOrientation.N;
            }
            return AdventurerOrientation.S;
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

    abstract AdventurerOrientation getNewOrientation(String rotation);

    abstract Point getNewCoordinates(Point point, Point maxPoint);
}
