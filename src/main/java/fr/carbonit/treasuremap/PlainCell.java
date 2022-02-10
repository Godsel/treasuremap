package fr.carbonit.treasuremap;


public class PlainCell extends MapCell {
    public PlainCell(Integer xCoordinate, Integer yCoordinate) {
        super(xCoordinate, yCoordinate);
    }

    public String toString() {
        return "P - " + xCoordinate + " - " + yCoordinate;
    }
}