package fr.carbonit.treasuremap.cell;


public class PlainCell extends MapCell {
    public PlainCell(Integer xCoordinate, Integer yCoordinate) {
        super(xCoordinate, yCoordinate);
    }

    public String toString() {
        return "P - " + coords.x + " - " + coords.y;
    }
}