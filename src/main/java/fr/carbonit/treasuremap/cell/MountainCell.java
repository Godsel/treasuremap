package fr.carbonit.treasuremap.cell;

public class MountainCell
        extends MapCell {
    public MountainCell(Integer xCoordinate, Integer yCoordinate) {
        super(xCoordinate, yCoordinate);
    }

    public String toString() {
        return "M - " + coordinates.x + " - " + coordinates.y;
    }

    @Override
    public Boolean isMountain() {
        return true;
    }
}
